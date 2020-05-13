namespace Nabu.Json

open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open System
open System.Reflection
open Nabu.Interop
open Nabu.Memorize

module private Memorized =
    let getUnionTag (t: System.Type) = memorize FSharpValue.PreComputeUnionTagReader t
    let getUnionCases (t: System.Type) = memorize FSharpType.GetUnionCases t
    let getUnionCaseFields = memorize (fun (uci: UnionCaseInfo) -> uci.GetFields ()) 
    let getUnionCaseFieldValues = memorize FSharpValue.PreComputeUnionReader

    let constructUnionCase = memorize FSharpValue.PreComputeUnionConstructor

    let findAttributeOnType<'attr when 'attr :> Attribute> = memorize (fun (t: Type) ->
        let attribute = t.GetCustomAttribute<'attr> ()
        if obj.ReferenceEquals(attribute, null) then None
        else Some attribute)

module private Helpers =
    let findUnionCase = Memorized.getUnionCases >> flip Array.tryFind
    let findUnionCaseByName objType caseName = findUnionCase objType (fun i -> i.Name.Equals caseName)
    let findUnionCaseWithFieldCount objType caseName count =
        findUnionCase objType (fun i ->
            let fields = i.GetFields ()
            i.Name.Equals(caseName) && Array.length fields = count)
    let findUnionCaseWithFieldNames objType caseName names =
        let pred (f: PropertyInfo) n = f.Name = n
        findUnionCase objType (fun i ->
            let caseFields = i.GetFields ()
            i.Name.Equals(caseName) &&
            Array.exists2 pred caseFields names)

    let makeUnionCaseFromJToken serializer (valueTokens: Linq.JToken []) caseInfo  =
        Memorized.getUnionCaseFields caseInfo |>
        Array.zip valueTokens |>
        Array.map (fun (jv, pi) -> jv.ToObject(pi.PropertyType, serializer)) |>
        Memorized.constructUnionCase caseInfo

    let unionCaseTryOfProperties objType serializer caseName (properties: Linq.JProperty []) =
        let fieldNameQuery = properties |> Array.map (fun p -> p.Name)
        let fieldValues = properties |> Array.map (fun p -> p.Value)
        findUnionCaseWithFieldNames objType caseName fieldNameQuery |>
            Option.map (makeUnionCaseFromJToken serializer fieldValues)

module Attributes =
    // Sets the default case to use when reading JSON. 
    type JsonReadDefaultUnionCaseAttribute(caseName: string) =
        inherit Attribute ()
        member _.Case = caseName 

    type JsonUnionCaseSerializeMethod = AsArray | AsObject

    type JsonUnionCaseSerializeMethodAttribute() =
        inherit Attribute()
        member _.Method = JsonUnionCaseSerializeMethod.AsObject
    
module Converters =
    let failreadwith s = raise (JsonReaderException s)
    let failreadwithf format = Printf.ksprintf failreadwith format

    type UnionJsonObject =
    // 1. Case with no fields: "Case1" -> Case1
    | CaseName of string
    // 2. Case with a single field: {"Case2": "value"} -> Case2 of "value"
    // 3. Case with multiple fields as an array: {"Case3": ["value1", true]} -> Case3 of "value1" * true
    | CaseArrayValueObject of string * Linq.JToken []
    // 4. Case with multiple fields as an object: {"Case4": {"field1": "value1", "field2": true}} -> Case4 of field1 = "value1" * field2 = true
    | CaseObjectValueObject of string * Linq.JProperty []
    // 5. A plain JS object, but there exists an annotation on the type defining a default case
    | NormalObject of Linq.JProperty []
    
    let readJToken (jToken: Linq.JToken) =
        // Case 1
        if jToken.Type = Linq.JTokenType.String then
            jToken.ToObject<string> () |> CaseName |> Ok
        elif jToken.Type = Linq.JTokenType.Object then
            let jObject = jToken :?> Linq.JObject
            let jObjectProps = jObject.Properties()

            // Case 5
            if Seq.length jObjectProps <> 1 then
                jObjectProps |> Array.ofSeq |> NormalObject |> Ok
            else
                let caseProperty = Seq.head jObjectProps
                let caseName = caseProperty.Name
                let caseValueToken = caseProperty.Value

                // Case 4
                if caseValueToken.Type = Linq.JTokenType.Object then
                    let caseFieldObject = caseValueToken :?> Linq.JObject
                    let caseFieldProperties = caseFieldObject.Properties () |> Array.ofSeq
                    CaseObjectValueObject (caseName, caseFieldProperties) |> Ok
                // Case 3
                elif caseValueToken.Type = Linq.JTokenType.Array then
                    let caseFieldArray = caseValueToken :?> Linq.JArray
                    let array = Array.empty<Linq.JToken>
                    caseFieldArray.CopyTo(array, 0)
                    CaseArrayValueObject (caseName, array) |> Ok
                // Case 2
                else
                    CaseArrayValueObject (caseName, [|caseValueToken|]) |> Ok
        else
            sprintf "Invalid token %O" jToken |> Error

    type UnionConverter() =
        inherit Newtonsoft.Json.JsonConverter()

        let canConvertMemorized =
            // Is it a union that isn't a list?
            memorize (fun objType -> FSharpType.IsUnion objType && 
                not (objType.GetTypeInfo().IsGenericType && objType.GetGenericTypeDefinition() = typedefof<_ list>))
        
        override _.CanConvert objectType = canConvertMemorized objectType

        override _.ReadJson (reader, objectType, existingValue, serializer) = 
            let jToken = Linq.JToken.Load reader

            // Check that it's not null
            if isNull jToken then
                null
            else
                match readJToken jToken with
                | Ok (CaseName caseName) -> 
                    match Helpers.findUnionCaseWithFieldCount objectType caseName 0 with
                    | Some caseInfo -> Memorized.constructUnionCase caseInfo [||]
                    | None -> failreadwithf "Cannot parse token %O: Case with name %s and no fields does not exist." jToken caseName
                | Ok (CaseArrayValueObject (caseName, values)) ->
                    match Helpers.findUnionCaseByName objectType caseName with
                    | Some caseInfo -> Helpers.makeUnionCaseFromJToken serializer values caseInfo
                    | None -> failreadwithf "Cannot parse token %O: No match for case named %s with fields: %O" jToken caseName values
                | Ok (CaseObjectValueObject (caseName, properties)) ->
                    match Helpers.unionCaseTryOfProperties objectType serializer caseName properties with
                    | Some result -> result
                    | None -> 
                        let fieldNames = properties |> Array.map (fun p -> p.Name)
                        failreadwithf "Cannot parse token %O: No match for case named %s with fields with names: %O" jToken caseName fieldNames
                | Ok (NormalObject properties) ->
                    let defaultCaseName = 
                        match Memorized.findAttributeOnType<Attributes.JsonReadDefaultUnionCaseAttribute> objectType with
                        | Some attr -> attr.Case
                        | None -> failreadwithf "Cannot parse object %O: No default union case available." jToken
                    
                    match Helpers.unionCaseTryOfProperties objectType serializer defaultCaseName properties with
                    | Some result -> result
                    | None -> failreadwithf "No case with name %s found." defaultCaseName
                | Error message -> failreadwith message