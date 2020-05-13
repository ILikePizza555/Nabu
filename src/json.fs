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
    let getUnionCaseFields = memorize (fun (uci: UnionCaseInfo) -> uci.GetFields) 
    let getUnionCaseFieldValues = memorize FSharpValue.PreComputeUnionReader

    let constructUnionCase = memorize FSharpValue.PreComputeUnionConstructor

module private Helpers =
    let findUnionCase = Memorized.getUnionCases >> flip Array.tryFind
    let findUnionCaseByName objType caseName = findUnionCase objType (fun i -> i.Name.Equals caseName)

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

