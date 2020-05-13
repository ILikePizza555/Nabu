namespace Nabu.Json

open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open System
open System.Reflection
open Nabu.Memorize

module private Memorized =
    let getUnionTag (t: System.Type) = memorize FSharpValue.PreComputeUnionTagReader t
    let getUnionCases (t: System.Type) = memorize FSharpType.GetUnionCases t
    let getUnionCaseFields = memorize (fun (uci: UnionCaseInfo) -> uci.GetFields) 
    let getUnionCaseFieldValues = memorize FSharpValue.PreComputeUnionReader

    let constructUnionCase = memorize FSharpValue.PreComputeUnionConstructor

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
    type UnionConverter() =
        inherit Newtonsoft.Json.JsonConverter()

        let canConvertMemorized =
            // Is it a union that isn't a list?
            memorize (fun objType -> FSharpType.IsUnion objType && 
                not (objType.GetTypeInfo().IsGenericType && objType.GetGenericTypeDefinition() = typedefof<_ list>))
        
        override _.CanConvert objectType = canConvertMemorized objectType

