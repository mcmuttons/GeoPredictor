namespace GeoPredictor

open System.IO
open System.Text.Json
open System.Text.Json.Serialization

module Serializer =
    let serializeOptions =  JsonFSharpOptions
                                .Default()
                                .ToJsonSerializerOptions()
                            |> fun options ->
                                options.PropertyNameCaseInsensitive <- true
                                options

                            

    let deserialize<'T> faultValue (json:string) =
        try
            JsonSerializer.Deserialize<'T>(json, serializeOptions)
        with
            | _ -> faultValue

    // Serialize to and from json file
    let fileDeserialize<'T> faultValue filepath =
        if File.Exists(filepath) then
            deserialize<'T> faultValue (File.ReadAllText(filepath))
        else 
            raise (FileNotFoundException($"File not found: {filepath}"))

    let fileSerialize isBatchRead filepath content  =
        if not isBatchRead then
            let fullPath = filepath
            let serialized = JsonSerializer.Serialize(content, serializeOptions)
            try
                File.WriteAllText(fullPath, serialized)
            with
            | _ -> ()
