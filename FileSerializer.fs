namespace GeoPredictor

open Observatory.Framework.Interfaces
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

module FileSerializer =
    let serializeOptions = JsonFSharpOptions
                            .Default()
                            .ToJsonSerializerOptions()

    // Serialize to and from json file
    let deserializeFromFile<'T> faultValue path filename =
        let fullPath = path + filename
        match File.Exists(fullPath) with
        | true ->
            try
                JsonSerializer.Deserialize<'T>(File.ReadAllText(fullPath), serializeOptions)
            with
                | _ -> faultValue
        | false -> 
            faultValue

    let serializeToFile (core:IObservatoryCore) filename content  =
        if not core.IsLogMonitorBatchReading then
            let fullPath = core.PluginStorageFolder + filename
            let serialized = JsonSerializer.Serialize(content, serializeOptions)
            try
                File.WriteAllText(fullPath, serialized)
            with
            | _ -> ()
