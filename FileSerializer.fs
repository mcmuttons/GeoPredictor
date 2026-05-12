namespace GeoPredictor

open Observatory.Framework.Interfaces
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

module FileSerializer =
    let serializeOptions = JsonFSharpOptions
                            .Default()
                            .ToJsonSerializerOptions()

    let deserialize<'T> faultValue (json:string) =
        try
            JsonSerializer.Deserialize<'T>(json, serializeOptions)
        with
            | _ -> faultValue

    // Serialize to and from json file
    let fileDeserialize<'T> faultValue path filename =
        let fullPath = path + filename

        if File.Exists(fullPath) then
            deserialize<'T> faultValue (File.ReadAllText(fullPath))
        else
            faultValue

    let fileSerialize (core:IObservatoryCore) filename content  =
        if not core.IsLogMonitorBatchReading then
            let fullPath = core.PluginStorageFolder + filename
            let serialized = JsonSerializer.Serialize(content, serializeOptions)
            try
                File.WriteAllText(fullPath, serialized)
            with
            | _ -> ()
