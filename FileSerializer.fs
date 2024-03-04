namespace GeoPredictor

open Observatory.Framework.Interfaces
open System.IO
open System.Text.Json

module FileSerializer =

    // Type specifically for serialization since the JsonSerializer doesn't play well with discriminated unions
    type SerializableCodexData = { Sig:string; Reg:string }

    // Serialize and deserialize the codex unlocks. A little ugly since JsonSerializer isn't a fan of discriminated unions
    let deserializeCodexUnlocks (json:string) =
        JsonSerializer.Deserialize<Set<SerializableCodexData>> json
        |> Set.map (fun cu -> { Signal = Parser.toGeoSignalFromSerialization cu.Sig; Region = Parser.toRegion cu.Reg })

    let serializeCodexUnlocks codexUnlocks =
        let serializableCodexUnlocks =
            codexUnlocks 
                |> Set.map (fun cu -> { Sig = Parser.toGeoSignalOut cu.Signal; Reg = Parser.toRegionOut cu.Region })
                           
        JsonSerializer.Serialize serializableCodexUnlocks 

    // Serialize and deserialize internal settings
    let deserializeInteralSettings (json:string) =
        JsonSerializer.Deserialize<InternalSettings> json

    let serializeInternalSettings settings =
        JsonSerializer.Serialize settings

    // read from and write to file
    let deserializeFromFile faultValue path filename deserializer =
        let fullPath = path + filename
        match File.Exists(fullPath) with
        | true ->
            try
                File.ReadAllText(fullPath) |> deserializer
            with
                | _ -> faultValue
        | false -> 
            faultValue

    let serializeToFile (core:IObservatoryCore) filename serializer codexUnlocks  =
        if not core.IsLogMonitorBatchReading then
            let fullPath = core.PluginStorageFolder + filename
            let serialized = codexUnlocks |> serializer
            try
                File.WriteAllText(fullPath, serialized)
            with
            | _ -> ()
