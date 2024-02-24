namespace GeoPredictor

module Predictor =

    let getGeologyPredictions bodyType (volcanism:Volcanism) =
        match bodyType with
        | MetalRichBody -> 
            match volcanism.Type with
            | IronMagma -> [ SulphurDioxideFumaroleSignal; SulphurDioxideGasVentSignal ]
            | SilicateMagma -> [ SulphurDioxideFumaroleSignal; SulphurDioxideGasVentSignal ]
            | SilicateVapourGeysers -> [ SilicateVapourFumaroleSignal; SilicateVapourGasVentSignal ]
            | _ -> [ UnexpectedVolcanismType volcanism.Type ]
        | HighMetalContentBody ->
            match volcanism.Type with
            | IronMagma -> [ SulphurDioxideFumaroleSignal; SulphurDioxideGasVentSignal; IronMagmaLavaSpoutSignal ]
            | SilicateMagma -> [ SulphurDioxideFumaroleSignal; SulphurDioxideGasVentSignal; SilicateMagmaLavaSpoutSignal ]
            | SilicateVapourGeysers -> [ SilicateVapourFumaroleSignal; SilicateVapourGasVentSignal; SilicateMagmaLavaSpoutSignal ]
            | _ -> [ UnexpectedVolcanismType volcanism.Type ]
        | RockyBody ->
            match (volcanism.Level, volcanism.Type) with
            | _, IronMagma -> [ SulphurDioxideFumaroleSignal; SulphurDioxideGasVentSignal; IronMagmaLavaSpoutSignal ]
            | _, SilicateVapourGeysers -> [ SilicateVapourFumaroleSignal; SilicateVapourGasVentSignal; SilicateMagmaLavaSpoutSignal ]
            | Minor, SilicateMagma -> [ SulphurDioxideFumaroleSignal; SulphurDioxideGasVentSignal; SilicateMagmaLavaSpoutSignal ]
            | Minor, WaterMagma -> [ WaterGeyserSignal; WaterFumaroleSignal; WaterGasVentSignal ]
            | _ -> [ UnexpectedVolcanismType volcanism.Type ]
        | RockyIceBody ->
            match (volcanism.Level, volcanism.Type) with
            | Unspecified, IronMagma -> [ SulphurDioxideIceFumaroleSignal; IronMagmaLavaSpoutSignal ]
            | Minor, IronMagma -> [ SulphurDioxideIceFumaroleSignal; SulphurDioxideFumaroleSignal; SulphurDioxideGasVentSignal; IronMagmaLavaSpoutSignal ]
            | Major, SilicateMagma -> [ SilicateVapourIceFumaroleSignal; SulphurDioxideGasVentSignal; SilicateMagmaLavaSpoutSignal ]
            | Unspecified, SilicateMagma -> [ SilicateVapourIceFumaroleSignal; SilicateMagmaLavaSpoutSignal; SulphurDioxideGasVentSignal ]
            | Minor, SilicateMagma -> [ SilicateVapourIceFumaroleSignal; SulphurDioxideGasVentSignal ]
            | Major, WaterMagma -> [ WaterIceGeyserSignal; WaterIceFumaroleSignal; WaterGasVentSignal ]
            | Unspecified, WaterMagma -> [ WaterIceFumaroleSignal ]
            | Minor, WaterMagma -> [ WaterIceFumaroleSignal ]
            | Major, SilicateVapourGeysers -> [ SilicateVapourIceFumaroleSignal; SilicateVapourGasVentSignal; SilicateMagmaLavaSpoutSignal ]
            | Minor, SilicateVapourGeysers -> [ SilicateVapourIceFumaroleSignal; SilicateVapourGasVentSignal; SilicateMagmaLavaSpoutSignal ]
            | Major, WaterGeysers -> [ WaterIceGeyserSignal; WaterIceFumaroleSignal; WaterGasVentSignal ]
            | Unspecified, WaterGeysers -> [ WaterIceFumaroleSignal; WaterGasVentSignal; WaterIceGeyserSignal ]
            | Minor, WaterGeysers -> [ WaterIceFumaroleSignal; WaterFumaroleSignal; WaterGasVentSignal ]
            | Minor, CarbonDioxideGeysers -> [ CarbonDioxideIceGeyserSignal; CarbonDioxideIceFumaroleSignal; CarbonDioxideFumaroleSignal; CarbonDioxideGasVentSignal ]
            | _ -> [ UnexpectedVolcanismType volcanism.Type ]
        | IcyBody ->
            match (volcanism.Level, volcanism.Type) with
            | _, WaterGeysers -> [ WaterIceGeyserSignal; WaterIceFumaroleSignal ]
            | _, WaterMagma -> [ WaterIceGeyserSignal; WaterIceFumaroleSignal ]
            | _, CarbonDioxideGeysers -> [ CarbonDioxideIceGeyserSignal; CarbonDioxideIceFumaroleSignal ]
            | Minor, AmmoniaMagma -> [ AmmoniaIceGeyserSignal; AmmoniaIceFumaroleSignal ]
            | Minor, NitrogenMagma -> [ NitrogenIceGeyserSignal; NitrogenIceFumaroleSignal ]
            | Minor, MethaneMagma -> [ MethaneIceGeyserSignal; MethaneIceFumaroleSignal ]
            | _ -> [ UnexpectedVolcanismType volcanism.Type ]
        | NonLandable bt ->
            [ UnexpectedVolcanismType (UnknownVolcanism $"Nonlandable {bt}!? How did you get here?") ]
        | BodyTypeNotYetSet ->
            [ UnexpectedVolcanismType (UnknownVolcanism "This body type was never set!")]
