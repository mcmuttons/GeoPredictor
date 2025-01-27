namespace GeoPredictor

module Predictor =

    let getGeologyPredictions bodyType (volcanism:Volcanism) =
        match bodyType with
        | MetalRichBody -> 
            match volcanism.Type with
            | IronMagma -> [ SulphurDioxideFumaroleSignal; SulphurDioxideGasVentSignal ]
            | SilicateMagma -> [ SulphurDioxideFumaroleSignal; SulphurDioxideGasVentSignal; SilicateVapourGasVentSignal ]
            | SilicateVapourGeysers -> [ SilicateVapourFumaroleSignal; SilicateVapourGasVentSignal ]
            | _ -> [ UnexpectedSignal $"Unexpected volcanism: {Parser.toVolcanismOut volcanism}" ]
        | HighMetalContentBody ->
            match volcanism.Type with
            | IronMagma -> [ SulphurDioxideFumaroleSignal; SulphurDioxideGasVentSignal; IronMagmaLavaSpoutSignal ]
            | SilicateMagma -> [ SulphurDioxideFumaroleSignal; SulphurDioxideGasVentSignal; SilicateMagmaLavaSpoutSignal; SilicateVapourGasVentSignal ]
            | SilicateVapourGeysers -> [ SilicateVapourFumaroleSignal; SilicateVapourGasVentSignal; SilicateMagmaLavaSpoutSignal ]
            | _ -> [ UnexpectedSignal $"Unexpected volcanism: {Parser.toVolcanismOut volcanism}" ]
        | RockyBody ->
            match (volcanism.Level, volcanism.Type) with
            | _, IronMagma -> [ SulphurDioxideFumaroleSignal; SulphurDioxideGasVentSignal; IronMagmaLavaSpoutSignal ]
            | _, SilicateVapourGeysers -> [ SilicateVapourFumaroleSignal; SilicateVapourGasVentSignal; SilicateMagmaLavaSpoutSignal ]
            | Minor, SilicateMagma -> [ SulphurDioxideFumaroleSignal; SulphurDioxideGasVentSignal; SilicateMagmaLavaSpoutSignal ]
            | Minor, WaterMagma -> [ WaterGeyserSignal; WaterFumaroleSignal; WaterGasVentSignal ]
            | _ -> [ UnexpectedSignal $"Unexpected volcanism: {Parser.toVolcanismOut volcanism}" ]
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
            | Minor, WaterGeysers -> [ WaterIceGeyserSignal; WaterIceFumaroleSignal; WaterGasVentSignal ]
            | Minor, CarbonDioxideGeysers -> [ CarbonDioxideIceGeyserSignal; CarbonDioxideIceFumaroleSignal; CarbonDioxideFumaroleSignal; CarbonDioxideGasVentSignal ]
            | _ -> [ UnexpectedSignal $"Unexpected volcanism: {Parser.toVolcanismOut volcanism}" ]
        | IcyBody ->
            match (volcanism.Level, volcanism.Type) with
            | _, WaterGeysers -> [ WaterIceGeyserSignal; WaterIceFumaroleSignal ]
            | _, WaterMagma -> [ WaterIceGeyserSignal; WaterIceFumaroleSignal ]
            | _, CarbonDioxideGeysers -> [ CarbonDioxideIceGeyserSignal; CarbonDioxideIceFumaroleSignal ]
            | Minor, AmmoniaMagma -> [ AmmoniaIceGeyserSignal; AmmoniaIceFumaroleSignal ]
            | Minor, NitrogenMagma -> [ NitrogenIceGeyserSignal; NitrogenIceFumaroleSignal ]
            | Minor, MethaneMagma -> [ MethaneIceGeyserSignal; MethaneIceFumaroleSignal ]
            | _ -> [ UnexpectedSignal $"Unexpected volcanism: {Parser.toVolcanismOut volcanism}" ]
        | NonLandable bt ->
            [ UnexpectedSignal $"Nonlandable {bt}!? How did you get here?" ]
        | BodyTypeNotYetSet ->
            [ UnexpectedSignal "Body type not set!" ]
