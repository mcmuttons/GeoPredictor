namespace GeoPredictor

open FSharp.Data.UnitSystems.SI.UnitSymbols

type VolcanismType =
    | WaterMagma
    | SulphurDioxideMagma
    | AmmoniaMagma
    | MethaneMagma
    | NitrogenMagma
    | SilicateMagma
    | IronMagma
    | WaterGeysers
    | CarbonDioxideGeysers
    | AmmoniaGeysers
    | MethaneGeysers
    | NitrogenGeysers
    | HeliumGeysers
    | SilicateVapourGeysers
    | UnknownVolcanism of string

type VolcanismLevel =
    | Minor
    | Major
    | Unspecified

type BodyType =
    | MetalRichBody
    | HighMetalContentBody
    | RockyBody
    | IcyBody
    | RockyIceBody
    | NonLandable of string

type GeoFeature =
    | IceGeyser
    | IceFumarole
    | Geyser
    | Fumarole
    | GasVent
    | Lava

type GeoType =
    | Water 
    | SulphurDioxide 
    | Ammonia 
    | Methane
    | Nitrogen
    | Silicate
    | Iron
    | CarbonDioxide
    | SilicateVapour 

type Volcanism = { Level:VolcanismLevel; Type:VolcanismType }

module Parser =
    // Utility for easy pipelining
    let replace (original:string) replacement (string:string) =
        string.Replace(original, replacement)

    let split (string:string) =
        string.Split ' '
    
    // Input parsing

    // Parse volcanism
    let private toVolcanismType (volcanism:string) =
        match volcanism with
        | v when v.Contains("water magma") -> WaterMagma
        | v when v.Contains("sulphur dioxide magma") -> SulphurDioxideMagma
        | v when v.Contains("ammonia magma") -> AmmoniaMagma
        | v when v.Contains("methane magma") -> MethaneMagma
        | v when v.Contains("nitrogen magma") -> NitrogenMagma
        | v when v.Contains("silicate magma") -> SilicateMagma
        | v when v.Contains("metallic magma") -> IronMagma
        | v when v.Contains("water geysers") -> WaterGeysers
        | v when v.Contains("carbon dioxide geysers") -> CarbonDioxideGeysers
        | v when v.Contains("ammonia geysers") -> AmmoniaGeysers
        | v when v.Contains("methane geysers") -> MethaneGeysers
        | v when v.Contains("nitrogen geysers") -> NitrogenGeysers
        | v when v.Contains("helium geysers") -> HeliumGeysers
        | v when v.Contains("silicate vapour geysers") -> SilicateVapourGeysers
        | _ -> UnknownVolcanism volcanism

    let private toVolcanismTypeOutput volcanismType =
        match volcanismType with
        | WaterMagma -> "Water Magma"
        | SulphurDioxideMagma -> "Sulphur Dioxide Magma"
        | AmmoniaMagma -> "Ammonia Magma"
        | MethaneMagma -> "Methane Magma"
        | NitrogenMagma -> "Nitrogen Magma"
        | SilicateMagma -> "Silicate Magma"
        | IronMagma -> "Iron Magma"
        | WaterGeysers -> "Water Geysers"
        | CarbonDioxideGeysers -> "Carbon Dioxide Geysers"
        | AmmoniaGeysers -> "Ammonia Geysers"
        | MethaneGeysers -> "Methane Geysers"
        | NitrogenGeysers -> "Nitrogen Geysers"
        | HeliumGeysers -> "Helium Geysers"
        | SilicateVapourGeysers -> "Silicate Vapour Geysers"
        | UnknownVolcanism v -> $"Unknown volcanism {v} (wat?)"

    let private toVolcanismLevel (volcanism:string) =
        match volcanism with
        | v when v.StartsWith("minor") -> Minor
        | v when v.StartsWith("major") -> Major
        | _ -> Unspecified

    let private toVolcanismLevelOutput level =
        match level with
        | Minor -> "Minor "
        | Major -> "Major "
        | Unspecified -> ""

    let private buildVolcanism volcanism =
        { Level = toVolcanismLevel volcanism; Type = toVolcanismType volcanism }
    
    let toVolcanism volcanism =
        volcanism
        |> replace " volcanism" ""
        |> buildVolcanism 

    let toVolcanismOutput volcanism =
        toVolcanismLevelOutput volcanism.Level + toVolcanismTypeOutput volcanism.Type

    // Parse body type
    let toBodyType bodyType =
        match bodyType with
        | "Metal rich body" -> MetalRichBody
        | "High metal content body" -> HighMetalContentBody
        | "Rocky body" -> RockyBody
        | "Icy body" -> IcyBody
        | "Rocky ice body" -> RockyIceBody
        | _ -> NonLandable bodyType

    let toBodyTypeOutput bodyType =
        match bodyType with
        | MetalRichBody -> "Metal Rich"
        | HighMetalContentBody -> "High Metal Content"
        | RockyBody -> "Rocky"
        | IcyBody -> "Icy"
        | RockyIceBody -> "Rocky Ice"
        | NonLandable bt -> $"Nonlandable {bt} (why am I here?)"

    // All possible permutations of the types of geology and volcanisms to match on codex scans. I'm pretty sure a lot of 
    // these combinations are invalid, and that especially some of the magmas don't occur at all on landable bodies, even 
    // though they're theoretically listed as possibles, but this way none will slip through the cracks while checking, 
    // especially in case there's a permutation out there that hasn't been discovered yet.
    let geoTypes = [
        "$Codex_Ent_IceGeysers_WaterMagma_Name;";
        "$Codex_Ent_IceFumarole_WaterMagma_Name;";
        "$Codex_Ent_Geysers_WaterMagma_Name;";
        "$Codex_Ent_Fumarole_WaterMagma_Name;";
        "$Codex_Ent_Gas_Vents_WaterMagma_Name;";

        "$Codex_Ent_IceGeysers_SulphurDioxideMagma_Name;";
        "$Codex_Ent_IceFumarole_SulphurDioxideMagma_Name;";
        "$Codex_Ent_Geysers_SulphurDioxideMagma_Name;";
        "$Codex_Ent_Fumarole_SulphurDioxideMagma_Name;";
        "$Codex_Ent_Gas_Vents_SulphurDioxideMagma_Name;";

        "$Codex_Ent_IceGeysers_AmmoniaMagma_Name;";
        "$Codex_Ent_IceFumarole_AmmoniaMagma_Name;";
        "$Codex_Ent_Geysers_AmmoniaMagma_Name;";
        "$Codex_Ent_Fumarole_AmmoniaMagma_Name;";
        "$Codex_Ent_Gas_Vents_AmmoniaMagma_Name;";

        "$Codex_Ent_IceGeysers_MethaneMagma_Name;";
        "$Codex_Ent_IceFumarole_MethaneMagma_Name;";
        "$Codex_Ent_Geysers_MethaneMagma_Name;";
        "$Codex_Ent_Fumarole_MethaneMagma_Name;";
        "$Codex_Ent_Gas_Vents_MethaneMagma_Name;";

        "$Codex_Ent_IceGeysers_NitrogenMagma_Name;";
        "$Codex_Ent_IceFumarole_NitrogenMagma_Name;";
        "$Codex_Ent_Geysers_NitrogenMagma_Name;";
        "$Codex_Ent_Fumarole_NitrogenMagma_Name;";
        "$Codex_Ent_Gas_Vents_NitrogenMagma_Name;";

        "$Codex_Ent_Lava_Spouts_SilicateMagma_Name;";
        "$Codex_Ent_Lava_Spouts_IronMagma_Name;";

        "$Codex_Ent_IceGeysers_WaterGeysers_Name;";
        "$Codex_Ent_IceFumarole_WaterGeysers_Name;";
        "$Codex_Ent_Geysers_WaterGeysers_Name;";
        "$Codex_Ent_Fumarole_WaterGeysers_Name;";
        "$Codex_Ent_Gas_Vents_WaterGeysers_Name;";
 
        "$Codex_Ent_IceGeysers_CarbonDioxideGeysers_Name;";
        "$Codex_Ent_IceFumarole_CarbonDioxideGeysers_Name;";
        "$Codex_Ent_Geysers_CarbonDioxideGeysers_Name;";
        "$Codex_Ent_Fumarole_CarbonDioxideGeysers_Name;";
        "$Codex_Ent_Gas_Vents_CarbonDioxideGeysers_Name;";

        "$Codex_Ent_IceGeysers_AmmoniaGeysers_Name;";
        "$Codex_Ent_IceFumarole_AmmoniaGeysers_Name;";
        "$Codex_Ent_Geysers_AmmoniaGeysers_Name;";
        "$Codex_Ent_Fumarole_AmmoniaGeysers_Name;";
        "$Codex_Ent_Gas_Vents_AmmoniaGeysers_Name;";

        "$Codex_Ent_IceGeysers_MethaneGeysers_Name;";
        "$Codex_Ent_IceFumarole_MethaneGeysers_Name;";
        "$Codex_Ent_Geysers_MethaneGeysers_Name;";
        "$Codex_Ent_Fumarole_MethaneGeysers_Name;";
        "$Codex_Ent_Gas_Vents_MethaneGeysers_Name;";

        "$Codex_Ent_IceGeysers_NitrogenGeysers_Name;";
        "$Codex_Ent_IceFumarole_NitrogenGeysers_Name;";
        "$Codex_Ent_Geysers_NitrogenGeysers_Name;";
        "$Codex_Ent_Fumarole_NitrogenGeysers_Name;";
        "$Codex_Ent_Gas_Vents_NitrogenGeysers_Name;";

        "$Codex_Ent_IceGeysers_HeliumGeysers_Name;";
        "$Codex_Ent_IceFumarole_HeliumGeysers_Name;";
        "$Codex_Ent_Geysers_HeliumGeysers_Name;";
        "$Codex_Ent_Fumarole_HeliumGeysers_Name;";
        "$Codex_Ent_Gas_Vents_HeliumGeysers_Name;";

        "$Codex_Ent_IceGeysers_SilicateVapourGeysers_Name;";
        "$Codex_Ent_IceFumarole_SilicateVapourGeysers_Name;";
        "$Codex_Ent_Geysers_SilicateVapourGeysers_Name;";
        "$Codex_Ent_Fumarole_SilicateVapourGeysers_Name;";
        "$Codex_Ent_Gas_Vents_SilicateVapourGeysers_Name;"]

