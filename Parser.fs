namespace GeoPredictor

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
    | VolcanismNotYetSet

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
    | BodyTypeNotYetSet

type Region =
    | GalacticCentre
    | EmpyreanStraits
    | RykersHope
    | OdinsHold
    | NormaArm
    | ArcadianStream
    | Izanami
    | InnerOrionPerseusConflux
    | InnerScutumCentaurusArm
    | NormaExpanse
    | TrojanBelt
    | TheVeils
    | NewtonsVault
    | TheConduit
    | OuterOrionPerseusConflux
    | OrionCygnusArm
    | Temple
    | InnerOrionSpur
    | HawkingsGap
    | DrymansPoint
    | SagittariusCarinaArm
    | MareSomnia
    | Acheron
    | FormorianFrontier
    | HieronymusDelta
    | OuterScutumCentaurusArm
    | OuterArm
    | AquilasHalo
    | ErrantMarches
    | PerseusArm
    | FormidineRift
    | VulcanGate
    | ElysianShore
    | SanguineousRim
    | OuterOrionSpur
    | AchillessAltar
    | Xibalba
    | LyrasSong
    | Tenebrae
    | TheAbyss
    | KeplersCrest
    | TheVoid
    | UnknownRegion of string

type GeologySignal =
    | WaterIceGeyserSignal
    | WaterIceFumaroleSignal
    | WaterGeyserSignal
    | WaterFumaroleSignal
    | WaterGasVentSignal
    | SulphurDioxideIceFumaroleSignal
    | SulphurDioxideFumaroleSignal
    | SulphurDioxideGasVentSignal
    | SilicateVapourIceFumaroleSignal
    | SilicateVapourFumaroleSignal
    | SilicateVapourGasVentSignal
    | CarbonDioxideIceGeyserSignal
    | CarbonDioxideIceFumaroleSignal
    | CarbonDioxideFumaroleSignal
    | CarbonDioxideGasVentSignal
    | AmmoniaIceGeyserSignal
    | AmmoniaIceFumaroleSignal
    | NitrogenIceGeyserSignal
    | NitrogenIceFumaroleSignal
    | MethaneIceGeyserSignal
    | MethaneIceFumaroleSignal
    | IronMagmaLavaSpoutSignal
    | SilicateMagmaLavaSpoutSignal
    | UnexpectedValue of string

type Volcanism = { Level:VolcanismLevel; Type:VolcanismType }

module Parser =
    // Utility for easy pipelining
    let replace (original:string) replacement (string:string) =
        string.Replace(original, replacement)

    let split (string:string) =
        string.Split ' '

    let isNotNullOrEmpty string =
        match string with
            | null -> false
            | "" -> false
            | _ -> true

    
    // Input parsing

    // Parse volcanism
    let private toVolcanismType (volcanism:string) =
        match volcanism with
        | v when v.Contains("water magma") -> WaterMagma
        | v when v.Contains("sulphur dioxide magma") -> SulphurDioxideMagma
        | v when v.Contains("ammonia magma") -> AmmoniaMagma
        | v when v.Contains("methane magma") -> MethaneMagma
        | v when v.Contains("nitrogen magma") -> NitrogenMagma
        | v when v.Contains("rocky magma") -> SilicateMagma
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
        | UnknownVolcanism v -> $"Unknown volcanism: {v}"
        | VolcanismNotYetSet -> "Volcanism not set"

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

    let toVolcanismNotYetSet =
        { Level = Unspecified; Type = VolcanismNotYetSet }

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
        | HighMetalContentBody -> "HMC"
        | RockyBody -> "Rocky"
        | IcyBody -> "Icy"
        | RockyIceBody -> "Rocky Ice"
        | NonLandable bt -> $"Nonlandable {bt} (why am I here?)"
        | BodyTypeNotYetSet -> "Type not set"

    // Parse Region
    let toRegion region =
        match region with
           | "$Codex_RegionName_1;" | "Galactic Centre" -> GalacticCentre
           | "$Codex_RegionName_2;" | "Empyrean Straits" -> EmpyreanStraits
           | "$Codex_RegionName_3;" | "Ryker's Hope" -> RykersHope
           | "$Codex_RegionName_4;" | "Odin's Hold" -> OdinsHold
           | "$Codex_RegionName_5;" | "Norma Arm" -> NormaArm
           | "$Codex_RegionName_6;" | "Arcadian Stream" -> ArcadianStream
           | "$Codex_RegionName_7;" | "Izanami" -> Izanami
           | "$Codex_RegionName_8;" | "Inner Orion-Perseus Conflux" -> InnerOrionPerseusConflux
           | "$Codex_RegionName_9;" | "Inner Scutum-Centaurus Arm" -> InnerScutumCentaurusArm
           | "$Codex_RegionName_10;" | "Norma Expanse" -> NormaExpanse
           | "$Codex_RegionName_11;" | "Trojan Belt" -> TrojanBelt
           | "$Codex_RegionName_12;" | "The Veils" -> TheVeils
           | "$Codex_RegionName_13;" | "Newton's Vault" -> NewtonsVault
           | "$Codex_RegionName_14;" | "The Conduit" -> TheConduit
           | "$Codex_RegionName_15;" | "Outer Orion-Perseus Conflux" -> OuterOrionPerseusConflux 
           | "$Codex_RegionName_16;" | "Orion-Cygnus Arm" -> OrionCygnusArm
           | "$Codex_RegionName_17;" | "Temple" -> Temple
           | "$Codex_RegionName_18;" | "Inner Orion Spur" -> InnerOrionSpur
           | "$Codex_RegionName_19;" | "Hawking's Gap" -> HawkingsGap
           | "$Codex_RegionName_20;" | "Dryman's Point" -> DrymansPoint
           | "$Codex_RegionName_21;" | "Sagittarius-Carina Arm" -> SagittariusCarinaArm
           | "$Codex_RegionName_22;" | "Mare Somnia" -> MareSomnia
           | "$Codex_RegionName_23;" | "Acheron" -> Acheron
           | "$Codex_RegionName_24;" | "Formorian Frontier" -> FormorianFrontier
           | "$Codex_RegionName_25;" | "Hieronymus Delta" -> HieronymusDelta
           | "$Codex_RegionName_26;" | "Outer Scutum-Centaurus Arm" -> OuterScutumCentaurusArm
           | "$Codex_RegionName_27;" | "Outer Arm" -> OuterArm
           | "$Codex_RegionName_28;" | "Aquila's Halo" -> AquilasHalo
           | "$Codex_RegionName_29;" | "Errant Marches" -> ErrantMarches
           | "$Codex_RegionName_30;" | "Perseus Arm" -> PerseusArm
           | "$Codex_RegionName_31;" | "Formidine Rift" -> FormidineRift
           | "$Codex_RegionName_32;" | "Vulcan Gate" -> VulcanGate
           | "$Codex_RegionName_33;" | "Elysian Shore" -> ElysianShore
           | "$Codex_RegionName_34;" | "Sanguineous Rim" -> SanguineousRim
           | "$Codex_RegionName_35;" | "Outer Orion Spur" -> OuterOrionSpur
           | "$Codex_RegionName_36;" | "Achilles's Altar" -> AchillessAltar
           | "$Codex_RegionName_37;" | "Xibalba" -> Xibalba
           | "$Codex_RegionName_38;" | "Lyra's Song" -> LyrasSong
           | "$Codex_RegionName_39;" | "Tenebrae" -> Tenebrae
           | "$Codex_RegionName_40;" | "The Abyss" -> TheAbyss
           | "$Codex_RegionName_41;" | "Kepler's Crest" -> KeplersCrest
           | "$Codex_RegionName_42;" | "The Void" -> TheVoid
           | _ -> UnknownRegion region

    // Parse geological signal type
    let toGeoSignalOutput signal =
        match signal with
        | WaterIceGeyserSignal -> "Water Ice Geysers"
        | WaterIceFumaroleSignal -> "Water Ice Fumaroles"
        | WaterGeyserSignal -> "Water Geysers"
        | WaterFumaroleSignal -> "Water Fumaroles"
        | WaterGasVentSignal -> "Water Gas Vents"
        | SulphurDioxideIceFumaroleSignal -> "Sulphur Dioxide Ice Fumaroles"
        | SulphurDioxideFumaroleSignal -> "Sulphur Dioxide Fumaroles"
        | SulphurDioxideGasVentSignal -> "Sulphur Dioxide Gas Vents"
        | SilicateVapourIceFumaroleSignal -> "Silicate Vapour Ice Fumaroles"
        | SilicateVapourFumaroleSignal -> "Silicate Vapour Fumaroles"
        | SilicateVapourGasVentSignal -> "Silicate Vapour Gas Vents"
        | CarbonDioxideIceGeyserSignal -> "Carbon Dioxide Ice Geysers"
        | CarbonDioxideIceFumaroleSignal -> "Carbon Dioxide Ice Fumaroles"
        | CarbonDioxideFumaroleSignal -> "Carbon Dioxide Fumaroles"
        | CarbonDioxideGasVentSignal -> "Carbon Dioxide Gas Vents"
        | AmmoniaIceGeyserSignal -> "Ammonia Ice Geysers"
        | AmmoniaIceFumaroleSignal -> "Ammonia Ice Fumaroles"
        | NitrogenIceGeyserSignal -> "Nitrogen Ice Geysers"
        | NitrogenIceFumaroleSignal -> "Nitrogen Ice Fumaroles"
        | MethaneIceGeyserSignal -> "Methane Ice Geysers"
        | MethaneIceFumaroleSignal -> "Methane Ice Fumaroles"
        | IronMagmaLavaSpoutSignal -> "Iron Magma Lava Spouts"
        | SilicateMagmaLavaSpoutSignal -> "Silicate Magma Lava Spouts"
        | UnexpectedValue vt -> $"Found {vt} and didn't expect it!"

    let toGeoSignal signal =
        match signal with 
        | "$Codex_Ent_IceGeysers_WaterMagma_Name;" -> WaterIceGeyserSignal
        | "$Codex_Ent_IceFumarole_WaterMagma_Name;" -> WaterIceFumaroleSignal
        | "$Codex_Ent_Geysers_WaterMagma_Name;" -> WaterGeyserSignal
        | "$Codex_Ent_Fumarole_WaterMagma_Name;" -> WaterFumaroleSignal
        | "$Codex_Ent_Gas_Vents_WaterMagma_Name;" -> WaterGasVentSignal

        | "$Codex_Ent_IceFumarole_SulphurDioxideMagma_Name;" -> SulphurDioxideIceFumaroleSignal
        | "$Codex_Ent_Fumarole_SulphurDioxideMagma_Name;" -> SulphurDioxideFumaroleSignal
        | "$Codex_Ent_Gas_Vents_SulphurDioxideMagma_Name;" -> SulphurDioxideGasVentSignal
        
        | "$Codex_Ent_IceGeysers_AmmoniaMagma_Name;" -> AmmoniaIceGeyserSignal
        | "$Codex_Ent_IceFumarole_AmmoniaMagma_Name;" -> AmmoniaIceFumaroleSignal
        
        | "$Codex_Ent_IceGeysers_MethaneMagma_Name;" -> MethaneIceGeyserSignal
        | "$Codex_Ent_IceFumarole_MethaneMagma_Name;" -> MethaneIceFumaroleSignal
        
        | "$Codex_Ent_IceGeysers_NitrogenMagma_Name;" -> NitrogenIceGeyserSignal
        | "$Codex_Ent_IceFumarole_NitrogenMagma_Name;" -> NitrogenIceGeyserSignal
        
        | "$Codex_Ent_Lava_Spouts_SilicateMagma_Name;" -> SilicateMagmaLavaSpoutSignal
        | "$Codex_Ent_Lava_Spouts_IronMagma_Name;" -> IronMagmaLavaSpoutSignal
        
        | "$Codex_Ent_IceGeysers_WaterGeysers_Name;" -> WaterIceGeyserSignal
        | "$Codex_Ent_IceFumarole_WaterGeysers_Name;" -> WaterIceFumaroleSignal
        | "$Codex_Ent_Geysers_WaterGeysers_Name;" -> WaterGeyserSignal
        | "$Codex_Ent_Fumarole_WaterGeysers_Name;" -> WaterFumaroleSignal
        | "$Codex_Ent_Gas_Vents_WaterGeysers_Name;" -> WaterGasVentSignal
        
        | "$Codex_Ent_IceGeysers_CarbonDioxideGeysers_Name;" -> CarbonDioxideIceGeyserSignal
        | "$Codex_Ent_IceFumarole_CarbonDioxideGeysers_Name;" -> CarbonDioxideIceFumaroleSignal
        | "$Codex_Ent_Fumarole_CarbonDioxideGeysers_Name;" -> CarbonDioxideFumaroleSignal
        | "$Codex_Ent_Gas_Vents_CarbonDioxideGeysers_Name;" -> CarbonDioxideGasVentSignal
        
        | "$Codex_Ent_IceGeysers_AmmoniaGeysers_Name;" -> AmmoniaIceGeyserSignal
        | "$Codex_Ent_IceFumarole_AmmoniaGeysers_Name;" -> AmmoniaIceFumaroleSignal
        
        | "$Codex_Ent_IceGeysers_MethaneGeysers_Name;" -> MethaneIceGeyserSignal
        | "$Codex_Ent_IceFumarole_MethaneGeysers_Name;" -> MethaneIceFumaroleSignal
        
        | "$Codex_Ent_IceGeysers_NitrogenGeysers_Name;" -> NitrogenIceGeyserSignal
        | "$Codex_Ent_IceFumarole_NitrogenGeysers_Name;" -> NitrogenIceFumaroleSignal
               
        | "$Codex_Ent_IceFumarole_SilicateVapourGeysers_Name;" -> SilicateVapourIceFumaroleSignal
        | "$Codex_Ent_Fumarole_SilicateVapourGeysers_Name;" -> SilicateVapourFumaroleSignal
        | "$Codex_Ent_Gas_Vents_SilicateVapourGeysers_Name;" -> SilicateVapourGasVentSignal

        | _ -> UnexpectedValue $"Unexpected geology scan: {signal}"



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

