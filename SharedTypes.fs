namespace GeoPredictor

open FSharp.Data.UnitSystems.SI.UnitSymbols

// Settings that aren't exposed to Observatory, but tracked only in GeoPredictor
type InternalSettings = { HasReadAllBeenRun:bool }

// A unique ID for a body
type BodyId = { SystemAddress:uint64; BodyId:int }

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
    | UnexpectedSignal of string

type Volcanism = { Level:VolcanismLevel; Type:VolcanismType }

// Has this geo been predicted, matched, or come as a complete surprise?
type PredictionStatus =
    | Predicted
    | Matched
    | Surprise
    | Unmatched

// A body with geology
type GeoBody = { Name:string; BodyType:BodyType; Volcanism:Volcanism; Temp:float32<K>; Count:int; GeosFound:Map<GeologySignal,PredictionStatus>; Notified:bool; Region:Region }

// A single codex entry
type CodexUnit = { Signal:GeologySignal; Region:Region }
