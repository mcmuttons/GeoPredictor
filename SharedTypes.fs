namespace GeoPredictor

open FSharp.Data.UnitSystems.SI.UnitSymbols
open System

// Settings that aren't exposed to Observatory, but tracked only in GeoPredictor
type InternalSettings = { HasReadAllBeenRun:bool; Version:Version }

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
    | CodexPredicted
    | Matched
    | Surprise
    | Unmatched

// A single codex entry
type CodexUnit = { Signal:GeologySignal; Region:Region }

// Materials
type [<Measure>] percent

type MaterialName =
    | Carbon
    | Vanadium
    | Niobium
    | Yttrium
    | Phosphorus
    | Chromium
    | Molybdenum
    | Technetium
    | Sulphur
    | Manganese
    | Cadmium
    | Ruthenium
    | Iron
    | Zinc
    | Tin
    | Selenium
    | Nickel
    | Germanium
    | Tungsten
    | Tellurium
    | Rhenium
    | Arsenic
    | Mercury
    | Polonium
    | Lead
    | Zirconium
    | Boron
    | Antimony
    | UnknownMaterial of string

type MaterialGrade =
    | Grade1
    | Grade2
    | Grade3
    | Grade4
    | UnknownGrade

type MaterialCategory =
    | Category1
    | Category2
    | Category3
    | Category4
    | Category5
    | Category6
    | Category7
    | UnknownCategory

type Material = { MaterialName:MaterialName; Grade:MaterialGrade; Category:MaterialCategory; Percent:float32<percent> }

// A body with geology
type GeoBody = { 
    BodyName:string; 
    ShortName:string; 
    BodyType:BodyType; 
    Volcanism:Volcanism; 
    Temp:float32<K>; 
    Count:int; 
    GeosFound:Map<GeologySignal,PredictionStatus>; 
    Notified:bool; 
    Region:Region;
    Materials:Material seq }