﻿namespace GeoPredictor

open Observatory.Framework
open Observatory.Framework.Files.Journal
open Observatory.Framework.Interfaces
open System.Collections.ObjectModel
open System.Reflection

type GeoDetail = { Type:string }
type ScannedBody = { Name:string; Volcanism:string; Temp:float32; Count:int; GeosFound:List<GeoDetail> }
type BodyId = { SystemAddress:uint64; BodyId:int }
type GeoRow = { Body:string; Count:string; Type:string; Volcanism:string; Temp:string }

type Worker() =

    // mutable
    let mutable (Core:IObservatoryCore) = null
    let mutable (UI:PluginUI) = null
    let mutable currentSystem = 0UL
    let mutable ScannedBodies = Map.empty
    let mutable GridCollection = ObservableCollection<obj>()

    // immutable
    let geoSignalType = "$SAA_SignalType_Geological;"
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

    let version = Assembly.GetCallingAssembly().GetName().Version.ToString()         
   
    let buildGridEntry body =   
        let firstRow = 
            { Body = body.Name;
                Count = body.Count.ToString();
                Type = "";
                Volcanism = body.Volcanism;
                Temp = (floor body.Temp).ToString() + "K" }
        match body.GeosFound.IsEmpty with
            | true -> [ firstRow ]
            | false ->
                [firstRow] |> List.append
                    (body.GeosFound
                        |> List.map (fun d -> { Body = ""; Count = ""; Type = d.Type; Volcanism = ""; Temp = ""}))                               

    let buildGridRows currentSystem scannedBodies =
        scannedBodies
        |> Map.filter (fun k _ -> k.SystemAddress = currentSystem)
        |> Seq.collect (fun body -> buildGridEntry body.Value) 

    let buildNullRow = { Body = null; Count = null; Type = null; Volcanism = null; Temp = null }
    let buildHeaderRow = { Body = "GeoPredictor v" + version; Count = ""; Type = ""; Volcanism = ""; Temp = "" }

    let setCurrentSystem oldSystem newSystem = 
        match oldSystem = 0UL || oldSystem <> newSystem with
            | true -> newSystem
            | false -> oldSystem

    let updateGrid worker (core:IObservatoryCore) gridRows =
        match core.IsLogMonitorBatchReading with
            | true -> ()
            | false ->
                core.ClearGrid(worker, buildNullRow)
                core.AddGridItem(worker, buildHeaderRow)
                core.AddGridItems(worker, Seq.cast(gridRows))

    let isNotNullOrEmpty string =
        match string with
            | null -> false
            | "" -> false
            | _ -> true

    let addBodyDetails id name volcanism temp (bodies:Map<BodyId, ScannedBody>) =
        match bodies |> Map.tryFind(id) with
        | Some body ->
            bodies.Add (id, { body with Name = name; Volcanism = volcanism; Temp = temp })
        | None -> 
            bodies.Add (id, { Name = name; Volcanism = volcanism; Temp = temp; Count = 0; GeosFound = List.empty })

    let addGeoDetails id name count (bodies:Map<BodyId, ScannedBody>) =
        match bodies |> Map.tryFind(id) with
        | Some body ->
            bodies.Add (id, { body with Count = count })
        | None ->
            bodies.Add (id, { Name = name; Volcanism = ""; Temp = 0f; Count = count; GeosFound = List.empty })

    let addFoundDetails id geotype (bodies:Map<BodyId, ScannedBody>) =
        match bodies |> Map.tryFind(id) with
        | Some body ->
            match body.GeosFound |> List.tryFind(fun g -> g.Type = geotype) with
            | Some geo -> bodies
            | None -> bodies.Add (id, { body with GeosFound = body.GeosFound |> List.append [{ Type = geotype }]})
        | None ->
            bodies.Add (id, { Name = ""; Volcanism = ""; Temp = 0f; Count = 0; GeosFound = [{ Type = geotype }]})


    interface IObservatoryWorker with 
        member this.Load core = 
            Core <- core
            
            GridCollection.Add(buildNullRow)
            UI <- PluginUI(GridCollection)

        member this.JournalEvent event =
            match (event:JournalBase) with 
                | :? Scan as scan ->
                    match scan.Landable && scan.Volcanism |> isNotNullOrEmpty with
                        | true -> 
                            let id = { SystemAddress = scan.SystemAddress; BodyId = scan.BodyID }
                            ScannedBodies <- addBodyDetails id scan.BodyName scan.Volcanism scan.SurfaceTemperature ScannedBodies
                            buildGridRows currentSystem ScannedBodies |> updateGrid this Core
                        | false -> ()

                | :? SAASignalsFound as sigs ->  
                    sigs.Signals 
                    |> Seq.filter (fun s -> s.Type = geoSignalType)
                    |> Seq.iter (fun s ->
                        let id = { SystemAddress = sigs.SystemAddress; BodyId = sigs.BodyID }
                        ScannedBodies <- addGeoDetails id sigs.BodyName s.Count ScannedBodies)
                    buildGridRows currentSystem ScannedBodies |> updateGrid this Core

                | :? CodexEntry as codexEntry ->
                    let id = { BodyId = codexEntry.BodyID; SystemAddress = codexEntry.SystemAddress }
                    match geoTypes |> List.tryFind (fun t -> t = codexEntry.Name) with
                    | Some _ ->
                        let id = { SystemAddress = codexEntry.SystemAddress; BodyId = codexEntry.BodyID }
                        ScannedBodies <- addFoundDetails id codexEntry.Name_Localised ScannedBodies
                        buildGridRows currentSystem ScannedBodies |> updateGrid this Core
                    | None -> ()

                | :? FSDJump as jump ->
                    if not ((jump :? CarrierJump) && (not (jump :?> CarrierJump).Docked)) then 
                        currentSystem <- setCurrentSystem currentSystem jump.SystemAddress
                        buildGridRows currentSystem ScannedBodies |> updateGrid this Core               

                | :? Location as location ->
                    currentSystem <- setCurrentSystem currentSystem location.SystemAddress
                    buildGridRows currentSystem ScannedBodies |> updateGrid this Core

                | _ -> ()

        member this.LogMonitorStateChanged args =
            if LogMonitorStateChangedEventArgs.IsBatchRead args.NewState then
                Core.ClearGrid(this, buildNullRow)
            elif LogMonitorStateChangedEventArgs.IsBatchRead args.PreviousState then
                buildGridRows currentSystem ScannedBodies |> updateGrid this Core

        member this.get_Name () = "GeoPredictor"
        member this.get_Version () = version
        member this.get_PluginUI () = UI
        member this.get_Settings () = ()
        member this.set_Settings settings = ()

