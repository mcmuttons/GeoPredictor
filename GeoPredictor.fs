namespace GeoPredictor

open Observatory.Framework
open Observatory.Framework.Files.Journal
open Observatory.Framework.Interfaces
open System.Collections.ObjectModel
open System.Reflection

type GeoDetail = { Type:string }
type ScannedBody = { Name:string; Volcanism:string; Temp:float32; Count:int; GeosFound:List<GeoDetail> }
type BodyId = { SystemAddress:uint64; BodyId:int }
type GeoRow = { Body:string; Count:string; Type:string; Volcanism:string; Temp:string }

type Settings() =
    let mutable onlyShowCurrentSystem = true
    let mutable onlyShowWithScans = false

    let needsUIUpdate = new Event<_>()

    member this.NeedsUIUpdate = needsUIUpdate.Publish

    [<SettingDisplayName("Show only current system  ")>]
    member this.OnlyShowCurrentSystem
        with get() = onlyShowCurrentSystem
        and set(setting) = 
            onlyShowCurrentSystem <- setting
            needsUIUpdate.Trigger()


    [<SettingDisplayName("Show only bodies with scans  ")>]
    member this.OnlyShowWithScans
        with get() = onlyShowWithScans
        and set(setting) = onlyShowWithScans <- setting


type Worker() as self =

    // mutable
    let mutable (Core:IObservatoryCore) = null
    let mutable (UI:PluginUI) = null
    let mutable currentSystem = 0UL
    let mutable ScannedBodies = Map.empty
    let mutable GridCollection = ObservableCollection<obj>()
    let mutable isPluginLoaded = false   
    let mutable Settings = new Settings()

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
        match body.GeosFound |> List.isEmpty with
            | true -> [ firstRow ]
            | false ->
                (body.GeosFound
                    |> List.map (fun d -> { Body = body.Name; Count = ""; Type = d.Type; Volcanism = ""; Temp = ""}))
                    |> List.append [ firstRow ]

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

    let filterForCurrentSysIfNecessary onlyCurrent isBatch currentSys bodies =
        match isBatch with
        | true -> bodies
        | false ->
            match onlyCurrent with
            | false -> bodies
            | true -> bodies |> Map.filter(fun k _ -> k.SystemAddress = currentSys)

    let updateUI worker core (settings:Settings) isBatch currentSys bodies = 
        bodies 
        |> filterForCurrentSysIfNecessary settings.OnlyShowCurrentSystem isBatch currentSys
        |> Seq.collect (fun body -> buildGridEntry body.Value)
        |> updateGrid worker core                            

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

            isPluginLoaded <- true

        member this.JournalEvent event =
            match (event:JournalBase) with 
                | :? Scan as scan ->
                    match scan.Landable && scan.Volcanism |> isNotNullOrEmpty with
                        | true -> 
                            ScannedBodies <- addBodyDetails { SystemAddress = scan.SystemAddress; BodyId = scan.BodyID } scan.BodyName scan.Volcanism scan.SurfaceTemperature ScannedBodies
                            ScannedBodies |> updateUI this Core Settings false currentSystem
                        | false -> ()

                | :? SAASignalsFound as sigs ->  
                    sigs.Signals 
                    |> Seq.filter (fun s -> s.Type = geoSignalType)
                    |> Seq.iter (fun s ->
                        ScannedBodies <- addGeoDetails { SystemAddress = sigs.SystemAddress; BodyId = sigs.BodyID } sigs.BodyName s.Count ScannedBodies)
                    ScannedBodies |> updateUI this Core Settings false currentSystem

                | :? CodexEntry as codexEntry ->
                    let id = { BodyId = codexEntry.BodyID; SystemAddress = codexEntry.SystemAddress }
                    match geoTypes |> List.tryFind (fun t -> t = codexEntry.Name) with
                    | Some _ ->
                        ScannedBodies <- addFoundDetails { SystemAddress = codexEntry.SystemAddress; BodyId = codexEntry.BodyID } codexEntry.Name_Localised ScannedBodies
                        ScannedBodies |> updateUI this Core Settings false currentSystem
                    | None -> ()

                | :? FSDJump as jump ->
                    if not ((jump :? CarrierJump) && (not (jump :?> CarrierJump).Docked)) then 
                        currentSystem <- setCurrentSystem currentSystem jump.SystemAddress
                        ScannedBodies |> updateUI this Core Settings false currentSystem

                | :? Location as location ->
                    currentSystem <- setCurrentSystem currentSystem location.SystemAddress
                    ScannedBodies |> updateUI this Core Settings false currentSystem

                | _ -> ()

        member this.LogMonitorStateChanged args =
            if LogMonitorStateChangedEventArgs.IsBatchRead args.NewState then
                Core.ClearGrid(this, buildNullRow)
            elif LogMonitorStateChangedEventArgs.IsBatchRead args.PreviousState then
                Settings.OnlyShowCurrentSystem <- false
                ScannedBodies |> updateUI this Core Settings true currentSystem

        member this.Name with get() = "GeoPredictor"
        member this.Version with get() = version
        member this.PluginUI with get() = UI

        member this.Settings 
            with get() = Settings
            and set(settings) = 
                Settings <- settings :?> Settings
                Settings.NeedsUIUpdate.Add(fun () -> ScannedBodies |> updateUI this Core Settings false currentSystem)
            
