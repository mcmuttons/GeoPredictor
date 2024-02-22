namespace GeoPredictor

open Observatory.Framework
open Observatory.Framework.Files.Journal
open Observatory.Framework.Interfaces
open System.Collections.ObjectModel
open System.Reflection

// Detailed info about a geology scan; currently just a string, but I anticipate more elements and the type gives it purpose
type GeoDetail = { Type:string }

// A body with geology
type GeoBody = { Name:string; Volcanism:string; Temp:float32; Count:int; GeosFound:List<GeoDetail> }

// A unique ID for a body
type BodyId = { SystemAddress:uint64; BodyId:int }

// An row of data to be displayed in the UI
type UIOutputRow = { Body:string; Count:string; Type:string; Volcanism:string; Temp:string }

// Public settings for Observatory
type Settings() =
    let mutable onlyShowCurrentSystem = true
    let mutable onlyShowWithScans = false

    // Event that triggers for Settings that require UI updates when changed
    let needsUIUpdate = new Event<_>()
    member this.NeedsUIUpdate = needsUIUpdate.Publish

    // Only show data for the current system; requires UI update
    [<SettingDisplayName("Show only current system  ")>]
    member this.OnlyShowCurrentSystem
        with get() = onlyShowCurrentSystem
        and set(setting) = 
            onlyShowCurrentSystem <- setting
            needsUIUpdate.Trigger()
    
    // Only show data for bodies where geological features have been scanned; requires UI update
    [<SettingDisplayName("Show only bodies with scans  ")>]
    member this.OnlyShowWithScans
        with get() = onlyShowWithScans
        and set(setting) = 
            onlyShowWithScans <- setting
            needsUIUpdate.Trigger()


type Worker() =

    // Mutable values for interop
    let mutable (Core:IObservatoryCore) = null                  // For interacting with Observatory Core
    let mutable (UI:PluginUI) = null                            // For updating the Observatory UI
    let mutable currentSystem = 0UL                             // ID of the system we're currently in
    let mutable ScannedBodies = Map.empty                       // List of all scanned bodies since the Observatory session began
    let mutable GridCollection = ObservableCollection<obj>()    // For initializing UI grid
    let mutable Settings = new Settings()                       // Settings for Observatory

    // Immutable internal values
    let geoSignalType = "$SAA_SignalType_Geological;"           // Journal value for a geological signal

        // All possible permutations of the types of geology and volcanisms. I'm pretty sure a lot of these combinations are
        // invalid, and that especially some of the magmas don't occur at all, even though they're theoretically listed
        // as possibles, but this way none will slip through the cracks while checking, especially in case there's a permutation
        // out there that hasn't been discovered yet.
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
    
    // Null row for initializing the UI
    let buildNullRow = { Body = null; Count = null; Type = null; Volcanism = null; Temp = null }

    // Update current system if it has changed
    let setCurrentSystem oldSystem newSystem = 
        match oldSystem = 0UL || oldSystem <> newSystem with
            | true -> newSystem
            | false -> oldSystem
    
    // Filter bodies to those with registered comp. scans
    let filterForOnlyShowWithScans onlyScans bodies =
        match onlyScans with
        | false -> bodies
        | true -> bodies |> Map.filter(fun _ b -> not (b.GeosFound |> Seq.isEmpty ))

    // Filter bodies to those in current system
    let filterForShowOnlyCurrentSys onlyCurrent currentSys bodies =
        match onlyCurrent with
        | false -> bodies
        | true -> bodies |> Map.filter(fun id _ -> id.SystemAddress = currentSys)

    // Filter the bodies down to what should be shown in the UI
    let filterBodiesForOutput (settings:Settings) currentSys bodies =
        bodies
        |> filterForShowOnlyCurrentSys settings.OnlyShowCurrentSystem currentSys
        |> filterForOnlyShowWithScans settings.OnlyShowWithScans

    // Build a grid entry for a body; if it has scans add those in separate grid lines underneath
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

    // Repaint the UI
    let updateGrid worker (core:IObservatoryCore) gridRows =
        match core.IsLogMonitorBatchReading with
            | true -> ()
            | false ->
                core.ClearGrid(worker, buildNullRow)
                core.AddGridItems(worker, Seq.cast(gridRows))

    // Filter bodies for display, turn them into a single list of entries, then update the UI
    let updateUI worker core (settings:Settings) currentSys bodies = 
        bodies 
        |> filterBodiesForOutput settings currentSys
        |> Seq.collect (fun body -> buildGridEntry body.Value)
        |> updateGrid worker core                            

    // Check if a string has content
    let isNotNullOrEmpty string =
        match string with
            | null -> false
            | "" -> false
            | _ -> true

    // If a body already exists, update its details with name, volcanism and temperature, otherwise create a new body
    let addBodyDetails id name volcanism temp (bodies:Map<BodyId, GeoBody>) =
        match bodies |> Map.tryFind(id) with
        | Some body ->
            bodies.Add (id, { body with Name = name; Volcanism = volcanism; Temp = temp })
        | None -> 
            bodies.Add (id, { Name = name; Volcanism = volcanism; Temp = temp; Count = 0; GeosFound = List.empty })

    // If a body already exists, update its count of geological signal, otherwise create a new body
    let addGeoDetails id name count (bodies:Map<BodyId, GeoBody>) =
        match bodies |> Map.tryFind(id) with
        | Some body ->
            bodies.Add (id, { body with Count = count })
        | None ->
            bodies.Add (id, { Name = name; Volcanism = ""; Temp = 0f; Count = count; GeosFound = List.empty })

    // If a body already exists, and the type of geology has not already been scanned, add the geology; if no body, create a new one
    let addFoundDetails id geotype (bodies:Map<BodyId, GeoBody>) =
        match bodies |> Map.tryFind(id) with
        | Some body ->
            match body.GeosFound |> List.tryFind(fun g -> g.Type = geotype) with
            | Some geo -> bodies
            | None -> bodies.Add (id, { body with GeosFound = body.GeosFound |> List.append [{ Type = geotype }]})
        | None ->
            bodies.Add (id, { Name = ""; Volcanism = ""; Temp = 0f; Count = 0; GeosFound = [{ Type = geotype }]})   
            
    // Interface for interop with Observatory, and entry point for the DLL.
    // The goal has been to keep all mutable operations within this scope to isolate imperative code as much as
    // possible. 
    interface IObservatoryWorker with  
    
        // Initialize interop and UI
        member this.Load core = 
            Core <- core
            
            GridCollection.Add(buildNullRow)
            UI <- PluginUI(GridCollection)

        // Handle journal events
        member this.JournalEvent event =
            match (event:JournalBase) with 
                | :? Scan as scan ->
                    
                    // When a body is scanned (FSS, Proximity or NAV beacon), save/update it if it's landable and has volcanism and update the UI
                    match scan.Landable && scan.Volcanism |> isNotNullOrEmpty with
                        | true -> 
                            ScannedBodies <- addBodyDetails { SystemAddress = scan.SystemAddress; BodyId = scan.BodyID } scan.BodyName scan.Volcanism scan.SurfaceTemperature ScannedBodies
                            ScannedBodies |> updateUI this Core Settings currentSystem
                        | false -> ()

                | :? SAASignalsFound as sigs ->
                    
                    // When signals are discovered, save/update them if they're geology and update the UI
                    sigs.Signals 
                    |> Seq.filter (fun s -> s.Type = geoSignalType)
                    |> Seq.iter (fun s ->
                        ScannedBodies <- addGeoDetails { SystemAddress = sigs.SystemAddress; BodyId = sigs.BodyID } sigs.BodyName s.Count ScannedBodies)
                    ScannedBodies |> updateUI this Core Settings currentSystem

                | :? CodexEntry as codexEntry ->

                    // When something is scanned with the comp. scanner, save/update the result if it's geological, then update the UI
                    let id = { BodyId = codexEntry.BodyID; SystemAddress = codexEntry.SystemAddress }
                    match geoTypes |> List.tryFind (fun t -> t = codexEntry.Name) with
                    | Some _ ->
                        ScannedBodies <- addFoundDetails { SystemAddress = codexEntry.SystemAddress; BodyId = codexEntry.BodyID } codexEntry.Name_Localised ScannedBodies
                        ScannedBodies |> updateUI this Core Settings currentSystem
                    | None -> ()

                | :? FSDJump as jump ->

                    // Update current system after an FSD jump, then update the UI
                    if not ((jump :? CarrierJump) && (not (jump :?> CarrierJump).Docked)) then 
                        currentSystem <- setCurrentSystem currentSystem jump.SystemAddress
                        ScannedBodies |> updateUI this Core Settings currentSystem

                | :? Location as location ->

                    // Update current system when our location is updated, then update the UI
                    currentSystem <- setCurrentSystem currentSystem location.SystemAddress
                    ScannedBodies |> updateUI this Core Settings currentSystem

                | _ -> ()

        member this.LogMonitorStateChanged args =

            // If data is being batch read (Read All), then wait until it's done before updating the UI
            if LogMonitorStateChangedEventArgs.IsBatchRead args.NewState then
                Core.ClearGrid(this, buildNullRow)
            elif LogMonitorStateChangedEventArgs.IsBatchRead args.PreviousState then
                ScannedBodies |> updateUI this Core Settings currentSystem

        member this.Name with get() = "GeoPredictor"
        member this.Version with get() = Assembly.GetCallingAssembly().GetName().Version.ToString()
        member this.PluginUI with get() = UI

        member this.Settings 
            with get() = Settings
            and set(settings) = 
                Settings <- settings :?> Settings

                // Update the UI if a setting that requires it has been changed by subscribing to event
                Settings.NeedsUIUpdate.Add(fun () -> ScannedBodies |> updateUI this Core Settings currentSystem)
            
