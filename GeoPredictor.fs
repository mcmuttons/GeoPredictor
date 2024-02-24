namespace GeoPredictor

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Observatory.Framework
open Observatory.Framework.Files.Journal
open Observatory.Framework.Interfaces
open System.Collections.ObjectModel
open System.Reflection

// Detailed info about a geology scan; currently just a string, but I anticipate more elements and the type gives it purpose
type GeoDetail = { Type:string }

// A body with geology
type GeoBody = { Name:string; BodyType:string; Volcanism:string; Temp:float32<K>; Count:int; GeosFound:List<GeoDetail>; Notified:bool }

// A unique ID for a body
type BodyId = { SystemAddress:uint64; BodyId:int }

// An row of data to be displayed in the UI
type UIOutputRow = { Body:string; BodyType: string; Count:string; Type:string; Volcanism:string; Temp:string }

// Public settings for Observatory
type Settings() =
    let mutable onlyShowCurrentSystem = true
    let mutable onlyShowWithScans = false
    let mutable notifyOnGeoBody = true

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

    // Turn on and off notifications on found geological bodies
    [<SettingDisplayName("Notify on new geological body  ")>]
    member this.NotifyOnGeoBody
        with get() = notifyOnGeoBody
        and set(setting) = notifyOnGeoBody <- setting


type Worker() =

    // Mutable values for interop
    let mutable (Core:IObservatoryCore) = null                  // For interacting with Observatory Core
    let mutable (UI:PluginUI) = null                            // For updating the Observatory UI
    let mutable currentSystem = 0UL                             // ID of the system we're currently in
    let mutable GeoBodies = Map.empty                           // List of all scanned bodies since the Observatory session began
    let mutable GridCollection = ObservableCollection<obj>()    // For initializing UI grid
    let mutable Settings = new Settings()                       // Settings for Observatory

    // Immutable internal values
    let geoSignalType = "$SAA_SignalType_Geological;"           // Journal value for a geological signal
    
    // Null row for initializing the UI
    let buildNullRow = { Body = null; BodyType = null; Count = null; Type = null; Volcanism = null; Temp = null }

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

    // Build detail grid lines if there are geological scans
    let buildGeoDetailEntries body =
        match body.GeosFound |> List.isEmpty with
            | true -> []
            | false ->
                (body.GeosFound
                    |> List.map (fun d -> { Body = body.Name; BodyType = ""; Count = ""; Type = d.Type; Volcanism = ""; Temp = ""}))

    // Build a grid entry for a body, with detail entries if applicable
    let buildGridEntry body =   
        let firstRow = {
            Body = body.Name;
            BodyType = body.BodyType;
            Count = 
                match body.Count with 
                | 0 -> "FSS or DSS for count" 
                | _ -> body.Count.ToString();
            Type = "";
            Volcanism = body.Volcanism;
            Temp = (floor (float body.Temp)).ToString() + "K" }

        body
        |> buildGeoDetailEntries
        |> List.append [firstRow]

    // Repaint the UI
    let updateGrid worker (core:IObservatoryCore) gridRows =
        match core.IsLogMonitorBatchReading with
            | true -> ()
            | false ->
                core.ClearGrid(worker, buildNullRow)
                core.AddGridItems(worker, Seq.cast(gridRows))

    // Filter bodies for display, turn them into a single list of entries, then update the UI
    let updateUI worker core settings currentSys bodies = 
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
    let buildScannedBody id name bodyType volcanism temp bodies =
        match bodies |> Map.tryFind(id) with
        | Some body -> { body with Name = name; Volcanism = volcanism; Temp = temp }
        | None -> { Name = name; BodyType = bodyType; Volcanism = volcanism; Temp = temp; Count = 0; GeosFound = List.empty; Notified = false }

    // If a body already exists, update its count of geological signal, otherwise create a new body
    let buildSignalCountBody id name count bodies =
        match bodies |> Map.tryFind(id) with
        | Some body -> { (body:GeoBody) with Count = count }
        | None -> { Name = name; BodyType = ""; Volcanism = ""; Temp = 0f<K>; Count = count; GeosFound = List.empty; Notified = false }             

    // If a body already exists, and the type of geology has not already been scanned, add the geology; if no body, create a new one
    let buildFoundDetailBody id geotype bodies =
        match bodies |> Map.tryFind(id) with
        | Some body ->
            match body.GeosFound |> List.tryFind(fun g -> g.Type = geotype) with
            | Some geo -> body
            | None -> { body with GeosFound = body.GeosFound |> List.append [{ Type = geotype }]}
        | None ->
            { Name = ""; BodyType = ""; Volcanism = ""; Temp = 0f<K>; Count = 0; GeosFound = [{ Type = geotype }]; Notified = false }
    
    // Format notification text for output
    let formatGeoPlanetNotification volcanism temp count =
        match count <> 0 with
        | true -> $"Landable body with {count} geological signals, and {volcanism} at {floor (float temp)}K."
        | false -> $"Landable body with geological signals, and {volcanism} at {floor (float temp)}K. FSS or DSS for count."

    // Build a notification for found geological signals
    let buildGeoPlanetNotification volcanism temp count =
        NotificationArgs (
            Title = "Geological signals",
            Detail = formatGeoPlanetNotification volcanism temp count)
            
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
                            let id = { SystemAddress = scan.SystemAddress; BodyId = scan.BodyID }
                            let body = buildScannedBody id scan.BodyName scan.PlanetClass scan.Volcanism (scan.SurfaceTemperature * 1.0f<K>) GeoBodies
                            
                            match body.Notified || not Settings.NotifyOnGeoBody with
                            | true -> 
                                GeoBodies <- GeoBodies.Add(id, body)
                            | false ->
                                Core.SendNotification(buildGeoPlanetNotification body.Volcanism body.Temp body.Count) |> ignore
                                GeoBodies <- GeoBodies.Add(id, { body with Notified = true })

                            GeoBodies |> updateUI this Core Settings currentSystem
                        | false -> ()

                | :? FSSBodySignals as sigs ->                   
                    // When signals are discovered through FSS, save/update them if they're geology and update the UI, display a notification
                    sigs.Signals 
                    |> Seq.filter (fun s -> s.Type = geoSignalType)
                    |> Seq.iter (fun s ->
                        let id = { SystemAddress = sigs.SystemAddress; BodyId = sigs.BodyID }
                        GeoBodies <- GeoBodies.Add(id, buildSignalCountBody id sigs.BodyName s.Count GeoBodies))

                    GeoBodies |> updateUI this Core Settings currentSystem

                | :? SAASignalsFound as sigs ->                   
                    // When signals are discovered through DSS, save/update them if they're geology and update the UI, display a notification
                    sigs.Signals 
                    |> Seq.filter (fun s -> s.Type = geoSignalType)
                    |> Seq.iter (fun s ->
                        let id = { SystemAddress = sigs.SystemAddress; BodyId = sigs.BodyID }
                        GeoBodies <- GeoBodies.Add(id, buildSignalCountBody id sigs.BodyName s.Count GeoBodies))

                    GeoBodies |> updateUI this Core Settings currentSystem

                | :? CodexEntry as codexEntry ->
                    // When something is scanned with the comp. scanner, save/update the result if it's geological, then update the UI
                    let id = { BodyId = codexEntry.BodyID; SystemAddress = codexEntry.SystemAddress }
                    match Types.geoTypes |> List.tryFind (fun t -> t = codexEntry.Name) with
                    | Some _ -> 
                        let id = { SystemAddress = codexEntry.SystemAddress; BodyId = codexEntry.BodyID }
                        GeoBodies <- GeoBodies.Add(id, buildFoundDetailBody id codexEntry.Name_Localised GeoBodies)
                        GeoBodies |> updateUI this Core Settings currentSystem
                    | None -> ()

                | :? FSDJump as jump ->
                    // Update current system after an FSD jump, then update the UI
                    if not ((jump :? CarrierJump) && (not (jump :?> CarrierJump).Docked)) then 
                        currentSystem <- setCurrentSystem currentSystem jump.SystemAddress
                        GeoBodies |> updateUI this Core Settings currentSystem

                | :? Location as location ->
                    // Update current system when our location is updated, then update the UI
                    currentSystem <- setCurrentSystem currentSystem location.SystemAddress
                    GeoBodies |> updateUI this Core Settings currentSystem

                | _ -> ()

        member this.LogMonitorStateChanged args =

            // If data is being batch read (Read All), then wait until it's done before updating the UI
            if LogMonitorStateChangedEventArgs.IsBatchRead args.NewState then
                Core.ClearGrid(this, buildNullRow)
            elif LogMonitorStateChangedEventArgs.IsBatchRead args.PreviousState then
                GeoBodies |> updateUI this Core Settings currentSystem

        member this.Name with get() = "GeoPredictor"
        member this.Version with get() = Assembly.GetCallingAssembly().GetName().Version.ToString()
        member this.PluginUI with get() = UI

        member this.Settings 
            with get() = Settings
            and set(settings) = 
                Settings <- settings :?> Settings

                // Update the UI if a setting that requires it has been changed by subscribing to event
                Settings.NeedsUIUpdate.Add(fun () -> GeoBodies |> updateUI this Core Settings currentSystem)
            
