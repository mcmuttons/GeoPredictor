namespace GeoPredictor

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Observatory.Framework
open Observatory.Framework.Files.Journal
open Observatory.Framework.Interfaces
open System.Collections.ObjectModel
open System.Reflection
open System.Text.Json
open EliteDangerousRegionMap


// Has this geo been predicted, matched, or come as a complete surprise?
type PredictionStatus =
    | Predicted
    | Matched
    | Surprise
    | Unmatched

// A body with geology
type GeoBody = { Name:string; BodyType:BodyType; Volcanism:Volcanism; Temp:float32<K>; Count:int; GeosFound:Map<GeologySignal,PredictionStatus>; Notified:bool; Region:Region }

// A unique ID for a body
type BodyId = { SystemAddress:uint64; BodyId:int }

// An row of data to be displayed in the UI
type UIOutputRow = { Body:string; Count:string; Found:string; Type:string; BodyType: string; Volcanism:string; Temp:string; Region:string }

type CodexUnit = { Signal:GeologySignal; Region:Region }

type SerializableCodexData = { Sig:string; Reg:string }


// Public settings for Observatory
type Settings() =
    let mutable notifyOnGeoBody = true
    let mutable verboseNotifications = true
    let mutable onlyShowCurrentSystem = true
    let mutable onlyShowWithScans = false
    let mutable onlyShowFailedPredictionBodies = false

    // Event that triggers for Settings that require UI updates when changed
    let needsUIUpdate = new Event<_>()
    member this.NeedsUIUpdate = needsUIUpdate.Publish

    // Turn on and off notifications on found geological bodies
    [<SettingDisplayName("Notify on new geological body  ")>]
    member this.NotifyOnGeoBody
        with get() = notifyOnGeoBody
        and set(setting) = notifyOnGeoBody <- setting

    // Verbose notifications
    [<SettingDisplayName("Verbose notifications  ")>]
    member this.VerboseNotifications
        with get() = verboseNotifications
        and set(setting) = verboseNotifications <- setting

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

    // Only show data for bodies where prediction failed; requires UI update
    [<SettingDisplayName("Show only bodies with failed prediction  ")>]
    member this.OnlyShowFailedPredictionBodies
        with get() = onlyShowFailedPredictionBodies
        and set(setting) =
            onlyShowFailedPredictionBodies <- setting
            needsUIUpdate.Trigger()



type Worker() =

    // Mutable values for interop
    let mutable (Core:IObservatoryCore) = null                  // For interacting with Observatory Core
    let mutable (UI:PluginUI) = null                            // For updating the Observatory UI
    let mutable CurrentSystem = 0UL                             // ID of the system we're currently in
    let mutable CurrentRegion = UnknownRegion "Uninitialized"   // Region we're currently in
    let mutable GeoBodies = Map.empty                           // Map of all scanned bodies since the Observatory session began
    let mutable GridCollection = ObservableCollection<obj>()    // For initializing UI grid
    let mutable Settings = new Settings()                       // Settings for Observatory
    let mutable CodexUnlocks = Set.empty                        // Set of all codex entries unlocked so far

    // Immutable internal values
    let externalVersion = "GeoPredictor v1.3"
    let geoSignalType = "$SAA_SignalType_Geological;"           // Journal value for a geological signal
    let predictionSuccess = "\u2714"                            // Heavy check mark
    let predictionUnknown = "\u2754"                            // White question mark
    let predictionFailed = "\u274C"                             // Red X
    let newCodexEntry = "\U0001F537"                            // Blue diamond

    // Null row for initializing the UI
    let buildNullRow = { Body = null; Count = null; Found = null; Type = null; BodyType = null; Volcanism = null; Temp = null; Region = null }
    let buildHeaderRow = { Body = externalVersion; Count = ""; Found = ""; Type = ""; BodyType = ""; Volcanism = ""; Temp = ""; Region = "" }

    // Update current system if it has changed
    let setCurrentSystem oldSystem newSystem = 
        match oldSystem = 0UL || oldSystem <> newSystem with
            | true -> newSystem
            | false -> oldSystem

    // Filter bodies to those with registered comp. scans
    let filterForShowOnlyWithScans onlyScans bodies =
        match onlyScans with
        | false -> bodies
        | true -> bodies |> Map.filter(fun _ b -> b.GeosFound |> Map.values |> Seq.contains Matched || b.GeosFound |> Map.values |> Seq.contains Surprise)

    // Filter bodies to those in current system
    let filterForShowOnlyCurrentSys onlyCurrent currentSys bodies =
        match onlyCurrent with
        | false -> bodies
        | true -> bodies |> Map.filter(fun id _ -> id.SystemAddress = currentSys)

    // Filter bodies to only those with failed predictions
    let filterForShowOnlyFailedPredictions onlyFailed bodies =
        match onlyFailed with
        | false -> bodies
        | true -> bodies |> Map.filter(fun _ b -> b.GeosFound |> Map.values |> Seq.contains Surprise)

    // Filter the bodies down to what should be shown in the UI
    let filterBodiesForOutput (settings:Settings) currentSys bodies =
        bodies
        |> filterForShowOnlyCurrentSys settings.OnlyShowCurrentSystem currentSys
        |> filterForShowOnlyWithScans settings.OnlyShowWithScans
        |> filterForShowOnlyFailedPredictions settings.OnlyShowFailedPredictionBodies

    // Build detail grid lines if there are geological scans
    let buildGeoDetailEntries codexUnlocks body =
        match body.GeosFound |> Map.isEmpty with
            | true -> []
            | false -> (
                body.GeosFound
                    |> Map.toList
                    |> List.map (fun (s,d) -> 
                        {   Body = body.Name; 
                            BodyType = ""; 
                            Count = ""; 
                            Found = 
                                match d with 
                                | Matched -> predictionSuccess 
                                | Predicted -> 
                                    match codexUnlocks |> Set.contains { Signal = s; Region = body.Region } with
                                    | true -> predictionUnknown 
                                    | false -> predictionUnknown + newCodexEntry
                                | Unmatched -> "" 
                                | Surprise -> predictionFailed                             
                            Type = Parser.toGeoSignalOut s; 
                            Volcanism = ""; 
                            Temp = "";
                            Region = ""}))

    // Build a grid entry for a body, with detail entries if applicable
    let buildGridEntry codexUnlocks body =   
        let firstRow = {
            Body = body.Name;
            BodyType = Parser.toBodyTypeOut body.BodyType;
            Count = 
                match body.Count with 
                | 0 -> "FSS/DSS" 
                | _ -> body.Count.ToString();
            Found = ""
            Type = "";
            Volcanism = Parser.toVolcanismOut body.Volcanism;
            Temp = (floor (float body.Temp)).ToString() + "K"
            Region = Parser.toRegionOut body.Region}

        body
        |> buildGeoDetailEntries codexUnlocks
        |> List.append [firstRow]

    // Repaint the UI
    let updateGrid worker (core:IObservatoryCore) gridRows =
        match core.IsLogMonitorBatchReading with
            | true -> ()
            | false ->
                core.ClearGrid(worker, buildNullRow)
                core.AddGridItem(worker, buildHeaderRow)
                core.AddGridItems(worker, Seq.cast(gridRows))

    // Filter bodies for display, turn them into a single list of entries, then update the UI
    let updateUI worker core settings currentSys codexUnlocks bodies = 
        bodies 
        |> filterBodiesForOutput settings currentSys
        |> Seq.collect (fun body -> buildGridEntry codexUnlocks body.Value)
        |> updateGrid worker core                            

    // If a body already exists, update its details with name, volcanism and temperature, otherwise create a new body    
    let buildScannedBody id name bodyType volcanism temp region bodies =
        let predictedGeos = 
            Predictor.getGeologyPredictions bodyType volcanism
            |> List.map (fun p -> p, Predicted)
            |> Map.ofList   

        match bodies |> Map.tryFind(id) with
        | Some body -> { body with Name = name; BodyType = bodyType; Volcanism = volcanism; Temp = temp ; GeosFound = if body.GeosFound.IsEmpty then predictedGeos else body.GeosFound }
        | None -> { Name = name; BodyType = bodyType; Volcanism = volcanism; Temp = temp; Count = 0; GeosFound = predictedGeos; Notified = false; Region = region }

    // If a body already exists, update its count of geological signal, otherwise create a new body
    let buildSignalCountBody id name count region bodies =
        match bodies |> Map.tryFind(id) with
        | Some body -> { (body:GeoBody) with Count = count }
        | None -> { Name = name; BodyType = BodyTypeNotYetSet; Volcanism = Parser.toVolcanismNotYetSet; Temp = 0f<K>; Count = count; GeosFound = Map.empty; Notified = false; Region = region }             

    // If a body already exists, and the type of geology has not already been scanned, add the geology; if no body, create a new one
    let buildFoundDetailBody id signal region bodies =
        match bodies |> Map.tryFind(id) with
        | Some body ->
            match body.GeosFound |> Map.tryFind(signal) with
            | Some geo -> 
                match geo with
                | Predicted -> { body with GeosFound = body.GeosFound |> Map.add signal Matched }
                | _ -> body
            | None -> { body with GeosFound = body.GeosFound |> Map.add signal Surprise }
        | None ->
            { Name = ""; BodyType = BodyTypeNotYetSet; Volcanism = Parser.toVolcanismNotYetSet; Temp = 0f<K>; Count = 0; GeosFound = Map.empty |> Map.add signal Unmatched; Notified = false; Region = region }
    
    // Format notification text for output
    let formatGeoPlanetNotification verbose volcanism temp count =
        let volcanismLowerCase = (Parser.toVolcanismOut volcanism).ToLower()
        match (count <> 0, verbose) with
        | true, true -> $"Landable body with {count} geological signals, and {volcanismLowerCase} at {floor (float temp)}K."
        | true, false -> $"{count} geological signals found"
        | false, true -> $"Landable body with geological signals, and {volcanismLowerCase} at {floor (float temp)}K. FSS or DSS for count."
        | false, false -> "Geological signals found"

    // Build a notification for found geological signals
    let buildGeoPlanetNotification verbose volcanism temp count =
        NotificationArgs (
            Title = "Geological signals",
            Detail = formatGeoPlanetNotification verbose volcanism temp count)

    let serializeCodexUnlocks codexUnlocks =
        let serializable =
            codexUnlocks
            |> Set.map (fun cu -> { Sig = Parser.toGeoSignalOut cu.Signal; Reg = Parser.toRegionOut cu.Region })
        JsonSerializer.Serialize(serializable)

    let deserializeCodexUnlocks (json:string) =
        let deserialized = 
            JsonSerializer.Deserialize<Set<SerializableCodexData>> json
            |> Set.map (fun cu -> { Signal = Parser.toGeoSignalFromSerialization cu.Sig; Region = Parser.toRegion cu.Reg })
        deserialized


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
                    if scan.Landable && scan.Volcanism |> Parser.isNotNullOrEmpty then
                        let id = { SystemAddress = scan.SystemAddress; BodyId = scan.BodyID }
                        let body = 
                            buildScannedBody 
                                id 
                                scan.BodyName 
                                (Parser.toBodyType scan.PlanetClass) 
                                (Parser.toVolcanism scan.Volcanism) 
                                (scan.SurfaceTemperature * 1.0f<K>) 
                                CurrentRegion
                                GeoBodies
                            
                        match body.Notified || not Settings.NotifyOnGeoBody with
                        | true -> 
                            GeoBodies <- GeoBodies.Add(id, body)
                        | false ->
                            Core.SendNotification(buildGeoPlanetNotification Settings.VerboseNotifications body.Volcanism body.Temp body.Count) |> ignore
                            GeoBodies <- GeoBodies.Add(id, { body with Notified = true })

                        GeoBodies |> updateUI this Core Settings CurrentSystem CodexUnlocks

                | :? SAASignalsFound as sigs ->                   
                    // When signals are discovered through DSS, save/update them if they're geology and update the UI, display a notification
                    sigs.Signals 
                    |> Seq.filter (fun s -> s.Type = geoSignalType)
                    |> Seq.iter (fun s ->
                        let id = { SystemAddress = sigs.SystemAddress; BodyId = sigs.BodyID }
                        GeoBodies <- GeoBodies.Add(id, buildSignalCountBody id sigs.BodyName s.Count CurrentRegion GeoBodies))

                    GeoBodies |> updateUI this Core Settings CurrentSystem CodexUnlocks

                | :? CodexEntry as codexEntry ->
                    // When something is scanned with the comp. scanner, save/update the result if it's geological, then update the UI
                    match Parser.geoTypes |> List.tryFind (fun t -> t = codexEntry.Name) with
                    | Some _ -> 
                        let id = { BodyId = codexEntry.BodyID; SystemAddress = codexEntry.SystemAddress }
                        let signal = Parser.toGeoSignalFromJournal codexEntry.Name
                        let region = Parser.toRegion codexEntry.Region

                        if codexEntry.IsNewEntry then
                            CodexUnlocks <- CodexUnlocks |> Set.add { Signal = signal; Region = region }

                        GeoBodies <- GeoBodies.Add(id, buildFoundDetailBody id signal region GeoBodies)
                        GeoBodies |> updateUI this Core Settings CurrentSystem CodexUnlocks
                    | None -> ()

                | :? FSDJump as jump ->
                    // Update current system after an FSD jump, then update the UI
                    if not ((jump :? CarrierJump) && (not (jump :?> CarrierJump).Docked)) then 
                        let struct (x, y, z) = jump.StarPos
                        CurrentRegion <- Parser.toRegion (RegionMap.FindRegion(x, y, z)).Name
                        CurrentSystem <- setCurrentSystem CurrentSystem jump.SystemAddress
                        GeoBodies |> updateUI this Core Settings CurrentSystem CodexUnlocks

                | :? Location as location ->
                    // Update current system when our location is updated, then update the UI
                    let struct (x, y, z) = location.StarPos
                    CurrentRegion <- Parser.toRegion (RegionMap.FindRegion(x, y, z)).Name
                    CurrentSystem <- setCurrentSystem CurrentSystem location.SystemAddress
                    GeoBodies |> updateUI this Core Settings CurrentSystem CodexUnlocks

                | _ -> ()

        member this.LogMonitorStateChanged args =

            // If data is being batch read (Read All), then wait until it's done before updating the UI
            if LogMonitorStateChangedEventArgs.IsBatchRead args.NewState then
                Core.ClearGrid(this, buildNullRow)
            elif LogMonitorStateChangedEventArgs.IsBatchRead args.PreviousState then
                GeoBodies |> updateUI this Core Settings CurrentSystem CodexUnlocks
                let serialized = serializeCodexUnlocks CodexUnlocks
                deserializeCodexUnlocks serialized |> ignore


        member this.Name with get() = "GeoPredictor"
        member this.Version with get() = Assembly.GetCallingAssembly().GetName().Version.ToString()
        member this.PluginUI with get() = UI

        member this.Settings 
            with get() = Settings
            and set(settings) = 
                Settings <- settings :?> Settings

                // Update the UI if a setting that requires it has been changed by subscribing to event
                Settings.NeedsUIUpdate.Add(fun () -> GeoBodies |> updateUI this Core Settings CurrentSystem CodexUnlocks)
            
