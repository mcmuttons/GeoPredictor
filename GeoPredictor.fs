namespace GeoPredictor

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Observatory.Framework
open Observatory.Framework.Files.Journal
open Observatory.Framework.Interfaces
open System.Collections.ObjectModel
open System.Reflection
open EliteDangerousRegionMap

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

    let mutable InternalSettings =                              // Initialize internal settings that don't get exposed to Observatory
        { HasReadAllBeenRun = false }

    // Immutable internal values
    let geoSignalType = "$SAA_SignalType_Geological;"                       // Journal value for a geological signal
    let codexUnlocksFileName = "GeoPredictor-CodexUnlocks.json"             // Filename to save codex status in
    let internalSettingsFileName = "GeoPredictor-InternalSettings.json"     // Filename to save internal settings in

    // Update current system if it has changed
    let setCurrentSystem oldSystem newSystem = 
        match oldSystem = 0UL || oldSystem <> newSystem with
            | true -> newSystem
            | false -> oldSystem

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
            

    // Interface for interop with Observatory, and entry point for the DLL.
    // The goal has been to keep all mutable operations within this scope to isolate imperative code as much as
    // possible. 
    interface IObservatoryWorker with  
    
        // Initialize interop and UI
        member this.Load core = 
            Core <- core
            
            GridCollection.Add(UIUpdater.buildNullRow)
            UI <- PluginUI(GridCollection)

            CodexUnlocks <- FileSerializer.deserializeFromFile Set.empty Core.PluginStorageFolder codexUnlocksFileName FileSerializer.deserializeCodexUnlocks
            InternalSettings <- FileSerializer.deserializeFromFile InternalSettings Core.PluginStorageFolder internalSettingsFileName FileSerializer.deserializeInteralSettings


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

                        GeoBodies |> UIUpdater.updateUI this Core Settings InternalSettings.HasReadAllBeenRun CurrentSystem CodexUnlocks

                | :? SAASignalsFound as sigs ->                   
                    // When signals are discovered through DSS, save/update them if they're geology and update the UI, display a notification
                    sigs.Signals 
                    |> Seq.filter (fun s -> s.Type = geoSignalType)
                    |> Seq.iter (fun s ->
                        let id = { SystemAddress = sigs.SystemAddress; BodyId = sigs.BodyID }
                        GeoBodies <- GeoBodies.Add(id, buildSignalCountBody id sigs.BodyName s.Count CurrentRegion GeoBodies))

                    GeoBodies |> UIUpdater.updateUI this Core Settings InternalSettings.HasReadAllBeenRun CurrentSystem CodexUnlocks

                | :? CodexEntry as codexEntry ->
                    // When something is scanned with the comp. scanner, save/update the result if it's geological, then update the UI
                    match Parser.geoTypes |> List.tryFind (fun t -> t = codexEntry.Name) with
                    | Some _ -> 
                        let id = { BodyId = codexEntry.BodyID; SystemAddress = codexEntry.SystemAddress }
                        let signal = Parser.toGeoSignalFromJournal codexEntry.Name
                        let region = Parser.toRegion codexEntry.Region

                        if codexEntry.IsNewEntry then
                            CodexUnlocks <- CodexUnlocks |> Set.add { Signal = signal; Region = region }
                            CodexUnlocks |> FileSerializer.serializeToFile Core codexUnlocksFileName FileSerializer.serializeCodexUnlocks 

                        GeoBodies <- GeoBodies.Add(id, buildFoundDetailBody id signal region GeoBodies)
                        GeoBodies |> UIUpdater.updateUI this Core Settings InternalSettings.HasReadAllBeenRun CurrentSystem CodexUnlocks
                    | None -> ()

                | :? FSDJump as jump ->
                    // Update current system after an FSD jump, then update the UI
                    if not ((jump :? CarrierJump) && (not (jump :?> CarrierJump).Docked)) then 
                        let struct (x, y, z) = jump.StarPos
                        CurrentRegion <- Parser.toRegion (RegionMap.FindRegion(x, y, z)).Name
                        CurrentSystem <- setCurrentSystem CurrentSystem jump.SystemAddress
                        GeoBodies |> UIUpdater.updateUI this Core Settings InternalSettings.HasReadAllBeenRun CurrentSystem CodexUnlocks

                | :? Location as location ->
                    // Update current system when our location is updated, then update the UI
                    let struct (x, y, z) = location.StarPos
                    CurrentRegion <- Parser.toRegion (RegionMap.FindRegion(x, y, z)).Name
                    CurrentSystem <- setCurrentSystem CurrentSystem location.SystemAddress
                    GeoBodies |> UIUpdater.updateUI this Core Settings InternalSettings.HasReadAllBeenRun CurrentSystem CodexUnlocks

                | _ -> ()

        member this.LogMonitorStateChanged args =
            if args.NewState.HasFlag(LogMonitorState.Batch) then                // started read all
                Core.ClearGrid(this, UIUpdater.buildNullRow)
            elif args.NewState.HasFlag(LogMonitorState.PreRead) then ()         // started preread
            elif args.PreviousState.HasFlag(LogMonitorState.PreRead) then       // finished preread
                GeoBodies |> UIUpdater.updateUI this Core Settings InternalSettings.HasReadAllBeenRun CurrentSystem CodexUnlocks
            elif args.PreviousState.HasFlag(LogMonitorState.Batch) then         // finished read all
                InternalSettings <- { InternalSettings with HasReadAllBeenRun = true }
                GeoBodies |> UIUpdater.updateUI this Core Settings InternalSettings.HasReadAllBeenRun CurrentSystem CodexUnlocks
                CodexUnlocks |> FileSerializer.serializeToFile Core codexUnlocksFileName FileSerializer.serializeCodexUnlocks
                InternalSettings |> FileSerializer.serializeToFile Core internalSettingsFileName FileSerializer.serializeInternalSettings


        member this.Name with get() = "GeoPredictor"
        member this.Version with get() = Assembly.GetCallingAssembly().GetName().Version.ToString()
        member this.PluginUI with get() = UI

        member this.Settings 
            with get() = Settings
            and set(settings) = 
                Settings <- settings :?> Settings

                // Update the UI if a setting that requires it has been changed by subscribing to event
                Settings.NeedsUIUpdate.Add(fun () -> GeoBodies |> UIUpdater.updateUI this Core Settings InternalSettings.HasReadAllBeenRun CurrentSystem CodexUnlocks)
            
