namespace GeoPredictor

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Observatory.Framework
open Observatory.Framework.Files.Journal
open Observatory.Framework.Interfaces
open System
open System.Collections.ObjectModel
open System.Reflection
open EliteDangerousRegionMap

type Notification = { Title:string; Verbose:string; Terse: string }
type System = { ID:uint64; Name:string }

type WorkerState = {
    Core: IObservatoryCore                      // For interacting with Observatory Core    
    UI: PluginUI                                // For updating the Observatory UI
    GridCollection: ObservableCollection<obj>   // For initializing UI grid
    CurrentSystem: System                       // ID of the system we're currently in
    CurrentRegion: Region                       // Region we're currently in
    CurrentCommander: string                    // Commander name for the current journal file
    GeoBodies: Map<BodyId, GeoBody>             // Map of all scanned bodies since the Observatory session began
    CodexUnlocks: Map<string, Set<CodexUnit>>   // Set of all codex entries unlocked so far
    Settings: Settings                          // Settings for Observatory
    IsValidEliteVersion: bool                   // Is the log from Odyssey or not
    InternalSettings: InternalSettings          // Internal settings that don't get exposed to Observatory
}

type Worker() =

    let mutable State = {
        Core = null :> IObservatoryCore
        UI = null :> PluginUI
        GridCollection = ObservableCollection<obj>() 
        CurrentSystem = { ID = 0UL; Name = "" }
        CurrentRegion = UnknownRegion "Uninitialized"
        CurrentCommander = ""
        GeoBodies = Map.empty
        CodexUnlocks = Map.empty
        Settings = new Settings()
        IsValidEliteVersion = false
        InternalSettings = { HasReadAllBeenRun = false; Version = Version(0,0) }
    }

    // Immutable internal values
    let geoSignalType = "$SAA_SignalType_Geological;"                       // Journal value for a geological signal
    let codexUnlocksFileName = "GeoPredictor-CodexUnlocks.json"             // Filename to save codex status in
    let internalSettingsFileName = "GeoPredictor-InternalSettings.json"     // Filename to save internal settings in
    let settingsVersion = Version(1,0)                                      // Version of the settings file

    // Update current system if it has changed
    let setCurrentSystem oldSys newSys = 
        match oldSys.ID = 0UL || oldSys.ID <> newSys.ID with
            | true -> newSys
            | false -> oldSys

    let getPredictedGeos bodyType volcanism region codexUnlocks =
        Predictor.getGeologyPredictions bodyType volcanism
        |> List.map (fun p -> 
            p, 
            match codexUnlocks |> Set.contains { Signal = p; Region = region } with
            | true -> Predicted 
            | false -> CodexPredicted)
        |> Map.ofList   

    // If a body already exists, update its details with name, volcanism and temperature, otherwise create a new body    
    let buildScannedBody id (scan:Scan) region system codexUnlocks bodies =
        let shortName = 
            scan.BodyName 
            |> Parser.replace system.Name "" 
            |> Parser.trim
        let bodyType = Parser.toBodyType scan.PlanetClass
        let volcanism = Parser.toVolcanism scan.Volcanism
        let temp = scan.SurfaceTemperature * 1.0f<K>
        let materials = scan.Materials |> Seq.map (fun m -> Parser.toMaterial (m.Percent * 1.0f<percent>) m.Name)
        let predictedGeos = getPredictedGeos bodyType volcanism region codexUnlocks

        match bodies |> Map.tryFind(id) with
        | Some body -> { 
            body with 
                BodyName = scan.BodyName; 
                ShortName = shortName; 
                BodyType = bodyType; 
                Volcanism = volcanism; 
                Temp = temp ; 
                GeosFound = if body.GeosFound.IsEmpty then predictedGeos else body.GeosFound;
                Materials = materials }
        | None -> { 
                BodyName = scan.BodyName; 
                ShortName = shortName; 
                BodyType = bodyType; 
                Volcanism = volcanism; 
                Temp = temp; 
                Count = 0; 
                GeosFound = predictedGeos; 
                Notified = false; 
                Region = region; 
                Materials = materials }

    // If a body already exists, update its count of geological signal, otherwise create a new body
    let buildSignalCountBody id name count region bodies =
        match bodies |> Map.tryFind(id) with
        | Some body -> { (body:GeoBody) with Count = count }
        | None -> { 
            BodyName = name; 
            ShortName = ""; 
            BodyType = BodyTypeNotYetSet; 
            Volcanism = Parser.toVolcanismNotYetSet; 
            Temp = 0f<K>; 
            Count = count; 
            GeosFound = Map.empty; 
            Notified = false; 
            Region = region; 
            Materials = Seq.empty }             

    // If a body already exists, and the type of geology has not already been scanned, add the geology; if no body, create a new one
    let buildFoundDetailBody id entry bodies =
        match bodies |> Map.tryFind(id) with
        | Some body ->
            match body.GeosFound |> Map.tryFind(entry.Signal) with
            | Some geo -> 
                match geo with
                | Predicted | CodexPredicted -> { body with GeosFound = body.GeosFound |> Map.add entry.Signal Matched }
                | _ -> body
            | None -> { body with GeosFound = body.GeosFound |> Map.add entry.Signal Surprise }
        | None -> { 
            BodyName = ""; 
            ShortName = ""; 
            BodyType = BodyTypeNotYetSet; 
            Volcanism = Parser.toVolcanismNotYetSet; 
            Temp = 0f<K>; 
            Count = 0; 
            GeosFound = Map.empty |> Map.add entry.Signal Unmatched; 
            Notified = false; 
            Region = entry.Region; 
            Materials = Seq.empty }

    // Set all bodies that have a signal/region combo set as CodexPredicted to Predicted
    // Used when a new Codex entry has been scanned, and all other finds of it should lose their codex predicted status
    let updateAllPredictedCodexEntriesForNewFind entry bodies =
        bodies |> Map.map (
            fun _ body -> 
                match body.GeosFound |> Map.tryFind(entry.Signal) with
                | Some p when p = CodexPredicted && body.Region = entry.Region -> { body with GeosFound = body.GeosFound |> Map.add entry.Signal Predicted }
                | _ -> body)
        
    
    // Format notification text for output
    let buildGeoPlanetNotification shortBody volcanism count bodyType =
        let volcanismLowerCase = (Parser.toVolcanismOut volcanism).ToLower()
        let bodyTypeText = Parser.toBodyTypeOut bodyType
        let title = match shortBody |> Parser.isNotNullOrEmpty with | true -> $"Body {shortBody}" | false -> "Geological Signals"

        match count = 0 with
        | true -> {
            Title = title;
            Verbose = $"{bodyTypeText} body with geological signals, and {volcanismLowerCase}. FSS or DSS for count."
            Terse = "Geological signals found" }
        | false -> { 
            Title = title; 
            Verbose = $"{bodyTypeText} body with {count} geological signals, and {volcanismLowerCase}."
            Terse = $"{count} geological signals found" }

    let buildNewCodexEntryNotification shortBody geosFound =
        let possibleNewGeosText = 
            geosFound             
            |> Map.filter (fun _ s -> s = CodexPredicted)
            |> Map.keys
            |> Seq.map (fun s -> Parser.toGeoSignalOut s)
            |> String.concat ", "

        {   Title = match shortBody |> Parser.isNotNullOrEmpty with | true -> $"Body {shortBody}" | false -> "New Codex Geo";
            Verbose = $"New geological Codex entries: {possibleNewGeosText}.";
            Terse = "New geological Codex entries." }
    
    // Build a notification for found geological signals
    let buildNotificationArgs bodyID verbose notification =
        NotificationArgs (
            Title = notification.Title,
            Sender = "GeoPredictor",
            CoalescingId = Nullable bodyID,
            ExtendedDetails = notification.Verbose,
            Detail = match verbose with | true -> notification.Verbose | false -> notification.Terse)
            

    // Check if the version supports modern signals
    let getIfValidEliteVersion (version:string) odyssey =
        match version with
        | null -> odyssey
        | _ -> version.StartsWith("4.") || odyssey
        
    
    //
    // Helpers for the interface functions that deal with mutable data
    //

    // Repaint the UI
    let updateGrid worker state gridRows =
        state.Core.ClearGrid(worker, UIUpdater.buildNullRow)
        state.Core.AddGridItem(worker, { UIUpdater.emptyRow with Body = UIUpdater.externalVersion; Type = "CMDR: " + state.CurrentCommander })
        if not state.InternalSettings.HasReadAllBeenRun then
            state.Core.AddGridItem(worker, { UIUpdater.emptyRow with Type = UIUpdater.firstRunMessage })
        else
            state.Core.AddGridItems(worker, Seq.cast(gridRows))

    // Filter bodies for display, turn them into a single list of entries, then update the UI
    let updateUI state worker =
        match state.Core.IsLogMonitorBatchReading with
            | true -> ()
            | false -> 
                state.GeoBodies
                |> UIUpdater.filterBodiesForOutput state.Settings state.CurrentSystem.ID
                |> Seq.collect (fun body -> UIUpdater.buildGridEntry state.Settings state.CodexUnlocks body.Value)
                |> updateGrid worker state


    let saveNewCodexUnlocks unlocks =
        State <- { State with CodexUnlocks = State.CodexUnlocks.Add(State.CurrentCommander, unlocks) }
        State.CodexUnlocks |> FileSerializer.serialize State.Core codexUnlocksFileName

    // Interface for interop with Observatory, and entry point for the DLL.
    // The goal has been to keep all mutable operations within this scope to isolate imperative code as much as
    // possible. 
    interface IObservatoryWorker with  
    
        // Initialize interop and UI
        member this.Load core = 
            let internalSettings, codexUnlocks =
                match FileSerializer.deserialize<InternalSettings> State.InternalSettings core.PluginStorageFolder internalSettingsFileName with
                | { HasReadAllBeenRun = false; Version = v } when v < settingsVersion ->
                    { HasReadAllBeenRun = false; Version = settingsVersion }, Map.empty
                | settings ->
                    settings, FileSerializer.deserialize<Map<string,Set<CodexUnit>>> Map.empty core.PluginStorageFolder codexUnlocksFileName

            let gridCollection = ObservableCollection<obj>()
            gridCollection.Add(UIUpdater.buildNullRow)

            State <- 
                { State with 
                    Core = core
                    InternalSettings = internalSettings
                    CodexUnlocks = codexUnlocks
                    GridCollection = gridCollection
                    UI = PluginUI(gridCollection)
                }
           
        // Handle journal events
        member this.JournalEvent event =
            match (event:JournalBase) with 
                | :? LoadGame as load ->
                    State <- 
                        { State with 
                            IsValidEliteVersion = getIfValidEliteVersion load.GameVersion load.Odyssey
                            CurrentCommander = load.Commander
                        }

                    this |> updateUI State
                    
                | :? FileHeader as newFile ->
                    // When a new file is being read, override the file version
                    State <- { State with IsValidEliteVersion = getIfValidEliteVersion newFile.GameVersion newFile.Odyssey }

                | :? Scan as scan ->                
                    // When a body is scanned (FSS, Proximity or NAV beacon), save/update it if it's landable and has volcanism and update the UI
                    if State.IsValidEliteVersion && scan.Landable && scan.Volcanism |> Parser.isNotNullOrEmpty then
                        let id = { SystemAddress = scan.SystemAddress; BodyId = scan.BodyID }

                        let unlocks = 
                            match State.CodexUnlocks |> Map.tryFind State.CurrentCommander with
                            | Some unlocks -> unlocks
                            | None -> Set.empty

                        let body = 
                            buildScannedBody 
                                id 
                                scan
                                State.CurrentRegion
                                State.CurrentSystem
                                unlocks
                                State.GeoBodies

                        let buildWorthMappingMessage systemAddress bodyID =
                            let message = ResizeArray<obj>()
                            message.Add("SetWorthVisiting")
                            message.Add(systemAddress)
                            message.Add(bodyID)
                            message.Add(true)
                            message
                            
                        match not body.Notified && State.Settings.NotifyOnGeoBody with
                        | true ->
                            State.Core.SendNotification(
                                buildGeoPlanetNotification body.ShortName body.Volcanism body.Count body.BodyType
                                |> buildNotificationArgs scan.BodyID State.Settings.VerboseNotifications) 
                            |> ignore

                            State <- { State with GeoBodies = State.GeoBodies.Add(id, { body with Notified = true }) }
                        | false -> 
                            State <- { State with GeoBodies = State.GeoBodies.Add(id, body) }

                        if body.GeosFound |> Map.exists (fun _ s -> s = CodexPredicted) && State.Settings.NotifyOnNewGeoCodex && not body.Notified then
                            State.Core.SendNotification(
                                buildNewCodexEntryNotification body.ShortName body.GeosFound
                                |> buildNotificationArgs scan.BodyID State.Settings.VerboseNotifications)
                            |> ignore

                            if State.Settings.NotifyEvaluatorOnNewGeoCodex then
                                State.Core.SendPluginMessage(this, "Evaluator", buildWorthMappingMessage scan.SystemAddress scan.BodyID)
                        else
                            if State.Settings.NotifyEvaluatorOnGeoBody then
                                State.Core.SendPluginMessage(this, "Evaluator", buildWorthMappingMessage scan.SystemAddress scan.BodyID)
                            
                        this |> updateUI State

                | :? SAASignalsFound as sigs ->                   
                    // When signals are discovered through DSS, save/update them if they're geology and update the UI, display a notification
                    if State.IsValidEliteVersion then
                        sigs.Signals 
                        |> Seq.filter (fun s -> s.Type = geoSignalType)
                        |> Seq.iter (fun s ->
                            let id = { SystemAddress = sigs.SystemAddress; BodyId = sigs.BodyID }
                            State <- { State with GeoBodies = State.GeoBodies.Add(id, buildSignalCountBody id sigs.BodyName s.Count State.CurrentRegion State.GeoBodies) } )

                        this |> updateUI State

                | :? CodexEntry as codexEntry ->
                    // When something is scanned with the comp. scanner, save/update the result if it's geological, then update the UI
                    match Parser.geoTypes |> List.tryFind (fun t -> t = codexEntry.Name) with
                    | Some _ -> 
                        let id = { BodyId = codexEntry.BodyID; SystemAddress = codexEntry.SystemAddress }
                        let entry = { Signal = Parser.toGeoSignalFromJournal codexEntry.Name; Region = Parser.toRegion codexEntry.Region }

                        if State.IsValidEliteVersion then
                            State <- { State with GeoBodies = State.GeoBodies.Add(id, buildFoundDetailBody id entry State.GeoBodies) }

                        // Check if the current commander already has this entry. If not, then add it.
                        match State.CodexUnlocks |> Map.tryFind State.CurrentCommander with
                        | Some unlocks -> 
                            match unlocks |> Set.contains entry with
                            | true -> ()
                            | false -> 
                                unlocks 
                                |> Set.add entry
                                |> saveNewCodexUnlocks
                                State <- { State with GeoBodies = State.GeoBodies |> updateAllPredictedCodexEntriesForNewFind entry }

                        | None -> 
                            Set.empty 
                            |> Set.add entry 
                            |> saveNewCodexUnlocks
                            State <- { State with GeoBodies = State.GeoBodies |> updateAllPredictedCodexEntriesForNewFind entry }
                    
                        this |> updateUI State
                    | None -> ()

                | :? FSDJump as jump ->
                    // Update current system after an FSD jump, then update the UI
                    if not ((jump :? CarrierJump) && (not (jump :?> CarrierJump).Docked)) then 
                        let struct (x, y, z) = (jump.StarPos.x, jump.StarPos.y, jump.StarPos.z)
                        State <- 
                            { State with 
                                CurrentRegion = Parser.toRegion (RegionMap.FindRegion(x, y, z)).Name
                                CurrentSystem = setCurrentSystem State.CurrentSystem { ID = jump.SystemAddress; Name = jump.StarSystem } 
                            }

                        this |> updateUI State

                | :? Location as location ->
                    // Update current system when our location is updated, then update the UI
                    let struct (x, y, z) = (location.StarPos.x, location.StarPos.y, location.StarPos.z)

                    State <-
                        { State with 
                            CurrentRegion = Parser.toRegion (RegionMap.FindRegion(x, y, z)).Name
                            CurrentSystem = setCurrentSystem State.CurrentSystem { ID = location.SystemAddress; Name = location.StarSystem }
                        }

                    this |> updateUI State

                | _ -> ()

        member this.LogMonitorStateChanged args =
            if args.NewState.HasFlag(LogMonitorState.Batch) then                // started read all
                State.Core.ClearGrid(this, UIUpdater.buildNullRow)
            elif args.NewState.HasFlag(LogMonitorState.PreRead) then ()         // started preread
            elif args.PreviousState.HasFlag(LogMonitorState.PreRead) then       // finished preread
                this |> updateUI State
            elif args.PreviousState.HasFlag(LogMonitorState.Batch) then         // finished read all
                State <- { State with InternalSettings = { State.InternalSettings with HasReadAllBeenRun = true } } 

                this |> updateUI State

                State.CodexUnlocks |> FileSerializer.serialize State.Core codexUnlocksFileName
                State.InternalSettings |> FileSerializer.serialize State.Core internalSettingsFileName


        member this.Name with get() = "GeoPredictor"
        member this.Version with get() = Assembly.GetExecutingAssembly().GetName().Version.ToString(3)
        member this.PluginUI with get() = State.UI
        member this.ColumnSorter with get() = Observatory.Framework.Sorters.NoOpColumnSorter()

        member this.AboutInfo with get() = AboutInfo (
            AuthorName = "mcmuttons",
            Description = "Predicts the type and number geological signals on a body after scanning, and tracks whether or not they're in your personal codex or not. Provides filterable info about which materials can be found on the bodies with geological signals to help surface prospecting.",
            ShortName = "GeoPredictor",
            FullName = "GeoPredictor",
            Links = ResizeArray<AboutLink> [ AboutLink ( "GeoPredictor on Github, where you can find documentation and the latest version", "https://github.com/mcmuttons/GeoPredictor" ) ]
            )       

        member this.Settings 
            with get() = State.Settings
            and set(settings) = 
                State <- { State with Settings = settings :?> Settings }

                // Update the UI if a setting that requires it has been changed by subscribing to event
                State.Settings.NeedsUIUpdate.Add(fun () -> this |> updateUI State)
            
