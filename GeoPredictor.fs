namespace GeoPredictor

open Observatory.Framework
open Observatory.Framework.Files.Journal
open Observatory.Framework.Interfaces
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Reflection

type GeoDetail = { Type:string; Scanned:bool }
type BodyDetail = { Name:string; Count:int; GeosFound:Map<string, GeoDetail> }
type Material = { Name:string; Percent:float32}
type ScannedBody = { Name:string; Materials:Material list; Volcanism:string; Temp:float32 }

type GeoRow = { Body:string; Count:string; Type:string; Volcanism:string; Temp:string }

type Worker() =

    // mutable
    let mutable (Core:IObservatoryCore) = null
    let mutable (UI:PluginUI) = null
    let mutable (currentSystem:uint64) = 0UL
    let mutable GeoBodies = Map.empty 
    let mutable ScannedBodies = Map.empty
    let mutable GridCollection = ObservableCollection<obj>()

    // immutable
    let geoSignalType = "$SAA_SignalType_Geological;"
    let version = Assembly.GetCallingAssembly().GetName().Version.ToString()         
   
    let buildGridRows (bodyDetails:BodyDetail seq) scannedBodies =
        bodyDetails
        |> Seq.map (fun d ->
            match scannedBodies |> Seq.tryFind(fun b -> b.Name = d.Name) with
            | Some b ->
                { Body = d.Name; 
                  Count = d.Count.ToString(); 
                  Type = "Some fucken geology, I dunno";
                  Volcanism = b.Volcanism;
                  Temp = (floor b.Temp).ToString() + "K" }
            | None ->
                { Body = d.Name; Count = d.Count.ToString(); Type = "Oh no! Unable to map geo signals to planetary data!"; Volcanism = ""; Temp = "" } )

    let buildHeaderRow = { Body = "GeoPredictor v" + version; Count = ""; Type = ""; Volcanism = ""; Temp = "" }

    let setCurrentSystem oldSystem newSystem = 
        match oldSystem = 0UL || oldSystem <> newSystem with
            | true -> newSystem
            | false -> oldSystem

    let updateGrid worker gridRows =
        match Core.IsLogMonitorBatchReading with
            | true -> ()
            | false ->
                Core.ClearGrid(worker, buildHeaderRow)
                Core.AddGridItems(worker, Seq.cast(gridRows))

    interface IObservatoryWorker with 
        member this.Load core = 
            Core <- core
            
            GridCollection.Add(buildHeaderRow)
            UI <- PluginUI(GridCollection)

        member this.JournalEvent event =
            match (event:JournalBase) with 
                | :? Scan as scan ->
                    if not (ScannedBodies.ContainsKey(scan.BodyName)) then
                        ScannedBodies <- ScannedBodies.Add (
                            scan.BodyName,
                            { Name = scan.BodyName; Materials = List.empty; Volcanism = scan.Volcanism; Temp = scan.SurfaceTemperature })
                        buildGridRows GeoBodies.Values ScannedBodies.Values |> updateGrid this

                | :? SAASignalsFound as signalsFound ->  
                    if not (GeoBodies.ContainsKey(signalsFound.BodyName)) then  
                        signalsFound.Signals 
                        |> Seq.filter (fun s -> s.Type = geoSignalType)
                        |> Seq.iter (fun s ->
                            GeoBodies <- GeoBodies.Add (
                                signalsFound.BodyName,
                                { Name = signalsFound.BodyName; 
                                  Count = s.Count; 
                                  GeosFound = Map.empty} ))
                        buildGridRows GeoBodies.Values ScannedBodies.Values |> updateGrid this

                | :? FSDJump as jump ->
                    match ((jump :? CarrierJump) && (not (jump :?> CarrierJump).Docked)) with
                        | true -> ()
                        | false -> 
                            currentSystem <- setCurrentSystem currentSystem jump.SystemAddress
                            buildGridRows GeoBodies.Values ScannedBodies.Values |> updateGrid this

                | :? Location as location ->
                    currentSystem <- setCurrentSystem currentSystem location.SystemAddress
                    buildGridRows GeoBodies.Values ScannedBodies.Values |> updateGrid this

                | _ -> ()

        member this.LogMonitorStateChanged args =
            if LogMonitorStateChangedEventArgs.IsBatchRead args.NewState then
                Core.ClearGrid(this, buildHeaderRow)
            elif LogMonitorStateChangedEventArgs.IsBatchRead args.PreviousState then
                buildGridRows GeoBodies.Values ScannedBodies.Values |> updateGrid this

        member this.get_Name () = "GeoPredictor"
        member this.get_Version () = version
        member this.get_PluginUI () = UI
        member this.get_Settings () = ()
        member this.set_Settings settings = ()

