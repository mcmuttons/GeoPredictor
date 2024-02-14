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
    let mutable (Core:IObservatoryCore) = null
    let mutable (UI:PluginUI) = null
    let GeoBodies = new Dictionary<string, BodyDetail>() 
    let ScannedBodies = new Dictionary<string, ScannedBody>()
    let GridCollection = new ObservableCollection<obj>()

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
                        ScannedBodies.Add (
                            scan.BodyName,
                            { Name = scan.BodyName; Materials = List.empty; Volcanism = scan.Volcanism; Temp = scan.SurfaceTemperature })
                    updateGrid this (buildGridRows GeoBodies.Values ScannedBodies.Values)

                | :? SAASignalsFound as signalsFound ->  
                    if not (GeoBodies.ContainsKey(signalsFound.BodyName)) then  
                        signalsFound.Signals 
                        |> Seq.filter (fun s -> s.Type = geoSignalType)
                        |> Seq.iter (fun s ->
                            GeoBodies.Add (
                                signalsFound.BodyName,
                                { Name = signalsFound.BodyName; Count = s.Count; GeosFound = Map.empty} ))
                    updateGrid this (buildGridRows GeoBodies.Values ScannedBodies.Values)

                | _ -> ()

        member this.LogMonitorStateChanged args =
            match (LogMonitorStateChangedEventArgs.IsBatchRead args.NewState) with
                | true -> ()
                | false -> updateGrid this (buildGridRows GeoBodies.Values ScannedBodies.Values)

        member this.get_Name () = "GeoPredictor"
        member this.get_Version () = version
        member this.get_PluginUI () = UI
        member this.get_Settings () = ()
        member this.set_Settings settings = ()

