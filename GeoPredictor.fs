namespace GeoPredictor

open Observatory.Framework
open Observatory.Framework.Files.Journal
open Observatory.Framework.Interfaces
open System.Collections.ObjectModel
open System.Reflection

type GeoDetail = { Type:string; Scanned:bool }
type BodyDetail = { Name:string; Count:int; GeosFound:Map<string, GeoDetail> }
type Material = { Name:string; Percent:float32}
type ScannedBody = { Name:string; Volcanism:string; Temp:float32 }
type BodyId = { BodyName:string; SystemAddress:uint64 }

type GeoRow = { Body:string; Count:string; Type:string; Volcanism:string; Temp:string }

type Worker() =

    // mutable
    let mutable (Core:IObservatoryCore) = null
    let mutable (UI:PluginUI) = null
    let mutable currentSystem = 0UL
    let mutable GeoBodies = Map.empty 
    let mutable ScannedBodies = Map.empty
    let mutable GridCollection = ObservableCollection<obj>()

    // immutable
    let geoSignalType = "$SAA_SignalType_Geological;"
    let version = Assembly.GetCallingAssembly().GetName().Version.ToString()         
   
    let buildGridRow bodyId body (geoBodies:Map<BodyId,BodyDetail>) =
        match geoBodies.TryFind bodyId with
        | Some detail -> 
            { Body = body.Name;
              Count = detail.Count.ToString();
              Type = "Some fucken geology, I dunno";
              Volcanism = body.Volcanism;
              Temp = (floor body.Temp).ToString() + "K" }  
        | None ->
            { Body = "Failed to link body to details!"; Count = ""; Type = ""; Volcanism = ""; Temp = "" }                

    let buildGridRows currentSystem geoBodies scannedBodies =
        scannedBodies
        |> Map.filter (fun k _ -> k.SystemAddress = currentSystem)
        |> Map.map (fun id body -> buildGridRow id body geoBodies)

    let buildNullRow = { Body = null; Count = null; Type = null; Volcanism = null; Temp = null }
    let buildHeaderRow = { Body = "GeoPredictor v" + version; Count = ""; Type = ""; Volcanism = ""; Temp = "" }

    let setCurrentSystem oldSystem newSystem = 
        match oldSystem = 0UL || oldSystem <> newSystem with
            | true -> newSystem
            | false -> oldSystem

    let updateGrid worker (core:IObservatoryCore) (gridRows:Map<_,_>) =
        match core.IsLogMonitorBatchReading with
            | true -> ()
            | false ->
                core.ClearGrid(worker, buildNullRow)
                core.AddGridItem(worker, buildHeaderRow)
                core.AddGridItems(worker, Seq.cast(gridRows.Values))

    let isNotNullOrEmpty string =
        match string with
            | null -> false
            | "" -> false
            | _ -> true

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
                            ScannedBodies <- ScannedBodies.Add(
                                { BodyName = scan.BodyName; SystemAddress = currentSystem },
                                { Name = scan.BodyName; Volcanism = scan.Volcanism; Temp = scan.SurfaceTemperature })
                            buildGridRows currentSystem GeoBodies ScannedBodies |> updateGrid this Core
                        | false -> ()

                | :? SAASignalsFound as signalsFound ->  
                    signalsFound.Signals 
                    |> Seq.filter (fun s -> s.Type = geoSignalType)
                    |> Seq.iter (fun s ->
                        GeoBodies <- GeoBodies.Add (
                            { BodyName = signalsFound.BodyName; SystemAddress = signalsFound.SystemAddress },
                            { Name = signalsFound.BodyName; Count = s.Count; GeosFound = Map.empty} ))
                    buildGridRows currentSystem GeoBodies ScannedBodies |> updateGrid this Core

                | :? FSDJump as jump ->
                    if not ((jump :? CarrierJump) && (not (jump :?> CarrierJump).Docked)) then 
                        currentSystem <- setCurrentSystem currentSystem jump.SystemAddress
                        buildGridRows currentSystem GeoBodies ScannedBodies |> updateGrid this Core
                                

                | :? Location as location ->
                    currentSystem <- setCurrentSystem currentSystem location.SystemAddress
                    buildGridRows currentSystem GeoBodies ScannedBodies |> updateGrid this Core

                | _ -> ()

        member this.LogMonitorStateChanged args =
            if LogMonitorStateChangedEventArgs.IsBatchRead args.NewState then
                Core.ClearGrid(this, buildNullRow)
            elif LogMonitorStateChangedEventArgs.IsBatchRead args.PreviousState then
                buildGridRows currentSystem GeoBodies ScannedBodies |> updateGrid this Core

        member this.get_Name () = "GeoPredictor"
        member this.get_Version () = version
        member this.get_PluginUI () = UI
        member this.get_Settings () = ()
        member this.set_Settings settings = ()

