namespace GeoPredictor

open Observatory.Framework
open Observatory.Framework.Files.Journal
open Observatory.Framework.Files.ParameterTypes
open Observatory.Framework.Interfaces
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Reflection

type GeoDetail = { Type:string; Scanned:bool }
type BodyDetail = { Name:string; Count:int; GeosFound:Map<string, GeoDetail> }
type Material = { Name:string; Percent:float32}
type ScannedBody = { Name:string; Materials:Material list; Volcanism:string; Temp:float32 }

type GeoRow (body, count, geoType, volcanism, temp) =
    member val Body = body
    member val Count = count
    member val Type = geoType
    member val Volcanism = volcanism
    member val Temp = temp

    new() = GeoRow("", "", "", "", "")

type Worker() =
    let mutable (Core:IObservatoryCore) = null
    let mutable (UI:PluginUI) = null
    let GeoBodies = new Dictionary<string, BodyDetail>() 
    let ScannedBodies = new Dictionary<string, ScannedBody>()
    let GridCollection = new ObservableCollection<obj>()

    let geoSignalType = "$SAA_SignalType_Geological;"
            
   
    let BuildGridRows (bodyDetails:BodyDetail seq) scannedBodies =
        bodyDetails
        |> Seq.map (fun d ->
            match scannedBodies |> Seq.tryFind(fun b -> b.Name = d.Name) with
            | Some b ->
                GeoRow(
                    d.Name, 
                    d.Count.ToString(),
                    "Some fucken geology, I dunno",
                    b.Volcanism,
                    (floor b.Temp).ToString() + "K")
            | None ->
                GeoRow(
                    d.Name,
                    d.Count.ToString(),
                    "Failed to find matching scanned planet!",
                    "",
                    ""))
                
    let convertMaterialsToRecords (materials:MaterialComposition seq) =
        materials
        |> Seq.map (fun m -> { Name = m.Name; Percent = m.Percent })
        |> Seq.toList

    interface IObservatoryWorker with 
        member this.Load core = 
            Core <- core
            
            GridCollection.Add(GeoRow())
            UI <- PluginUI(GridCollection)
                
            ()

        member this.JournalEvent event =
            match (event:JournalBase) with 
                | :? Scan as scan ->
                    if not (ScannedBodies.ContainsKey(scan.BodyName)) then
                        ScannedBodies.Add (
                            scan.BodyName,
                            { Name = scan.BodyName; Materials = List.empty; Volcanism = scan.Volcanism; Temp = scan.SurfaceTemperature })

                | :? SAASignalsFound as signalsFound ->  
                    if not (GeoBodies.ContainsKey(signalsFound.BodyName)) then  
                        signalsFound.Signals 
                        |> Seq.filter (fun s -> s.Type = geoSignalType)
                        |> Seq.iter (fun s ->
                            GeoBodies.Add (
                                signalsFound.BodyName,
                                { Name = signalsFound.BodyName; Count = s.Count; GeosFound = Map.empty} ))
                    () 
                | _ -> ()

        member this.LogMonitorStateChanged args =
            Core.ClearGrid(this, GeoRow())

            if not (LogMonitorStateChangedEventArgs.IsBatchRead args.NewState) && not Core.IsLogMonitorBatchReading then 
                if not Core.IsLogMonitorBatchReading then
                    let rows = BuildGridRows GeoBodies.Values ScannedBodies.Values                      

                    Core.AddGridItems(this, Seq.cast(rows))
            ()

        member this.get_Name () = "GeoPredictor"
        member this.get_Version () = Assembly.GetCallingAssembly().GetName().Version.ToString()
        member this.get_PluginUI () = UI
        member this.get_Settings () = ()
        member this.set_Settings settings = ()

