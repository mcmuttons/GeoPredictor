namespace GeoPredictor

open Observatory.Framework
open Observatory.Framework.Files.Journal
open Observatory.Framework.Interfaces
open Predictor
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Reflection

type BodyDetail = { Name:string; Count:int; GeosFound:Map<string, GeoDetail> }
type Material = { Name:string; Percent:float}
type ScannedBody = { Name:string; Materials:seq<Material>; Volcanism:string; Temp:float }

type Worker() =
    let mutable (Core:IObservatoryCore) = null
    let mutable (UI:PluginUI) = null
    let GeoBodies = new Dictionary<string, BodyDetail>() 
    let SystemBodies = new Dictionary<string, ScannedBody>()

    let GridCollection = new ObservableCollection<obj>()

    let geoSignalType = "$SAA_SignalType_Geological;"
            
    let BuildGridRows (bodyDetails:seq<BodyDetail>) =
        bodyDetails |>
            Seq.map (fun d -> 
                GeoRow(
                    d.Name, 
                    d.Count.ToString() , 
                    "Some fucken geology, I dunno", 
                    "Bomb ass methane volcanism", 
                    "Frickin' cold!"))

    interface IObservatoryWorker with 
        member this.Load core = 
            Core <- core
            
            GridCollection.Add(GeoRow())
            UI <- PluginUI(GridCollection)
                
            ()

        member this.JournalEvent event =
            match (event:JournalBase) with                
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
                    let rows = 
                        BuildGridRows GeoBodies.Values
                        |> Seq.cast

                    Core.AddGridItems(this, rows)
            ()

        member this.get_Name () = "GeoPredictor"
        member this.get_Version () = Assembly.GetCallingAssembly().GetName().Version.ToString()
        member this.get_PluginUI () = UI
        member this.get_Settings () = ()
        member this.set_Settings settings = ()

