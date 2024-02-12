namespace GeoPredictor

open Observatory.Framework
open Observatory.Framework.Files.Journal
open Observatory.Framework.Interfaces
open Predictor
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Reflection

type BodyId = { Id:int; Address:uint64 }
type BodyDetail = { Name:string; Count:int; GeosFound:Map<string, GeoDetail> }

type Worker() =
    let mutable (Core:IObservatoryCore) = null
    let mutable (UI:PluginUI) = null
    let GeoBodies = new Dictionary<BodyId, BodyDetail>() 

    let GridCollection = new ObservableCollection<obj>()

    let geoSignalType = "$SAA_SignalType_Geological"

    let UpdateUIGrid(worker) =
        if Core.IsLogMonitorBatchReading then ()
        else
            GeoBodies
            |> Seq.iter (fun kv -> 
                Core.AddGridItem(
                    worker, 
                    GeoRow(
                        kv.Value.Name, 
                        kv.Value.Count.ToString() , 
                        "Some fucken geology, I dunno", 
                        "Bomb ass methane volcanism", 
                        "Frickin' cold!")))       
            ()       

   
    interface IObservatoryWorker with 
        member this.Load core = 
            Core <- core
            
            GridCollection.Add(GeoRow())
            UI <- PluginUI(GridCollection)
                
            ()

        member this.JournalEvent event =
            match (event:JournalBase) with
                | :? SAASignalsFound as signalsFound ->  
                    let bodyId = { Id = signalsFound.BodyID; Address = signalsFound.SystemAddress }                            

                    if not (GeoBodies.ContainsKey(bodyId)) then       
                        GeoBodies.Add (
                            bodyId,
                            { Name = signalsFound.BodyName; Count = 69; GeosFound = Map.empty} )
                        //signalsFound.Signals
                        //|> Seq.map (fun s ->
                        //                GeoBodies.Add(
                        //                    bodyId,
                        //                    { Name = signalsFound.BodyName; 
                        //                      Count = s.Count
                        //                      GeosFound = Map.empty<string, GeoDetail> }))
                        //|> ignore
                    else
                        () 
                | _ -> ()

        member this.LogMonitorStateChanged args =
            if (LogMonitorStateChangedEventArgs.IsBatchRead args.NewState) then 
                Core.ClearGrid(this, GeoRow())
            else UpdateUIGrid(this)
            ()

        member this.get_Name () = "GeoPredictor"
        member this.get_Version () = Assembly.GetCallingAssembly().GetName().Version.ToString()
        member this.get_PluginUI () = UI
        member this.get_Settings () = ()
        member this.set_Settings settings = ()

