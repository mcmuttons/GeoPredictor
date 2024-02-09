namespace GeoPredictor

open Observatory.Framework
open Observatory.Framework.Files.Journal
open Observatory.Framework.Interfaces
open System.Collections.ObjectModel
open System.Reflection

type GeoGrid (body, count, volcanism, temp) =
    member val Body = body
    member val Count = count
    member val Volcanism = volcanism
    member val Temp = temp

    new() = GeoGrid("", "", "", "")

type Worker() =
    let mutable (Core:IObservatoryCore) = null
    let (GridCollection:ObservableCollection<obj>) = ObservableCollection<obj>()
    let mutable (UI:PluginUI) = null

    interface IObservatoryWorker with 
        member this.Load core = 
            Core <- core
            
            GridCollection.Add(GeoGrid())
            UI <- PluginUI(GridCollection)
                
            ()

        member this.JournalEvent event =
            match (event:JournalBase) with
                | _ -> ()


        member this.get_Name () = "GeoPredictor"
        member this.get_Version () = Assembly.GetCallingAssembly().GetName().Version.ToString()
        member this.get_PluginUI () = UI
        member this.get_Settings () = ()
        member this.set_Settings settings = ()
        member this.StatusChange status = ()
        member this.LogMonitorStateChanged args = ()

        // Deprecated, but needed for explicit interface
        member this.ReadAllStarted () = ()
        member this.ReadAllFinished () = ()

