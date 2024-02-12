module Predictor

open Observatory.Framework.Files.ParameterTypes

type GeoRow (body, count, geoType, volcanism, temp) =
    member val Body = body
    member val Count = count
    member val Type = geoType
    member val Volcanism = volcanism
    member val Temp = temp

    new() = GeoRow("", "", "", "", "")

type GeoDetail = { Type:string; Scanned:bool }

let private createSignal (signal:Signal) =
    GeoRow("Some body to love", signal.Count.ToString(), signal.Type, "Bomb ass methane volcanism", "Too fucking hot!")

let parseSignals signals =
    signals
    |> Seq.toList
    |> List.map createSignal