﻿namespace GeoPredictor

open Observatory.Framework
open Observatory.Framework.Interfaces
open System.Reflection

module GridBuilder =

    // An row of data to be displayed in the UI
    type UIOutputRow = { 
        [<ColumnSuggestedWidth(250)>]
        Body:string;
        
        [<ColumnSuggestedWidth(85)>]
        Count:string; 

        [<ColumnSuggestedWidth(85)>]
        Found:string; 

        [<ColumnSuggestedWidth(250)>]
        Type:string; 

        [<ColumnSuggestedWidth(150)>]
        BodyType: string; 

        [<ColumnSuggestedWidth(300)>]
        Materials: string; 

        [<ColumnSuggestedWidth(250)>]
        Volcanism:string; 

        [<ColumnSuggestedWidth(85)>]
        Temp:string; 

        [<ColumnSuggestedWidth(100)>]
        Region:string }

    // Null row for initializing the UI
    let nullRow = { Body = null; Count = null; Found = null; Type = null; BodyType = null; Materials = null; Volcanism = null; Temp = null; Region = null }
    let emptyRow = { Body = ""; Count = ""; Found = ""; Type = ""; BodyType = ""; Materials = ""; Volcanism = ""; Temp = ""; Region = "" }

    // Version for output
    let externalVersion = "GeoPredictor " + Assembly.GetExecutingAssembly().GetName().Version.ToString(3)

    // Symbols for output
    let predictionSuccess = "\u2714"    // Heavy check mark
    let predictionUnknown = "\u2754"    // White question mark
    let predictionFailed = "\u274C"     // Red X
    let newCodexEntry = "\U0001F537"    // Blue diamond
    let warning = "\u2757"              // Red exclamation mark

 
    // Filter bodies to those with registered comp. scans
    let filterForShowOnlyWithScans onlyScans bodies =
        match onlyScans with
        | false -> bodies
        | true -> bodies |> Map.filter(fun _ b -> b.GeosFound |> Map.values |> Seq.contains Matched || b.GeosFound |> Map.values |> Seq.contains Surprise)

    // Filter bodies to those in current system
    let filterForShowOnlyCurrentSys onlyCurrent currentSys bodies =
        match onlyCurrent with
        | false -> bodies
        | true -> bodies |> Map.filter(fun id _ -> id.SystemAddress = currentSys)

    // Filter bodies to only those with failed predictions
    let filterForShowOnlyFailedPredictions onlyFailed bodies =
        match onlyFailed with
        | false -> bodies
        | true -> bodies |> Map.filter(fun _ b -> b.GeosFound |> Map.values |> Seq.contains Surprise)

    // Filter the bodies down to what should be shown in the UI
    let filterBodiesForOutput (settings:Settings) currentSys bodies =
        bodies
        |> filterForShowOnlyCurrentSys settings.OnlyShowCurrentSystem currentSys
        |> filterForShowOnlyWithScans settings.OnlyShowWithScans
        |> filterForShowOnlyFailedPredictions settings.OnlyShowFailedPredictionBodies

    // Build detail grid lines if there are geological scans
    let buildGeoDetailEntries codexUnlocks body =
        match body.GeosFound |> Map.isEmpty with
            | true -> []
            | false -> (
                body.GeosFound
                    |> Map.toList
                    |> List.map (fun (s,d) -> 
                        { emptyRow with                          
                            Body = body.BodyName; 
                            Found = 
                                match d with 
                                | Matched -> predictionSuccess 
                                | Predicted -> predictionUnknown
                                | CodexPredicted -> predictionUnknown + newCodexEntry
                                | Unmatched -> "" 
                                | Surprise -> predictionFailed                             
                            Type = Parser.toGeoSignalOut s; }))

    // Build a grid entry for a body, with detail entries if applicable
    let filterMaterialsForOutput (settings:Settings) materials =
        materials
        |> if settings.HideGrade1Materials then Seq.filter(fun m -> not (m.Grade = Grade1)) else id
        |> if settings.HideGrade2Materials then Seq.filter(fun m -> not (m.Grade = Grade2)) else id
        |> if settings.HideGrade3Materials then Seq.filter(fun m -> not (m.Grade = Grade3)) else id
        |> if settings.HideGrade4Materials then Seq.filter(fun m -> not (m.Grade = Grade4)) else id
        |> Seq.sortByDescending (fun m -> m.Grade)
        |> Seq.map (fun m -> sprintf "%s (%.1f%%)" (Parser.toMaterialOut settings.UseChemicalSymbols m.MaterialName) m.Percent)
        |> String.concat ", "

    let buildGridEntry settings codexUnlocks body =   
        let firstRow = 
            { emptyRow with
                Body = body.BodyName
                BodyType = Parser.toBodyTypeOut body.BodyType
                Count = 
                    match body.Count with 
                    | 0 -> "FSS/DSS" 
                    | _ when body.GeosFound.Values |> Seq.exists (fun p -> p = Surprise ) -> $"{body.Count} ({body.Count + 1}?)"
                    | _ when body.Count <> body.GeosFound.Count -> $"{body.Count} ({body.GeosFound.Count})"
                    | _ -> body.Count.ToString()
                Type = 
                    match body.GeosFound.Values |> Seq.exists (fun p -> p = Surprise) with
                    | true -> $"{warning}Possible additional geo{warning}"
                    | false -> ""
                Materials = body.Materials |> filterMaterialsForOutput settings
                Volcanism = Parser.toVolcanismOut body.Volcanism
                Temp = (floor (float body.Temp)).ToString() + "K"
                Region = Parser.toRegionOut body.Region }

        body
        |> buildGeoDetailEntries codexUnlocks
        |> List.append [firstRow]

    let firstRunMessage =
        [   "Click 'Read All' to update database!"
            "NOTE: This can take several"
            "minutes, but only needs to"
            "be done once!"
            ""
            "Go make some coffee."
            "I dunno."
            ""
            "ALSO NOTE: If your Elite game"
            "logs are incomplete, you might"
            "get false Codex positives. If"
            "you scan the geo again, it"
            "should be remembered :)" ]
        |> String.concat "\n"

    let buildGrid hasReadAllBeenRun currentCommander gridRows =
        let versionRow = Seq.singleton { emptyRow with Body = externalVersion; Type = "CMDR: " + currentCommander }
        let gridItems = 
            match hasReadAllBeenRun with
            | true -> gridRows
            | false -> Seq.singleton { emptyRow with Type = firstRunMessage } 

        Seq.append versionRow gridItems
