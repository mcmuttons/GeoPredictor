namespace GeoPredictor

open Observatory.Framework.Interfaces

module UIUpdater =

    // An row of data to be displayed in the UI
    type UIOutputRow = { Body:string; Count:string; Found:string; Type:string; BodyType: string; Volcanism:string; Temp:string; Region:string }

    // Null row for initializing the UI
    let buildNullRow = { Body = null; Count = null; Found = null; Type = null; BodyType = null; Volcanism = null; Temp = null; Region = null }
    let emptyRow = { Body = ""; Count = ""; Found = ""; Type = ""; BodyType = ""; Volcanism = ""; Temp = ""; Region = "" }

    // Version for output
    let externalVersion = "GeoPredictor v1.4"

    // Symbols for output
    let predictionSuccess = "\u2714"                                        // Heavy check mark
    let predictionUnknown = "\u2754"                                        // White question mark
    let predictionFailed = "\u274C"                                         // Red X
    let newCodexEntry = "\U0001F537"                                        // Blue diamond

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
                        {   Body = body.Name; 
                            BodyType = ""; 
                            Count = ""; 
                            Found = 
                                match d with 
                                | Matched -> predictionSuccess 
                                | Predicted -> predictionUnknown
                                | CodexPredicted -> predictionUnknown + newCodexEntry
                                | Unmatched -> "" 
                                | Surprise -> predictionFailed                             
                            Type = Parser.toGeoSignalOut s; 
                            Volcanism = ""; 
                            Temp = "";
                            Region = ""}))

    // Build a grid entry for a body, with detail entries if applicable
    let buildGridEntry codexUnlocks body =   
        let firstRow = {
            Body = body.Name;
            BodyType = Parser.toBodyTypeOut body.BodyType;
            Count = 
                match body.Count with 
                | 0 -> "FSS/DSS" 
                | _ -> body.Count.ToString();
            Found = ""
            Type = "";
            Volcanism = Parser.toVolcanismOut body.Volcanism;
            Temp = (floor (float body.Temp)).ToString() + "K"
            Region = Parser.toRegionOut body.Region}

        body
        |> buildGeoDetailEntries codexUnlocks
        |> List.append [firstRow]

    let firstRunMessage =
        [   "Click 'Read All', please!"
            "NOTE: This can take several"
            "minutes, but only needs to"
            "be done once!"
            ""
            "Go make some coffee."
            "I dunno."
            ""
            "ALSO NOTE: If you're missing"
            "logs, you might get false"
            "Codex positives. If you scan"
            "those, it should remember"
            "from then on. :)" ]
        |> String.concat "\n"

    // Repaint the UI
    let updateGrid worker (core:IObservatoryCore) hasReadAllBeenRun gridRows =
        match core.IsLogMonitorBatchReading with
            | true -> ()
            | false ->
                core.ClearGrid(worker, buildNullRow)
                core.AddGridItem(worker, { emptyRow with Body = externalVersion })
                if not hasReadAllBeenRun then
                    core.AddGridItem(worker, { emptyRow with Type = firstRunMessage })
                core.AddGridItems(worker, Seq.cast(gridRows))

    // Filter bodies for display, turn them into a single list of entries, then update the UI
    let updateUI worker core settings hasReadAllBeenRun currentSys codexUnlocks bodies = 
        bodies 
        |> filterBodiesForOutput settings currentSys
        |> Seq.collect (fun body -> buildGridEntry codexUnlocks body.Value)
        |> updateGrid worker core hasReadAllBeenRun
