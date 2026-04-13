namespace GeoPredictor

open Observatory.Framework
open Observatory.Framework.Interfaces
open System.Reflection

module GridBuilder =

    // An row of data to be displayed in the UI
    type UIOutputRow = { 
        [<ColumnSuggestedWidth(250)>]
        Body:string;
        
        [<ColumnSuggestedWidth(85)>]
        Signals:string; 

        [<ColumnSuggestedWidth(500)>]
        Details:string; 
    }

    // Null row for initializing the UI
    let nullRow = { Body = null; Signals = null; Details = null }
    let emptyRow = { Body = ""; Signals = ""; Details = "" }

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
                            Body = "" // body.BodyName; 
                            Signals = 
                                match d with 
                                | Matched -> predictionSuccess 
                                | Predicted -> predictionUnknown
                                | CodexPredicted -> predictionUnknown + newCodexEntry
                                | Unmatched -> "" 
                                | Surprise -> predictionFailed                             
                            Details = Parser.toGeoSignalOut s; }))

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

    let firstRow body = 
        { emptyRow with
            Body = body.BodyName
            Signals = 
                match body.Count with 
                | 0 -> "FSS/DSS" 
                | _ when body.GeosFound.Values |> Seq.exists (fun p -> p = Surprise ) -> $"{body.Count} ({body.Count + 1}?)"
                | _ when body.Count <> body.GeosFound.Count -> $"{body.Count} ({body.GeosFound.Count})"
                | _ -> body.Count.ToString()
            Details = 
                sprintf "%s body with %s" (Parser.toBodyTypeOut body.BodyType) (Parser.toVolcanismOut body.Volcanism)
        }
                
    let addMaterialsRow (body:GeoBody) (settings:Settings) firstRow =
        let materials = body.Materials |> filterMaterialsForOutput settings
        match materials with
        | "" -> [firstRow]
        | _ ->  [firstRow; { emptyRow with Body = ""; Signals = ""; Details = $"Materials: {materials}" }]
         
    let addWarningRow body rows =
        match body.GeosFound.Values |> Seq.exists (fun p -> p = Surprise) with
        | false -> rows
        | true -> rows |> List.append [{ emptyRow with Body = ""; Signals = warning; Details = $"{warning}Possible additional geo{warning}" }]

    let buildGridEntry settings codexUnlocks body =   
        body
        |> buildGeoDetailEntries codexUnlocks
        |> List.append (
            firstRow body 
            |> addMaterialsRow body settings 
            |> addWarningRow body)

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
        let versionRow = { emptyRow with Body = externalVersion; Details = "CMDR: " + currentCommander }
        let separatorRow = { emptyRow with Body = "--------------------"; Signals = ""; Details = "-----------------------------" }
        let gridItems = 
            match hasReadAllBeenRun with
            | true -> gridRows
            | false -> Seq.singleton { emptyRow with Details = firstRunMessage } 

        Seq.append [versionRow; separatorRow] gridItems
