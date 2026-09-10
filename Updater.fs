namespace GeoPredictor

open System
open System.Net.Http
open System.IO

module Updater =  

    type VersionInfo = {
        Version:Version
        MinCoreVersion:Version
        DownloadUrl:string
    }

    type UpdateStatus =
    | NoUpdate
    | UpdateAvailable of versionInfo:VersionInfo


    let versionCheckUrl = "https://raw.githubusercontent.com/mcmuttons/GeoPredictor/refs/heads/master/version.json"

    let getNewestVersion (currentVersion:VersionInfo) (lastChecked:DateTime) =
            async {
                use http = new HttpClient()
                let! response = http.GetAsync(versionCheckUrl) |> Async.AwaitTask
                if response.IsSuccessStatusCode then
                    let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                    let versionInfo = Serializer.deserialize<VersionInfo> currentVersion content
                    return Some versionInfo
                else
                    return None
            }
    
    // Ahoy, here there be side effects!
    let downloadUpdate (versionInfo:VersionInfo) savePath =
        async {
            use http = new HttpClient()
            let! response = http.GetAsync(versionInfo.DownloadUrl) |> Async.AwaitTask 
            if response.IsSuccessStatusCode then
                let filename = versionInfo.DownloadUrl.Split('/') |> Array.last
                let filepath = Path.Combine(savePath, filename)
                let! updateStream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
                use filestream = File.Create(filepath)
                do! updateStream.CopyToAsync(filestream) |> Async.AwaitTask
                ()
            else
                ()
        }

    let checkForUpdates obsVersion gpVersion lastChecked =
        // do last checked check here

        let currentVersionInfo = { Version = gpVersion; MinCoreVersion = obsVersion; DownloadUrl = "" }

        async {
            let! result = getNewestVersion currentVersionInfo lastChecked
            match result with
            | Some versionInfo ->
                if versionInfo.Version > gpVersion then
                    return UpdateAvailable versionInfo
                else
                    return NoUpdate
            | None ->
                return NoUpdate
        }


    // These versions are synchronous versions, since Observatory's update check is synchronous
    // so that they can be called from the main thread.
    
    // This function is also side-effect-ey as fuck
    let downloadUpdateSync versionInfo savePath (http:HttpClient) =
        let response = http.GetAsync(versionInfo.DownloadUrl).Result
        if response.IsSuccessStatusCode then
            let filename = versionInfo.DownloadUrl.Split('/') |> Array.last
            let filepath = Path.Combine(savePath, filename)
            let updateStream = response.Content.ReadAsStream()
            use filestream = File.Create(filepath)
            updateStream.CopyTo(filestream)
            updateStream.Flush()
            updateStream.Dispose()
            ()
        else
            ()

    let getNewestVersionSync currentVersion (http:HttpClient) =
        let response = http.GetAsync(versionCheckUrl).Result
        if response.IsSuccessStatusCode then
            let content = response.Content.ReadAsStringAsync().Result
            let versionInfo = Serializer.deserialize<VersionInfo> currentVersion content 
            Some versionInfo
        else
            None

    let checkForUpdatesSync obsVersion gpVersion (lastChecked:DateTime) http =
        // TODO: Activate the update time check before release
        if (DateTime.Now - lastChecked).TotalHours < 2.0 then
            NoUpdate
        else
            let currentVersionInfo = { Version = gpVersion; MinCoreVersion = obsVersion; DownloadUrl = "" }

            let result = getNewestVersionSync currentVersionInfo http
            match result with
            | Some versionInfo ->
                if versionInfo.Version > gpVersion then
                    UpdateAvailable versionInfo
                else
                    NoUpdate
            | None ->
                NoUpdate
