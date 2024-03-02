﻿namespace GeoPredictor

open Observatory.Framework

type Settings() =
    let mutable notifyOnGeoBody = true
    let mutable notifyOnNewGeoCodex = true
    let mutable verboseNotifications = true
    let mutable onlyShowCurrentSystem = true
    let mutable onlyShowWithScans = false
    let mutable onlyShowFailedPredictionBodies = false

    // Event that triggers for Settings that require UI updates when changed
    let needsUIUpdate = new Event<_>()
    member this.NeedsUIUpdate = needsUIUpdate.Publish

    // Turn on and off notifications on found geological bodies
    [<SettingDisplayName("Notify on new geological body  ")>]
    member this.NotifyOnGeoBody
        with get() = notifyOnGeoBody
        and set(setting) = notifyOnGeoBody <- setting

    // Turn on and off notifications on predicted new Codex entries
    [<SettingDisplayName("Notify on possible new codex entry  ")>]
    member this.NotifyOnNewGeoCodex
        with get() = notifyOnNewGeoCodex
        and set(setting) = notifyOnNewGeoCodex <- setting

    // Verbose notifications
    [<SettingDisplayName("Verbose notifications  ")>]
    member this.VerboseNotifications
        with get() = verboseNotifications
        and set(setting) = verboseNotifications <- setting

    // Only show data for the current system; requires UI update
    [<SettingDisplayName("Show only current system  ")>]
    member this.OnlyShowCurrentSystem
        with get() = onlyShowCurrentSystem
        and set(setting) = 
            onlyShowCurrentSystem <- setting
            needsUIUpdate.Trigger()
    
    // Only show data for bodies where geological features have been scanned; requires UI update
    [<SettingDisplayName("Show only bodies with scans  ")>]
    member this.OnlyShowWithScans
        with get() = onlyShowWithScans
        and set(setting) = 
            onlyShowWithScans <- setting
            needsUIUpdate.Trigger()

    // Only show data for bodies where prediction failed; requires UI update
    [<SettingDisplayName("Show only bodies with failed prediction  ")>]
    member this.OnlyShowFailedPredictionBodies
        with get() = onlyShowFailedPredictionBodies
        and set(setting) =
            onlyShowFailedPredictionBodies <- setting
            needsUIUpdate.Trigger()
