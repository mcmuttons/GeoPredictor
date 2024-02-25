# GeoPredictor Plugin for Observatory Core v1.3
**Note:** This plugin requires [Observatory Core](https://github.com/Xjph/ObservatoryCore) to work, so if you don't have that, start there! :)

[Direct download of GeoPredictor.eop](https://github.com/mcmuttons/GeoPredictor/releases/download/v1.3/GeoPredictor.eop)

GeoPredictor aims to make it easier to get an overview over geological signals on landable bodies. It attempts to predict which geological features you will find on the surface, so that you'll know what to expect. 

It also has the option to pop up a standard Observatory notification when a body with geological signals is scanned.

## Understanding the data
Here is a sample entry for the system **Phae Aeb ZG-U c16-10**, where body **D 2** has some geology, and the results in the Found column tell you the status of each.

![image](https://github.com/mcmuttons/GeoPredictor/assets/668213/3882b92a-1f44-4304-9b2e-fa7d3438a599)

- The Sulphur Dioxide Fumaroles (with the check mark) have been both predicted and found for this body. This is perfect!
- The Silicate Vapour Gas Vents (with the red X) have not been predicted, but found anyway. Oh no!
- The Sulphur Dioxide Gas Vents (with the question mark) have been predicted for this body, but not found. Since there are only 2 geological signals, according to the Count column, most likely this isn't here, since we found an unexpected geological signal to take its place. However, it's worth a look around, since there are instances where Elite reports the wrong number, even though it's rare.

## Installation
- Download the newest release from this page.
- Close Observatory if it's open.
- Double click the newest release file. It should be called `GeoPredictor.eop`.
- Observatory should open with GeoPredictor installed.

## Settings
You can find the settings in Observatory by selecting `Core` on the left side, and then the `GeoPredictor` tab. All settings are cumulative if applicable, so if you turn on both showing only current system and showing only bodies with scanned geology, then you will see only bodies with scanned geology in the current system. 

Note that `Read All` also adheres to these settings, so you might have to change them to see everything that has been read.

### Notify on new geological body
This turns on and off whether a standard Observatory notification should be shown when a geological body is scanned.

Default: **on**

### Show only current system
This will only show bodies with geology on the in the system you're in. Otherwise you'll see all the bodies since this Observatory session started. 

Default: **on**

### Show only bodies with scans
This will only show bodies where at least one geological item has been scanned on the surface. This mostly used for exporting reports, but perhaps you'll find the view useful as well. :)

Default: **off**

### Show only bodies with failed prediction
This will only show bodies where GeoPredictor has found something it didn't predict. 

Default: **off**

## Helping the developer :)
Should you come across an unexpected entry (red X), then you could help me quite a lot by either noting down all the details (including the body type, volcanism and temp), or just sending me a screenshot of the entry, including the body info. You can either track me down on Discord (mcmuttons) -- I often hang out both on the Observatory and Stellar Cartography Guild Disocord servers, or you can just start in Issue here on my github page and put the info there.

Thank you!!

