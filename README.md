# GeoPredictor Plugin for Observatory Core
**Note:** This plugin requires [Observatory Core](https://github.com/Xjph/ObservatoryCore) to work, so if you don't have that, start there! :)

GeoPredictor aims to make it easier to get an overview over geological signals on landable bodies. Going forward, it will also track which are in your codex or not and will aim to predict which kinds of geological signals you might find on a given body.

## Installation
- Download the newest release from this page.
- Close Observatory if it's open.
- Double click the newest release file. It should be called `GeoPredictor.eop`.
- Observatory should open with GeoPredictor installed.

## Settings
You can find the settings in Observatory by selecting `Core` on the left side, and then the `GeoPredictor` tab. All settings are cumulative if applicable, so if you turn on both showing only current system and showing only bodies with scanned geology, then you will see only bodies with scanned geology in the current system. 

Note that `Read All` also adheres to these settings, so you might have to change them to see everything that has been read.

### Show only current system
This will only show bodies with geology on the in the system you're in. Otherwise you'll see all the bodies since this Observatory session started. 

Default: **on**

### Show only bodies with scans
This will only show bodies where at least one geological item has been scanned on the surface. This mostly used for exporting reports, but perhaps you'll find the view useful as well. :)

Default: **off**

## Exporting data to help the developer :)
The more data I have, the easier it is for me to analyze and figure out what determines which types of geology show up on a planet, in hopes of trying to predict it before you have to spend you precious time landing on a body only to realize it's yet another ice water geyser. :D So if you'd like to help with sending data, these are the steps:

1. Click `Core`
2. In the `Export Options` tab, set the export style to `tab separated`, and choose a folder to save in
3. In the `GeoPredictor` tab, set `Show only current system` to **off** and `Show only bodies with scans` to **on**
4. Click `Read All`. If you have a lot of data (and most of us do :) then this can take as long as several minutes, where Observatory seems unresponsive. Wait it out, and once you can interact again (you'll probably see the `Read All` button change color slightly), then click on `Export`.
5. Observatory will create one file for every plugin in the folder you chose in step 2, and one of them will have the name GeoPredictor in it (the names are built from date and some other data).
6. Find me on Discord (user: mcmuttons) or just create an `Issue` here at github with the file included to get it to me. Thank you!
