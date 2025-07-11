# GeoPredictor Plugin for Observatory Core
**Note:** This plugin requires [Observatory Core](https://github.com/Xjph/ObservatoryCore) to work, so if you don't have that, start there! :)

[Direct download of GeoPredictor.eop if you're using ObservatoryCore v1.2.0 or newer](https://github.com/mcmuttons/GeoPredictor/releases/download/v2.3.0/GeoPredictor.eop)

[Direct download of GeoPredictor.eop if you're using an older ObservatoryCore version (no longer updated)](https://github.com/mcmuttons/GeoPredictor/releases/download/v1.4.5/GeoPredictor.eop)

GeoPredictor tracks what geological signal types you've discovered, and predicts which you can expect to find before landing on a body. It will also let you know if the geological feature will be new to your Codex. Mistakes can happen, feel free to post an issue here if they do. :)

## Understanding the data
The meaning of the columns are as follows:
- **Body:** The full name of the body that we're looking at
- **Count:** How many signals are reported to be found on this body. In the case of a surprising result, there might be more. If there is a second number in parentheses (like `3 (4)`), then there will be more geological features on this body than Elite's DSS count indicates.
- **Found:** Icons indicating the status of the current geology:
  - ❔: A geological feature has been predicted, but not yet verified
  - 🔷: A new geological codex entry has been predicted, but not yet verified
  - ✔️: A geological feature has been predicted and then verified by scanning it with a comp. scanner on your ship or SRV
  - :x:: An unexpected geological feature has been found. If you still have ❔ in your list, it might worth it to keep looking.
- **Type:** The specific name of the predicted or found geological feature
- **Body Type:** What kind of body we're looking at. F.ex. `HMC` (High Metal Content), `Rocky Ice`, etc.
- **Materials:** What materials can be found on the body and the percentages. You can filter these in the options, and switch between full names or chemical symbols
- **Volcanism:** The type of volcanism the body has
- **Temp:** The average temperature of the body
- **Region:** The galactic region the body exists in

Here is a sample entry for the system **Bleia Eohn KG-D b16-1**, where body **5** has some geology, and the results in the Found column tell you the status of each.

![image](https://github.com/user-attachments/assets/16e2c502-9e94-4b04-96e8-749ae3421487)

- The Carbon Dioxide Ice Geysers and Fumaroles (marked with :heavy_check_mark:) have been both predicted and found for this body. This is perfect!
- The Silicate Vapour and Silicate Magma features (marked with both ❔ and 🔷) have been predicted for this body (due to the ❔), but not found, and should also be a new codex entry (due to the 🔷). Codex entries are tracked per region, so Silicate Vapour Fumaroles in Inner Orion Spur is different from Silicate Vapour Fumaroles in Lyra's Song.
- Missing from the example is a feature that has not been predicted, but found anyway. It will be marked with :x:, along with the message, `❗Possible additional geo❗`. If there still is a geological feature still marked with a ❔, it's worth looking for it, despite the signal count. 

## Installation
- Download the newest release from this page.
- Close Observatory if it's open.
- Double click the newest release file. It should be called `GeoPredictor.eop`.
- Observatory should open with GeoPredictor installed.
- The first time you run it, there should be a message reminding you to click the `Read All` button to set the plugin up for first time use. This goes through all of your game journals that Elite has saved on your computer and figures out which geological features you've scanned before. Until you've run `Read All`, this message will continue to appear, since GeoPredictor won't have full functionality until you have. Please note that if your Elite journals are incomplete (files deleted, you reinstalled on a new computer and didn't bring your files, etc), GeoPredictor won't be able to see that you've encountered geology recorded in the missing files and you will get false positives. This is easily fixed by scanning the geology in question.

## Settings
You can find the settings for GeoPredictor by either right-clicking the GeoPredictor tab in the main view and choosing `GeoPredictor Settings` or going to the `Core` tab and clicking the far right icon with the little down arrow on it and selecting `GeoPredictor Settings` there. All settings are cumulative if applicable, so if you turn on both showing only current system and showing only bodies with scanned geology, then you will see only bodies with scanned geology in the current system. 

Note that `Read All` also adheres to these settings, so you might have to change them to see everything that has been read.

The settings are divided into categories.

### Notifications 

#### Notify on new geological body
This turns on and off whether a standard Observatory notification should be shown when a geological body is scanned.

Default: **on**

#### Notify on possible new codex entry
This turns on and off whether a new codex entry notification should be shown when a geological body is scanned

Default **on**

#### Verbose notifications
This turns on and off whether notifcation text should be verbose or terse. Especially for those using voice with their notifications, terse might be best. Either way, all data scanned ends up in the list view on the GeoPredictor tab.

Default **on**

### Evaluator Integration

#### Tell Evaluator to visit on new geological body
This will add any body with geological signals to Evaluator's visit list.

Default **off**

#### Tell Evaluator to visit on possible new geological codex entry
This will only add bodies with new geological codex entries to Evaluator's visit list.

### Display

#### Show only current system
This will only show bodies with geology on the in the system you're in. Otherwise you'll see all the bodies since this Observatory session started. 

Default: **on**

#### Show only bodies with scans
This will only show bodies where at least one geological item has been scanned on the surface. This mostly used for exporting reports, but perhaps you'll find the view useful as well. :)

Default: **off**

#### Show only bodies with failed prediction
This will only show bodies where GeoPredictor has found something it didn't predict. This is mostly useful for debugging or reporting errors.

Default: **off**

#### Hide grade 1 materials
This will hide all grade 1 materials on a body from the materials list. These are the most common. All data is saved, so if you turn it off, they'll be there again.

Default: **off**

This setting also exists for grades 2, 3 and 4 (which are the rarest materials)

#### Use chemical symbol insteead of element name
This makes the materials list much more compact by showing for example `Cu` instead of `Copper`, or `Pb` instead of `Lead`. If you prefer full names, turn this off.

Default: **on**


## Helping the developer :)
Should you come across an unexpected entry (red X), then you could help me quite a lot by letting me know. There are a few ways to do get the data:
- Go to the settings, filter on `Show only bodies with failed prediction`, then export the data with the Export button. The file will end up in the folder specified in the `Export Options`.
- Take a screenshot of the Observatory window or part of it, with the all the information for that body visible
- Just note down the details (including the body type, volcanism and temp)

You can either track me down on Discord (mcmuttons) -- I'm regularly on the official ObservatoryCore Discord server, or you can just start an Issue here on my github page and put the info there.

Thank you and good luck out there!!

