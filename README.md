# network_analysis
A way to visualize GA or Adobe Analytics user paths

To use this you are going to need to setup the GA API https://code.markedmondson.me/googleAnalyticsR/

  -- OR --

You need to to the RsiteCatalyst API https://randyzwitch.com/rsitecatalyst/

Once you have the tool of your choice ready to use with the R you should be able to run the script and generate your own reports
* Table of current and previous page 
* Entrance and exit report
* Page importance (network density scaled 0 to 100)
* Network map of pages

You can also view this in shiny form here https://jnt0009.shinyapps.io/network_analysis/

Note that if you are trying to return a lot of data the poor shiny server will run out of memory and fail.  This is due to the data not being sampled and may change in further releases
