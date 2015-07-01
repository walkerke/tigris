# tigris

Download and use Census TIGER shapefiles in R

Current version: __0.0.5__ (updated 1 July 2015)

`tigris` is an R package that allows users to directly download and use TIGER/Line shapefiles (<https://www.census.gov/geo/maps-data/data/tiger-line.html>) from the US Census Bureau.  

To install the package, issue the following command in R: 

```
devtools::install_github('walkerke/tigris')
```

__Version 0.0.5 release highlights:__

* Many new geographies are available for download - state legislative districts, urbanized areas, Native American areas, and lots more!
* For many geographies (e.g. states, counties, Census tracts, etc.) an optional argument, `detailed = FALSE`, can be supplied to download simplified (smaller) shapefiles.  While not recommended for analysis, their smaller size makes them better for mapping.  
* A new function, `lookup_code`, allows you to look up FIPS codes for states and counties interactively.  
* Zip Code Tabulation Areas (ZCTAs) have an optional parameter, `starts_with`, that allows you to supply an argument to return only those ZCTAs that begin with that string

__Basic usage:__

```r
library(tigris)

# Basic plot of US urbanized areas

ua <- urban_areas(detailed = FALSE)

plot(ua)

```

![Basic plot](https://dl.dropbox.com/s/evb5u8sm0q9k4sy/ua_plot.png)

```r
library(leaflet)

# Interactive Leaflet map (requires the leaflet R package)

ua %>% leaflet() %>% addTiles() %>% addPolygons(popup = ~NAME10)

```

![Interactive map](https://dl.dropbox.com/s/c4ozukojr7ittwv/atlanta.PNG)


For more information on how to use this package, please view the RPubs at <http://rpubs.com/walkerke/tigris>. 

__Goals for 0.1 release and CRAN submission:__

* All Census geographies supported
* Better and more informative error handling
* Improved and more complete documentation
* More in this space as the package develops...
