# tigris

[![Travis-CI Build Status](https://travis-ci.org/walkerke/tigris.svg?branch=master)](https://travis-ci.org/walkerke/tigris)  ![](http://www.r-pkg.org/badges/version/tigris)  ![](http://cranlogs.r-pkg.org/badges/tigris)

Download and use Census TIGER/Line shapefiles in R

`tigris` is an R package that allows users to directly download and use TIGER/Line shapefiles (<https://www.census.gov/geo/maps-data/data/tiger-line.html>) from the US Census Bureau.  

To install the package from CRAN, issue the following command in R: 

```
install.packages('tigris')
```

Or, get the development version from GitHub: 

```
devtools::install_github('walkerke/tigris')
```

__In Version 0.5.3__: 

* tigris now downloads data to a temporary directory rather than creating a cache directory by default to conform with CRAN policies.  To enable caching of data, set `options(tigris_use_cache = TRUE)` in your R script or .Rprofile.

* tigris now supports simple features!  To load your data as an object of class `sf`, specify `class = "sf"` in the function call, or set this globally with `options(tigris_class = "sf")`.  

* Historic boundaries are available for states, counties, Census tracts, block groups, and ZCTAs; 1990 (`cb = TRUE` only, and not for ZCTAs), 2000, and 2010 boundaries can be obtained with the `year` parameter in an associated function call.  

To learn how to use the package, I'd recommend the following materials: 

* My article in _The R Journal_, ["tigris: An R Package to Access and Work with Geographic Data from the US Census Bureau"](https://journal.r-project.org/archive/2016/RJ-2016-043/index.html)
* [A webinar I gave with Ari Lamstein on tigris in April 2017:](https://www.youtube.com/watch?v=lZuVxVONK9g&__s=hpmyiy9wyzwapfzug5q9)

__Available datasets:__

Please note: cartographic boundary files in __tigris__ are not available for 2011 and 2012.  

| Function | Datasets available | Years available |
|------------------------------------------|------------------------------------------------|------------------------------|
| nation | cartographic (1:5m; 1:20m) | 2013, 2014, 2015 |
| divisions | cartographic (1:500k; 1:5m; 1:20m) | 2013, 2014, 2015 |
| regions | cartographic (1:500k; 1:5m; 1:20m) | 2013, 2014, 2015 |
| states | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 1990, 2000, 2010, 2011, 2012, 2013, 2014, 2015 |
| counties | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 1990, 2000, 2010, 2011, 2012, 2013, 2014, 2015 |
| tracts | TIGER/Line; cartographic (1:500k) | 1990, 2000, 2010, 2011, 2012, 2013, 2014, 2015 |
| block_groups | TIGER/Line; cartographic (1:500k) | 1990, 2000, 2010, 2011, 2012, 2013, 2014, 2015 |
| blocks | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| places | TIGER/Line; cartographic (1:500k) | 2011, 2012, 2013, 2014, 2015 |
| pumas | TIGER/Line; cartographic (1:500k) | 2012, 2013, 2014, 2015 |
| school_districts | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| zctas | TIGER/Line; cartographic (1:500k) | 2012, 2013, 2014, 2015 |
| congressional_districts (114th Congress) | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 2014, 2015 |
| state_legislative_districts | TIGER/Line; cartographic (1:500k) | 2011, 2012, 2013, 2014, 2015 |
| voting_districts | TIGER/Line | 2012 |
| area_water | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| linear_water | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| coastline | TIGER/Line | 2013, 2014, 2015 |
| core_based_statistical_areas | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 2011, 2012, 2013, 2014, 2015 |
| combined_statistical_areas | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 2011, 2012, 2013, 2014, 2015 |
| metro_divisions | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| new_england | TIGER/Line; cartographic (1:500k) | 2011, 2012, 2013, 2014, 2015 |
| county_subdivisions | TIGER/Line; cartographic (1:500k) | 2011, 2012, 2013, 2014, 2015 |
| urban_areas | TIGER/Line; cartographic (1:500k) | 2012, 2013, 2014, 2015 |
| primary_roads | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| primary_secondary_roads | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| roads | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| rails | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| native_areas | TIGER/Line; cartographic (1:500k) | 2011, 2012, 2013, 2014, 2015 |
| alaska_native_regional_corporations | TIGER/Line; cartographic (1:500k) | 2011, 2012, 2013, 2014, 2015 |
| tribal_block_groups | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| tribal_census_tracts | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| tribal_subdivisions_national | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| landmarks | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| military | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |




 
