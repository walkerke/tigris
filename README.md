# tigris

[![Travis-CI Build Status](https://travis-ci.org/walkerke/tigris.svg?branch=master)](https://travis-ci.org/walkerke/tigris)  ![](http://www.r-pkg.org/badges/version/tigris)  ![](http://cranlogs.r-pkg.org/badges/tigris)

Download and use Census TIGER/Line shapefiles in R

__tigris__ is an R package that allows users to directly download and use TIGER/Line shapefiles (<https://www.census.gov/geo/maps-data/data/tiger-line.html>) from the US Census Bureau.  

Want to learn how to use __tigris__? [Take the DataCamp course _Analyzing US Census Data in R_!](https://www.datacamp.com/courses/analyzing-us-census-data-in-r)

To install the package from CRAN, issue the following command in R: 

```
install.packages('tigris')
```

Or, get the development version from GitHub: 

```
devtools::install_github('walkerke/tigris')
```

__In version 0.6__: 

* You are now able to specify a custom cache directory for your data.  To set the cache directory, use the new `tigris_cache_dir()` function: 

```r
library(tigris)
tigris_cache_dir("PATH TO YOUR CUSTOM CACHE DIRECTORY")
# Restart R
options(tigris_use_cache = TRUE)
```

* tigris now defaults to the 2016 TIGER/Line and cartographic boundary shapefiles. 

* Thanks to contributors from Transport Foundry (https://github.com/transportfoundry), tigris now includes some geolocator functions.  For example, to find out the Census block ID of a given address: 

```r
call_geolocator("2850 S University Dr", "Fort Worth", "TX")

Address (2850 S University Dr Fort Worth TX) returned more than one address match. The first match was returned.
[1] "484391042014000"
```

* Stay tuned for batch geocoding support in a future release!

To learn how to use the package, I'd recommend the following materials: 

* My article in _The R Journal_, ["tigris: An R Package to Access and Work with Geographic Data from the US Census Bureau"](https://journal.r-project.org/archive/2016/RJ-2016-043/index.html)
* [A webinar I gave with Ari Lamstein on tigris in April 2017](https://www.youtube.com/watch?v=lZuVxVONK9g&__s=hpmyiy9wyzwapfzug5q9)

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
| blocks | TIGER/Line | 2000, 2010, 2011, 2012, 2013, 2014, 2015 |
| places | TIGER/Line; cartographic (1:500k) | 2011, 2012, 2013, 2014, 2015 |
| pumas | TIGER/Line; cartographic (1:500k) | 2012, 2013, 2014, 2015 |
| school_districts | TIGER/Line | 2011, 2012, 2013, 2014, 2015 |
| zctas | TIGER/Line; cartographic (1:500k) | 2000, 2010, 2012, 2013, 2014, 2015 |
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




 
