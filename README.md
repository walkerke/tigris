# tigris

<img src=img/tigris_sticker.png width="50%">

[![Build Status](https://travis-ci.org/walkerke/tigris.svg?branch=master)](https://travis-ci.org/walkerke/tigris) ![CRAN Badge](http://www.r-pkg.org/badges/version/tigris)  ![CRAN Downloads](http://cranlogs.r-pkg.org/badges/tigris)

__tigris__ is an R package that allows users to directly download and use TIGER/Line shapefiles (<https://www.census.gov/geo/maps-data/data/tiger-line.html>) from the US Census Bureau.  

To install the package from CRAN, issue the following command in R: 

```
install.packages('tigris')
```

Or, get the development version from GitHub: 

```
devtools::install_github('walkerke/tigris')
```

As of version 0.9.1, tigris defaults to the 2018 TIGER/Line and Cartographic Boundary shapefiles.  

To learn how to use the package, I'd recommend the following materials: 

* My article in _The R Journal_, ["tigris: An R Package to Access and Work with Geographic Data from the US Census Bureau"](https://journal.r-project.org/archive/2016/RJ-2016-043/index.html)
* [A webinar I gave with Ari Lamstein on tigris in April 2017](https://www.youtube.com/watch?v=lZuVxVONK9g&__s=hpmyiy9wyzwapfzug5q9)

__Available datasets:__

Please note: cartographic boundary files in __tigris__ are not available for 2011 and 2012, and have not yet been released for 2019.  

| Function | Datasets available | Years available |
|------------------------------------------|------------------------------------------------|------------------------------|
| nation | cartographic (1:5m; 1:20m) | 2013-2019 |
| divisions | cartographic (1:500k; 1:5m; 1:20m) | 2013-2019 |
| regions | cartographic (1:500k; 1:5m; 1:20m) | 2013-2019 |
| states | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 1990, 2000, 2010-2019 |
| counties | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 1990, 2000, 2010-2019 |
| tracts | TIGER/Line; cartographic (1:500k) | 1990, 2000, 2010-2019 |
| block_groups | TIGER/Line; cartographic (1:500k) | 1990, 2000, 2010-2019 |
| blocks | TIGER/Line | 2000, 2010-2019 |
| places | TIGER/Line; cartographic (1:500k) | 2011-2019 |
| pumas | TIGER/Line; cartographic (1:500k) | 2012-2019 |
| school_districts | TIGER/Line | 2011-2019 |
| zctas | TIGER/Line; cartographic (1:500k) | 2000, 2010, 2012-2019 |
| congressional_districts | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 2011-2019 |
| state_legislative_districts | TIGER/Line; cartographic (1:500k) | 2011-2019 |
| voting_districts | TIGER/Line | 2012 |
| area_water | TIGER/Line | 2011-2019 |
| linear_water | TIGER/Line | 2011-2019 |
| coastline | TIGER/Line | 2013-2019 |
| core_based_statistical_areas | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 2011-2019 |
| combined_statistical_areas | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 2011-2019 |
| metro_divisions | TIGER/Line | 2011-2019 |
| new_england | TIGER/Line; cartographic (1:500k) | 2011-2019 |
| county_subdivisions | TIGER/Line; cartographic (1:500k) | 2011-2019 |
| urban_areas | TIGER/Line; cartographic (1:500k) | 2012-2019 |
| primary_roads | TIGER/Line | 2011-2019 |
| primary_secondary_roads | TIGER/Line | 2011-2019 |
| roads | TIGER/Line | 2011-2019 |
| rails | TIGER/Line | 2011-2019 |
| native_areas | TIGER/Line; cartographic (1:500k) | 2011-2019 |
| alaska_native_regional_corporations | TIGER/Line; cartographic (1:500k) | 2011-2019 |
| tribal_block_groups | TIGER/Line | 2011-2019 |
| tribal_census_tracts | TIGER/Line | 2011-2019 |
| tribal_subdivisions_national | TIGER/Line | 2011-2019 |
| landmarks | TIGER/Line | 2011-2019 |
| military | TIGER/Line | 2011-2019 |




 
