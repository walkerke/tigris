# tigris

<img src=tools/readme/tigris_sticker.png width="250">

[![R build status](https://github.com/walkerke/tigris/workflows/R-CMD-check/badge.svg)](https://github.com/walkerke/tigris/actions) ![CRAN Badge](http://www.r-pkg.org/badges/version/tigris)  ![CRAN Downloads](http://cranlogs.r-pkg.org/badges/tigris)

__Package not working (April 2025)?__. 

Install the patch release with `pak::pak("walkerke/tigris@ftp-patch")`

__tigris__ is an R package that allows users to directly download and use TIGER/Line shapefiles (<https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html>) from the US Census Bureau.  

To install the package from CRAN, issue the following command in R: 

```
install.packages('tigris')
```

Or, get the development version from GitHub: 

```
devtools::install_github('walkerke/tigris')
```

__tigris__ functions return [simple features objects](https://r-spatial.github.io/sf/) with a default year of 2022, though later years are available (tigris maintains consistency with the most recent 5-year American Community Survey).  To get started, choose a function from the table below and use it with a state and/or county if required. You'll get back an sf object for use in your mapping and spatial analysis projects: 

```r
library(tigris)
library(ggplot2)

manhattan_roads <- roads("NY", "New York")

ggplot(manhattan_roads) + 
  geom_sf() + 
  theme_void()
```

<img src=tools/readme/ny_roads.png>

__tigris__ only returns feature geometries for US Census data which default to the coordinate reference system NAD 1983 (EPSG: 4269). For US Census demographic data (optionally pre-joined to tigris geometries), try the [tidycensus package](https://walker-data.com/tidycensus/).  For help deciding on an appropriate coordinate reference system for your project, take a look at the [crsuggest package](https://github.com/walkerke/crsuggest).  

To learn more about how to use __tigris__, read [Chapter 5 of the book _Analyzing US Census Data: Methods, Maps, and Models in R_](https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html). 

Python users may also be interested in [__pygris__, a Python port of the tigris package](https://walker-data.com/pygris/).  

__Available datasets:__

Please note: cartographic boundary files in __tigris__ are not available for 2011 and 2012.  

| Function | Datasets available | Years available |
|------------------------------------------|------------------------------------------------|------------------------------|
| `nation()` | cartographic (1:5m; 1:20m) | 2013-2023 |
| `divisions()` | cartographic (1:500k; 1:5m; 1:20m) | 2013-2023 |
| `regions()` | cartographic (1:500k; 1:5m; 1:20m) | 2013-2023 |
| `states()` | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 1990, 2000, 2010-2024 |
| `counties()` | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 1990, 2000, 2010-2024 |
| `tracts()` | TIGER/Line; cartographic (1:500k) | 1990, 2000, 2010-2024 |
| `block_groups()` | TIGER/Line; cartographic (1:500k) | 1990, 2000, 2010-2024 |
| `blocks()` | TIGER/Line | 2000, 2010-2024 |
| `places()` | TIGER/Line; cartographic (1:500k) | 2011-2024 |
| `pumas()` | TIGER/Line; cartographic (1:500k) | 2012-2024 |
| `school_districts()` | TIGER/Line; cartographic | 2011-2024 |
| `zctas()` | TIGER/Line; cartographic (1:500k) | 2000, 2010, 2012-2024 |
| `congressional_districts()` | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 2011-2024 |
| `state_legislative_districts()` | TIGER/Line; cartographic (1:500k) | 2011-2024 |
| `voting_districts()` | TIGER/Line | 2012, 2020 |
| `area_water()` | TIGER/Line | 2011-2024 |
| `linear_water()` | TIGER/Line | 2011-2024 |
| `coastline` | TIGER/Line() | 2013-2024 |
| `core_based_statistical_areas()` | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 2011-2021, 2023-2024 (not available for 2022) |
| `combined_statistical_areas()` | TIGER/Line; cartographic (1:500k; 1:5m; 1:20m) | 2011-2024 |
| `metro_divisions()` | TIGER/Line | 2011-2024 |
| `new_england()` | TIGER/Line; cartographic (1:500k) | 2011-2022 (no longer used by Census) |
| `county_subdivisions()` | TIGER/Line; cartographic (1:500k) | 2010-2024 |
| `urban_areas()` | TIGER/Line; cartographic (1:500k) | 2012-2024 |
| `primary_roads()` | TIGER/Line | 2011-2024 |
| `primary_secondary_roads()` | TIGER/Line | 2011-2024 |
| `roads()` | TIGER/Line | 2011-2024 |
| `rails()` | TIGER/Line | 2011-2024 |
| `native_areas()` | TIGER/Line; cartographic (1:500k) | 2011-2024 |
| `alaska_native_regional_corporations()` | TIGER/Line; cartographic (1:500k) | 2011-2024 |
| `tribal_block_groups()` | TIGER/Line | 2011-2024 |
| `tribal_census_tracts()` | TIGER/Line | 2011-2024 |
| `tribal_subdivisions_national()` | TIGER/Line | 2011-2024 |
| `landmarks()` | TIGER/Line | 2011-2024 |
| `military()` | TIGER/Line | 2011-2024 |




 
