# `counties()` works

    Code
      counties(cb = TRUE, progress_bar = FALSE)
    Output
      Simple feature collection with 3235 features and 12 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -179.1467 ymin: -14.5487 xmax: 179.7785 ymax: 71.38782
      Geodetic CRS:  NAD83
      First 10 features:
         STATEFP COUNTYFP COUNTYNS        GEOIDFQ GEOID        NAME
      1       06      085 00277307 0500000US06085 06085 Santa Clara
      2       06      059 00277294 0500000US06059 06059      Orange
      3       17      043 00422191 0500000US17043 17043      DuPage
      4       04      027 00023901 0500000US04027 04027        Yuma
      5       01      003 00161527 0500000US01003 01003     Baldwin
      6       01      085 00161568 0500000US01085 01085     Lowndes
      7       18      007 00450403 0500000US18007 18007      Benton
      8       23      017 00581294 0500000US23017 23017      Oxford
      9       23      025 00581298 0500000US23025 23025    Somerset
      10      24      021 01711211 0500000US24021 24021   Frederick
                   NAMELSAD STUSPS STATE_NAME LSAD       ALAND     AWATER
      1  Santa Clara County     CA California   06  3343891498   33597067
      2       Orange County     CA California   06  2054504217  405282838
      3       DuPage County     IL   Illinois   06   849236415   22006224
      4         Yuma County     AZ    Arizona   06 14280726743   13242852
      5      Baldwin County     AL    Alabama   06  4117933903 1132678359
      6      Lowndes County     AL    Alabama   06  1854651686   23271386
      7       Benton County     IN    Indiana   06  1052608712     236033
      8       Oxford County     ME      Maine   06  5379296165  255883190
      9     Somerset County     ME      Maine   06 10163862548  437891943
      10   Frederick County     MD   Maryland   06  1711005007   17630724
                               geometry
      1  MULTIPOLYGON (((-122.2027 3...
      2  MULTIPOLYGON (((-118.1146 3...
      3  MULTIPOLYGON (((-88.26288 4...
      4  MULTIPOLYGON (((-114.8163 3...
      5  MULTIPOLYGON (((-88.02858 3...
      6  MULTIPOLYGON (((-86.90851 3...
      7  MULTIPOLYGON (((-87.52637 4...
      8  MULTIPOLYGON (((-71.08 45.3...
      9  MULTIPOLYGON (((-70.55279 4...
      10 MULTIPOLYGON (((-77.67716 3...

---

    Code
      counties(progress_bar = FALSE)
    Output
      Simple feature collection with 3235 features and 18 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -179.2311 ymin: -14.60181 xmax: 179.8597 ymax: 71.43979
      Geodetic CRS:  NAD83
      First 10 features:
         STATEFP COUNTYFP COUNTYNS GEOID        GEOIDFQ       NAME          NAMELSAD
      1       40      075 01101825 40075 0500000US40075      Kiowa      Kiowa County
      2       46      079 01265776 46079 0500000US46079       Lake       Lake County
      3       37      033 01008542 37033 0500000US37033    Caswell    Caswell County
      4       48      377 01383974 48377 0500000US48377   Presidio   Presidio County
      5       39      057 01074041 39057 0500000US39057     Greene     Greene County
      6       48      489 01384030 48489 0500000US48489    Willacy    Willacy County
      7       72      021 01804490 72021 0500000US72021    Bayamón Bayamón Municipio
      8       35      028 01702366 35028 0500000US35028 Los Alamos Los Alamos County
      9       40      023 01101799 40023 0500000US40023    Choctaw    Choctaw County
      10      20      041 00484990 20041 0500000US20041  Dickinson  Dickinson County
         LSAD CLASSFP MTFCC CSAFP CBSAFP METDIVFP FUNCSTAT      ALAND    AWATER
      1    06      H1 G4020  <NA>   <NA>     <NA>        A 2629039892  40296743
      2    06      H1 G4020  <NA>   <NA>     <NA>        A 1457916151  31746795
      3    06      H1 G4020  <NA>   <NA>     <NA>        A 1102042927   8293623
      4    06      H1 G4020  <NA>   <NA>     <NA>        A 9985057447   1773188
      5    06      H1 G4020   212  19430     <NA>        A 1071302625   6798109
      6    06      H1 G4020   154  39700     <NA>        A 1529680719 502856285
      7    13      H1 G4020   490  41980     <NA>        A  115097577    314191
      8    06      H1 G4020   105  31060     <NA>        A  282626746    208105
      9    06      H1 G4020  <NA>   <NA>     <NA>        A 1995281898  75475686
      10   06      H1 G4020  <NA>   <NA>     <NA>        A 2193895344  12632459
            INTPTLAT     INTPTLON                       geometry
      1  +34.9214893 -098.9816168 MULTIPOLYGON (((-98.95506 3...
      2  +44.0284497 -097.1232229 MULTIPOLYGON (((-96.88886 4...
      3  +36.3943252 -079.3396193 MULTIPOLYGON (((-79.14343 3...
      4  +30.0058912 -104.2616192 MULTIPOLYGON (((-104.9808 3...
      5  +39.6874785 -083.8948943 MULTIPOLYGON (((-84.10668 3...
      6  +26.4818607 -097.5947313 MULTIPOLYGON (((-97.86187 2...
      7  +18.3494195 -066.1686259 MULTIPOLYGON (((-66.17269 1...
      8  +35.8700469 -106.3079679 MULTIPOLYGON (((-106.3997 3...
      9  +34.0276995 -095.5540792 MULTIPOLYGON (((-95.99158 3...
      10 +38.8677353 -097.1579434 MULTIPOLYGON (((-97.25952 3...

# `school_districts()` works

    Code
      school_districts(state = state, progress_bar = FALSE)
    Output
      Simple feature collection with 48 features and 15 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -111.0546 ymin: 40.99477 xmax: -104.0522 ymax: 45.00582
      Geodetic CRS:  NAD83
      First 10 features:
         STATEFP UNSDLEA   GEOID          GEOIDFQ                              NAME
      1       56   00730 5600730 9700000US5600730   Albany County School District 1
      2       56   01030 5601030 9700000US5601030   Carbon County School District 1
      3       56   01090 5601090 9700000US5601090 Big Horn County School District 4
      4       56   01260 5601260 9700000US5601260 Sublette County School District 9
      5       56   01420 5601420 9700000US5601420 Big Horn County School District 1
      6       56   01460 5601460 9700000US5601460 Big Horn County School District 2
      7       56   01470 5601470 9700000US5601470 Campbell County School District 1
      8       56   01700 5601700 9700000US5601700   Carbon County School District 2
      9       56   01980 5601980 9700000US5601980  Laramie County School District 1
      10      56   02070 5602070 9700000US5602070     Park County School District 6
         LSAD LOGRADE HIGRADE MTFCC SDTYP FUNCSTAT       ALAND    AWATER    INTPTLAT
      1    00      KG      12 G5420  <NA>        E 11070062783  89554047 +41.6655141
      2    00      KG      12 G5420  <NA>        E  9773622068 104730604 +41.6701404
      3    00      KG      12 G5420  <NA>        E  2907604928  12397426 +44.3037320
      4    00      KG      12 G5420  <NA>        E  5167822793  44824894 +42.4702137
      5    00      KG      12 G5420  <NA>        E  1166359258   2773120 +44.6511700
      6    00      KG      12 G5420  <NA>        E  1131309907  34337878 +44.8645112
      7    00      KG      12 G5420  <NA>        E 12437291865  11010427 +44.1896860
      8    00      KG      12 G5420  <NA>        E 10770613185  66803667 +41.6969310
      9    00      KG      12 G5420  <NA>        E  4110915772   3903091 +41.3214688
      10   00      KG      12 G5420  <NA>        E  6602950909  32056395 +44.3491549
             INTPTLON                       geometry
      1  -105.7218826 MULTIPOLYGON (((-106.3233 4...
      2  -107.3473386 MULTIPOLYGON (((-107.93 41....
      3  -107.8189003 MULTIPOLYGON (((-108.6839 4...
      4  -110.2065072 MULTIPOLYGON (((-110.6238 4...
      5  -108.4826306 MULTIPOLYGON (((-108.7416 4...
      6  -108.1368525 MULTIPOLYGON (((-108.6838 4...
      7  -105.5261402 MULTIPOLYGON (((-106.0252 4...
      8  -106.5669033 MULTIPOLYGON (((-107.1303 4...
      9  -104.9361616 MULTIPOLYGON (((-105.2807 4...
      10 -109.4725605 MULTIPOLYGON (((-110.1726 4...

# `tracts()` works

    Code
      tracts(state = state, county = county, progress_bar = FALSE)
    Output
      Simple feature collection with 1 feature and 13 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -104.8999 ymin: 42.60879 xmax: -104.0526 ymax: 43.50333
      Geodetic CRS:  NAD83
        STATEFP COUNTYFP TRACTCE       GEOID              GEOIDFQ NAME
      6      56      027  957200 56027957200 1400000US56027957200 9572
                 NAMELSAD MTFCC FUNCSTAT      ALAND  AWATER    INTPTLAT     INTPTLON
      6 Census Tract 9572 G5020        S 6801380681 4969450 +43.0621590 -104.4683727
                              geometry
      6 POLYGON ((-104.8999 43.4996...

# `block_groups()` work

    Code
      block_groups(state = state, county = county, progress_bar = FALSE)
    Output
      Simple feature collection with 2 features and 13 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -104.8999 ymin: 42.60879 xmax: -104.0526 ymax: 43.50333
      Geodetic CRS:  NAD83
          STATEFP COUNTYFP TRACTCE BLKGRPCE        GEOID               GEOIDFQ
      212      56      027  957200        2 560279572002 1500000US560279572002
      213      56      027  957200        1 560279572001 1500000US560279572001
               NAMELSAD MTFCC FUNCSTAT      ALAND  AWATER    INTPTLAT     INTPTLON
      212 Block Group 2 G5030        S 2363987159 2430652 +43.0111913 -104.2438245
      213 Block Group 1 G5030        S 4437393522 2538798 +43.0890643 -104.5914885
                                geometry
      212 POLYGON ((-104.4524 42.7674...
      213 POLYGON ((-104.8999 43.4996...

# `blocks()` work

    Code
      blocks(state = state, county = county, progress_bar = FALSE)
    Output
      Simple feature collection with 1276 features and 17 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -104.8999 ymin: 42.60879 xmax: -104.0526 ymax: 43.50333
      Geodetic CRS:  NAD83
      First 10 features:
          STATEFP20 COUNTYFP20 TRACTCE20 BLOCKCE20         GEOID20
      132        56        027    957200      1293 560279572001293
      174        56        027    957200      2105 560279572002105
      215        56        027    957200      2389 560279572002389
      216        56        027    957200      1307 560279572001307
      217        56        027    957200      1333 560279572001333
      263        56        027    957200      2007 560279572002007
      316        56        027    957200      2006 560279572002006
      317        56        027    957200      1074 560279572001074
      376        56        027    957200      1508 560279572001508
      603        56        027    957200      1733 560279572001733
                         GEOIDFQ20     NAME20 MTFCC20 UR20 UACE20 FUNCSTAT20  ALAND20
      132 1000000US560279572001293 Block 1293   G5040    R   <NA>          S 36670966
      174 1000000US560279572002105 Block 2105   G5040    R   <NA>          S  1250728
      215 1000000US560279572002389 Block 2389   G5040    R   <NA>          S 13140207
      216 1000000US560279572001307 Block 1307   G5040    R   <NA>          S  4114767
      217 1000000US560279572001333 Block 1333   G5040    R   <NA>          S  3803026
      263 1000000US560279572002007 Block 2007   G5040    R   <NA>          S  4842763
      316 1000000US560279572002006 Block 2006   G5040    R   <NA>          S  1994240
      317 1000000US560279572001074 Block 1074   G5040    R   <NA>          S  4617320
      376 1000000US560279572001508 Block 1508   G5040    R   <NA>          S  1147670
      603 1000000US560279572001733 Block 1733   G5040    R   <NA>          S    18135
          AWATER20  INTPTLAT20   INTPTLON20 HOUSING20 POP20
      132    85055 +43.2071353 -104.5057956         2     0
      174        0 +43.1756275 -104.3109365         0     0
      215        0 +42.6336499 -104.0734553         0     0
      216        0 +43.1903018 -104.4073020         0     0
      217        0 +43.1310128 -104.3762124         0     0
      263    37473 +43.4172361 -104.1139795         0     0
      316        0 +43.4114497 -104.0920324         0     0
      317        0 +43.3831090 -104.7228213         0     0
      376        0 +43.0072368 -104.7304868         0     0
      603        0 +42.7748502 -104.6182320         0     0
                                geometry
      132 POLYGON ((-104.539 43.25165...
      174 POLYGON ((-104.3214 43.1662...
      215 POLYGON ((-104.0941 42.6406...
      216 POLYGON ((-104.4423 43.2011...
      217 POLYGON ((-104.389 43.13834...
      263 POLYGON ((-104.1318 43.4211...
      316 POLYGON ((-104.1005 43.4082...
      317 POLYGON ((-104.7367 43.3906...
      376 POLYGON ((-104.7408 43.0043...
      603 POLYGON ((-104.6188 42.7746...

# `zctas()` works

    Code
      zctas(state = state, year = 2010, progress_bar = FALSE)
    Output
      Simple feature collection with 179 features and 11 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -111.0569 ymin: 40.99461 xmax: -104.0522 ymax: 45.00589
      Geodetic CRS:  NAD83
      First 10 features:
         STATEFP10 ZCTA5CE10 GEOID10 CLASSFP10 MTFCC10 FUNCSTAT10    ALAND10 AWATER10
      1         56     82052 5682052        B5   G6350          S   35269485        0
      2         56     82731 5682731        B5   G6350          S 1370911666  2105168
      3         56     82325 5682325        B5   G6350          S 1730855934  2224353
      4         56     82225 5682225        B5   G6350          S 3554463786  2985174
      5         56     82430 5682430        B5   G6350          S    6297169   374521
      6         56     83124 5683124        B5   G6350          S  251540950    14137
      7         56     83114 5683114        B5   G6350          S  992953290   527066
      8         56     82635 5682635        B5   G6350          S   41304462        0
      9         56     82450 5682450        B5   G6350          S   83628221        0
      10        56     82842 5682842        B5   G6350          S   38039151     6807
          INTPTLAT10   INTPTLON10 PARTFLG10                       geometry
      1  +41.1185509 -105.3086125         N MULTIPOLYGON (((-105.2875 4...
      2  +44.8247692 -105.2921039         N MULTIPOLYGON (((-105.0767 4...
      3  +41.2469594 -106.7131155         N MULTIPOLYGON (((-107.6938 4...
      4  +42.9857099 -104.3239521         N MULTIPOLYGON (((-104.0531 4...
      5  +43.8154543 -108.1849348         N MULTIPOLYGON (((-108.1775 4...
      6  +41.7731091 -110.2340116         N MULTIPOLYGON (((-110.4015 4...
      7  +42.2053822 -110.9765703         N MULTIPOLYGON (((-111.0464 4...
      8  +43.3981527 -106.2223903         N MULTIPOLYGON (((-106.2149 4...
      9  +44.5051794 -109.4355582         N MULTIPOLYGON (((-109.5092 4...
      10 +44.5730104 -106.9325212         N MULTIPOLYGON (((-106.9343 4...

# `county_subdivisions()` works

    Code
      county_subdivisions(state = state, county = county, progress_bar = FALSE)
    Output
      Simple feature collection with 2 features and 16 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -104.8999 ymin: 42.60879 xmax: -104.0526 ymax: 43.50333
      Geodetic CRS:  NAD83
         STATEFP COUNTYFP COUSUBFP COUSUBNS      GEOID             GEOIDFQ
      46      56      027    91128 01939725 5602791128 0600000US5602791128
      47      56      027    93603 01939726 5602793603 0600000US5602793603
                  NAME          NAMELSAD LSAD CLASSFP MTFCC FUNCSTAT      ALAND
      46 East Niobrara East Niobrara CCD   22      Z5 G4040        S 2363987159
      47 West Niobrara West Niobrara CCD   22      Z5 G4040        S 4437393522
          AWATER    INTPTLAT     INTPTLON                       geometry
      46 2430652 +43.0111913 -104.2438245 POLYGON ((-104.4524 42.7674...
      47 2538798 +43.0890643 -104.5914885 POLYGON ((-104.8999 43.4996...

