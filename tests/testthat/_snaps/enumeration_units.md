# enumeration unit functions work

    Code
      counties(cb = TRUE, progress_bar = FALSE)
    Message
      Retrieving data for the year 2023
    Output
      Simple feature collection with 3235 features and 12 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -179.1467 ymin: -14.5487 xmax: 179.7785 ymax: 71.38782
      Geodetic CRS:  NAD83
      First 10 features:
         STATEFP COUNTYFP COUNTYNS        GEOIDFQ GEOID        NAME
      1       01      003 00161527 0500000US01003 01003     Baldwin
      2       01      069 00161560 0500000US01069 01069     Houston
      3       01      005 00161528 0500000US01005 01005     Barbour
      4       01      119 00161585 0500000US01119 01119      Sumter
      5       05      091 00069166 0500000US05091 05091      Miller
      6       05      133 00069182 0500000US05133 05133      Sevier
      7       05      093 00069899 0500000US05093 05093 Mississippi
      8       06      037 00277283 0500000US06037 06037 Los Angeles
      9       06      087 00277308 0500000US06087 06087  Santa Cruz
      10      06      097 01657246 0500000US06097 06097      Sonoma
                   NAMELSAD STUSPS STATE_NAME LSAD       ALAND     AWATER
      1      Baldwin County     AL    Alabama   06  4117725048 1132887203
      2      Houston County     AL    Alabama   06  1501742235    4795415
      3      Barbour County     AL    Alabama   06  2292160151   50523213
      4       Sumter County     AL    Alabama   06  2340898915   24634880
      5       Miller County     AR   Arkansas   06  1616257232   36848741
      6       Sevier County     AR   Arkansas   06  1459636819   45919661
      7  Mississippi County     AR   Arkansas   06  2336409866   72224609
      8  Los Angeles County     CA California   06 10515988121 1785003256
      9   Santa Cruz County     CA California   06  1152818089  419720203
      10      Sonoma County     CA California   06  4080103614  497291856
                               geometry
      1  MULTIPOLYGON (((-88.02858 3...
      2  MULTIPOLYGON (((-85.71209 3...
      3  MULTIPOLYGON (((-85.74803 3...
      4  MULTIPOLYGON (((-88.41492 3...
      5  MULTIPOLYGON (((-94.04343 3...
      6  MULTIPOLYGON (((-94.47732 3...
      7  MULTIPOLYGON (((-90.2888 35...
      8  MULTIPOLYGON (((-118.6044 3...
      9  MULTIPOLYGON (((-122.3177 3...
      10 MULTIPOLYGON (((-123.5335 3...

---

    Code
      counties(progress_bar = FALSE)
    Message
      Retrieving data for the year 2023
    Output
      Simple feature collection with 3235 features and 18 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -179.2311 ymin: -14.60181 xmax: 179.8597 ymax: 71.43979
      Geodetic CRS:  NAD83
      First 10 features:
         STATEFP COUNTYFP COUNTYNS GEOID        GEOIDFQ        NAME
      1       31      039 00835841 31039 0500000US31039      Cuming
      2       53      069 01513275 53069 0500000US53069   Wahkiakum
      3       35      011 00933054 35011 0500000US35011     De Baca
      4       31      109 00835876 31109 0500000US31109   Lancaster
      5       31      129 00835886 31129 0500000US31129    Nuckolls
      6       72      085 01804523 72085 0500000US72085 Las Piedras
      7       46      099 01265772 46099 0500000US46099   Minnehaha
      8       48      327 01383949 48327 0500000US48327      Menard
      9       06      091 00277310 06091 0500000US06091      Sierra
      10      21      053 00516873 21053 0500000US21053     Clinton
                      NAMELSAD LSAD CLASSFP MTFCC CSAFP CBSAFP METDIVFP FUNCSTAT
      1          Cuming County   06      H1 G4020  <NA>   <NA>     <NA>        A
      2       Wahkiakum County   06      H1 G4020  <NA>   <NA>     <NA>        A
      3         De Baca County   06      H1 G4020  <NA>   <NA>     <NA>        A
      4       Lancaster County   06      H1 G4020   339  30700     <NA>        A
      5        Nuckolls County   06      H1 G4020  <NA>   <NA>     <NA>        A
      6  Las Piedras Municipio   13      H1 G4020   490  41980     <NA>        A
      7       Minnehaha County   06      H1 G4020  <NA>  43620     <NA>        A
      8          Menard County   06      H1 G4020  <NA>   <NA>     <NA>        A
      9          Sierra County   06      H1 G4020  <NA>   <NA>     <NA>        A
      10        Clinton County   06      H1 G4020  <NA>   <NA>     <NA>        A
              ALAND   AWATER    INTPTLAT     INTPTLON                       geometry
      1  1477563029 10772508 +41.9158651 -096.7885168 MULTIPOLYGON (((-96.55515 4...
      2   680980771 61564427 +46.2946377 -123.4244583 MULTIPOLYGON (((-123.7276 4...
      3  6016818946 29090018 +34.3592729 -104.3686961 MULTIPOLYGON (((-104.8934 3...
      4  2169269688 22850324 +40.7835474 -096.6886584 MULTIPOLYGON (((-96.68493 4...
      5  1489645187  1718484 +40.1764918 -098.0468422 MULTIPOLYGON (((-98.2737 40...
      6    87748419    32509 +18.1871483 -065.8711890 MULTIPOLYGON (((-65.85703 1...
      7  2089707837 18182409 +43.6674723 -096.7957261 MULTIPOLYGON (((-96.45339 4...
      8  2336237980   613559 +30.8852677 -099.8588613 MULTIPOLYGON (((-99.7712 30...
      9  2468694578 23299110 +39.5769252 -120.5219926 MULTIPOLYGON (((-120.5559 3...
      10  510391738 21636754 +36.7272577 -085.1360977 MULTIPOLYGON (((-85.00988 3...

---

    Code
      school_districts(state = state, progress_bar = FALSE)
    Message
      Retrieving data for the year 2023
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
      1    00      KG      12 G5420  <NA>        E 11070062818  89554047 +41.6655141
      2    00      KG      12 G5420  <NA>        E  9773622024 104730646 +41.6701404
      3    00      KG      12 G5420  <NA>        E  2907604929  12397425 +44.3037320
      4    00      KG      12 G5420  <NA>        E  5167822811  44824877 +42.4702137
      5    00      KG      12 G5420  <NA>        E  1166359256   2773120 +44.6511700
      6    00      KG      12 G5420  <NA>        E  1131309902  34337878 +44.8645112
      7    00      KG      12 G5420  <NA>        E 12437291886  11010427 +44.1919991
      8    00      KG      12 G5420  <NA>        E 10770613188  66803667 +41.6969310
      9    00      KG      12 G5420  <NA>        E  4110915784   3903090 +41.3214688
      10   00      KG      12 G5420  <NA>        E  6602950909  32056395 +44.3491549
             INTPTLON                       geometry
      1  -105.7218826 MULTIPOLYGON (((-106.3233 4...
      2  -107.3473386 MULTIPOLYGON (((-107.93 41....
      3  -107.8189003 MULTIPOLYGON (((-108.6839 4...
      4  -110.2065072 MULTIPOLYGON (((-110.6238 4...
      5  -108.4826306 MULTIPOLYGON (((-108.7416 4...
      6  -108.1368525 MULTIPOLYGON (((-108.6838 4...
      7  -105.5170141 MULTIPOLYGON (((-106.0252 4...
      8  -106.5669033 MULTIPOLYGON (((-107.1303 4...
      9  -104.9361616 MULTIPOLYGON (((-105.2807 4...
      10 -109.4725605 MULTIPOLYGON (((-110.1726 4...

---

    Code
      tracts(state = state, county = county, progress_bar = FALSE)
    Message
      Retrieving data for the year 2023
    Output
      Simple feature collection with 1 feature and 13 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -104.8999 ymin: 42.60879 xmax: -104.0526 ymax: 43.50333
      Geodetic CRS:  NAD83
          STATEFP COUNTYFP TRACTCE       GEOID              GEOIDFQ NAME
      114      56      027  957200 56027957200 1400000US56027957200 9572
                   NAMELSAD MTFCC FUNCSTAT      ALAND  AWATER    INTPTLAT
      114 Census Tract 9572 G5020        S 6801380673 4969450 +43.0621590
              INTPTLON                       geometry
      114 -104.4683727 POLYGON ((-104.8999 43.4996...

---

    Code
      block_groups(state = state, county = county)
    Message
      Retrieving data for the year 2023
    Output
      Simple feature collection with 2 features and 13 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -104.8999 ymin: 42.60879 xmax: -104.0526 ymax: 43.50333
      Geodetic CRS:  NAD83
          STATEFP COUNTYFP TRACTCE BLKGRPCE        GEOID               GEOIDFQ
      66       56      027  957200        1 560279572001 1500000US560279572001
      149      56      027  957200        2 560279572002 1500000US560279572002
               NAMELSAD MTFCC FUNCSTAT      ALAND  AWATER    INTPTLAT     INTPTLON
      66  Block Group 1 G5030        S 4437393519 2538798 +43.0890643 -104.5914885
      149 Block Group 2 G5030        S 2363987154 2430652 +43.0111913 -104.2438245
                                geometry
      66  POLYGON ((-104.8999 43.4996...
      149 POLYGON ((-104.4524 42.7674...

---

    Code
      blocks(state = state, county = county, progress_bar = FALSE)
    Message
      Retrieving data for the year 2023
    Output
      Simple feature collection with 1276 features and 17 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -104.8999 ymin: 42.60879 xmax: -104.0526 ymax: 43.50333
      Geodetic CRS:  NAD83
      First 10 features:
         STATEFP20 COUNTYFP20 TRACTCE20 BLOCKCE20         GEOID20
      7         56        027    957200      1039 560279572001039
      8         56        027    957200      2374 560279572002374
      9         56        027    957200      1768 560279572001768
      10        56        027    957200      1328 560279572001328
      11        56        027    957200      1270 560279572001270
      12        56        027    957200      1287 560279572001287
      32        56        027    957200      1068 560279572001068
      33        56        027    957200      1275 560279572001275
      34        56        027    957200      1681 560279572001681
      48        56        027    957200      1263 560279572001263
                        GEOIDFQ20    NAME20 MTFCC20 UR20 UACE20 FUNCSTAT20  ALAND20
      7  1000000US560279572001039 Block1039   G5040    R   <NA>          S  1448084
      8  1000000US560279572002374 Block2374   G5040    R   <NA>          S   354373
      9  1000000US560279572001768 Block1768   G5040    R   <NA>          S 45107596
      10 1000000US560279572001328 Block1328   G5040    R   <NA>          S  4159736
      11 1000000US560279572001270 Block1270   G5040    R   <NA>          S  1320148
      12 1000000US560279572001287 Block1287   G5040    R   <NA>          S 18768165
      32 1000000US560279572001068 Block1068   G5040    R   <NA>          S  3677543
      33 1000000US560279572001275 Block1275   G5040    R   <NA>          S 14349463
      34 1000000US560279572001681 Block1681   G5040    R   <NA>          S  3540248
      48 1000000US560279572001263 Block1263   G5040    R   <NA>          S 19386790
         AWATER20  INTPTLAT20   INTPTLON20 HOUSING20 POP20
      7         0 +43.4411881 -104.8840127         0     0
      8         0 +42.6661756 -104.0987438         0     0
      9         0 +42.6450661 -104.5798050         3     2
      10    30277 +43.1089792 -104.4315139         0     0
      11        0 +43.2039031 -104.6955923         0     0
      12        0 +43.1098523 -104.5120357         1     4
      32        0 +43.4127443 -104.6859831         0     0
      33        0 +43.1741871 -104.6759531         0     0
      34    30718 +42.7647927 -104.4978133         0     0
      48        0 +43.2484121 -104.8578082         0     0
                               geometry
      7  POLYGON ((-104.8945 43.4467...
      8  POLYGON ((-104.1039 42.6632...
      9  POLYGON ((-104.6429 42.6333...
      10 POLYGON ((-104.4624 43.1088...
      11 POLYGON ((-104.7113 43.1987...
      12 POLYGON ((-104.5532 43.1102...
      32 POLYGON ((-104.7137 43.4110...
      33 POLYGON ((-104.7024 43.1769...
      34 POLYGON ((-104.5173 42.7696...
      48 POLYGON ((-104.8983 43.2649...

---

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

---

    Code
      county_subdivisions(state = state, county = county)
    Message
      Retrieving data for the year 2023
    Output
      Simple feature collection with 2 features and 16 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -104.8999 ymin: 42.60879 xmax: -104.0526 ymax: 43.50333
      Geodetic CRS:  NAD83
         STATEFP COUNTYFP COUSUBFP COUSUBNS      GEOID             GEOIDFQ
      10      56      027    91128 01939725 5602791128 0600000US5602791128
      11      56      027    93603 01939726 5602793603 0600000US5602793603
                  NAME          NAMELSAD LSAD CLASSFP MTFCC FUNCSTAT      ALAND
      10 East Niobrara East Niobrara CCD   22      Z5 G4040        S 2363987154
      11 West Niobrara West Niobrara CCD   22      Z5 G4040        S 4437393519
          AWATER    INTPTLAT     INTPTLON                       geometry
      10 2430652 +43.0111913 -104.2438245 POLYGON ((-104.4524 42.7674...
      11 2538798 +43.0890643 -104.5914885 POLYGON ((-104.8999 43.4996...

