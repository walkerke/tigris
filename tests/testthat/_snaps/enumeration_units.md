# enumeration unit functions work

    Code
      counties(cb = TRUE, progress_bar = FALSE)
    Message
      Retrieving data for the year 2022
    Output
      Simple feature collection with 3235 features and 12 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -179.1467 ymin: -14.5487 xmax: 179.7785 ymax: 71.38782
      Geodetic CRS:  NAD83
      First 10 features:
         STATEFP COUNTYFP COUNTYNS       AFFGEOID GEOID         NAME
      1       01      069 00161560 0500000US01069 01069      Houston
      2       01      023 00161537 0500000US01023 01023      Choctaw
      3       01      005 00161528 0500000US01005 01005      Barbour
      4       01      107 00161580 0500000US01107 01107      Pickens
      5       01      033 00161542 0500000US01033 01033      Colbert
      6       04      012 00043540 0500000US04012 04012       La Paz
      7       04      001 00025441 0500000US04001 04001       Apache
      8       05      081 00066874 0500000US05081 05081 Little River
      9       05      121 00069178 0500000US05121 05121     Randolph
      10      06      037 00277283 0500000US06037 06037  Los Angeles
                    NAMELSAD STUSPS STATE_NAME LSAD       ALAND     AWATER
      1       Houston County     AL    Alabama   06  1501742235    4795415
      2       Choctaw County     AL    Alabama   06  2365900083   19114321
      3       Barbour County     AL    Alabama   06  2292160151   50523213
      4       Pickens County     AL    Alabama   06  2282835044   22621093
      5       Colbert County     AL    Alabama   06  1535742270   79160396
      6        La Paz County     AZ    Arizona   06 11646086560   36514347
      7        Apache County     AZ    Arizona   06 29003497233   54139714
      8  Little River County     AR   Arkansas   06  1375943077   83115227
      9      Randolph County     AR   Arkansas   06  1688445989   10370823
      10  Los Angeles County     CA California   06 10515988166 1785003207
                               geometry
      1  MULTIPOLYGON (((-85.71209 3...
      2  MULTIPOLYGON (((-88.47323 3...
      3  MULTIPOLYGON (((-85.74803 3...
      4  MULTIPOLYGON (((-88.34043 3...
      5  MULTIPOLYGON (((-88.13925 3...
      6  MULTIPOLYGON (((-114.7312 3...
      7  MULTIPOLYGON (((-110.0007 3...
      8  MULTIPOLYGON (((-94.48558 3...
      9  MULTIPOLYGON (((-91.40687 3...
      10 MULTIPOLYGON (((-118.6044 3...

---

    Code
      counties(progress_bar = FALSE)
    Message
      Retrieving data for the year 2022
    Output
      Simple feature collection with 3235 features and 17 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -179.2311 ymin: -14.60181 xmax: 179.8597 ymax: 71.43979
      Geodetic CRS:  NAD83
      First 10 features:
         STATEFP COUNTYFP COUNTYNS GEOID        NAME              NAMELSAD LSAD
      1       31      039 00835841 31039      Cuming         Cuming County   06
      2       53      069 01513275 53069   Wahkiakum      Wahkiakum County   06
      3       35      011 00933054 35011     De Baca        De Baca County   06
      4       31      109 00835876 31109   Lancaster      Lancaster County   06
      5       31      129 00835886 31129    Nuckolls       Nuckolls County   06
      6       72      085 01804523 72085 Las Piedras Las Piedras Municipio   13
      7       46      099 01265772 46099   Minnehaha      Minnehaha County   06
      8       48      327 01383949 48327      Menard         Menard County   06
      9       06      091 00277310 06091      Sierra         Sierra County   06
      10      21      053 00516873 21053     Clinton        Clinton County   06
         CLASSFP MTFCC CSAFP CBSAFP METDIVFP FUNCSTAT      ALAND   AWATER    INTPTLAT
      1       H1 G4020  <NA>   <NA>     <NA>        A 1477644346 10691216 +41.9158651
      2       H1 G4020  <NA>   <NA>     <NA>        A  680980770 61564427 +46.2946377
      3       H1 G4020  <NA>   <NA>     <NA>        A 6016818946 29090018 +34.3592729
      4       H1 G4020  <NA>   <NA>     <NA>        A 2169272978 22847034 +40.7835474
      5       H1 G4020  <NA>   <NA>     <NA>        A 1489645185  1718484 +40.1764918
      6       H1 G4020  <NA>   <NA>     <NA>        A   87748418    32509 +18.1871483
      7       H1 G4020  <NA>   <NA>     <NA>        A 2089707824 18182409 +43.6674723
      8       H1 G4020  <NA>   <NA>     <NA>        A 2336237980   613559 +30.8852677
      9       H1 G4020  <NA>   <NA>     <NA>        A 2468694582 23299110 +39.5769252
      10      H1 G4020  <NA>   <NA>     <NA>        A  510391738 21636754 +36.7272577
             INTPTLON                       geometry
      1  -096.7885168 MULTIPOLYGON (((-96.55515 4...
      2  -123.4244583 MULTIPOLYGON (((-123.7276 4...
      3  -104.3686961 MULTIPOLYGON (((-104.8934 3...
      4  -096.6886584 MULTIPOLYGON (((-96.68493 4...
      5  -098.0468422 MULTIPOLYGON (((-98.2737 40...
      6  -065.8711890 MULTIPOLYGON (((-65.85703 1...
      7  -096.7957261 MULTIPOLYGON (((-96.6091 43...
      8  -099.8588613 MULTIPOLYGON (((-99.7712 30...
      9  -120.5219926 MULTIPOLYGON (((-120.5559 3...
      10 -085.1360977 MULTIPOLYGON (((-85.00988 3...

---

    Code
      school_districts(state = state, progress_bar = FALSE)
    Message
      Retrieving data for the year 2022
    Output
      Simple feature collection with 48 features and 14 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -111.0546 ymin: 40.99477 xmax: -104.0522 ymax: 45.00582
      Geodetic CRS:  NAD83
      First 10 features:
         STATEFP UNSDLEA   GEOID                              NAME LSAD LOGRADE
      1       56   00730 5600730   Albany County School District 1   00      KG
      2       56   01030 5601030   Carbon County School District 1   00      KG
      3       56   01090 5601090 Big Horn County School District 4   00      KG
      4       56   01260 5601260 Sublette County School District 9   00      KG
      5       56   01420 5601420 Big Horn County School District 1   00      KG
      6       56   01460 5601460 Big Horn County School District 2   00      KG
      7       56   01470 5601470 Campbell County School District 1   00      KG
      8       56   01700 5601700   Carbon County School District 2   00      KG
      9       56   01980 5601980  Laramie County School District 1   00      KG
      10      56   02070 5602070     Park County School District 6   00      KG
         HIGRADE MTFCC SDTYP FUNCSTAT       ALAND    AWATER    INTPTLAT     INTPTLON
      1       12 G5420  <NA>        E 11070455488  89161376 +41.6655141 -105.7218826
      2       12 G5420  <NA>        E  9773622024 104730646 +41.6701404 -107.3473386
      3       12 G5420  <NA>        E  2907604929  12397425 +44.3037320 -107.8189003
      4       12 G5420  <NA>        E  5167822811  44824877 +42.4702137 -110.2065072
      5       12 G5420  <NA>        E  1166359256   2773120 +44.6511700 -108.4826306
      6       12 G5420  <NA>        E  1131309902  34337878 +44.8645112 -108.1368525
      7       12 G5420  <NA>        E 12437291886  11010427 +44.1919991 -105.5170141
      8       12 G5420  <NA>        E 10770761378  66655476 +41.6969310 -106.5669033
      9       12 G5420  <NA>        E  4110915785   3903090 +41.3214688 -104.9361616
      10      12 G5420  <NA>        E  6602950909  32056395 +44.3491549 -109.4725605
                               geometry
      1  MULTIPOLYGON (((-106.3233 4...
      2  MULTIPOLYGON (((-107.93 41....
      3  MULTIPOLYGON (((-108.6839 4...
      4  MULTIPOLYGON (((-110.6238 4...
      5  MULTIPOLYGON (((-108.7416 4...
      6  MULTIPOLYGON (((-108.6838 4...
      7  MULTIPOLYGON (((-106.0252 4...
      8  MULTIPOLYGON (((-107.1303 4...
      9  MULTIPOLYGON (((-105.2807 4...
      10 MULTIPOLYGON (((-110.1726 4...

---

    Code
      tracts(state = state, county = county, progress_bar = FALSE)
    Message
      Retrieving data for the year 2022
    Output
      Simple feature collection with 1 feature and 12 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -104.8999 ymin: 42.60879 xmax: -104.0526 ymax: 43.50333
      Geodetic CRS:  NAD83
          STATEFP COUNTYFP TRACTCE       GEOID NAME          NAMELSAD MTFCC FUNCSTAT
      155      56      027  957200 56027957200 9572 Census Tract 9572 G5020        S
               ALAND  AWATER    INTPTLAT     INTPTLON                       geometry
      155 6801380673 4969450 +43.0621590 -104.4683727 POLYGON ((-104.8999 43.4996...

---

    Code
      block_groups(state = state, county = county)
    Message
      Retrieving data for the year 2022
    Output
      Simple feature collection with 2 features and 12 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -104.8999 ymin: 42.60879 xmax: -104.0526 ymax: 43.50333
      Geodetic CRS:  NAD83
          STATEFP COUNTYFP TRACTCE BLKGRPCE        GEOID      NAMELSAD MTFCC FUNCSTAT
      134      56      027  957200        2 560279572002 Block Group 2 G5030        S
      135      56      027  957200        1 560279572001 Block Group 1 G5030        S
               ALAND  AWATER    INTPTLAT     INTPTLON                       geometry
      134 2363987154 2430652 +43.0111913 -104.2438245 POLYGON ((-104.4524 42.7674...
      135 4437393519 2538798 +43.0890643 -104.5914885 POLYGON ((-104.8999 43.4996...

---

    Code
      blocks(state = state, county = county, progress_bar = FALSE)
    Message
      Retrieving data for the year 2022
    Output
      Simple feature collection with 1276 features and 17 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -104.8999 ymin: 42.60879 xmax: -104.0526 ymax: 43.50333
      Geodetic CRS:  NAD83
      First 10 features:
          STATEFP20 COUNTYFP20 TRACTCE20 BLOCKCE20         GEOID20     NAME20 MTFCC20
      54         56        027    957200      2201 560279572002201 Block 2201   G5040
      55         56        027    957200      1199 560279572001199 Block 1199   G5040
      125        56        027    957200      1504 560279572001504 Block 1504   G5040
      155        56        027    957200      1243 560279572001243 Block 1243   G5040
      160        56        027    957200      2048 560279572002048 Block 2048   G5040
      264        56        027    957200      1293 560279572001293 Block 1293   G5040
      277        56        027    957200      2007 560279572002007 Block 2007   G5040
      288        56        027    957200      1153 560279572001153 Block 1153   G5040
      302        56        027    957200      2123 560279572002123 Block 2123   G5040
      362        56        027    957200      1547 560279572001547 Block 1547   G5040
          UR20 UACE20 UATYPE20 FUNCSTAT20  ALAND20 AWATER20  INTPTLAT20   INTPTLON20
      54     R   <NA>     <NA>          S  8866181    86693 +42.9798277 -104.1113322
      55     R   <NA>     <NA>          S  3811997     7638 +43.3294969 -104.2879296
      125    R   <NA>     <NA>          S  9827734     8882 +43.0026660 -104.7049317
      155    R   <NA>     <NA>          S  2538762    47083 +43.2755955 -104.6118058
      160    R   <NA>     <NA>          S 22345809    79209 +43.2901757 -104.2030190
      264    R   <NA>     <NA>          S 36670966    85055 +43.2071353 -104.5057956
      277    R   <NA>     <NA>          S  4842763    37473 +43.4172361 -104.1139795
      288    R   <NA>     <NA>          S  9210410     3330 +43.3683952 -104.2583045
      302    R   <NA>     <NA>          S        0     2048 +43.1325171 -104.2815422
      362    R   <NA>     <NA>          S 88474355   343508 +42.8416211 -104.6808224
          HOUSING20 POP20                       geometry
      54          0     0 POLYGON ((-104.1379 42.9888...
      55          0     0 POLYGON ((-104.3045 43.3305...
      125         2     1 POLYGON ((-104.7248 42.9802...
      155         2    11 POLYGON ((-104.6192 43.2721...
      160         0     0 POLYGON ((-104.2433 43.2519...
      264         2     0 POLYGON ((-104.539 43.25165...
      277         0     0 POLYGON ((-104.1318 43.4211...
      288         1     0 POLYGON ((-104.2736 43.3685...
      302         0     0 POLYGON ((-104.2822 43.1326...
      362         7    14 POLYGON ((-104.7629 42.8410...

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
      Retrieving data for the year 2022
    Output
      Simple feature collection with 2 features and 18 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -104.8999 ymin: 42.60879 xmax: -104.0526 ymax: 43.50333
      Geodetic CRS:  NAD83
         STATEFP COUNTYFP COUSUBFP COUSUBNS      GEOID          NAME
      10      56      027    91128 01939725 5602791128 East Niobrara
      11      56      027    93603 01939726 5602793603 West Niobrara
                  NAMELSAD LSAD CLASSFP MTFCC CNECTAFP NECTAFP NCTADVFP FUNCSTAT
      10 East Niobrara CCD   22      Z5 G4040     <NA>    <NA>     <NA>        S
      11 West Niobrara CCD   22      Z5 G4040     <NA>    <NA>     <NA>        S
              ALAND  AWATER    INTPTLAT     INTPTLON                       geometry
      10 2363987154 2430652 +43.0111913 -104.2438245 POLYGON ((-104.4524 42.7674...
      11 4437393519 2538798 +43.0890643 -104.5914885 POLYGON ((-104.8999 43.4996...

