# landmarks works

    Code
      area_landmarks
    Output
      Simple feature collection with 529 features and 10 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -111.0546 ymin: 40.99826 xmax: -104.0526 ymax: 45.00354
      Geodetic CRS:  NAD83
      First 10 features:
         STATEFP ANSICODE        AREAID                     FULLNAME MTFCC    ALAND
      1       56     <NA>  110359253246           Casper Mtn Co Park K2180  1740902
      2       56     <NA>  110359253289 Natrona County International K2457 13192890
      3       56     <NA>  110488957528       Wyoming State Hospital K1231   724442
      4       56     <NA> 1101176920542                         <NA> K2180     1537
      5       56     <NA> 1101176920546                         <NA> K2582    14101
      6       56     <NA> 1101176920548                         <NA> K2582     8124
      7       56     <NA> 1101176920552                         <NA> K2582      673
      8       56     <NA> 1101176920556                         <NA> K2582     3244
      9       56     <NA> 1101176920560                         <NA> K2582    21334
      10      56     <NA>  110556855823                         <NA> K2582     2036
         AWATER    INTPTLAT     INTPTLON PARTFLG                       geometry
      1       0 +42.7408876 -106.3124022       N POLYGON ((-106.3234 42.7357...
      2       0 +42.9064867 -106.4590682       N POLYGON ((-106.4864 42.9102...
      3   14545 +41.2630055 -110.9449275       N POLYGON ((-110.9537 41.2626...
      4       0 +44.3590920 -104.8111529       N POLYGON ((-104.8116 44.3586...
      5       0 +44.6952015 -104.5864957       N POLYGON ((-104.5871 44.6959...
      6       0 +44.9152600 -105.0855918       N POLYGON ((-105.0862 44.9151...
      7       0 +44.7143862 -104.9539984       N POLYGON ((-104.9542 44.7142...
      8       0 +44.2613906 -104.2995749       N POLYGON ((-104.3 44.26165, ...
      9       0 +44.3960851 -104.3455080       N POLYGON ((-104.3463 44.3968...
      10      0 +41.8363002 -105.8667824       N POLYGON ((-105.8671 41.8361...

---

    Code
      point_landmarks
    Output
      Simple feature collection with 2583 features and 5 fields
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -111.0429 ymin: 40.99804 xmax: -104.071 ymax: 45.00064
      Geodetic CRS:  NAD83
      First 10 features:
         STATEFP ANSICODE      POINTID FULLNAME MTFCC                   geometry
      1       56     <NA> 110226932740     <NA> C3061 POINT (-109.7996 42.80469)
      2       56     <NA> 110226934586     <NA> C3061 POINT (-110.1294 42.39396)
      3       56     <NA> 110227803635 Hospital K1231 POINT (-110.1106 42.55615)
      4       56     <NA> 110228653664     <NA> C3061 POINT (-105.5639 41.30763)
      5       56     <NA> 110228656612     <NA> C3061  POINT (-105.553 41.32617)
      6       56     <NA> 110228656624     <NA> C3061 POINT (-105.5557 41.32709)
      7       56     <NA> 110228656780     <NA> C3061 POINT (-105.5716 41.33125)
      8       56     <NA> 110228656875     <NA> C3061 POINT (-105.5666 41.32112)
      9       56     <NA> 110228656907     <NA> C3061 POINT (-105.5655 41.32104)
      10      56     <NA> 110228656928     <NA> C3061 POINT (-105.5585 41.32274)

# military works

    Code
      military(progress_bar = FALSE)
    Output
      Simple feature collection with 706 features and 7 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -159.7881 ymin: 13.30706 xmax: 174.156 ymax: 64.87795
      Geodetic CRS:  NAD83
      First 10 features:
                   AREAID                                      FULLNAME MTFCC
      1  2899039483766321                   National Guard Williamstown G6500
      2  2899039483766323                    Naval Academy North Severn G6500
      3  2899039483766330 National Guard Gunpowder Military Reservation G6500
      4  2899039483766334                 Mission Gorge Recreation Area G6500
      5  2899039483766342              Coast Guard Station Chetco River G6500
      6  2899039483766347              Marine Corps Base Camp Pendleton G6500
      7  2899039483766352                                   Fort Carson G6500
      8  2899039483766358              Naval Air Station Patuxent River G6500
      9  2899039483766359                      Ellsworth Air Force Base G6500
      10 2899039483766360      Marine Corps Recruit Depot Parris Island G6500
             ALAND  AWATER    INTPTLAT     INTPTLON                       geometry
      1     408639       0 +39.3530515 -081.4421202 MULTIPOLYGON (((-81.44821 3...
      2    3353480    1540 +38.9936821 -076.4641843 MULTIPOLYGON (((-76.47621 3...
      3    1032570   10372 +39.4306656 -076.5068215 MULTIPOLYGON (((-76.51083 3...
      4    1766813       0 +32.8095738 -117.0873805 MULTIPOLYGON (((-117.1056 3...
      5      13665       0 +42.0463787 -124.2681800 MULTIPOLYGON (((-124.2685 4...
      6  507314182 1467423 +33.3802371 -117.3901006 MULTIPOLYGON (((-117.5961 3...
      7  558748820  824401 +38.5608226 -104.8280969 MULTIPOLYGON (((-104.9671 3...
      8   24942145 2254382 +38.2848482 -076.4076612 MULTIPOLYGON (((-76.4638 38...
      9   21911656   49194 +44.1501521 -103.0898924 MULTIPOLYGON (((-103.1291 4...
      10  29746163 2562367 +32.3401212 -080.6933109 MULTIPOLYGON (((-80.74397 3...

