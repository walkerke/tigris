# landmarks works

    Code
      area_landmarks
    Output
      Simple feature collection with 530 features and 10 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -111.0546 ymin: 40.99826 xmax: -104.0526 ymax: 45.00354
      Geodetic CRS:  NAD83
      First 10 features:
         STATEFP ANSICODE        AREAID               FULLNAME MTFCC    ALAND AWATER
      1       56     <NA> 1104470922860  St John's Medical Ctr K1231    65616      0
      2       56     <NA>  110460800571                   <NA> K2582    12971      0
      3       56     <NA>  110460800575                   <NA> K2582      911      0
      4       56     <NA>  110460800579                   <NA> K2582     8757      0
      5       56     <NA>  110460800583                   <NA> K2582    19469      0
      6       56     <NA>  110460799841       Dubois K-12 Schl K2543    60581      0
      7       56     <NA>  110460799842         Popo Agie Park K2180    78700      0
      8       56     <NA>  110460799867        North Side Park K2180    39302      0
      9       56     <NA> 1104702249950 Fossil Butte Natl Mnmt K2181 33855806      0
      10      56     <NA> 1103718389027   Sutherland West Park K2180    31708      0
            INTPTLAT     INTPTLON PARTFLG                       geometry
      1  +43.4807310 -110.7506020       N POLYGON ((-110.7523 43.4817...
      2  +42.8828260 -108.7566937       N POLYGON ((-108.7579 42.8831...
      3  +42.7148502 -108.6460773       N POLYGON ((-108.6463 42.7149...
      4  +43.1562202 -108.7073793       N POLYGON ((-108.7081 43.1564...
      5  +43.2402600 -108.1343364       N POLYGON ((-108.1361 43.2405...
      6  +43.5418398 -109.6298682       N POLYGON ((-109.6318 43.5419...
      7  +42.8321627 -108.7424144       N POLYGON ((-108.7447 42.8316...
      8  +42.8437040 -108.7387819       N POLYGON ((-108.74 42.84472,...
      9  +41.8561439 -110.7622948       N POLYGON ((-110.8147 41.8899...
      10 +44.2611854 -105.5119653       N POLYGON ((-105.5139 44.2616...

---

    Code
      point_landmarks
    Output
      Simple feature collection with 2623 features and 5 fields
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
    Message
      Retrieving data for the year 2024
    Output
      Simple feature collection with 858 features and 7 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -159.7881 ymin: 13.30706 xmax: 174.156 ymax: 64.87795
      Geodetic CRS:  NAD83
      First 10 features:
                AREAID                   FULLNAME MTFCC    ALAND AWATER    INTPTLAT
      1   110509768024   Tripler Army Medical Ctr K2110  1443458      0 +21.3620086
      2   110509767829              Makua Mil Res K2110 17406401      0 +21.5307680
      3   110435324283        Stone Ranch Mil Res K2110  7440946      0 +41.3654109
      4   110507841923  Ng Havre de Grace Mil Res K2110   304109      0 +39.5327115
      5   110509768183           Aliamanu Mil Res K2110  2104132      0 +21.3609483
      6   110509768096        Waianae-Kai Mil Res K2110    34407      0 +21.4469221
      7   110509768142 Schofield Barracks Mil Res K2110 20509113      0 +21.4993768
      8   110509768059           Helemano Mil Res K2110  1204608      0 +21.5361123
      9  1108310660208   Ng Mta Gunpowder Mil Res K2110  1032570  10372 +39.4306656
      10 1108301385692          Ng Papago Mil Res K2110  1844509      0 +33.4673291
             INTPTLON                       geometry
      1  -157.8896492 MULTIPOLYGON (((-157.8982 2...
      2  -158.2068764 MULTIPOLYGON (((-158.2406 2...
      3   -72.2716641 MULTIPOLYGON (((-72.26356 4...
      4   -76.1041621 MULTIPOLYGON (((-76.11008 3...
      5  -157.9117716 MULTIPOLYGON (((-157.9238 2...
      6  -158.1909990 MULTIPOLYGON (((-158.1931 2...
      7  -158.0824984 MULTIPOLYGON (((-158.1151 2...
      8  -158.0194801 MULTIPOLYGON (((-158.0267 2...
      9   -76.5068215 MULTIPOLYGON (((-76.51083 3...
      10 -111.9621997 MULTIPOLYGON (((-111.9698 3...

