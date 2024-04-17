# states works

    Code
      list_states(tigris_states)
    Output
       [1] "West Virginia"                               
       [2] "Florida"                                     
       [3] "Illinois"                                    
       [4] "Minnesota"                                   
       [5] "Maryland"                                    
       [6] "Rhode Island"                                
       [7] "Idaho"                                       
       [8] "New Hampshire"                               
       [9] "North Carolina"                              
      [10] "Vermont"                                     
      [11] "Connecticut"                                 
      [12] "Delaware"                                    
      [13] "New Mexico"                                  
      [14] "California"                                  
      [15] "New Jersey"                                  
      [16] "Wisconsin"                                   
      [17] "Oregon"                                      
      [18] "Nebraska"                                    
      [19] "Pennsylvania"                                
      [20] "Washington"                                  
      [21] "Louisiana"                                   
      [22] "Georgia"                                     
      [23] "Alabama"                                     
      [24] "Utah"                                        
      [25] "Ohio"                                        
      [26] "Texas"                                       
      [27] "Colorado"                                    
      [28] "South Carolina"                              
      [29] "Oklahoma"                                    
      [30] "Tennessee"                                   
      [31] "Wyoming"                                     
      [32] "Hawaii"                                      
      [33] "North Dakota"                                
      [34] "Kentucky"                                    
      [35] "United States Virgin Islands"                
      [36] "Commonwealth of the Northern Mariana Islands"
      [37] "Guam"                                        
      [38] "Maine"                                       
      [39] "New York"                                    
      [40] "Nevada"                                      
      [41] "Alaska"                                      
      [42] "American Samoa"                              
      [43] "Michigan"                                    
      [44] "Arkansas"                                    
      [45] "Mississippi"                                 
      [46] "Missouri"                                    
      [47] "Montana"                                     
      [48] "Kansas"                                      
      [49] "Indiana"                                     
      [50] "Puerto Rico"                                 
      [51] "South Dakota"                                
      [52] "Massachusetts"                               
      [53] "Virginia"                                    
      [54] "District of Columbia"                        
      [55] "Iowa"                                        
      [56] "Arizona"                                     

---

    Code
      filter_state(tigris_states, "Wyoming")
    Output
      Simple feature collection with 1 feature and 14 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -111.0546 ymin: 40.99477 xmax: -104.0522 ymax: 45.00582
      Geodetic CRS:  NAD83
         REGION DIVISION STATEFP  STATENS GEOID STUSPS    NAME LSAD MTFCC FUNCSTAT
      31      4        8      56 01779807    56     WY Wyoming   00 G4000        A
                ALAND     AWATER    INTPTLAT     INTPTLON
      31 251458711911 1867504105 +42.9896591 -107.5443922
                               geometry
      31 MULTIPOLYGON (((-106.3212 4...

---

    Code
      grep_state(tigris_states, "north")
    Output
      [1] "North Carolina"                              
      [2] "North Dakota"                                
      [3] "Commonwealth of the Northern Mariana Islands"

