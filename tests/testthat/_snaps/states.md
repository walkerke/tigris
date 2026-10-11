# states works

    Code
      list_states(tigris_states)
    Output
       [1] "Alabama"                                     
       [2] "Alaska"                                      
       [3] "American Samoa"                              
       [4] "Arizona"                                     
       [5] "Arkansas"                                    
       [6] "California"                                  
       [7] "Colorado"                                    
       [8] "Commonwealth of the Northern Mariana Islands"
       [9] "Connecticut"                                 
      [10] "Delaware"                                    
      [11] "District of Columbia"                        
      [12] "Florida"                                     
      [13] "Georgia"                                     
      [14] "Guam"                                        
      [15] "Hawaii"                                      
      [16] "Idaho"                                       
      [17] "Illinois"                                    
      [18] "Indiana"                                     
      [19] "Iowa"                                        
      [20] "Kansas"                                      
      [21] "Kentucky"                                    
      [22] "Louisiana"                                   
      [23] "Maine"                                       
      [24] "Maryland"                                    
      [25] "Massachusetts"                               
      [26] "Michigan"                                    
      [27] "Minnesota"                                   
      [28] "Mississippi"                                 
      [29] "Missouri"                                    
      [30] "Montana"                                     
      [31] "Nebraska"                                    
      [32] "Nevada"                                      
      [33] "New Hampshire"                               
      [34] "New Jersey"                                  
      [35] "New Mexico"                                  
      [36] "New York"                                    
      [37] "North Carolina"                              
      [38] "North Dakota"                                
      [39] "Ohio"                                        
      [40] "Oklahoma"                                    
      [41] "Oregon"                                      
      [42] "Pennsylvania"                                
      [43] "Puerto Rico"                                 
      [44] "Rhode Island"                                
      [45] "South Carolina"                              
      [46] "South Dakota"                                
      [47] "Tennessee"                                   
      [48] "Texas"                                       
      [49] "United States Virgin Islands"                
      [50] "Utah"                                        
      [51] "Vermont"                                     
      [52] "Virginia"                                    
      [53] "Washington"                                  
      [54] "West Virginia"                               
      [55] "Wisconsin"                                   
      [56] "Wyoming"                                     

---

    Code
      list_states(tigris_states, sorted = FALSE)
    Output
       [1] "New York"                                    
       [2] "Nevada"                                      
       [3] "Alaska"                                      
       [4] "American Samoa"                              
       [5] "Vermont"                                     
       [6] "Connecticut"                                 
       [7] "Delaware"                                    
       [8] "New Mexico"                                  
       [9] "North Carolina"                              
      [10] "Wisconsin"                                   
      [11] "Oregon"                                      
      [12] "Nebraska"                                    
      [13] "Georgia"                                     
      [14] "Alabama"                                     
      [15] "Utah"                                        
      [16] "Ohio"                                        
      [17] "Oklahoma"                                    
      [18] "Tennessee"                                   
      [19] "Wyoming"                                     
      [20] "Massachusetts"                               
      [21] "Virginia"                                    
      [22] "District of Columbia"                        
      [23] "Iowa"                                        
      [24] "Arizona"                                     
      [25] "Texas"                                       
      [26] "Hawaii"                                      
      [27] "North Dakota"                                
      [28] "Kentucky"                                    
      [29] "United States Virgin Islands"                
      [30] "West Virginia"                               
      [31] "Florida"                                     
      [32] "Illinois"                                    
      [33] "Minnesota"                                   
      [34] "Maryland"                                    
      [35] "Rhode Island"                                
      [36] "Idaho"                                       
      [37] "New Hampshire"                               
      [38] "California"                                  
      [39] "New Jersey"                                  
      [40] "South Carolina"                              
      [41] "Michigan"                                    
      [42] "Arkansas"                                    
      [43] "Mississippi"                                 
      [44] "Missouri"                                    
      [45] "Montana"                                     
      [46] "Kansas"                                      
      [47] "Indiana"                                     
      [48] "Puerto Rico"                                 
      [49] "South Dakota"                                
      [50] "Colorado"                                    
      [51] "Pennsylvania"                                
      [52] "Washington"                                  
      [53] "Louisiana"                                   
      [54] "Commonwealth of the Northern Mariana Islands"
      [55] "Guam"                                        
      [56] "Maine"                                       

---

    Code
      filter_state(tigris_states, "Wyoming")
    Output
      Simple feature collection with 1 feature and 15 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -111.0546 ymin: 40.99477 xmax: -104.0522 ymax: 45.00582
      Geodetic CRS:  NAD83
         REGION DIVISION STATEFP  STATENS GEOID     GEOIDFQ STUSPS    NAME LSAD MTFCC
      19      4        8      56 01779807    56 0400000US56     WY Wyoming   00 G4000
         FUNCSTAT        ALAND     AWATER    INTPTLAT     INTPTLON
      19        A 251458188895 1868027102 +42.9896591 -107.5443922
                               geometry
      19 MULTIPOLYGON (((-106.3212 4...

---

    Code
      grep_state(tigris_states, "north")
    Output
      [1] "Commonwealth of the Northern Mariana Islands"
      [2] "North Carolina"                              
      [3] "North Dakota"                                

