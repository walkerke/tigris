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
      [1] "Commonwealth of the Northern Mariana Islands"
      [2] "North Carolina"                              
      [3] "North Dakota"                                

