library(tidyverse)

lookup <- read_csv("data-raw/zcta_county_rel_10.txt")

out <- select(lookup, ZCTA5, STATE, COUNTY)

write_rds(out, "data/zcta_lookup.rds")
