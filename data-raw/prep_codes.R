library(dplyr)

county_codes <- read.csv('data-raw/national_county.txt', header = FALSE, colClasses = "character")

state_codes <- read.table('data-raw/state.txt', header = TRUE, sep = "|", colClasses = "character")

names(county_codes) <- c("state", "state_code", "county_code", "county", "type")

names(state_codes) <- c("state_code", "state", "state_name", "statens")

fips_codes <- left_join(county_codes, state_codes, by = "state_code")

fips_codes <- select(fips_codes, state = state.x, state_code, state_name, county_code, county)

new_counties <- data.frame(state = c('SD', 'AK'), state_code = c('46', '02'),
                           state_name = c('South Dakota', 'Alaska'), county_code = c('102', '158'),
                           county = c('Oglala Lakota County', 'Kusilvak Census Area'))

fips_codes <- rbind(fips_codes, new_counties) %>%
  arrange(state_code, county_code)


save(fips_codes, file = "data/fips_codes.rda")
