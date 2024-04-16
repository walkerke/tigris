#' Download a roads shapefile into R
#'
#' From the Census Bureau: "The content of the all roads shapefile includes
#' primary roads, secondary roads, local neighborhood roads,
#' rural roads, city streets, vehicular trails (4WD), ramps, service drives,
#' walkways, stairways, alleys, and private roads."
#'
#' @param state A character vector of the two-digit FIPS code of the state of the county
#'        you'd like to download the roads for. Can also be state name or abbreviation
#'        (case-insensitive).
#' @param county A character vector of the three-digit FIPS code of the county you'd like
#'        the roads for. Can also be a county name.
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family transportation functions
#' @seealso <https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2020/TGRSHP2020_TechDoc.pdf>
#' @export
#' @returns
#' an sf object with columns
#'  - `LINEARID`: a unique line feature identifier [(source)](https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2019/TGRSHP2019_TechDoc.pdf)
#'  - `FULLNAME`: display name [(source)](https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2019/TGRSHP2019_TechDoc.pdf)
#'  - `RTTYP`: describes the types of roads used [(source)](https://www.census.gov/library/reference/code-lists/route-type-codes.html). Possible values are:
#'    - "C": county
#'    - "I": interstate
#'    - "M": common name
#'    - "O": other
#'    - "S": state regonized
#'    - "U": U.S.
#'  - `MTFCC`: 5-digit geographic code assignment [(see annual assignments)](https://www.census.gov/library/reference/code-lists/mt-feature-class-codes.html)
#' @examples \dontrun{
#' library(tigris)
#' library(ggplot2)
#' library(ggthemes)
#'
#' roads <- roads("Maine", "031")
#'
#' gg <- ggplot()
#' gg <- gg + geom_sf(data = roads,
#'                    color="black", fill="white", size=0.25)
#' gg <- gg + theme_map()
#' gg
#' }
roads <- function(state, county, year = NULL, ...) {

  year <- set_tigris_year(year, min_year = 2010)

  if (length(county) > 1) {
    r <- lapply(county, function(x) {
      roads(state = state, county = x, year = year, ...)
    }) %>%
      rbind_tigris()

    return(r)
  }

  state <- validate_state(state, require_state = TRUE)

  county <- validate_county(state, county, require_county = TRUE)

  url <- url_tiger("TIGER%s/ROADS/tl_%s_%s%s_roads", year, year, state, county)

  return(load_tiger(url, tigris_type = "road", ...))

}

#' Download a national primary roads shapefile into R
#'
#' From the Census Bureau: "Primary roads are generally divided, limited-access
#' highways within the Federal interstate highway system or under state
#' management. These highways are distinguished by the presence of interchanges
#' and are accessible by ramps and may include some toll highways."
#'
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @inherit roads return
#' @family transportation functions
#' @seealso <https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2020/TGRSHP2020_TechDoc.pdf>
#' @export
#' @examples \dontrun{
#' library(tigris)
#'
#' rds <- primary_roads()
#'
#' plot(rds$geometry)
#'
#' }
primary_roads <- function(year = NULL, ...) {

  year <- set_tigris_year(year, min_year = 2010)

  url <- url_tiger("TIGER%s/PRIMARYROADS/tl_%s_us_primaryroads",
                   year, year)

  return(load_tiger(url, tigris_type = "primary_roads", ...))

}

#' Download a primary & secondary roads shapefile into R
#'
#' From the Census Bureau: "Primary roads are generally divided, limited-access
#' highways within the Federal interstate highway system or under state
#' management. These highways are distinguished by the presence of interchanges
#' and are accessible by ramps and may include some toll highways. Secondary
#' roads are main arteries, usually in the U.S. highway, state highway, or
#' county highway system. These roads have one or more lanes of traffic in each
#' direction, may or may not be divided, and usually have at-grade intersections
#' with many other roads and driveways.
#'
#' @param state The two-digit FIPS code of the state of the county you'd like
#'        to download the roads for. Can also be state name or abbreviation
#'        (case-insensitive).
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @inherit roads return
#' @family transportation functions
#' @seealso <https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2020/TGRSHP2020_TechDoc.pdf>
#' @export
#' @examples \dontrun{
#' library(tigris)
#'
#' rds <- primary_secondary_roads()
#'
#' plot(rds$geometry)
#'
#' }
primary_secondary_roads <- function(state, year = NULL, ...) {

  year <- set_tigris_year(year, min_year = 2010)

  state <- validate_state(state, require_state = TRUE)

  url <- url_tiger("TIGER%s/PRISECROADS/tl_%s_%s_prisecroads",
                   year, year, state)

  return(load_tiger(url, tigris_type = "prim_sec_roads", ...))

}

#' Download a national rails shapefile into R
#'
#' National dataset for US railroads, including carlines, streetcars,
#' monorails, mass transit, cog rail, incline rail, and trams.
#'
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family transportation functions
#' @seealso <https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2020/TGRSHP2020_TechDoc.pdf>
#' @export
#' @examples \dontrun{
#' library(tigris)
#'
#' rls <- rails()
#'
#' plot(rls$geometry)
#'
#' }
rails <- function(year = NULL, ...) {

  year <- set_tigris_year(year, min_year = 2010)

  url <- url_tiger("TIGER%s/RAILS/tl_%s_us_rails", year, year)

  return(load_tiger(url, tigris_type = "rails", ...))

}


#' Download an address range features shapefile into R
#'
#' @param state The two-digit FIPS code of the state of the county you'd like
#'        to download the roads for. Can also be state name or abbreviation
#'        (case-insensitive).
#' @param county The three-digit FIPS code of the county you'd like the roads for.
#'        Can also be a county name.
#' @inheritParams load_tiger_doc_template
#' @inheritSection load_tiger_doc_template Additional Arguments
#' @family transportation functions
#' @seealso <https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2020/TGRSHP2020_TechDoc.pdf>
#' @export
address_ranges <- function(state, county, year = NULL, ...) {

  year <- set_tigris_year(year)

  state <- validate_state(state, require_state = TRUE)

  county <- validate_county(state, county, require_county = TRUE)

  url <- url_tiger("TIGER%s/ADDRFEAT/tl_%s_%s%s_addrfeat",
                   year, year, state, county)

  return(load_tiger(url, tigris_type = "address_range", ...))

}
