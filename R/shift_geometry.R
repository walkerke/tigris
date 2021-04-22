#' Shift and rescale Alaska, Hawaii, and Puerto Rico in a US-wide sf object
#'
#' @param input_sf The input sf dataset
#' @param state_column The column representing state IDs (optional). If NULL, a GEOID column
#'                     like those included in tigris or tidycensus is required.
#' @param preserve_area If TRUE, the areas of Alaska/Hawaii/Puerto Rico relative to the continental US
#'                      will be preserved.  Defaults to FALSE where Alaska is proportionally smaller
#'                      and Hawaii/Puerto Rico are proportionally larger.
#'
#' @return The input sf object with transformed geometry
#' @export
shift_geometry <- function(input_sf,
                           state_column = NULL,
                           preserve_area = FALSE) {

  # Check to see if the input is an sf object, otherwise exit
  if (!any(grepl("sf", class(input_sf)))) {
    stop("The input dataset must be an sf object.", call = FALSE)
  }

  # Check to see if there is a GEOID column to identify state information
  # If not, validate the state
  if (!is.null(state_column)) {
    input_sf$state_fips <- suppressWarnings(suppressMessages(tigris:::validate_state(input_sf[[state_column]])))
  } else {
    if (!"GEOID" %in% names(input_sf)) {
      stop("A GEOID column must be present in your data if the state_column argument is NULL",
           call. = FALSE)
    } else {
      input_sf$state_fips <- stringr::str_sub(input_sf$GEOID, 1, 2)
    }
  }

  # Alaska/Hawaii/PR centroids are necessary to put any dataset in the correct location
  minimal_states <- tigris::states(cb = TRUE, resolution = "20m")

  ak_crs <- 3338
  hi_crs <- 'ESRI:102007'
  pr_crs <- 32161

  ak_centroid <- minimal_states %>%
    dplyr::filter(GEOID == "02") %>%
    sf::st_transform(ak_crs) %>%
    sf::st_geometry() %>%
    sf::st_centroid()

  hi_centroid <- minimal_states %>%
    dplyr::filter(GEOID == "15") %>%
    sf::st_transform(hi_crs) %>%
    sf::st_geometry() %>%
    sf::st_centroid()

  pr_centroid <- minimal_states %>%
    dplyr::filter(GEOID == "72") %>%
    sf::st_transform(pr_crs) %>%
    sf::st_geometry() %>%
    sf::st_centroid()


  # Parse the geometries (thanks to Claus Wilke for code samples & inspiration)
  place_geometry_wilke <- function(geometry, position,
                                   scale = 1, centroid = st_centroid(geometry)) {
    (geometry - centroid) * scale +
      st_sfc(st_point(position))
  }

  us_lower48 <- dplyr::filter(input_sf, !state_fips %in% c("02", "15", "72")) %>%
    sf::st_transform('ESRI:102003')

  bb <- sf::st_bbox(us_lower48)

  us_alaska <- dplyr::filter(input_sf, state_fips == "02")

  us_hawaii <- dplyr::filter(input_sf, state_fips == "15")

  us_puerto_rico <- dplyr::filter(input_sf, state_fips == "72")

  # TODO: add option for Puerto Rico

  # Area not preserved (Alaska smaller, Hawaii bigger)
  if (!preserve_area) {

    # Rescale and shift Alaska
    ak_rescaled <- sf::st_transform(us_alaska, ak_crs)

    st_geometry(ak_rescaled) <- place_geometry_wilke(
      sf::st_geometry(ak_rescaled),
      c(bb$xmin + 0.08*(bb$xmax - bb$xmin),
        bb$ymin + 0.07*(bb$ymax - bb$ymin)),
      scale = 0.5,
      centroid = ak_centroid
    )

    sf::st_crs(ak_rescaled) <- 'ESRI:102003'

    # Rescale and shift Hawaii
    hi_rescaled <- sf::st_transform(us_hawaii, hi_crs)

    sf::st_geometry(hi_rescaled) <- place_geometry_wilke(
      sf::st_geometry(hi_rescaled),
      c(bb$xmin + 0.35*(bb$xmax - bb$xmin),
        bb$ymin + 0.*(bb$ymax - bb$ymin)),
      scale = 1.5,
      centroid = hi_centroid
    )

    st_crs(hi_rescaled) <- 'ESRI:102003'

    # Rescale and shift Puerto Rico
    pr_rescaled <- sf::st_transform(us_puerto_rico, pr_crs)

    sf::st_geometry(pr_rescaled) <- place_geometry_wilke(
      sf::st_geometry(pr_rescaled),
      c(bb$xmin + 0.65*(bb$xmax - bb$xmin),
        bb$ymin + 0.*(bb$ymax - bb$ymin)),
      scale = 2.5,
      centroid = pr_centroid
    )

    st_crs(pr_rescaled) <- 'ESRI:102003'

    output_data <- dplyr::bind_rows(us_lower48, ak_rescaled, hi_rescaled, pr_rescaled) %>%
      select(-state_fips)

    return(output_data)

  } else {

    # Area preserved (Alaska and Hawaii are true to size)

    # Shift Alaska but do not rescale
    ak_shifted <- sf::st_transform(us_alaska, 3338)

    st_geometry(ak_shifted) <- place_geometry_wilke(
      sf::st_geometry(ak_shifted),
      c(bb$xmin + 0.2*(bb$xmax - bb$xmin),
        bb$ymin - 0.13*(bb$ymax - bb$ymin)),
      scale = 1,
      centroid = ak_centroid
    )

    sf::st_crs(ak_shifted) <- 'ESRI:102003'

    # Shift Hawaii but do not rescale

    hi_shifted <- sf::st_transform(us_hawaii, 'ESRI:102007')

    sf::st_geometry(hi_shifted) <- place_geometry_wilke(
      sf::st_geometry(hi_shifted),
      c(bb$xmin + 0.6*(bb$xmax - bb$xmin),
        bb$ymin - 0.1*(bb$ymax - bb$ymin)),
      scale = 1,
      centroid = hi_centroid
    )

    st_crs(hi_shifted) <- 'ESRI:102003'

    # Shift Puerto Rico but do not rescale
    pr_shifted <- sf::st_transform(us_puerto_rico, pr_crs)

    sf::st_geometry(pr_shifted) <- place_geometry_wilke(
      sf::st_geometry(pr_shifted),
      c(bb$xmin + 0.75*(bb$xmax - bb$xmin),
        bb$ymin - 0.1*(bb$ymax - bb$ymin)),
      scale = 1,
      centroid = pr_centroid
    )

    st_crs(pr_shifted) <- 'ESRI:102003'

    output_data <- dplyr::bind_rows(us_lower48, ak_shifted, hi_shifted, pr_shifted) %>%
      select(-state_fips)

    return(output_data)

  }

}
