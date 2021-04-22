#' Shift and rescale Alaska, Hawaii, and Puerto Rico in a US-wide sf object
#'
#' @param input_sf The input sf dataset
#' @param geoid_column The GEOID column of the dataset that contains a state ID.  If used, will speed up
#'                     processing and avoid spatial overlay to infer locations.  Defaults to \code{NULL}.
#' @param preserve_area If TRUE, the areas of Alaska/Hawaii/Puerto Rico relative to the continental US
#'                      will be preserved.  Defaults to FALSE where Alaska is proportionally smaller
#'                      and Hawaii/Puerto Rico are proportionally larger.
#'
#' @return The input sf object with transformed geometry
#' @export
shift_geometry <- function(input_sf,
                           geoid_column = NULL,
                           preserve_area = FALSE) {

  # Check to see if the input is an sf object, otherwise exit
  if (!any(grepl("sf", class(input_sf)))) {
    stop("The input dataset must be an sf object.", call = FALSE)
  }

  # Get a set of minimal states which we'll need to use throughout the function
  minimal_states <- tigris::states(cb = TRUE, resolution = "20m", progress_bar = FALSE)

  # Make some bboxes to check to see if shifting geometry even makes sense
  ak_bbox <- minimal_states %>%
    dplyr::filter(GEOID == "02") %>%
    sf::st_bbox() %>%
    sf::st_as_sfc()

  hi_bbox <- minimal_states %>%
    dplyr::filter(GEOID == "15") %>%
    sf::st_bbox() %>%
    sf::st_as_sfc()

  pr_bbox <- minimal_states %>%
    dplyr::filter(GEOID == "72") %>%
    sf::st_bbox() %>%
    sf::st_as_sfc()

  input_sf <- sf::st_transform(input_sf, sf::st_crs(minimal_states))

  ak_check <- suppressMessages(sf::st_intersects(input_sf, ak_bbox, sparse = FALSE)[,1])
  hi_check <- suppressMessages(sf::st_intersects(input_sf, hi_bbox, sparse = FALSE)[,1])
  pr_check <- suppressMessages(sf::st_intersects(input_sf, pr_bbox, sparse = FALSE)[,1])

  if (!any(ak_check) && !any(hi_check) && !any(pr_check)) {
    stop("None of your features are in Alaska, Hawaii, or Puerto Rico; shifting geometry isn't necessary.",
         call. = FALSE)
  }
  # Check to see if there is a GEOID column to identify state information
  # If it is a GEOID that works (e.g. counties, tracts), then use it and avoid spatial inferences
  if (!is.null(geoid_column)) {
    input_sf$state_fips <- stringr::str_sub(input_sf[[geoid_column]], 1, 2)
  } else {
      # This is where we need to infer the location of the features
      # We can do this by checking to see where the input features intersect
      # the AK/HI/PR bounding boxes
      input_sf <- input_sf %>%
        sf::st_transform(sf::st_crs(minimal_states)) %>%
        dplyr::mutate(state_fips = dplyr::case_when(
          suppressMessages(sf::st_intersects(input_sf, ak_bbox, sparse = FALSE)[,1]) ~ "02",
          suppressMessages(sf::st_intersects(input_sf, hi_bbox, sparse = FALSE)[,1]) ~ "15",
          suppressMessages(sf::st_intersects(input_sf, pr_bbox, sparse = FALSE)[,1]) ~ "72",
          TRUE ~ "00"
        ))
    }


  # Alaska/Hawaii/PR centroids are necessary to put any dataset in the correct location
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
                                   scale = 1, centroid = sf::st_centroid(geometry)) {
    (geometry - centroid) * scale +
      sf::st_sfc(st_point(position))
  }

  cont_us <- dplyr::filter(minimal_states, !GEOID %in% c("02", "15", "72")) %>%
    sf::st_transform('ESRI:102003')

  us_lower48 <- dplyr::filter(input_sf, !state_fips %in% c("02", "15", "72")) %>%
    sf::st_transform('ESRI:102003')

  bb <- sf::st_bbox(cont_us)

  us_alaska <- dplyr::filter(input_sf, state_fips == "02")

  us_hawaii <- dplyr::filter(input_sf, state_fips == "15")

  us_puerto_rico <- dplyr::filter(input_sf, state_fips == "72")

  # Initialize the list in which shapes will be stored
  shapes_list <- list(us_lower48)

  # Area not preserved (Alaska smaller, Hawaii bigger)
  if (!preserve_area) {

    if(any(ak_check)) {
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

      shapes_list <- c(shapes_list, list(ak_rescaled))
    }

    if(any(hi_check)) {

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

      shapes_list <- c(shapes_list, list(hi_rescaled))

    }


    if(any(pr_check)) {
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

      shapes_list <- c(shapes_list, list(pr_rescaled))
    }


    output_data <- shapes_list %>%
      dplyr::bind_rows() %>%
      dplyr::select(-state_fips)

    return(output_data)

  } else {

    # Area preserved (Alaska and Hawaii are true to size)

    if(any(ak_check)) {
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

      shapes_list <- c(shapes_list, list(ak_shifted))
    }


    # Shift Hawaii but do not rescale
    if(any(hi_check)) {
      hi_shifted <- sf::st_transform(us_hawaii, 'ESRI:102007')

      sf::st_geometry(hi_shifted) <- place_geometry_wilke(
        sf::st_geometry(hi_shifted),
        c(bb$xmin + 0.6*(bb$xmax - bb$xmin),
          bb$ymin - 0.1*(bb$ymax - bb$ymin)),
        scale = 1,
        centroid = hi_centroid
      )

      st_crs(hi_shifted) <- 'ESRI:102003'

      shapes_list <- c(shapes_list, list(hi_shifted))
    }

    if(any(pr_check)) {

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

      shapes_list <- c(shapes_list, list(pr_shifted))

    }

    output_data <- shapes_list %>%
      dplyr::bind_rows() %>%
      dplyr::select(-state_fips)

    return(output_data)

  }

}
