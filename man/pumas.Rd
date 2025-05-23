% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pumas.R
\name{pumas}
\alias{pumas}
\title{Download a Public Use Microdata Area (PUMA) shapefile into R}
\usage{
pumas(state = NULL, cb = FALSE, year = NULL, ...)
}
\arguments{
\item{state}{The two-digit FIPS code (string) of the state you want. Can also
be state name or state abbreviation. When \code{NULL} and combined with
\code{cb = TRUE}, a national dataset of PUMAs will be returned when
\code{year} is either 2019 or 2020.}

\item{cb}{If cb is set to TRUE, download a generalized (1:500k)
states file.  Defaults to FALSE (the most detailed TIGER/Line file)}

\item{year}{the data year; defaults to 2024}

\item{...}{arguments to be passed to internal function \code{load_tiger}, which is not exported. See Additional Arguments.}
}
\description{
Public use microdata areas (PUMAs) are decennial census areas that have been
defined for the tabulation and dissemination of Public Use Microdata Sample
(PUMS) data, American Community Survey (ACS) data, and ACS period estimates.
For the 2010 Census, the State Data Centers (SDCs) in each state, the
District of Columbia, and the Commonwealth of Puerto Rico were given the
opportunity to delineate PUMAs within their state or
statistically equivalent entity. All PUMAs must nest within states and have
a minimum population threshold of 100,000 persons. 2010 PUMAs were built on
census tracts and cover the entirety of the United States, Puerto Rico,
Guam, and the U.S. Virgin Islands. Because they do not meet the minimum
population requirement, the Commonwealth of the Northern Mariana Islands
and American Samoa do not contain any 2010 PUMAs.
}
\section{Additional Arguments}{

Additional arguments that can be passed in \code{...} are:
\itemize{
\item \code{class} Desired class of return object: \code{"sf"} (the default) or \code{"sp"}.  sp classes should be considered deprecated as of tigris version 2.0, but legacy support is still available.
\item \code{progress_bar} If set to \code{FALSE}, do not display download progress bar (helpful for R Markdown documents). Defaults to \code{TRUE}.
\item \code{keep_zipped_shapefile} If set to \code{TRUE}, do not delete zipped shapefile (stored in temporary directory or \code{TIGRIS_CACHE_DIR}
depending on the configuration of global option \code{"tigris_use_cache"}). Defaults to \code{FALSE}.
\item \code{refresh} Whether to re-download cached shapefiles (\code{TRUE} or \code{FALSE}) . The default is either \code{FALSE} or the value of global
option \code{"tigris_refresh"} if it is set. Specifying this argument will override the behavior set in \code{"tigris_refresh"} global option.
\item \code{filter_by} Geometry used to filter the output returned by the function.  Can be an sf object, an object of class \code{bbox}, or a length-4 vector of format \code{c(xmin, ymin, xmax, ymax)} that can be converted to a bbox. Geometries that intersect the input to \code{filter_by} will be returned.
\item \code{protocol} Character string specifying the protocol to use for downloading files. Options are "ftp" or "http" (default). If "ftp", the URL will be modified to use FTP instead of HTTPS.
\item \code{timeout} Integer specifying the timeout in seconds for download operations if protocol is "ftp". Defaults to 1800 (30 minutes) to handle large files.
}
}

\examples{
\dontrun{
library(tigris)

us_states <- unique(fips_codes$state)[1:51]

continental_states <- us_states[!us_states \%in\% c("AK", "HI")]
pumas_list <- lapply(continental_states, function(x) {
  pumas(state = x, cb = TRUE, year = 2017)
  })

us_pumas <- rbind_tigris(pumas_list)

plot(us_pumas$geometry)
}
}
\seealso{
\url{https://www.census.gov/programs-surveys/geography/guidance/geo-areas/pumas.html}

Other general area functions: 
\code{\link{block_groups}()},
\code{\link{blocks}()},
\code{\link{counties}()},
\code{\link{county_subdivisions}()},
\code{\link{places}()},
\code{\link{school_districts}()},
\code{\link{states}()},
\code{\link{tracts}()},
\code{\link{zctas}()}
}
\concept{general area functions}
