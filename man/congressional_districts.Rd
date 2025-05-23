% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/legislative.R
\name{congressional_districts}
\alias{congressional_districts}
\title{Download a congressional districts shapefile into R}
\usage{
congressional_districts(
  state = NULL,
  cb = FALSE,
  resolution = "500k",
  year = NULL,
  ...
)
}
\arguments{
\item{state}{The two-digit FIPS code (string) of the state you want, or a
vector of codes if you want multiple states. Can also be state name
or state abbreviation.  If \code{NULL} (the default), returns the entire United States.}

\item{cb}{If cb is set to TRUE, download a generalized (1:500k)
cartographic boundary file.  Defaults to FALSE (the most detailed
TIGER/Line file).}

\item{resolution}{The resolution of the cartographic boundary file (if cb == TRUE).
Defaults to '500k'; options include '5m' (1:5 million) and '20m' (1:20 million).}

\item{year}{the data year; defaults to 2024}

\item{...}{arguments to be passed to internal function \code{load_tiger}, which is not exported. See Additional Arguments.}
}
\description{
Description from the US Census Bureau (see link for source):
Congressional districts are the 435 areas from which members are elected to the U.S. House of Representatives. After the apportionment of congressional seats among the states, which is based on decennial census population counts, each state with multiple seats is responsible for establishing congressional districts for the purpose of electing representatives. Each congressional district is to be as equal in population to all other congressional districts in a state as practicable. The boundaries and numbers shown for the congressional districts are those specified in the state laws or court orders establishing the districts within each state.
}
\details{
Congressional districts for the 108th through 112th sessions were established by the states based on the result of the 2000 Census. Congressional districts for the 113th through 116th sessions were established by the states based on the result of the 2010 Census. Boundaries are effective until January of odd number years (for example, January 2015, January 2017, etc.), unless a state initiative or court ordered redistricting requires a change. All states established new congressional districts in 2011-2012, with the exception of the seven single member states (Alaska, Delaware, Montana, North Dakota, South Dakota, Vermont, and Wyoming).

The current default in tigris reflects boundaries for the 118th Congress, which is available for years 2022 and 2023.  Older congressional district boundaries back to 2011 can be obtained by supplying the appropriate year. For the 119th Congress, use \code{year = 2024}.
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
library(leaflet)

cd118 <- congressional_districts(cb = TRUE, resolution = '20m', year = 2022)

leaflet(cd118) \%>\%
   addTiles() \%>\%
   addPolygons()
}
}
\seealso{
\url{https://www.census.gov/programs-surveys/geography/guidance/geo-areas/congressional-dist.html}

Other legislative district functions: 
\code{\link{state_legislative_districts}()},
\code{\link{voting_districts}()}
}
\concept{legislative district functions}
