% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tigris-package.R
\docType{package}
\name{tigris-package}
\alias{tigris-package}
\alias{tigris}
\title{Download and use U.S. Census TIGER shapefiles in R}
\description{
tigris is an R package that allows users to directly download and use TIGER/Line and
cartographic boundary shapefiles from the US Census Bureau in R. For an overview of the package, visit its repository at \url{https://github.com/walkerke/tigris} or read Chapter 5 of the book \emph{Analyzing US Census Data: Methods, Maps, and Models in R} at \url{https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html}.

Download TIGER/Line shapefiles from the United States Census Bureau (\url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}) and load into R as 'sf' objects.
}
\details{
Use option \code{tigris_use_cache} to tell \code{tigris} to cache
Census shapefile downloads. This is \code{FALSE} by default. e.g.
\code{options(tigris_use_cache=TRUE)}

Use option \code{tigris_refresh} to force a refresh of cached \code{tigris}
Shapefiles. e.g. \code{options(tigris_refresh=TRUE)}

Use option \code{tigris_year} to change the year for which you'd like to download data.
e.g. \code{options(tigris_year = 2017)}.  The default year for the package is 2022.

Use option \code{tigris_class} to specify the class of spatial object you'd like returned.
The default is \code{"sf"} for simple features objects.  If you'd like a legacy object
of class \code{Spatial*DataFrame}, use \code{options(tigris_class = "sp")}. Please note
that legacy sp objects are no longer formally supported in tigris.

Use the \code{protocol} argument in data download functions to specify whether to use FTP or HTTP
for downloading files. The default is \code{"ftp"}, which may work better in some environments
where HTTPS connections are restricted. For HTTPS downloads, use \code{protocol = "http"}.

Use the \code{timeout} parameter to control the timeout for downloading large files when the protocol is FTP. The default is
1800 seconds (30 minutes), which should be sufficient for most files. If you're downloading particularly
large files or have a slow connection, you may need to increase this value.
}
\note{
Several \code{options} and arguments control behavior of various \code{tigris} functions.
See \code{Details} for more information.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/walkerke/tigris}
  \item Report bugs at \url{https://github.com/walkerke/tigris/issues}
}

}
\author{
Kyle Walker (@kyle_e_walker)

\strong{Maintainer}: Kyle Walker \email{kyle@walker-data.com}

Other contributors:
\itemize{
  \item Bob Rudis \email{bob@rudis.net} [contributor]
  \item Eli Pousson \email{Eli.Pousson@baltimorecity.gov} [contributor]
}

}
\keyword{internal}
