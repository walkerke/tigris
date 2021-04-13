\name{call_geolocator_batch}

\title{
 Batch Geocoder for the Census API
}
\description{
Takes a dataframe of address data (street, city, state, zip) and breaks it into batches that are sent to the Census bureaus batch geocoding API. It returns the provided data frame with geocoding results attached as additional columns. 
}
\usage{
call_geolocator_batch(data, 
                      Street, 
                      City, 
                      State, 
                      ZIP, 
                      benchmark="Public_AR_Current", 
                      vintage="Current_Current", 
                      batch_size=1000)
}

\arguments{
  \item{Data}{Is the dataframe of address data. Should have columns that contain street number and name, City name, State, and ZIP code}
  \item{Street}{This is the name of the column inside the provided dataframe that contains the street name and number. This should be given in quotes. Ex. "addr1"}
  \item{City}{This is the name of the column inside the provided dataframe that contains the City name. This should be given in quotes. Ex. "City"}
  \item{State}{This is the name of the column inside the provided dataframe that contains the State name. This should be given in quotes. Ex. "State"}
  \item{ZIP}{This is the name of the column inside the provided dataframe that contains the ZIP code number. This should be given in quotes. Ex. "Zip5"}
  \item{benchmark}{This is the version of the Census bureaus master address file that you would like to use to search your addresses. This function defualts to the most current version but allows for searching others. The full list of available benchmarks can be found here \href{https://geocoding.geo.census.gov/geocoder/benchmarks}{Census Benchmarks}}
  \item{vintage}{This is the version of geographies inside the benchmark that you would like to use to search your addresses. This function defaults to the most current version but allows for searching others. The full list of vintages can eb found here \href{https://geocoding.geo.census.gov/geocoder/vintages?form}{Census Vintages}}
  \item{batch_size}{This determines how many records will be included in each batch. This has implications for memory usage as the splitting of your dataframe is turned into a list that then gets sent one at a time to the API. Another consideration is that setting this value too high may cause timeouts while waiting for the API to process your request. The API limits this to a maximum of 10,000 records in a single batch. The function defaults to 1,000 in a single batch.}
}
\details{
This function will take the provided dataframe and format it so that it can be processed by the Census batch Geocoding API. The API requires a very specific format of Unique ID value, Street Address, City, State, Zip with no column headers. To achieve this an index value is attached to each record and a duplicate of the dataframe is made. Any extra data is removed from the duplicate dataframe and it is split into batches that are held in a list. The function then writes a batch to a temprary CSV file that it submits to the API. It then saves the results and moves on to the next batch in the list. When all of the batches have been sent and all results have been recieved the function combines the batch results into one large dataframe that it then joins to the originally provided data by matching the index values. This means that with large datasets this function may become memory intensive. Limiting the amount of additional data or the number of records in a dataframe when sending it to this function may provide a workaround to any memory constraints.
}
\value{
The returned dataframe will contain 5 additional columns
%%  If it is a LIST, use
\item{Match}{A column indicating if a match was found}
\item{Match_type}{A column indicating if the match was an exact or appromiate match}
\item{Match_addr}{A column showing the address that the API was able to match to}
\item{Lon}{A column showing the Longetude of a matched address}
\item{Lat}{A column showing the Latitude of a matched address}
%% ...
}
\references{
The web interface for the API can be found here:
\href{https://geocoding.geo.census.gov/geocoder/locations/addressbatch?form}{Census Batch Geolocator}
}
\author{
Mark Richards, \email{Mark.Richards.002@gmail.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Here you can read the API documentation: \href{https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf}{Census API Documentation}
}
\examples{
#Generate some test data
Test_Batch<-data.frame(uid=c(1,2),
                       Street=c('4600 Silver Hill Road','400 15th St SE'),
                       City=c('Washington','Washington'),
                       State=c('DC','DC'),
                       ZIP=c(20233,20003))

#call the function with the test data
call_geolocator_batch(data=Test_Batch,
                      Street = 'Street',
                      City = 'City',
                      State = 'State',
                      ZIP = 'ZIP',
                      benchmark = 'Public_AR_Current',
                      vintage = 'Current_Current',
                      batch_size=10)

#Expected output
                Street       City     State   ZIP  Match      Match_type                      Match_Addr       lon       lat
1   1 4600 Silver Hill Road Washington    DC 20233 Match      Exact 4600 SILVER HILL RD, WASHINGTON, DC, 20233 -76.92744 38.845985
2   2        400 15th St SE Washington    DC 20003 Match      Exact      400 15TH ST SE, WASHINGTON, DC, 20003 -76.98365    38.884

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
