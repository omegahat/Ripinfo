\name{getIPLocation}
\alias{getIPLocation}
\alias{getIPCountry}
\title{Query location of one or more IP addresses}
\description{
  These two functions provide the interface to the ipinfo.com
  Web service for query the location of one or more IP addresses.
  \code{getIPCountry} returns the country name and code for
  the IP addresses.
  \code{getIPLocation} is a more computationally intensive
  query and also returns the longitude and latitude, city,
  postal code, region. If one only needs the country,
  use \code{getIPCountry} to relieve the burden
  on the ipinfo.com servers, and reduce marginally expensive computations
  within R.

  These functions can be called with one or more IP addresses.
  Requests with more than one IP address are broken in to
  multiple requests to the server with 25 entries or fewer per request.
}
\usage{
getIPLocation(ip, ..., curl = getCurlHandle(),
               stringsAsFactors = default.stringsAsFactors(),
                byHostName = grepl("[a-z]", ip))

getIPCountry (ip, ..., curl = getCurlHandle(),
               stringsAsFactors = default.stringsAsFactors(),
                byHostName = grepl("[a-z]", ip))
}
\arguments{
  \item{ip}{a character vector of IP addresses or names}
  \item{\dots}{additional parameters that are passed on to \code{\link[RCurl]{getForm}}}
  \item{curl}{a curl handle that is used to communicate the HTTP
    request. This can be specified by the caller  so as to reuse
    the same connection and avoid the "handshake"
    and also to specify different settings for the connection,
    e.g. passwords, proxy information, cookies, etc.
  }
  \item{stringsAsFactors}{whether the character vectors in the data
  frame result should be left  as character vectors or converted to
  factors}
\item{byHostName}{a logical vector which is used to control
  which URL we query. Typically, this does not need to be specified.
  A scalar value is used for recursive calls to direct that call
  as to whether we are dealing with host names or dotted-quad IP
  addresses.  If this is omitted, the function splits the
  IP addresses into homogeneous groups of host and dotted-quads and processes those.
 }
}
\value{
  A data frame with as many rows as there are elements in \code{ip}.
  \code{getIPCountry} has 4 elements:
  Ip, Status, CountryCode and CountryName.
  These are factors by default.

  \code{getIPLocation} returns a data frame with 10
  elements:
   Ip, Status, CountryCode, CountryName, RegionCode, RegionName, City,
   ZipPostalCode, Latitude, Longitude.
   Latitude and Longitude are numeric vectors.
   The remaining elements are  factors, or alternatively strings
   if \code{stringsAsFactors} is \code{FALSE}.
}
\references{
 \url{http://ipinfodb.com/ip_location_api.php}
}
\author{
Duncan Temple Lang
}

\seealso{
  \code{GeoIP} package to interface to the local
  MaxMinds GeoIP database via C code.

  The CSV or MySQL database of the available data can also be
  used directly. See ipinfodb.com.
}
\examples{

    a = getIPLocation(c("www.omegahat.org", "www.google.com"), stringsAsFactors = FALSE)
    sapply(a, class)
    names(a)


    getIPCountry(c("www.omegahat.org", "128.32.135.25", "www.google.com"))
    getIPCountry(c("www.omegahat.org", "www.google.com"))
    getIPCountry(c("169.237.46.32", "128.32.135.26"))

    getIPCountry(c("169.237.46.32", "128.32.135.26"), stringsAsFactors = FALSE)


     # 1000 IP addresses.  This takes about 30 seconds to run, depending
     # on the network, server load, etc..
   data(sampleIP)
   tm = system.time({sampleLocations = getIPLocation(sampleIP)})
   cat(tm/length(sampleIP), "seconds per IP\n")
   if(require(maps)) {
      map()
      with(sampleLocations, points(Longitude, Latitude, pch = ".", col = "red"))
   }
   table(sampleLocations$Status) # all okay
     # primary cities.
   sort( table(sampleLocations$City), decreasing = TRUE)[1:20]
}
\keyword{datagen}
\keyword{IO}
\concept{Web Service}

