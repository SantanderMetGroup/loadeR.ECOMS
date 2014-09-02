\name{loginECOMS_UDG}
\alias{loginECOMS_UDG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log in to the ECOMS User Data Gateway
}
\description{
Provides HTTP authentication for accessing the ECOMS-UDG.
}
\usage{
loginECOMS_UDG(username, password, proxy.host = NULL, proxy.port = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{username}{
A character string with the ECOMS UDG user 
}
  \item{password}{Character string. Authorized password}
  \item{proxy.host}{
In case of a proxy connection, the name of the proxy host, as a character string}
  \item{proxy.port}{In case of a proxy connection, an integer indicating the proxy port}
}
\details{
Only registered users can access the data server of the ECOMS-UDG. Prior to data access, users must log in.
Further details on registration for data access are provided in \url{http://meteo.unican.es/ecoms-udg/DataServer/Registration}.
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\references{
\url{http://www.meteo.unican.es/ecoms-udg}
}

\author{
\href{http://www.meteo.unican.es}{Santander Meteorology Group}
}
%\note{

%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{}
\examples{
\dontrun{
username <- "myUsername" 
password <- "myPassword" 
loginECOMS_UDG(myUsername, myPassword)

# In case the connection is performed via a proxy:

proxyHost <- "proxy.host.com" # Your institution Proxy configuration: host
proxyPort <- as.integer(1234) # and port
loginECOMS_UDG(myUsername, myPassword, proxy.host = proxyHost, proxy.port = proxyPort)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ utils }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
