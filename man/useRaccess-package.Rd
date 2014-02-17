\name{ecomsUDG.useRaccess-package}
\alias{ecomsUDG.useRaccess-package}
\docType{package}
\title{
Remote access to climate datasets stored in the ECOMS User Data Gateway
}
\description{
This package enables access to hindcast datasets remotely stored at the ECOMS User Data Gateway, allowing the retrieval of dimensional slices of seasonal forecast data. The use of this package requires prior registration. Please refer to the \href{http://meteo.unican.es/ecoms-udg}{ECOMS UDG wiki} for more details.
}
\details{
\tabular{ll}{
Package: \tab ecomsUDG.useRaccess\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-01-29\cr
License: \tab GPL 2.0\cr
}

This package provides a R-based interface for accessing and downloading data from the different seasonal forecast databases stored at the ECMOS User Data Gateway (UDG). The ECOMS-UDG provides a homogeneous access point to collections of impact-relevant variables. The aim of ECOMS-UDG is to gather different data sources (including third-party) with different terms of use in a single data server, so that users can access all the data and metadata they typically need (seasonal forecasts, reanalysis and observations) in a homogeneous and simple way, without worrying about the inherent complexities of data access, download and post-processing of the variables stored in distributed databases and data servers, such as the Earth System Grid Federation (ESGF) database or a variety of massive archive systems at different institutions.

As different use policies apply to the different datasets, a fine-grained user authorization scheme has been implemented to allow them. The user may accept the usage terms and conditions as a required step to apply for the dataset access role.

For registration in the ECOMS UDG, follow the instructions in \url{http://meteo.unican.es/ecoms-udg/DataServer/Registration}

}
\author{
\href{http://www.meteo.unican.es}{Santander Meteorology Group}

Maintainer: Joaquin Bedia <bediaj@unican.es>
}
\references{
\url{http://meteo.unican.es/ecoms-udg}
}
%\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
%\examples{
%~~ simple examples of the most important functions ~~
%}
