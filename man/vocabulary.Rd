\name{vocabulary}
\alias{vocabulary}
\docType{data}
\title{
Standard definition of climate variables
}
\description{
Definition of a standard naming convention (and units) for climate variables
}
\usage{data(vocabulary)}

\details{

The ECOMS-UDG provides a homogeneous access to the different available datasets through the vocabulary. Currently, only a reduced number of variables required by the different ECOMS activities (impact applications and downscaling studies) is provided by the portal. The list of variables has been identified (and is periodically updated) according to the user's needs, receiving feedback from EUPORIAS WP22 (climate information indices, CIIs), WP23 (impact models), WP21 (calibration and downscaling) and SPECS WP61 (pilot applications) and WP52 (calibration and downscaling). See the section on the assessment of user's needs, in the \href{http://meteo.unican.es/trac/wiki/ecoms-udg/EndUserNeeds}{ECOMS-UDG wiki page} for more details. 
}
\source{
The standard definition of variables is (partially) based on the  \href{http://cf-pcmdi.llnl.gov/documents/cf-standard-names/standard-name-table/16/cf-standard-name-table.html/}{NetCDF Climate and Forecast Metadata Convention}. The reference values for vocabulary definition and the current status of variable availability can be accessed at \url{http://meteo.unican.es/ecoms-udg/DataServer/ListOfVariables}.
}

\examples{
data(vocabulary)
print(vocabulary)
}

\seealso{
\code{\link{datasetInventory}}
}
\keyword{ datasets }
