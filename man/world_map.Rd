\name{world_map}
\alias{world_map}
\docType{data}
\title{
Map of the world countries with simplified boundaries
}
\description{
A \link[sp]{SpatialPolygonsDataFrame} object representing the countries of the world. 
}
\usage{data(world_map)}

\details{
This dataset is useful as a spatial reference for data visualization.
}
\source{
Provided by Bjorn Sandvik, thematicmapping.org.
Downloaded from:
\url{http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip}
}

\note{
The README file of the dataset gives the following warning:
\dQuote{Use this dataset with care, as several of the borders are disputed}.
}

\examples{
data(world_map)
proj4string(world_map)
}
\keyword{datasets}
 