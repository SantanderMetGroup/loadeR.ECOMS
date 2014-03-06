\name{land_map}
\alias{land_map}
\docType{data}
\title{
Map of the world land areas with simplified boundaries
}
\description{
A \link[sp]{SpatialPolygons} object representing the land areas of the world, including major islands. 
}
\usage{data(land_map)}

\details{
This dataset is useful as a spatial reference for data visualization. It may be also used for applying land/ocean masks.
}
\source{
\url{http://www.naturalearthdata.com/downloads/110m-physical-vectors/110m-land/}
}

\note{
v2.0.0
}

\examples{
data(land_map)
proj4string(land_map)
}
\keyword{datasets}
 