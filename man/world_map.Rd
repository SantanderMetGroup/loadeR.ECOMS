\name{world_map}
\alias{world_map}
\docType{data}
\title{
Map of the world
}
\description{
A \link[sp]{SpatialPolygons} object representing the countries of the world. 
}
\usage{data(world_map)}

\details{
Adapted from the world map provided in the maptools package (see \code{?maptools::wrld_simpl})
}
\source{
See \code{?maptools::wrld_simpl}
}

\examples{
data(world_map)
proj4string(world_map)
plot(world_map)
}
\keyword{datasets}
 