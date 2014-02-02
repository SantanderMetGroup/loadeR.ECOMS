\name{datasetInventory}
\alias{datasetInventory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary of hindcast database characteristics
}
\description{
Returns summary information about the main characteristics of hindcast datasets at the ECOMS UDG.
}
\usage{
datasetInventory(dataset = c("System4_seasonal_15", "System4_seasonal_51",
  "System4_annual_15", "CFSv2_seasonal_16"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{A character string indicating the hindcast database. Currently four datasets available (See Details).}
}
\details{
For further details on available hindcast datasets and variables, go to \url{https://meteo.unican.es/trac/wiki/EcomsUdg/DataServer/Datasets}
}
\value{
A list of length \emph{n}, where \emph{n} is the number of variables stored at the ECOMS UDG for that particular dataset. For each variable, the following items are returned:
\item{Description}{Description of the variable}
\item{DataType}{Character string indicating data type (i.e. float ...)}
\item{Units}{Character string indicating the units of the variable}
\item{TimeStep}{A \code{difftime} class object representing the time interval between consecutive values in the time dimension axis} 
\item{Dimensions}{A list of length x, containing the following information for each of the x dimensions:}
\itemize{
\item \code{Type} Character vector indicating the type of dimension (e.g. runtime, Lon...)
\item \code{Units}: Character vector indicating the units of the dimension axis
\item \code{Values}: A vector containing all the dimension values. In the case of runtimes, this is a vector of \code{POSIXlt} times. For the forecast times, the time span of the whole hindcast is indicated by a character string with the time range.
}
}

\references{
\url{https://meteo.unican.es/trac/wiki/EcomsUdg}
}
\author{
\href{http://www.meteo.unican.es}{Santander Meteorology Group}
}
%\note{
%%  ~~further notes~~
%}

%\section{Warning}{Note that the time span for the CFSvs hindcast is currently not returned} 

\seealso{
\code{\link{loadSeasonalForecast}}
}
%\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
