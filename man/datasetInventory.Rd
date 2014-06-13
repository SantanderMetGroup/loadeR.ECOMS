\name{datasetInventory}
\alias{datasetInventory}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Summary of hindcast database characteristics.
}

\description{
Returns summary information about the main characteristics of hindcast datasets at the ECOMS-UDG.
}

\usage{
datasetInventory(dataset)
}

\arguments{
  \item{dataset}{A character string indicating the hindcast database to be accessed. Currently accepted values are \code{System4_seasonal_15}, \code{System4_seasonal_51}, \code{System4_annual_15} and \code{CFSv2_seasonal_16}, where the notation indicates the GCM name, the temporal range and the default number of ensemble members returned respectively, separated by the lower bars.}
}

\details{
For further details on available hindcast datasets and variables, go to \url{http://meteo.unican.es/ecoms-udg/DataServer/Datasets}
}

\value{
A list of length \emph{n}, where \emph{n} is the number of variables stored at the ECOMS-UDG for that particular dataset. For each variable, the following items are returned:
\item{Description}{Description of the variable}
\item{DataType}{Character string indicating data type (i.e. float ...)}
\item{Units}{Character string indicating the units of the variable}
\item{Dimensions}{A list of length x, containing the following information for each of the x dimensions:}
\itemize{
\item \code{Type} Character vector indicating the type of dimension (e.g. runtime, Lon...)
\item \code{Units}: Character vector indicating the units of the dimension axis
\item \code{Values}: A vector containing all the dimension values. In the case of the forecast time dimension, the time span of the whole hindcast is indicated by a character \code{\link[base]{POSIXlt}} vector of length 2 with start and end. Additionally, for the time dimension, a fourth element is added to the list:
\item \code{TimeStep}: A character string indicating the time step between consecutive data along the time axis. Only for forecast time dimensions.
}
}

\references{
\url{http://meteo.unican.es/ecoms-udg}
}

\author{
\href{http://www.meteo.unican.es}{Santander Meteorology Group}
}

\seealso{
\code{\link{loadSeasonalForecast}}
}

