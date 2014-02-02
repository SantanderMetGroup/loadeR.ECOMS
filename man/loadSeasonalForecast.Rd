\name{loadSeasonalForecast}
\alias{loadSeasonalForecast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remote access to hindcast model data stored at the ECOMS UDG.
}
\description{
A simple interface for accesing and retrieving dimensional slices of the various seasonal forecast databases stored at the ECOMS User Data Gateway.
}
\usage{
loadSeasonalForecast(dataset = c("System4_seasonal_15", "System4_seasonal_51",
  "System4_annual_15", "CFSv2_seasonal_16"), var, dictionary = TRUE,
  members = NULL, lonLim = NULL, latLim = NULL,
  season = NULL, years = NULL, leadMonth = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{A character string indicating the hindcast database to be accessed. Currently accepted values are \code{System4_seasonal_15}, \code{System4_seasonal_51}, \code{System4_annual_15} and \code{CFSv2_seasonal_16}, where the notation indicates the GCM name, the temporal range and the default number of ensemble members returned respectively, separated by the lower bars. See Details}
  \item{var}{
Variable code (character string). This is the name of the variable either as coded in the dataset (i.e. as provided by \code{datasetInventory}) or according to the identifier code in the dictionary if provided (see the next argument).
}
  \item{dictionary}{
A logical flag indicating if a dictionary is being used. Default (recommended) is set to \code{TRUE}, meaning that the function will internally perform the necessary conversions to return the standard variables defined in the vocabulary. See details. 
}
  \item{members}{
A vector of integers indicating the members to be loaded. Default to \code{NULL}, which loads the default members. For instance, \code{members=1:5} will retrieve the first five members of the hindcast. 
}
  \item{lonLim}{
Vector of length = 2, with minimum and maximum longitude coordinates, in decimal degrees, of the bounding box selected. For single-point queries, a numeric value with the longitude coordinate. If \code{NULL} (default), the whole longitudinal range is selected (Note that this may lead to a large output object size). See details. 
}
  \item{latLim}{
Same as \code{lonLim}, but for the selection of the latitudinal range.
}
  \item{season}{
An integer vector specifying the desired season (in months, January = 1 ...). Options include one to several (contiguous) months. If \code{NULL} (default), the function will return all possible months given the lead month. See details. 
}
  \item{years}{
Optional vector of years to select. Default to all available years. See details. 
}
  \item{leadMonth}{
Integer value indicating the lead forecast time, relative to the first month of \code{season}. Note that \code{leadMonth=1} for \code{season=1} (January) corresponds to the December initialization. Default to 1} %The effect of the lead time in the forecast for a particular season can be analyzed by just changing this parameter. 
}

\value{
A list with the following elements providing the necessary information for data representation and analysis:
\item{VarName}{A character string indicating which is the variable returned. Same as value provided for argument \code{var}}
\item{isStandard}{Logical value indicating whether the variable returned is standard or not (i.e., wether the dictionary has been used or not)}
\item{MemberData}{A list of length \emph{n}, where \emph{n} = number of members of the ensemble indicated in the \code{member} argument. Each element of the dataset is a 2-D matrix of \emph{i} rows x \emph{j} columns, of \emph{i} forecast times and \emph{j} grid-points}
\item{LonLatCoords}{A SpatialPoints class object (library sp). This can be seen as a 2-D matrix of j rows (where j = number of grid points selected) and two columns corresponding to the longitude and latitude coordinates respectively}
\item{RunDates}{A \code{POSIXlt} time object corresponding to the initialization times selected. Despite redundancy, there is an initialization time associated to each forecast time for convenience}
\item{ForecastDates}{A list with two \code{POSIXlt} time elements of length \emph{i}, corresponding to the rows of each matrix in \code{MemberData}, defining the time boundaries of the time axis coordinates in the interval \emph{[start, end)}:}
      \itemize{ 
      \item \code{Start} Starting times of the verification period of the variable
      \item \code{End} time of the verification period of the variable
      }
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

\details{
For details on the different hindcast datasets available go to \url{https://meteo.unican.es/trac/wiki/EcomsUdg/DataServer/Datasets}.

The different nature of the various databases, models and variables, and the idiosyncratic naming and storage conventions often applied by the different modelling centres, makes necessary a previous homogeneization across datasets in order to implement a truly user-friendly toolbox for data access. This package achieves this aim by defining a common \code{\link{vocabulary}} to all climate datasets. The particular variables of each dataset (as returned by \code{\link{datasetInventory}}) are translated -and transformed if necessary- to the standard variables by means of a dictionary, provided by the argument \code{dictionary}. This feature is described in more detail in the following link: \url{https://meteo.unican.es/trac/wiki/EcomsUdg/RPackage/Homogeneization}. 

Regarding the selection of the spatial domain, it is possible to select the whole spatial domain of the datasets (currently global for all hindcasts available). In this case \code{lonLim = NULL} and \code{latLim = NULL}. More often, rectangular domains are easily defined by the minimum and maximum corner coordinates (for instance \code{lonLim=c(-10,10)} and \code{latLim=c(35,45)} indicates a rectangular window centered in the Iberian Peninsula), and single grid-cell values (for instance \code{lonLim=-3.21} and \code{latLim=41.087} for retrieving the data in the closest grid point to the point coordinate -3.21E, 41.087N. In the last two cases, the function operates by finding the nearest (euclidean distance) grid-points to the coordinates introduced.

The function has been implemented to access seasonal slices, as determined by the \code{season} argument. Seasons can be defined in several ways: A single month (e.g. \code{season=1} for January, a standard season (e.g. \code{season=c(1,2,3)} for JFM, or \code{season=c(12,1,2)} for DJF), or any period of consecutive months (e.g. \code{season=c(1:6)}, for the first half of the year). Seasons are returned for a given year period (defined by the years argument, e.g. \code{years=1981:2000}) with a homogeneous forecast lead time (as given by the leadMonth argument; e.g. \code{leadMonth=1} for one-month lead time) with respect to the first month of the selected season. For example, \code{season=c(1,2,3)} for \code{years=1995:2000} and \code{leadMonth=1} will return the following series: JFM 1995 from the December 1994 runtime forecast, ..., JFM 2000 from the December 1999 runtime forecast. Note that it is also possible to work with year-crossing seasons, such as DJF. In this case, \code{season=c(12,1,2)} for \code{years=1995:2000} and \code{leadMonth=1} will return the following series: DJF 1994/1995 (from the November 1994 runtime forecast), ..., DJF 1999/2000 (from the November 1999 runtime forecast). 
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{vocabulary}}, \code{\link{datasetInventory}}
}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utils }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
