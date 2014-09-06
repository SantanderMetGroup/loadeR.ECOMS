\name{loadECOMS}
\alias{loadECOMS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remote access to climate databases stored at the ECOMS-UDG
}
\description{
A simple interface for accesing and retrieving dimensional slices of the various climate databases stored at the ECOMS User Data Gateway (Forecasts, gridded observations and reanalysis).
}

\usage{
loadECOMS(dataset, var, dictionary = TRUE, members = NULL, lonLim = NULL, 
      latLim = NULL, season = NULL, years = NULL, leadMonth = 1, time = "none") 
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{A character string indicating the database to be accessed. Currently accepted values are \code{"System4_seasonal_15"}, \code{"System4_seasonal_51"}, \code{"System4_annual_15"} and \code{"CFSv2_seasonal_16"} for hindcasts (where the notation indicates the GCM name, the temporal range and the default number of ensemble members returned respectively, separated by the lower bars), \code{"WFDEI"} for the WATCH Forcing Dataset based on ERA-Interim (gridded observations) and \code{"NCEP"} for the NCEP/NCAR reanalysis. See details on available datasets.}
  
  \item{var}{
Variable code (character string). This is the name of the variable according to the R standard naming (see the next argument). For variables with vertical levels, the vertical level is specified next to the variable name followed by the \dQuote{@} symbol (e.g. \code{var = "z@700"} for geopotential heigth at 700 mb isobaric surface pressure level). It is also possible to enter the variable name as originally coded in the dataset to skip data homogenization, although this is not recommended (see the next argument). See details on available variables. 
}

  \item{dictionary}{
A logical flag indicating if a dictionary is used for variable homogenization. Default (strongly recommended) is set to \code{TRUE}, meaning that the function will internally perform the necessary homogenization steps to return the standard variables defined in the vocabulary (e.g. variable transformation, deaccumulation...). See details on data homogenization. 
}

  \item{members}{
A vector of integers indicating the members to be loaded. Default to \code{NULL}, which loads the default members (see details for the particularities concerning the CFSv2 dataset). For instance, \code{members=1:5} will retrieve the first five members of the hindcast. Discontinuous member selection (e.g. \code{members=c(1,5,7)}) is allowed. If the requested dataset is not a forecast or the requested variable is static (e.g. orography) it will be ignored.
}

  \item{lonLim}{
Vector of length = 2, with minimum and maximum longitude coordinates, in decimal degrees, of the bounding box selected. For single-point queries, a numeric value with the longitude coordinate. If \code{NULL} (default), the whole longitudinal range is selected (Note that this may lead to a large output object size). See details on the definition of spatial domains. 
}
  \item{latLim}{
Same as \code{lonLim}, but for the selection of the latitudinal range.
}

  \item{season}{
An integer vector specifying the desired season (in months, January = 1 ..., December = 12). Options include one to several (contiguous) months. For full year selections (not possible for all datasets, e.g. seasonal forecasts), the argument value must be set to \code{season = 1:12}. If the requested variable is static (e.g. orography) it will be ignored. See details on the definition of temporal slices. 
}

  \item{years}{
Optional vector of years to select. Default to all available years. If the requested variable is static (e.g. orography) it will be ignored. See details on the definition of temporal slices. 
}

  \item{leadMonth}{
Integer value indicating the lead forecast time, relative to the first month of \code{season}. Note that \code{leadMonth=1} for \code{season=1} (January) corresponds to the December initialization. Default to 1 (i.e., 1 lead month forecast). If the dataset is not a forecast or the requested variable is static (e.g. orography) it will be ignored. A message will be printed on screen in the first case if its value is different from \code{NULL}. See details on initialization times.
}

  \item{time}{A character vector indicating the temporal filtering/aggregation of the output data. Default to \code{"none"}, which returns the original time series as stored in the dataset. For sub-daily variables, instantantaneous data at selected verification times can be filtered using one of the character strings \code{"00"}, \code{"06"}, \code{"12"} and \code{"18"}. If daily aggregated data are required use \code{"DD"}. If the requested variable is static (e.g. orography) it will be ignored. See details for time aggregation options.
}
}


\value{
A list with the following elements providing the necessary information for data representation and analysis:
\item{Variable}{A list with three elements:}
\itemize{ 
      \item \code{varName} A character string indicating which is the variable returned. Same as value provided for argument \code{var}
      \item \code{isStandard} Logical value indicating whether the variable returned is standard or not (i.e., wether the dictionary has been used -the default- or not.)
      \item \code{level} A numeric value indicating the vertical level of the variable (\code{NULL} for 2D variables)}
\item{Data}{A N-dimensional array. The number of dimensions (N) depends on the type of request given that dimensions of length one are dropped. Thus, N can take values from 4 (several members for a rectangular domain with different values for longitude, latitude, ensemble and time dimensions) to 1 (atomic vector), for single-point and single-member selections, for which only the time dimension is required. The dimensions are labelled by the \dQuote{dimnames} attribute, and are always arranged in canonical order (i.e.: [member, time, level, lat, lon]).}
\item{xyCoords}{A list with \code{x} and \code{y} components, as required by many standard mapping functions in R (see e.g. \code{\link[graphics]{image}}. In addition, the attribute \code{projection} provides geo-referencing information for more advanced spatial operations/conversions, in the form of a character string following the \href{http://trac.osgeo.org/proj/}{PROJ.4 specifications}.}
\item{Dates}{A list with two \code{\link[base]{POSIXct}} time elements of the same length as the \sQuote{time} dimension in \code{Data}, defining the time boundaries of the time axis coordinates in the interval \emph{[start, end)}, or if the loaded field is static, a character string indicating it. See details.}
\item{InitializationDates}{A \code{\link[base]{POSIXct}} time object corresponding to the initialization times selected. Only for forecast datasets. NA for static variables (e.g. orography). See details.}
\item{Members}{A character vector with the names of the ensemble members returned, in the same order as arranged in the \code{Data} array. Only for forecast datasets. NA for static variables (e.g. orography). See details.}

Additionally, there are three global attributes with metadata, (\code{"dataset"}, \code{"source"} and \code{"URL"}), providing information on the dataset loaded, their source (the ECOMS UDG) and the URL for reference.
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

\details{


\strong{Available datasets}


For details on the different hindcast datasets available go to \url{http://meteo.unican.es/ecoms-udg/DataServer/Datasets}. Currently, there are four different seasonal to annual hindcasts and one observational gridded dataset available at ECOMS-UDG. All of them are available through the common interface \code{loadECOMS}, and therefore the argument values may vary slightly. For instance, arguments \code{members} and \code{leadMonth} do not apply in the case of the observational gridded dataset (WFDEI), and are therefore ignored if their value is not NULL. Similarly, the output structure may vary consequently, and forecast data types include the initialization dates and the names of the chosen members, while this information is not included for other types of gridded data.


\strong{Available variables}


For the possible values that the argument \code{var} can take for each dataset at ECOMS-UDG, check the first column of the \href{http://meteo.unican.es/ecoms-udg/DataServer/ListOfVariables}{variables table}, that is continuously updated as new variables are made available. The table also contains further details regarding the temporal aggregation/resolution of each variable and the available vertical levels.


\strong{Variable homogeneization}


The different nature of the various databases, models and variables, and the idiosyncratic naming and storage conventions often applied by the different modelling centres, makes necessary a previous homogeneization across datasets in order to implement a truly user-friendly toolbox for data access. This package achieves this aim by defining a common \code{\link{vocabulary}} to all climate datasets. The particular variables of each dataset are translated -and transformed if necessary- to the standard variables by means of a dictionary, provided by the argument \code{dictionary}. In essence, the \file{dictionary} is a csv file particular for each individual dataset, containing the necessary information for performing the unit conversions to match the standard variable definitions contained in the \code{\link{vocabulary}}. This feature is described in more detail in \href{http://meteo.unican.es/trac/wiki/EcomsUdg/RPackage/Homogeneization}{this section} of the ECOMS UDG wiki. 

\strong{Ensemble member definition}

In the case of the CFSv2 reforecast there are four initializations (4 cycles) from every 5th day running for 9 months (see \href{http://meteo.unican.es/ecoms-udg/DataServer/Datasets/CFSv2}{CFSv2 members} for more detailed information of members' construction). Thus, the lagged-time configuration of members results in a different number of possible members depending on the initialization chosen (more precisely, 24 members excepting the November initializations, which have 28). For better comparability with its ECMWF's counterpart (the System4 seasonal forecast of 15 members), \code{loadECOMS} defines by default an ensemble of 16 members for each lead month and forecast season in the case of CFS, although it is possible to request up to 24/28. 


\strong{Definition of spatial domains}


Regarding the selection of the spatial domain, it is possible to select the whole spatial domain of the datasets (currently global for all hindcasts available). In this case \code{lonLim=NULL} and \code{latLim=NULL}. More often, rectangular domains are defined by the minimum and maximum coordinates in longitude and latitude (for instance \code{lonLim=c(-10,10)} and \code{latLim=c(35,45)} indicates a rectangular window centered in the Iberian Peninsula), and single grid-cell values (for instance \code{lonLim=-3.21} and \code{latLim=41.087} for retrieving the data in the closest grid point to the point coordinate -3.21E, 41.087N. In the last two cases, the function operates by finding the nearest (euclidean distance) grid-points to the coordinates introduced. (NOTE: Currently the single-point option is disabled for the NCEP dataset, that only accepts rectangular domain selections.)

The returned value by \code{xyCoords} varies accordingly, and it fits the common data structure of many R plotting functions (see \link[grDevices]{xy.coords} for obtaining a more detailed info).

The spatial definition of the data is associated to a specific coordinate reference system via the \sQuote{proj4string} slot of \code{xyCoords}, thus enabling the direct application of geospatial operations such as projection transformations, spatial overlay methods etc with the appropriate R methods.




\strong{Definition of temporal slices}


The function has been implemented to access seasonal slices, as determined by the \code{season} argument. Seasons can be defined in several ways: A single month (e.g. \code{season=1} for January, a standard season (e.g. \code{season=c(1,2,3)} for JFM, or \code{season=c(12,1,2)} for DJF), or any period of consecutive months (e.g. \code{season=c(1:6)}, for the first half of the year). Seasons are returned for a given year period (defined by the years argument, e.g. \code{years=1981:2000}) with a homogeneous forecast lead time (as given by the leadMonth argument; e.g. \code{leadMonth=1} for one-month lead time) with respect to the first month of the selected season. For example, \code{season=c(1,2,3)} for \code{years=1995:2000} and \code{leadMonth=1} will return the following series: JFM 1995 from the December 1994 runtime forecast, ..., JFM 2000 from the December 1999 runtime forecast. Note that it is also possible to work with year-crossing seasons, such as DJF. In this case, \code{season=c(12,1,2)} for \code{years=1995:2000} and \code{leadMonth=1} will return the following series: DJF 1994/1995 (from the November 1994 runtime forecast), ..., DJF 1999/2000 (from the November 1999 runtime forecast). 


In case the whole year is needed (instead of a particular season), the argument \code{season} can be omitted. In this case, its default value is \code{NULL}, equivalent to setting \code{season = 1:12}. The same applies to the argument \code{years}, and all available years are returned when ommitted.

\strong{Initialization times}

The characteristics of the \code{InitializationDates} output vary depending on the dataset. In the case of models that have simultaneous initializations for different members (e.g. System4), the output is just a vector of initialization times (one per year selected). Unlike the simultaneous initializations scheme, the lagged runtime configuration of members used by some other models (e.g. CFSv2) results in different initialization times for the same forecast times of different members. In this case, the \code{InitializationDates} are included in a list whose elements are named as the corresponding member.

By default, 16 members are returned for the CFSv2 hindcast for better comparability with the 15 members returned by the ECMWF's System4 seasonal hindcast. However, note that up to 24 members can be obtained from the CFSv2 hindcast (i.e., \code{members=1:24}), and 28 in the case of the November initializations. See the \href{http://meteo.unican.es/ecoms-udg/DataServer/Datasets/CFSv2}{technical documentation} at the ECOMS-UDG wiki for more details on the lagged runtimes configuration of the CFSv2 hindcast.  


\strong{Temporal filtering / aggregation}

The argument \code{time} controls the temporal filtering/aggregation options that may apply for a variable. In case of daily mean data, this can be obtained in two different ways:
\enumerate{
\item For variables that are already stored as daily means in the dataset, both \code{"DD"} and \code{"none"} return the required daily output
\item In case of 6-hourly data, if \code{"DD"} is chosen, the function will compute the daily mean as the averaged 6-hourly outputs for each day, printing an information message on screen. If the variable is precipitation or a radiation (\code{var} = \code{"tp"}, \code{"rsds"} or \code{"rlds"}), the result of the aggregation will be the sum rather than the mean (i.e., it will return the daily accumulated value). In the same way, if the variable is a daily maximum/minimum (i.e., \code{var = "tasmax"}/\code{var = "tasmin"}), the corresponding function will be applied to the 6-h outputs on a daily basis.
\item In case of 12-hourly data, daily mean data (\code{"DD"}) won't be computed, and the function will raise an error. In this particular case, the only accepted \code{time} argument values will be \code{"none"}, \code{"00"} and \code{"12"}.
}

}



\references{

\url{http://meteo.unican.es/ecoms-udg}

%Saha, S. et al., 2013. The NCEP Climate Forecast System Version 2. J Clim. DOI: 10.1175/JCLI-D-12-00823.1.

}


\author{
\href{http://www.meteo.unican.es}{Santander Meteorology Group}
}


\examples{\dontrun{
# Go to <http://www.meteo.unican.es/ecoms-udg/RPackage/Examples>
}}

