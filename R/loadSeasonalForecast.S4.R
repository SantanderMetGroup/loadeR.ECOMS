#' Load user-defined subsets from System4 datasets
#' 
#' Load user-defined subsets from System4 datasets considering the ensemble dimensions and
#' other particular characteristics of System4 datasets. This is a subroutine of
#'  \code{\link{loadSeasonalForecast}}.
#'  
#'  @param dataset character string indicating the dataset requested.
#'  @param var character string indicating the variable to download.
#'  @param grid A java GeoGrid.
#'  @param dic A single-row data.frame, as returned by \code{dictionaryLookup}.
#'  @param members Numeric vector indicating the members to be retrieved.
#'  @param latLon A list of geolocation parameters as returned by \code{getLatLonDomainForecast}
#'  @param runTimePars A list of parameters defining de initializations to be taken and other.
#'  auxiliary parameters, as returned by \code{getRunTimeDomain}.
#'  @param time Verification time. See \code{\link{loadSeasonalForecast}}
#'  @return A list of components, as returned by \code{\link{loadSeasonalForecast}}.
#'  @author J. Bedia \email{joaquin.bedia@@gmail.com}

loadSeasonalForecast.S4 <- function(dataset, var, grid, dic, members, latLon, runTimePars, time) {    
      memberRangeList <- getMemberDomain.S4(grid, dataset, members)
      foreTimePars <- getForecastTimeDomain.S4(grid, dataset, dic, runTimePars, time)
      mdArray <- makeSubset.S4(grid, latLon, runTimePars, memberRangeList, foreTimePars)
      if (!is.null(dic)) {
            isStandard <- TRUE
            mdArray <- dictionaryTransformForecast(dic, foreTimePars, mdArray)
      } else {
            isStandard <- FALSE
      }
      if (isTRUE(latLon$revLat)) {
            mdArray <- revArrayLatDim(mdArray, grid)
      }
      return(list("Variable" = list("varName" = var, "isStandard" = isStandard),
                  "Data" = mdArray,
                  "xyCoords" = c(latLon$xyCoords, "CRS_string" = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"), 
                  "Dates" = foreTimePars$forecastDates,
                  "InitializationDates" = runTimePars$runDates,
                  "Members" = names(memberRangeList)))
}
# End