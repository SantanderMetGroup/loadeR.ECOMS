#' Load user-defined subsets from System4 datasets
#' 
#' Load user-defined subsets from System4 datasets considering the ensemble dimensions and
#' other particular characteristics of System4 datasets. This is a subroutine of
#'  \code{\link{loadSeasonalForecast}}.
#'  
#'  @param dataset character string indicating the dataset requested.
#'  @param var character string indicating the variable to download.
#'  @param grid A java GeoGrid.
#'  @param dic A single-row data.frame, as returned by \code{dictionaryLookup.ECOMS}.
#'  @param members Numeric vector indicating the members to be retrieved.
#'  @param latLon A list of geolocation parameters as returned by \code{getLatLonDomainForecast}
#'  @param runTimePars A list of parameters defining de initializations to be taken and other.
#'  auxiliary parameters, as returned by \code{getRunTimeDomain}.
#'  @param time Verification time. See \code{\link{loadSeasonalForecast}}
#'  @param derInterface A list of components indicating the interface for derived variables,
#'   when relevant. See details.
#'  @return A list of components, as returned by \code{\link{loadSeasonalForecast}}.
#'  @details In the case of derived variables, these are computed on-the-fly applying the
#'   appropriate method. The idea is computing the derived variable loading the minimum 
#'   object sizes at a time in order to optimize the available memory. Thus, different
#'   variants of the \code{\link{makeSubset.S4}} method (referred to as \sQuote{interfaces} in the dictionary
#'   and henceforth within the code) have been implemented, and called alternatively depending on the 
#'   variable to be derived. They operate in a time-slice basis, removing from the memory the input variables
#'   at each time step within the for loop once the derived variable is calculated,
#'   freeing as much space as possible. The different functions for deriving variables are named with the
#'   \sQuote{derive} prefix.
#'  @references \url{http://meteo.unican.es/ecoms-udg/ListOfVariables}
#'  @author J. Bedia \email{joaquin.bedia@@gmail.com}

loadSeasonalForecast.S4 <- function(dataset, gds, var, grid, dic, members, latLon, runTimePars, time, level, aggr.d, aggr.m, derInterface) {    
      memberRangeList <- getMemberDomain.S4(grid, dataset, members)
      foreTimePars <- getForecastTimeDomain.S4(grid, dataset, dic, runTimePars, time, aggr.d, aggr.m)
      cube <- switch(derInterface$deriveInterface,
                        none = makeSubset.S4(grid, latLon, runTimePars, memberRangeList, foreTimePars),
                        deriveSurfacePressure = deriveSurfacePressure.S4(gds, grid, latLon, runTimePars, memberRangeList, foreTimePars),
                        deriveSurfaceRelativeHumidity = deriveSurfaceRelativeHumidity.S4(gds, grid, latLon, runTimePars, memberRangeList, foreTimePars),
                        deriveSurfaceSpecificHumidity = deriveSurfaceSpecificHumidity.S4(gds, grid, latLon, runTimePars, memberRangeList, foreTimePars),
                        deriveSurfaceWindSpeed = deriveSurfaceWindSpeed.S4(gds, grid, latLon, runTimePars, memberRangeList, foreTimePars))
      foreTimePars <- NULL
      if (!is.null(dic)) {
            isStandard <- TRUE
            cube$mdArray <- dictionaryTransformForecast(dic, cube$foreTimePars, cube$mdArray)
            var <- derInterface$origVar
      } else {
            isStandard <- FALSE
      }
      if (isTRUE(latLon$revLat)) {
            cube$mdArray <- revArrayLatDim(cube$mdArray, grid)
      }
      # formatting initialization dates
      runTimePars$runDates <- format(as.POSIXct(runTimePars$runDates, tz = "GMT"), format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
      # Static fields
      if (dic$time_step == "static") {
            runTimePars$runDates <- NA
            names(memberRangeList) <- NA
      }
      Variable <- list("varName" = var, "level" = level)
      attr(Variable, "is_standard") <- isStandard
      if (isStandard) {
            data(vocabulary, envir = environment())
            attr(Variable, "units") <- as.character(vocabulary[grep(paste0("^", var, "$"), vocabulary$identifier,), 3])
            attr(Variable, "longname") <- as.character(vocabulary[grep(paste0("^", var, "$"), vocabulary$identifier,), 2])
      } else {
            attr(Variable, "units") <- "undefined"
            attr(Variable, "longname") <- "undefined"
      }
      attr(Variable, "daily_agg_cellfun") <- cube$foreTimePars$aggr.d
      attr(Variable, "monthly_agg_cellfun") <- cube$foreTimePars$aggr.m
      attr(Variable, "verification_time") <- time
      return(list("Variable" = Variable,
                  "Data" = cube$mdArray,
                  "xyCoords" = latLon$xyCoords,
                  "Dates" = cube$foreTimePars$forecastDates,
                  "InitializationDates" = runTimePars$runDates,
                  "Members" = names(memberRangeList)))
}
# End