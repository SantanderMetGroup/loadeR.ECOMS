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
#'  auxiliary parameters, as returned by \code{getRunTimeDomain.ECOMS}.
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
#'  @author J. Bedia 
#'  @importFrom loadeR C4R.vocabulary
#'  @importFrom loadeR dictionaryTransformForecast

loadSeasonalForecast.S4 <- function(dataset, gds, var, grid, dic, members, latLon, runTimePars, time, level, aggr.d, aggr.m, derInterface) {    
      memberRangeList <- getMemberDomain.S4(grid, dataset, members)
      foreTimePars <- getForecastTimeDomain.S4(grid, dataset, dic, runTimePars, time, aggr.d, aggr.m)
      verticalPars <- getVerticalLevelPars.ECOMS(grid, dataset, level)
      cube <- switch(derInterface$deriveInterface,
                        none = makeSubset.S4(grid, latLon, runTimePars, memberRangeList, foreTimePars, verticalPars),
                        deriveSurfacePressure = deriveSurfacePressure.S4(gds, grid, latLon, runTimePars, memberRangeList, foreTimePars),
                        deriveSurfaceRelativeHumidity = deriveSurfaceRelativeHumidity.S4(gds, grid, latLon, runTimePars, memberRangeList, foreTimePars),
                        deriveSurfaceSpecificHumidity = deriveSurfaceSpecificHumidity.S4(gds, grid, latLon, runTimePars, memberRangeList, foreTimePars),
                        deriveSurfaceWindSpeed = deriveSurfaceWindSpeed.S4(gds, grid, latLon, runTimePars, memberRangeList, foreTimePars))
      foreTimePars <- NULL
      if (!is.null(dic)) {
            isStandard <- TRUE
            cube$mdArray <- dictionaryTransformForecast(dic, cube$mdArray)
            var <- derInterface$origVar
      } else {
            isStandard <- FALSE
      }
      if (isTRUE(latLon$revLat)) {
            cube$mdArray <- revArrayLatDim(cube$mdArray)
      }
      # formatting initialization dates
      if (!is.null(runTimePars$runDates)) { ## Otherwise static variable
            runTimePars$runDates <- format(as.POSIXct(runTimePars$runDates, tz = "GMT"),
                                           format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
      }
      # Static fields
      if (dic$time_step == "static") {
            runTimePars$runDates <- NA
            names(memberRangeList) <- NA
            fakedate <- as.POSIXct("2000-01-01 00:00:00", tz = "GMT")
            cube$foreTimePars$forecastDates <- list("start" = fakedate, "end" = fakedate)
      }
      Variable <- list("varName" = var, "level" = level)
      attr(Variable, "use_dictionary") <- isStandard
      attr(Variable, "description") <- grid$getDescription()
      if (isStandard) {
            vocabulary <- C4R.vocabulary()
            attr(Variable, "units") <- as.character(vocabulary[grep(paste0("^", var, "$"), vocabulary$identifier), 3])
            attr(Variable, "longname") <- as.character(vocabulary[grep(paste0("^", var, "$"), vocabulary$identifier), 2])
      } else {
            attr(Variable, "units") <- "undefined"
            attr(Variable, "longname") <- "undefined"
      }
      attr(Variable, "daily_agg_cellfun") <- cube$foreTimePars$aggr.d
      attr(Variable, "monthly_agg_cellfun") <- cube$foreTimePars$aggr.m
      attr(Variable, "verification_time") <- time
      ## Monthly datasets
      if (grepl("m", dic$time_step)) {
            attr(Variable, "monthly_agg_cellfun") <- dic$aggr_fun
      }
      rtList <- rep(list(runTimePars$runDates), length(memberRangeList))
      names(rtList) <- names(memberRangeList)
      return(list("Variable" = Variable,
                  "Data" = cube$mdArray,
                  "xyCoords" = latLon$xyCoords,
                  "Dates" = cube$foreTimePars$forecastDates,
                  "InitializationDates" = rtList,
                  "Members" = names(memberRangeList)))
}
# End