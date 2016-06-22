loadSeasonalForecast.CFS = function(var, gds, grid, dic, latLon, runTimePars, time, level, aggr.d, aggr.m, derInterface, datasetName) {
      foreTimePars <- getForecastTimeDomain.CFS(grid, dic, runTimePars, time, aggr.d, aggr.m)
      cube <- switch(derInterface$deriveInterface,
            none = makeSubset.CFS(grid, latLon, runTimePars, foreTimePars),
            deriveSurfaceWindSpeed = deriveSurfaceWindSpeed.CFS(gds, grid, latLon, runTimePars, foreTimePars),
            deriveSurfaceRelativeHumidity = deriveSurfaceRelativeHumidity.CFS(gds, grid, latLon, runTimePars, foreTimePars, datasetName))
      foreTimePars <- NULL      
      if (!is.null(derInterface$deriveInterface)) {
            var <- derInterface$origVar
      }
      if (!is.null(dic)) {
            isStandard <- TRUE
            cube$mdArray <- loadeR::dictionaryTransformForecast(dic, cube$mdArray)
            var <- derInterface$origVar
      } else {
            isStandard <- FALSE
      }
      if (isTRUE(latLon$revLat)) {
            cube$mdArray <- revArrayLatDim(cube$mdArray, grid)
      }
      # formatting initialization dates
      for (x in 1:length(runTimePars$runDates)) {
            runTimePars$runDates[[x]] <- format(as.POSIXct(runTimePars$runDates[[x]], tz = "GMT"), format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
      }
      # variable info
      Variable <- list("varName" = var, "level" = level)
      attr(Variable, "use_dictionary") <- isStandard
      attr(Variable, "description") <- grid$getDescription()
      if (isStandard) {
            vocabulary <- UDG.vocabulary()
            attr(Variable, "units") <- as.character(vocabulary[grep(paste0("^", var, "$"), vocabulary$identifier), 3])
            attr(Variable, "longname") <- as.character(vocabulary[grep(paste0("^", var, "$"), vocabulary$identifier), 2])
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
                  "Members" = names(runTimePars$runTimeRanges)))
}
# End

