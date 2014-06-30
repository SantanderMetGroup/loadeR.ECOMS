loadSeasonalForecast.CFS = function(var, grid, dic, latLon, runTimePars, time, level) {
      gcs <- grid$getCoordinateSystem()
      foreTimePars <- getForecastTimeDomain.CFS(grid, dic, runTimePars, time)
      mdArray <- makeSubset.CFS(grid, latLon, runTimePars, foreTimePars)
      if (!is.null(dic)) {
            isStandard <- TRUE
            mdArray <- dictionaryTransformForecast(dic, foreTimePars, mdArray)
      } else {
            isStandard <- FALSE
      }
      if (isTRUE(latLon$revLat)) {
            mdArray <- revArrayLatDim(mdArray, grid)
      }
      # formatting initialization dates
      for (x in 1:length(runTimePars$runDates)){
            runTimePars$runDates[[x]] <- format(as.POSIXct(runTimePars$runDates[[x]], tz = "GMT"), format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
      }
      return(list("Variable" = list("varName" = var, "isStandard" = isStandard, "level" = level),
                  "Data" = mdArray,
                  "xyCoords" = c(latLon$xyCoords, "CRS_string" = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"), 
                  "Dates" = foreTimePars$forecastDates,
                  "InitializationDates" = runTimePars$runDates,
                  "Members" = names(runTimePars$runTimeRanges)))
}
# End

