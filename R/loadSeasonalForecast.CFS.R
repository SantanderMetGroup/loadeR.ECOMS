loadSeasonalForecast.CFS = function(var, grid, dic, latLon, runTimePars, time) {
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
      return(list("Variable" = list("varName" = var, "isStandard" = isStandard),
                  "Data" = mdArray,
                  "xyCoords" = c(latLon$xyCoords, "CRS_string" = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"), 
                  "Dates" = foreTimePars$forecastDates,
                  "InitializationDates" = runTimePars$runDates))
}
# End