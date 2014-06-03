loadSeasonalForecast.CFS = function(var, grid, dic, latLon, runTimePars, verifTime) {
      gcs <- grid$getCoordinateSystem()
      foreTimePars <- getForecastTimeDomain.CFS(grid, dic, runTimePars, verifTime)
      mdArray <- makeSubset.CFS(grid, latLon, runTimePars, foreTimePars)
      mdArray <- dictionaryTransformForecast(dic, foreTimePars, mdArray)
      if (isTRUE(latLon$revLat)) {
            mdArray <- revArrayLatDim(mdArray, grid)
      }
      isStandard <- TRUE
      if (is.null(dic)) {
            isStandard <- FALSE
      }
      return(list("Variable" = list("varName" = var, "isStandard" = isStandard),
                  "Data" = mdArray,
                  "xyCoords" = c(latLon$xyCoords, "CRS_string" = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"), 
                  "ForecastDates" = foreTimePars$forecastDates,
                  "InitializationDates" = runTimePars$runDates))
}
# End