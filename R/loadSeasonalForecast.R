loadSeasonalForecast = function(dataset = c("System4_seasonal_15", "System4_seasonal_51", "System4_annual_15", "CFSv2_seasonal_16"), var, dictionary = TRUE, members = NULL, lonLim = NULL, latLim = NULL, season = NULL, years = NULL, leadMonth = 1) {
      dataset <- match.arg(dataset)
      if (dataset == "System4_seasonal_15") {
            data.url <- "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Seasonal_15Members.ncml"
      }
      if (dataset == "System4_seasonal_51") {
            data.url <- "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Seasonal_51Members.ncml"
      }
      if (dataset == "System4_annual_15") {
            data.url <- "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Annual_15Members.ncml"
      }
      if (dataset == "CFSv2_seasonal_16") {
            data.url <- "http://www.meteo.unican.es/tds5/dodsC/cfs/agg/cfsAgg_fmrc.ncd"
      }
      if (isTRUE(dictionary)) {
            dicPath <- paste(find.package("ecomsUDG.Raccess"), "dictionaries", sep = "/")
            dic <- dictionaryLookup(list.files(dicPath, pattern = dataset, full.names = TRUE), var)
            var <- dic$short_name
      } else {
            dic <- NULL
      }
      season <- as.integer(season)
      if (min(season) < 1 | max(season) > 12) {
            stop("Invalid season definition")
      }
      leadMonth <- as.integer(leadMonth)
      if (leadMonth < 1) {
            stop("Invalid lead time definition")
      }
      gds <- J("ucar.nc2.dt.grid.GridDataset")$open(data.url)
      grid <- gds$findGridByShortName(var)
      if (is.null(grid)) {
            stop("Variable requested not found.\nCheck variables using 'datasetInventory'")
      }
      gcs <- grid$getCoordinateSystem()
      latLon <- getLatLonDomain(gcs, lonLim, latLim)
      timePars <- getTimeDomainForecast(gcs, season, years, leadMonth)
      if (grepl("^System4", dataset)) {
            out <- loadSeasonalForecast.S4(dataset, grid, gcs, dic, members, latLon, timePars)
      }
      if (grepl("^CFS", dataset)) {
            out <- loadSeasonalForecast.CFS(grid, gcs, dic, members, latLon, timePars)
      }
      gds$close()
      message("[",Sys.time(),"]", " Done")
      return(out)
}
# End