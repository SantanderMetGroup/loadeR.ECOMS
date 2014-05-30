#' Loads user-defined subsets of hindcast datasets stored at ECOMS-UDG
#' 

loadSeasonalForecast <- function(dataset, var, dictionary = TRUE, 
                                members = NULL, lonLim = NULL, latLim = NULL, season = NULL, years = NULL, leadMonth = 1,
                                verifTime = c(NULL, 0, 6, 12, 18), aggr = c("none", "mean", "min", "max", "sum")) {
      url <- dataURL(dataset)
      dic <- NULL
      if (isTRUE(dictionary)) {
            #dicPath <- file.path(find.package("ecomsUDG.Raccess"), "dictionaries", paste(dataset,".dic", sep = ""))
            # OJO PROVISIONAL
            dicPath <- "./inst/dictionaries/System4_seasonal_15.dic"
            dic <- dictionaryLookup(dicPath, var)
            var <- dic$short_name      
      }
      if (!is.null(season)) {
            season <- as.integer(season)
            if (min(season) < 1 | max(season) > 12) {
                  stop("Invalid season definition")
            }
      }
      leadMonth <- as.integer(leadMonth)
      if (leadMonth < 1) {
            stop("Invalid lead time definition")
      }
      gds <- J("ucar.nc2.dt.grid.GridDataset")$open(url$URL)
      grid <- gds$findGridByShortName(var)
      if (is.null(grid)) {
            stop("Variable requested not found.\nCheck variables using 'datasetInventory'")
      }
      latLon <- getLatLonDomainForecast(grid, lonLim, latLim)
      runTimePars <- getRunTimeDomain(grid, season, years, leadMonth)
      if (grepl("^System4", dataset)) {
            out <- loadSeasonalForecast.S4(dataset, var, grid, dic, members, latLon, runTimePars, verifTime)
      }
      gds$close()
      message("[",Sys.time(),"]", " Done")
      return(out)
}      
# End