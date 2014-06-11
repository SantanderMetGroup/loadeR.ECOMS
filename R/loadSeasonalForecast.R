#' Remote access to hindcast model data stored at the ECOMS-UDG.

#' A simple interface for accesing and retrieving dimensional slices of the
#'  various seasonal forecast databases stored at the ECOMS User Data Gateway.

loadSeasonalForecast <- function(dataset, var, dictionary = TRUE, 
                                members = NULL, lonLim = NULL, latLim = NULL, season = NULL,
                                years = NULL, leadMonth = 1, time = NULL) {
      time <- match.arg(time, choices = c("none", "00", "06", "12", "18", "DD"))
      url <- dataURL(dataset)
      dic <- NULL
      if (isTRUE(dictionary)) {
            #dicPath <- file.path(find.package("ecomsUDG.Raccess"), "dictionaries", paste(dataset,".dic", sep = ""))
            # OJO PROVISIONAL
            dicPath <- file.path("./inst/dictionaries", paste(dataset,".dic", sep = ""))
            dic <- dictionaryLookup(dicPath, var, time)
            shortName <- dic$short_name      
      } else {
            shortName <- var
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
      grid <- gds$findGridByShortName(shortName)
      if (is.null(grid)) {
            stop("Variable requested not found.\nCheck variables using 'datasetInventory'")
      }
      latLon <- getLatLonDomainForecast(grid, lonLim, latLim)
      runTimePars <- getRunTimeDomain(dataset, grid, members, season, years, leadMonth)
      # S4
      if (grepl("^System4", dataset)) {
            out <- loadSeasonalForecast.S4(dataset, var, grid, dic, members, latLon, runTimePars, time)
      }
      # CFSv2
      if (grepl("CFSv2", dataset)) {
            out <- loadSeasonalForecast.CFS(var, grid, dic, latLon, runTimePars, time)
            names(out$InitializationDates) <- paste("Member_", members, sep = "")
      }
      gds$close()
      out <- c(out, "Members" = list(paste("Member_", members, sep = "")))
      message("[",Sys.time(),"]", " Done")
      return(out)
}      
# End