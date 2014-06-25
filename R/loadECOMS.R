loadECOMS <- function(dataset, var, dictionary = TRUE, 
                     members = NULL, lonLim = NULL, latLim = NULL, season = NULL,
                     years = NULL, leadMonth = NULL, time = "none") {
      dataset <- match.arg(dataset, c("System4_seasonal_15", "System4_seasonal_51", "System4_annual_15", "CFSv2_seasonal_16", "WFDEI", "NCEP"))
      time <- match.arg(time, choices = c("none", "00", "06", "12", "18", "DD"))
      level <- findVerticalLevel(var)
      url <- dataURL(dataset)
      dic <- NULL
      if (isTRUE(dictionary)) {
            dicPath <- file.path(find.package("ecomsUDG.Raccess"), "dictionaries", paste(dataset,".dic", sep = ""))
            # devel
            # dicPath <- file.path("./inst/dictionaries", paste(dataset,".dic", sep = ""))
            dic <- dictionaryLookup(dicPath, var, time)
            shortName <- dic$short_name      
      } else {
            shortName <- var
      }
      if (is.null(season)) {
            stop("Argument 'season' must be provided")
      }
      if (min(season) < 1 | max(season) > 12) {
            stop("Invalid season definition")
      }
      # Season range constraints
      if (grepl("CFSv2", dataset) & (length(season) + leadMonth) > 9) {
            stop("Max. forecast extent is 9 months. Reduce season length or lead month value accordingly")            
      }
      if (grepl("System4_seasonal", dataset) & (length(season) + leadMonth) > 7) {
            stop("Max. forecast extent is 7 months. Reduce season length or lead month value accordingly")            
      }
      if (grepl("System4_annual", dataset) & (length(season) + leadMonth) > 13) {
            stop("Max. forecast extent is 13 months. Reduce season length or lead month value accordingly")            
      }
      if (dataset == "WFDEI" & !is.null(leadMonth)) {
            message("NOTE: The dataset is not a forecast. Argument 'leadMonth' will be ignored")
      }
      if (dataset == "WFDEI" & !is.null(members)) {
            message("NOTE: Argument 'members' will be ignored")
      }
      gds <- J("ucar.nc2.dt.grid.GridDataset")$open(url$URL)
      grid <- gds$findGridByShortName(shortName)
      if (is.null(grid)) {
            stop("Variable requested not found")#.\nCheck variables using 'datasetInventory'")
      }
      if (dataset == "WFDEI") {
            latLon <- getLatLonDomain(grid, lonLim, latLim)
            out <- loadGridDataset(var, grid, dic, level, season, years, time, latLon)
      } else {
            if (!is.null(members)) {
                  members <- sort(members)
            }
            if (is.null(leadMonth)) {
                  stop("A lead month for forecast initialization must be specified")
            }
            if (leadMonth < 0) {
                  stop("Invalid lead time definition")
            }
            if (leadMonth == 0) {
                  message("NOTE: 'leadMonth = 0' selected")
            }
            leadMonth <- as.integer(leadMonth)
            latLon <- getLatLonDomainForecast(grid, lonLim, latLim)      
            runTimePars <- getRunTimeDomain(dataset, grid, members, season, years, leadMonth)
            if (grepl("^System4", dataset)) {
                  out <- loadSeasonalForecast.S4(dataset, var, grid, dic, members, latLon, runTimePars, time)
            }
            if (grepl("CFSv2", dataset)) {
                  if (is.null(members)) {
                        members <- 1:16
                  }
                  out <- loadSeasonalForecast.CFS(var, grid, dic, latLon, runTimePars, time)
            }
      }
      gds$close()
      message("[",Sys.time(),"]", " Done")
      return(out)
}      
# End