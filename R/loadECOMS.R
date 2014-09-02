loadECOMS <- function(dataset, var, dictionary = TRUE, 
                     members = NULL, lonLim = NULL, latLim = NULL, season = NULL,
                     years = NULL, leadMonth = 1, time = "none") {
      dataset <- match.arg(dataset, c("System4_seasonal_15", "System4_seasonal_51", "System4_annual_15", "CFSv2_seasonal_16", "WFDEI", "NCEP"))
      time <- match.arg(time, choices = c("none", "00", "06", "12", "18", "DD"))
      derInterface <- deriveInterface(dataset, var, dictionary, time)
      var <- derInterface$leadVar
      aux.level <- findVerticalLevel(derInterface$leadVar)
      var <- aux.level$var
      level <- aux.level$level
      url <- dataURL(dataset)
      # Dictionary/shortName search
      if (isTRUE(dictionary)) {
            dicPath <- file.path(find.package("ecomsUDG.Raccess"), "dictionaries", paste(dataset,".dic", sep = ""))
            # for devel only # dicPath <- file.path("./inst/dictionaries", paste(dataset,".dic", sep = ""))
            dic <- dictionaryLookup.ECOMS(dicPath, derInterface, time)
            shortName <- dic$short_name
            if (dataset == "System4_seasonal_15" & (shortName == "u" | shortName == "v" | shortName == "z")) {
                  shortName <- paste(dic$short_name, level, "mb", sep = "")
            }
      } else {
            dic <- NULL
            shortName <- var
      }
      # Static variable requests
      if (dic$time_step == "static") {
            message("NOTE: The requested variable is static. All time-related arguments will be ignored")
            season <- 1
            years <- 2000
            time <- "none"
      } else {
            if (is.null(season)) {
                  stop("Argument 'season' must be provided")
            }
            if (min(season) < 1 | max(season) > 12) {
                  stop("Invalid season definition")
            }
      }
      # Exception in NCEP due to different grids within the same dataset
      if (dataset == "NCEP") {
            if (length(lonLim) == 1 || length(latLim) == 1) {
                  stop("Single-point selections are invalid for the NCEP dataset\nConsider using a small rectangular domain")
            }
      }
      # Note when loading gridded datasets
      if ((dataset == "WFDEI" | dataset == "NCEP") & !is.null(members)) {
            message("NOTE: The dataset is not a forecast. Argument 'members' will be ignored")      
      }
      # Discover dataset and open grid
      gds <- J("ucar.nc2.dt.grid.GridDataset")$open(url$URL)
      grid <- gds$findGridByShortName(shortName)
      if (is.null(grid)) {
            stop("Variable requested not found\nCheck available variables at http://meteo.unican.es/ecoms-udg/DataServer/ListOfVariables")
      }
      # Grid datasets
      if (dataset == "WFDEI" | dataset == "NCEP") {
            latLon <- getLatLonDomain(grid, lonLim, latLim)
            out <- loadGridDataset.ECOMS(var, grid, dic, level, season, years, time, latLon)
      # Forecasts
      } else {
            if (dic$time_step == "static") {
                  members <- 1
            }
            if (!is.null(members)) {
                  members <- sort(members)
            }
            if (is.null(leadMonth) & dic$time_step != "static") {
                  stop("A lead month for forecast initialization must be specified")
            }
            if (leadMonth < 0 & dic$time_step != "static") {
                  stop("Invalid lead time definition")
            }
            if (leadMonth == 0 & dic$time_step != "static") {
                  message("NOTE: 'leadMonth = 0' selected")
            }
            # Season range constraints
            if (dic$time_step != "static") {
                  if (grepl("CFSv2", dataset) & (length(season) + leadMonth) > 9) {
                        stop("Max. forecast extent is 9 months. Reduce season length or lead month value accordingly")            
                  }
                  if (grepl("System4_seasonal", dataset) & (length(season) + leadMonth) > 7) {
                        stop("Max. forecast extent is 7 months. Reduce season length or lead month value accordingly")            
                  }
                  if (grepl("System4_annual", dataset) & (length(season) + leadMonth) > 13) {
                        stop("Max. forecast extent is 13 months. Reduce season length or lead month value accordingly")            
                  }
            }
            leadMonth <- as.integer(leadMonth)
            latLon <- getLatLonDomainForecast(grid, lonLim, latLim)      
            runTimePars <- getRunTimeDomain(dataset, grid, members, season, years, leadMonth)
            if (grepl("^System4", dataset)) {
                  out <- loadSeasonalForecast.S4(dataset, gds, var, grid, dic, members, latLon, runTimePars, time, level, derInterface)
            }
            if (grepl("CFSv2", dataset)) {
                  if (is.null(members)) {
                        members <- 1:16
                  }
                  out <- loadSeasonalForecast.CFS(var, grid, dic, latLon, runTimePars, time, level)
            }
            if (derInterface$deriveInterface != "none") {
                  out$Variable$varName <- derInterface$origVar
            }
      }
      gds$close()
      message("[", Sys.time(), "]", " Done")
      attr(out$xyCoords, "projection") <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
      # Dimension ordering
      x <- attr(out$Data, "dimensions")
      if (length(x) > 1) {
            tab <- c("member", "time", "level", "lat", "lon")
            b <- na.exclude(match(tab, x))
            dimNames <- attr(out$Data, "dimensions")[b]
            out$Data <- aperm(out$Data, perm = b)    
            attr(out$Data, "dimensions")  <- dimNames
      }
      # Source Dataset and other metadata 
      attr(out, "dataset") <- dataset
      attr(out, "source") <- "ECOMS User Data Gateway" 
      attr(out, "URL") <- "<http://meteo.unican.es/ecoms-udg>"
      return(out)
}      
# End


