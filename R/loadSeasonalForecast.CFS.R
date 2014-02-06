loadSeasonalForecast.CFS = function(grid, gcs, dic, members, latLon, timePars) {
      if (is.null(members)) {
            members <- 1:16      
      }
      d <- timePars$runDatesAll$mday
      m <- timePars$runDatesAll$mon + 1
      y <- timePars$runDatesAll$year + 1900
      jan.inits <- which(((d == 12 | d == 17 | d == 22 | d == 27) & m == 12) | ((d == 1 | d == 6) & m == 1))
      feb.inits <- which(((d == 11 | d == 16 | d == 21 | d == 26 | d == 31) & m == 1) | (d == 5 & m == 2))
      mar.inits <- which(((d == 10 | d == 15 | d == 20 | d == 25) & m == 2) | ((d == 2 | d == 7) & m == 3))
      apr.inits <- which(((d == 12 | d == 17 | d == 22 | d == 27) & m == 3) | ((d == 1 | d == 6) & m == 4))
      may.inits <- which(((d == 11 | d == 16 | d == 21 | d == 26) & m == 4) | ((d == 1 | d == 6) & m == 5))
      jun.inits <- which(((d == 11 | d == 16 | d == 21 | d == 26 | d == 31) & m == 5) | (d == 5 & m == 6))
      jul.inits <- which(((d == 10 | d == 15 | d == 20 | d == 25 | d == 30) & m == 6) | (d == 5 & m == 7))
      aug.inits <- which(((d == 10 | d == 15 | d == 20 | d == 25 | d == 30) & m == 7) | (d == 4 & m == 8))
      sep.inits <- which(((d == 9 | d == 14 | d == 19 | d == 24 | d == 29) & m == 8) | (d == 3 & m == 9))
      oct.inits <- which(((d == 8 | d == 13 | d == 18 | d == 23 | d == 28) & m == 9) | (d == 3 & m == 10))
      nov.inits <- which(((d == 8 | d == 13 | d == 18 | d == 23 | d == 28) & m == 10) | ((d == 2 | d == 7) & m == 11))
      dec.inits <- which(((d == 12 | d == 17 | d == 22 | d == 27) & m == 11) | ((d == 2 | d == 7) & m == 12))
      init.list <- lapply(ls(pattern = "\\.inits$")[pmatch(tolower(month.abb), ls(pattern = "\\.inits$"))], function(x) get(x))
      rm(list = c("d", "m", "y", ls(pattern = "\\.inits$")))
      runTimesAll <- init.list[[timePars$validMonth]]
      rm(init.list)
      # runtimes
      runDatesValidMonth <- timePars$runDatesAll[runTimesAll]
      runTimes <- runTimesAll[which((runDatesValidMonth$year + 1900) %in% timePars$years)]
      runDates <- timePars$runDatesAll[runTimes]
      if (timePars$validMonth == 11 & (length(members) > 28 | any(members > 28))) {
            stop("Maximum number of members in this initialization is 28")
      }
      if (timePars$validMonth != 11 & (length(members) > 24 | any(members > 24))) {
            stop("Maximum number of members in this initialization is 24")
      }
      runTimes <- unlist(lapply(unique(runDates$year + 1900), function(x) runTimes[which(runDates$year + 1900 == x)[members]]))
      runDates <- timePars$runDatesAll[runTimes]
      runDatesEnsList <- list()
      runTimesEnsList <- list()
      for (i in 1:length(members)) {
            ind <- seq.int(i, by = length(members), length.out = length(unique(runDates$year)))
            runTimesEnsList[[i]] <- runTimes[ind]
            runDatesEnsList[[i]] <- runDates[ind]
            rm(ind)
      }
      # Forecast Dates
      foreTimesList <- list()
      foreDatesList <- list()
      for (i in 1:length(runTimesEnsList)) {
            foreTimesEnsList <- list()
            foreDatesEnsList <- list()
            for (j in 1:length(runTimesEnsList[[i]])) {
                  auxDateString <- gsub("\\[|]", "", unlist(strsplit(gcs$getTimeAxisForRun(as.integer(runTimesEnsList[[i]][j] - 1))$getCalendarDates()$toString(), ", ")))
                  auxDates <- as.POSIXlt(strptime(auxDateString, format = "%Y-%m-%dT%H:%M:%SZ"))
                  rm(auxDateString)
                  foreTimesEnsList[[j]] <- which((auxDates$mon + 1) %in% timePars$season)
                  foreDatesEnsList[[j]] <- auxDates[foreTimesEnsList[[j]]]
                  rm(auxDates)
            }
            foreTimesList[[i]] <- foreTimesEnsList
            foreDatesList[[i]] <- foreDatesEnsList
      }
      foreDates <- foreDatesList[[1]][[1]]
      for (j in 2:length(foreDatesList[[1]])) {
            foreDates <- c(foreDates, foreDatesList[[1]][[j]])
      }
      rm(foreDatesList)
      if (!is.null(dic)) {
            ltb <- as.difftime(dic$lower_time_bound, format = "%H", units = "hours")
            utb <- as.difftime(dic$upper_time_bound, format = "%H", units = "hours")
            foreDates <- foreDates - ltb
            foreDates <- as.POSIXlt(foreDates)
            foreDatesEnd <- foreDates + utb
      } else {
            varTimeStep <- difftime(foreDates[2],foreDates[1])
            foreDatesEnd <- foreDates + varTimeStep
      }
      foreDatesEnd <- as.POSIXlt(foreDatesEnd)
      # Data Retrieval
      dimNames <- unlist(strsplit(grid$getVariable()$getDimensionsString(), split="\\s"))
      if (length(grep("height_above_ground", dimNames)) == 0) {
            hag <- NA
      } else {
            hag <- 1
      }
      Members <- list()
      for (i in 1:length(members)) {
            message(paste("[", Sys.time(), "]", " Retrieving data from member ", i, " out of ", length(members), "...", sep = ""))
            Data <- matrix(ncol = nrow(latLon$Grid), nrow = length(foreDatesContList[[i]]))
            length(Members) <- i
            auxC <- 1
            latStart <- latLon$LatIndex[1]
            latCount <- length(latLon$LatIndex)
            for (j in 1:length(runTimesEnsList[[i]])) {
                  rt <- runTimesEnsList[[i]][j] - 1
                  ftStart <- foreTimesList[[i]][[j]][1] - 1
                  ftCount <- length(foreTimesList[[i]][[j]])
                  auxC <- c(auxC, ftCount)
                  DataRowRange <- seq.int(sum(auxC) - auxC[j + 1], sum(auxC[-1]))
                  # Dateline crossing
                  if (is.null(latLon$DatelineCrossInd)) {
                        lonStart <- latLon$LonIndex[1]
                        lonCount <- length(latLon$LonIndex)
                        if (is.na(hag)) {
                              start <- as.integer(c(rt, ftStart, latStart, lonStart))
                              stride <- as.integer(c(1, ftCount, latCount, lonCount))
                        } else {
                              start <- as.integer(c(rt, ftStart, 0, latStart, lonStart))
                              stride <- as.integer(c(1, ftCount, 1, latCount, lonCount))
                        }
                        aux <- grid$getVariable()$read(start, stride)$getStorage()
                        aux.mat <- matrix(aux, ncol = nrow(latLon$Grid), byrow = TRUE)
                        rm(aux)
                        Data[DataRowRange, ] <- aux.mat
                        rm(aux.mat)
                  } else {
                        lonStartList <- list(1 : (latLon$DatelineCrossInd), (latLon$DatelineCrossInd + 1) : length(latLon$LonIndex))
                        DataColRangeList <- list(1:(latLon$DatelineCrossInd * latCount), (latLon$DatelineCrossInd * latCount + 1) : nrow(latLon$Grid))
                        for (k in 1:length(lonStartList)) {
                              lonStart <- latLon$LonIndex[lonStartList[[k]][1]]
                              lonCount <- length(lonStartList[[k]])
                              if (is.na(hag)) {
                                    start <- as.integer(c(rt, ftStart, latStart, lonStart))
                                    stride <- as.integer(c(1, ftCount, latCount, lonCount))
                              } else {
                                    start <- as.integer(c(rt, ftStart, 0, latStart, lonStart))
                                    stride <- as.integer(c(1, ftCount, 1, latCount, lonCount))
                              }
                              aux <- grid$getVariable()$read(start, stride)$getStorage()
                              aux.mat <- matrix(aux, ncol = length(DataColRangeList[[k]]), byrow = TRUE)
                              rm(aux)
                              Data[DataRowRange, DataColRangeList[[k]]] <- aux.mat
                              rm(aux.mat)
                        }
                        latLon$Grid <- rbind(latLon$Grid[which(latLon$Grid[ ,1] < 0), ], latLon$Grid[which(latLon$Grid[ ,1] >= 0), ])
                  }
            }
            if (!is.null(dic)) {
                  is.standard <- TRUE
                  offset <- dic$offset
                  scaleFac <- dic$scale
                  Data <- Data * scaleFac + offset
            } else {
                  is.standard <- FALSE
            }
            Members[[i]] <- Data[1:length(foreDates), ]
      }
      return(list("VarName" = var, "isStandard" = is.standard, "MemberData" = Members, "LonLatCoords" = SpatialPoints(latLon$Grid), "RunDates" = runDates, "ForecastDates" = list("Start" = foreDates, "End" = foreDatesEnd)))
}
# End