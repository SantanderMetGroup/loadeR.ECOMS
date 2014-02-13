loadSeasonalForecast.S4 <- function(dataset, var, grid, gcs, dic, members, latLon, timePars) {    
      if (grepl("15$", dataset) & !all(members %in% 1:15)) {
            stop("Accepted values for argument 'members' are integers in the range [1,15]")
      }
      if (grepl("51$", dataset) & !all(members %in% 1:51)) {
            stop("Accepted values for argument 'members' are integers in the range [1,51]")
      }
      if (is.null(members)) {
            members <- gcs$getEnsembleAxis()$getCoordValues()
      } else {
            members <- members - 1
      }
      runTimesAll <- which(timePars$runDatesAll$mon == (timePars$validMonth - 1))
      if (length(runTimesAll) == 0) {
            stop(paste("Incompatible 'leadMonth' and 'season' argument values.\nInitializations in", paste(month.name[unique(timePars$runDatesAll$mon + 1)], collapse=", ")))
      }
      runDatesValidMonth <- timePars$runDatesAll[runTimesAll]
      runTimes <- runTimesAll[which((runDatesValidMonth$year + 1900) %in% timePars$years)]
      rm(runDatesValidMonth)
      rm(runTimesAll)
      runDates <- timePars$runDatesAll[runTimes]
      # Forecast Times
      foreTimesList <- list()
      foreDatesList <- list()
      for (i in 1:length(runTimes)) {
            auxDateString <- gsub("\\[|]", "", unlist(strsplit(gcs$getTimeAxisForRun(as.integer(runTimes[i] - 1))$getCalendarDates()$toString(), ", ")))
            auxDates <- as.POSIXlt(strptime(auxDateString, format = "%Y-%m-%dT%H:%M:%SZ"))
            rm(auxDateString)
            # En el modelo de alcance anual pueden entrar meses del a\~no siguiente en la seleccion de forecast times y hay que quitarlos
            ind <- which((auxDates$mon + 1) %in% timePars$season)
            if (!is.null(timePars$year.cross)) {
                  rm.ind <- which((auxDates$mon + 1) == timePars$season[timePars$year.cross] & (auxDates$year + 1900) == (timePars$years[i] + 1))
            } else {
                  if (timePars$season[1] < timePars$validMonth) {
                        rm.ind <- which((auxDates$mon + 1) %in% timePars$season & (auxDates$year + 1900) != (timePars$years[i] + 1))
                  } else {
                        rm.ind <- which((auxDates$mon + 1) %in% timePars$season & (auxDates$year + 1900) != (timePars$years[i]))
                  }
            }
            if (length(rm.ind) > 0) {
                  foreTimesList[[i]] <- ind[-match(rm.ind, ind)]
            } else {
                  foreTimesList[[i]] <- ind
            }
            if (!is.null(dic)) {
                  if (dic$deaccum == 1 & foreTimesList[[i]][1] > 1) {
                        foreTimesList[[i]] <- c(foreTimesList[[i]][1] - 1, foreTimesList[[i]])
                  }
            }
            foreDatesList[[i]] <- auxDates[foreTimesList[[i]]]
            rm(auxDates)
      }
      foreDates <- foreDatesList[[1]]
      rDates <- rep(runDates[1], length(foreDatesList[[1]]))
      if (length(foreDatesList) > 1) {
            for (i in 2:length(foreDatesList)) {
                  foreDates <- c(foreDates, foreDatesList[[i]])
                  rDates <- c(rDates, rep(runDates[i], length(foreDatesList[[i]])))
            }
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
      # Data retrieval
      Members <- list()
      foreDatesNew <- foreDates
      foreDatesEndNew <- foreDatesEnd
      rDatesNew <- rDates
      latStart <- latLon$LatIndex[1]
      latCount <- length(latLon$LatIndex)
      for (i in 1:length(members)) {
            Data <- matrix(ncol = nrow(latLon$Grid), nrow = length(foreDates))
            mem <- members[i]
            auxC <- 1
            for (j in 1:length(runTimes)) {
                  rt <- runTimes[j] - 1
                  ftStart <- foreTimesList[[j]][1] - 1
                  ftCount <- length(foreTimesList[[j]])
                  auxC <- c(auxC, ftCount)
                  DataRowRange <- seq.int(sum(auxC) - auxC[j + 1], sum(auxC[-1]))
                  # Dateline crossing issue
                  if (is.null(latLon$DatelineCrossInd)) {
                        lonStart <- latLon$LonIndex[1]
                        lonCount <- length(latLon$LonIndex)
                        start <- as.integer(c(mem, rt, ftStart, latStart, lonStart))
                        stride <- as.integer(c(1, 1, ftCount, latCount, lonCount))      
                        aux <- grid$getVariable()$read(start, stride)$getStorage()
                        aux.mat <- matrix(aux, ncol = nrow(latLon$Grid), byrow = TRUE)
                        rm(aux)
                        Data[DataRowRange, ] <- aux.mat
                        rm(aux.mat)
                  } else {
                        lonStartList <- list(1:(latLon$DatelineCrossInd), (latLon$DatelineCrossInd + 1):length(latLon$LonIndex))
                        DataColRangeList <- list(1:(latLon$DatelineCrossInd * latCount), (latLon$DatelineCrossInd * latCount + 1) : nrow(latLon$Grid))
                        for (k in 1:length(lonStartList)) {
                              lonStart <- latLon$LonIndex[lonStartList[[k]][1]]
                              lonCount <- length(lonStartList[[k]])
                              start <- as.integer(c(mem, rt, ftStart, latStart, lonStart))
                              stride <- as.integer(c(1, 1, ftCount, latCount, lonCount))      
                              aux <- grid$getVariable()$read(start, stride)$getStorage()
                              aux.mat <- matrix(aux, ncol = length(DataColRangeList[[k]]), byrow = TRUE)
                              rm(aux)
                              Data[DataRowRange, DataColRangeList[[k]]] <- aux.mat
                              rm(aux.mat)
                        }
                  }
            }
            if (!is.null(latLon$DatelineCrossInd)) {
                  Data <- t(t(Data)[order(-latLon$Grid[,2], latLon$Grid[,1]), ])
            }
            if (!is.null(dic)) {
                  is.standard <- TRUE
                  offset <- dic$offset
                  scaleFac <- dic$scale
                  Data <- Data * scaleFac + offset
                  if (dic$deaccum == 1) {
                        aux.mat.deaccum <- matrix(ncol = ncol(Data), nrow = nrow(Data))
                        for (j in 2:nrow(aux.mat.deaccum)) {
                              aux.mat.deaccum[j, ] <- Data[j, ] - Data[j-1, ]
                        }
                        initDays <- which((foreDates$mon + 1) %in% timePars$season == FALSE)
                        if (length(initDays) == 0) {
                              ind <- which((foreDates$mon + 1) %in% timePars$season[1])
                              initAux <- rep(1, length(ind))
                              for (j in 2:length(initAux)) {
                                    initAux[j] <- ind[j] - ind[j-1]
                              }
                              initDays <- ind[which(initAux > 1)]
                              aux.mat.deaccum[initDays, ] <- NA
                        } else {
                              aux.mat.deaccum <- aux.mat.deaccum[-initDays, ]
                              foreDatesNew <- foreDates[-initDays]
                              foreDatesEndNew <- foreDatesEnd[-initDays]
                              rDatesNew <- rDates[-initDays]
                        }
                        Data <- aux.mat.deaccum
                  }
            } else {
                  is.standard <- FALSE
            }
            Members[[i]] <- Data
      }
      rm(timePars)
      latLon$Grid <- SpatialPoints(latLon$Grid, proj4string = CRS("+proj=longlat +datum=WGS84"))
      if (length(latLon$Grid) > 1) {
            latLon$Grid <- SpatialGrid(points2grid(latLon$Grid, tolerance = 0.05), proj4string = CRS("+proj=longlat +datum=WGS84"))      
      }
      names(Members) <- paste("Member", members + 1, sep = "_")
      return(list("VarName" = var, "isStandard" = is.standard, "MemberData" = Members, "LonLatCoords" = latLon$Grid, "RunDates" = rDatesNew, "ForecastDates" = list("Start" = foreDatesNew, "End" = foreDatesEndNew)))
}
# End