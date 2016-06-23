deriveSurfaceRelativeHumidity.CFS <- function(gds, grid, latLon, runTimePars, foreTimePars, datasetName) {
      message("[", Sys.time(), "] Retrieving data subset ..." )
      # grid = uas      
      if (identical(datasetName, "CFSv2_seasonal_operative")) {
            grid.ps <- gds$findGridByName("pressfc") 
            grid.huss <- gds$findGridByName("q2m") 
      } else {
            grid.ps <- gds$findGridByName("ps") 
            grid.huss <- gds$findGridByName("huss")         
      }
      gcs <- grid$getCoordinateSystem()
      dimNames <- rev(names(scanVarDimensions(grid))) # reversed!
      z <- .jnew("ucar/ma2/Range", 0L, 0L)
      ens <- .jnull()
      aux.list <- rep(list(bquote()), length(runTimePars$runTimeRanges))
      for (i in 1:length(runTimePars$runTimeRanges)) {
            aux.list1 <- rep(list(bquote()), length(runTimePars$runTimeRanges[[i]]))
            for (j in 1:length(runTimePars$runTimeRanges[[i]])) {
                  rt <- runTimePars$runTimeRanges[[i]][[j]]
                  ft <- foreTimePars$ForeTimeRangesList[[i]][[j]]
                  aux.list2 <- rep(list(bquote()), length(latLon$llRanges))
                  for (k in 1:length(latLon$llRanges)) {
                        subSet <- grid$makeSubset(rt, ens, ft, z, latLon$llRanges[[k]]$get(0L), latLon$llRanges[[k]]$get(1L))
                        subSet.ps <- grid.ps$makeSubset(rt, ens, ft, z, latLon$llRanges[[k]]$get(0L), latLon$llRanges[[k]]$get(1L))
                        subSet.huss <- grid.huss$makeSubset(rt, ens, ft, z, latLon$llRanges[[k]]$get(0L), latLon$llRanges[[k]]$get(1L))
                        shapeArray <- rev(subSet$getShape())
                        dimNamesRef <- dimNames              
                        if (latLon$pointXYindex[1] >= 0) {
                              rm.dim <- grep("^lon", dimNamesRef)
                              shapeArray <- shapeArray[-rm.dim]
                              dimNamesRef <- dimNamesRef[-rm.dim]
                        }
                        if (latLon$pointXYindex[2] >= 0) {
                              rm.dim <- grep("^lat", dimNamesRef)
                              shapeArray <- shapeArray[-rm.dim]
                              dimNamesRef <- dimNamesRef[-rm.dim]
                        }
                        # Computation of derived hurs from huss, ps and tas
                        tas <- subSet$readDataSlice(-1L, -1L, -1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray() - 273.15
                        ps <- subSet.ps$readDataSlice(-1L, -1L, -1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray() * .01
                        huss <- subSet.huss$readDataSlice(-1L, -1L, -1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray()
                        # huss2hurs
                        hurs <- loadeR::huss2hurs(huss, tas, ps)
                        tas <- ps <- huss <- NULL
                        aux.list2[[k]] <- array(hurs, dim = shapeArray)
                        hurs <- NULL
                  }
                  aux.list1[[j]] <- do.call("abind", c(aux.list2, along = 1))
                  aux.list2 <- NULL
                  # Daily aggregator
                  if (foreTimePars$aggr.d != "none") {
                        aux.string <- paste((foreTimePars$forecastDates[[i]][[j]])$mon, (foreTimePars$forecastDates[[i]][[j]])$mday, sep = "-")
                        aux.factor <- factor(aux.string, levels = unique(aux.string), ordered = TRUE)
                        mar <- grep("^time", dimNamesRef, invert = TRUE)
                        aux.list1[[j]] <- apply(aux.list1[[j]], mar, function(x) {
                              tapply(x, INDEX = aux.factor, FUN = foreTimePars$aggr.d, na.rm = TRUE)
                        })
                        dimNamesRef <- c("time", dimNamesRef[mar])
                        # Convert dates to daily:
                        nhours <- length(aux.factor) / nlevels(aux.factor)
                        foreTimePars$forecastDates[[i]][[j]] <- foreTimePars$forecastDates[[i]][[j]][seq(1, by = nhours, length.out = nlevels(aux.factor))]
                  }
                  # Monthly aggregator
                  if (foreTimePars$aggr.m != "none") {
                        mes <- (foreTimePars$forecastDates[[i]][[j]])$mon
                        day <- (foreTimePars$forecastDates[[i]][[j]])$mday
                        mes <- factor(mes, levels = unique(mes), ordered = TRUE)
                        mar <- grep("^time", dimNamesRef, invert = TRUE)
                        aux.list1[[j]] <- apply(aux.list1[[j]], MARGIN = mar, FUN = function(x) {
                              tapply(x, INDEX = mes, FUN = foreTimePars$aggr.m)
                        })
                        dimNamesRef <- if (length(unique(mes)) > 1) {
                              c("time", dimNamesRef[mar])
                        } else {
                              dimNamesRef[mar]
                        }
                        foreTimePars$forecastDates[[i]][[j]] <- foreTimePars$forecastDates[[i]][[j]][which(day == 1)]
                  }
            }
            if (foreTimePars$aggr.m != "none") {
                  if (length(unique(mes)) > 1) {
                        aux.list[[i]] <- do.call("abind", c(aux.list1, along = grep("^time", dimNamesRef)))
                  } else {
                        aux.list[[i]] <- do.call("abind", c(aux.list1, along = -1L))
                        dimNamesRef <- c("time", dimNamesRef)
                  }
            } else {
                  aux.list[[i]] <- do.call("abind", c(aux.list1, along = grep("^time", dimNamesRef)))
            }
            aux.list1 <- NULL
      }
      mdArray <- do.call("abind", aux.list)
      aux.list <- NULL
      if (any(dim(mdArray) == 1)) {
            dimNames <- dimNamesRef[-which(dim(mdArray) == 1)]
            mdArray <- drop(mdArray)
      } else {
            dimNames <- dimNamesRef
      }
      if ("runtime" %in% dimNames) {
            dimNames <- gsub("runtime", "member", dimNames)
      }
      dimNames <- gsub("^time.*", "time", dimNames)
      mdArray <- unname(mdArray)
      attr(mdArray, "dimensions") <- dimNames
      # Date adjustment
      foreTimePars$forecastDates <- adjustDates.forecast(foreTimePars)
      return(list("mdArray" = mdArray, "foreTimePars" = foreTimePars))
}
# End