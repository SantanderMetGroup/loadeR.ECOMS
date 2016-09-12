#' Makes a logical subset of a CFSv2 GeoGrid
#' 
#' Makes a logical subset of a CFSv2 GeoGrid using the parameters specified by the user,
#' applying the java methods makeSubset and readDataSlice. Subroutine of \code{loadSeasonalForecast.CFS}
#' 
#' @param grid An input java GeoGrid
#' @param latLon A list of geolocation parameters, as returned by getLatLonDomainForecast
#' @param runTimePars A list of run time definition parameters, as returned by \code{getRunTimeDomain.ECOMS}
#' @param foreTimePars A list of forecast time definition parameters, as returned by \code{getForecastTimeDomain.CFS}
#' @return A list with the n-dimensional array of data and the modified foreTimePars with adjusted dates depending on the
#' temporal aggregation performed.
#' @details Dimensions of length one are dropped and the \dQuote{dimnames} attribute is consequently modified.
#' In the current version the Z dimension is ignored (and dropped), as it is not planned to include multi-level variables
#' in the ECOMS-UDG by the moment. Because of the lagged-runtime configuration of CFSv2 for member definition,
#' the dimension \sQuote{ensemble} doen not exist. In turn, this is created and included as a dimension in the returned array
#' from the run time parameters passed by runTimePars and foreTimePars.
#' 
#' This function performs the temporal aggregations.
#' 
#' @references \url{http://www.unidata.ucar.edu/software/thredds/v4.3/netcdf-java/v4.3/javadocAll/ucar/nc2/dt/grid/GeoGrid.html}
#' @author J Bedia and A. Cofi\~no
#' 
makeSubset.CFS <- function(grid, latLon, runTimePars, foreTimePars) {
      message("[", Sys.time(), "] Retrieving data subset ..." )
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
                        aux.list2[[k]] <- array(subSet$readDataSlice(-1L, -1L, -1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray(), dim = shapeArray)
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
                        mes <- factor(mes, levels = unique(mes), ordered = TRUE)
                        day <- (foreTimePars$forecastDates[[i]][[j]])$mday
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
      if (!is.null(foreTimePars$forecastDates[[1]][[1]])) { ## STATIC are null
            foreTimePars$forecastDates <- adjustDates.forecast(foreTimePars)
      }
      return(list("mdArray" = mdArray, "foreTimePars" = foreTimePars))
}
# End

