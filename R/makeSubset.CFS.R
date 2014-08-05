#' Makes a logical subset of a CFSv2 GeoGrid
#' 
#' Makes a logical subset of a CFSv2 GeoGrid using the parameters specified by the user,
#' applying the java methods makeSubset and readDataSlice. Subroutine of \code{loadSeasonalForecast.CFS}
#' 
#' @param grid An input java GeoGrid
#' @param latLon A list of geolocation parameters, as returned by getLatLonDomainForecast
#' @param runTimePars A list of run time definition parameters, as returned by \code{getRunTimeDomain}
#' @param foreTimePars A list of forecast time definition parameters, as returned by \code{getForecastTimeDomain.CFS}
#' @return A n-dimensional array. Dimensions are labelled by the \dQuote{dimnames} attribute
#' @details Dimensions of length one are dropped and the \dQuote{dimnames} attribute is consequently modified.
#' In the current version the Z dimension is ignored (and dropped), as it is not planned to include multi-level variables
#' in the ECOMS-UDG by the moment. Because of the lagged-runtime configuration of CFSv2 for member definition,
#' the dimension \sQuote{ensemble} doen not exist. In turn, this is created and included as a dimension in the returned array
#' from the run time parameters passed by runTimePars and foreTimePars.
#' @references \url{https://www.unidata.ucar.edu/software/thredds/current/netcdf-java/v4.0/javadocAll/ucar/nc2/dt/grid/GeoGrid.html}
#' @author J Bedia \email{joaquin.bedia@@gmail.com} and A. Cofi\~no
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
                              rm.dim <- grep(gcs$getXHorizAxis()$getDimensionsString(), dimNamesRef, fixed = TRUE)
                              shapeArray <- shapeArray[-rm.dim]
                              dimNamesRef <- dimNamesRef[-rm.dim]
                        }
                        if (latLon$pointXYindex[2] >= 0) {
                              rm.dim <- grep(gcs$getYHorizAxis()$getDimensionsString(), dimNamesRef, fixed = TRUE)
                              shapeArray <- shapeArray[-rm.dim]
                              dimNamesRef <- dimNamesRef[-rm.dim]
                        }
                        aux.list2[[k]] <- array(subSet$readDataSlice(-1L, -1L, -1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray(), dim = shapeArray)
                  }
                  # Sub-routine for daily aggregation from 6h data
                  if (!is.na(foreTimePars$dailyAggr)) {
                        aux.list1[[j]] <- toDD(do.call("abind", c(aux.list2, along = 1)), dimNamesRef, foreTimePars$dailyAggr)
                        dimNamesRef <- attr(aux.list1[[j]], "dimensions")
                  } else {
                        aux.list1[[j]] <- do.call("abind", c(aux.list2, along = 1))
                  }
            }
            aux.list[[i]] <- do.call("abind", c(aux.list1, along = grep("^time", dimNamesRef)))
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
      return(mdArray)
}
# End