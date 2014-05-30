#' Makes a logical subset of a System4 GeoGrid
#' 
#' Makes a logical subset of a System4 GeoGrid using the parameters specified by the user,
#' applying the java methods makeSubset and readDataSlice. Subroutine of loadSeasonalForecast.S4
#' 
#' @param grid An input java GeoGrid
#' @param latLon A list of geolocation parameters, as returned by getLatLonDomainForecast
#' @param memberRangeList A list of ensemble java ranges as returned by getMemberDomain.S4
#' @param runTimePars A list of run time definition parameters, as returned by getRunTimeDomain
#' @param foreTimePars A list of forecast time definition parameters, as returned by getForecastTimeDomain.S4
#' @return A n-dimensional array. Dimensions are labelled by the \dQuote{dimnames} attribute
#' @details Dimensions of length one are dropped and the \dQuote{dimnames} attribute is consequently modified.
#' In the current version the Z dimension is ignored (and dropped), as it is not planned to include multi-level variables
#' in the ECOMS-UDG by the moment.
#' @references \url{https://www.unidata.ucar.edu/software/thredds/current/netcdf-java/v4.0/javadocAll/ucar/nc2/dt/grid/GeoGrid.html}
#' @author J Bedia \email{joaquin.bedia@@gmail.com} and A. Cofi\~no

makeSubset.S4 <- function(grid, latLon, runTimePars, memberRangeList, foreTimePars) {
      gcs <- grid$getCoordinateSystem()
      dimNames <- rev(names(scanVarDimensions(grid))) # reversed!
      z <- .jnew("ucar/ma2/Range", 0L, 0L)
      aux.list <- rep(list(bquote()), length(memberRangeList))
      for (i in 1:length(memberRangeList)) {
            ens <- memberRangeList[[i]]
            aux.list1 <- rep(list(bquote()), length(runTimePars$runTimeRanges))
            for (j in 1:length(runTimePars$runTimeRanges)) {
                  rt <- runTimePars$runTimeRanges[[j]]
                  ft <- foreTimePars$ForeTimeRangesList[[j]]
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
                  aux.list1[[j]] <- do.call("abind", c(aux.list2, along = 1))
                  rm(aux.list2)
            }
            aux.list[[i]] <- do.call("abind", c(aux.list1, along = grep("^time", dimNamesRef)))
            rm(aux.list1)
      }
      mdArray <- do.call("abind", c(aux.list, along = grep(gcs$getEnsembleAxis()$getDimensionsString(), dimNamesRef, fixed = TRUE)))
      rm(aux.list)
      if (any(dim(mdArray) == 1)) {
            dimNames <- dimNamesRef[-which(dim(mdArray) == 1)]
            mdArray <- drop(mdArray)
      }
      attr(mdArray, "dimensions") <- dimNames
      return(mdArray)
}
# End