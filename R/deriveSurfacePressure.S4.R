#' Compute surface pressure in System4 datasets
#' 
#' Performs a basic calculation of surface pressure from sea-level pressure and
#'  surface temperature while minimizing the amount of data simultaneously loaded in memory.
#' 
#' @param gds A java \dQuote{GridDataset}. This is used to load all input java \dQuote{GeoGrid}'s to derive the target variable.
#' @param grid An input java GeoGrid. This is the grid of the \sQuote{leading var} (\code{leadVar}) previously loaded
#' for subsetting parameter retrieval (see \code{\link{deriveInterface}} for details).
#' @param latLon A list of geolocation parameters, as returned by getLatLonDomainForecast
#' @param memberRangeList A list of ensemble java ranges as returned by getMemberDomain.S4
#' @param runTimePars A list of run time definition parameters, as returned by getRunTimeDomain
#' @param foreTimePars A list of forecast time definition parameters, as returned by getForecastTimeDomain.S4
#' @return A n-dimensional array. Dimensions are labelled by the \dQuote{dimnames} attribute
#' @details The function essentially follows the same approach as \code{\link{makeSubset.S4}}, excepting that at each time step
#' it loads more than one \dQuote{GeoGrid} in order to apply the equations. 
#' @references \url{http://meteo.unican.es/ecoms-udg/DataServer/ListOfVariables}
#' @author J Bedia \email{joaquin.bedia@@gmail.com}, borrowing some MatLab code from S. Herrera. 

deriveSurfacePressure.S4 <- function(gds, grid, latLon, runTimePars, memberRangeList, foreTimePars) {
      message("[", Sys.time(), "] Retrieving data subset ..." )
      # grid = tas (K)
      grid.zs <- gds$findGridByName("zsfc") # surface geopotential (STATIC!) (m^2/s^2)
      grid.mslp <- gds$findGridByName("mslp") # sea level pressure (Pa)
      gcs <- grid$getCoordinateSystem()
      dimNames <- rev(names(scanVarDimensions(grid))) # reversed!
      z <- .jnew("ucar/ma2/Range", 0L, 0L)
      aux.foreDatesList <- rep(list(foreTimePars$forecastDates), length(memberRangeList))      
      foreTimePars$forecastDates <- NULL
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
                        subSet.zs <- grid.zs$makeSubset(z, z, z, z, latLon$llRanges[[k]]$get(0L), latLon$llRanges[[k]]$get(1L))
                        subSet.mslp <- grid.mslp$makeSubset(rt, ens, ft, z, latLon$llRanges[[k]]$get(0L), latLon$llRanges[[k]]$get(1L))
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
                        # Computation of derived hurs (from 'mslp2ps.m')
                        tas <- subSet$readDataSlice(-1L, -1L, -1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray()
                        zs.aux <- subSet.zs$readDataSlice(-1L, -1L, -1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray()
                        zs <- rep(zs.aux, length(tas) / length(zs.aux))
                        zs.aux <- NULL
                        mslp <- subSet.mslp$readDataSlice(-1L, -1L, -1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray()
                        ps <- mslp2ps(tas, zs, mslp)
                        zs <- NULL
                        mslp <- NULL
                        tas <- NULL
                        aux.list2[[k]] <- array(ps, dim = shapeArray)
                        ps <- NULL
                  }
                  aux.list1[[j]] <- do.call("abind", c(aux.list2, along = 1))
                  aux.list2 <- NULL
                  # Daily aggregator
                  if (foreTimePars$aggr.d != "none") {
                        aux.string <- paste((aux.foreDatesList[[i]][[j]])$mon, (aux.foreDatesList[[i]][[j]])$mday, sep = "-")
                        aux.factor <- factor(aux.string, levels = unique(aux.string))
                        mar <- grep("^time", dimNamesRef, invert = TRUE)
                        aux.list1[[j]] <- apply(aux.list1[[j]], mar, function(x) {
                              tapply(x, INDEX = aux.factor, FUN = foreTimePars$aggr.d, na.rm = TRUE)
                        })
                        dimNamesRef <- c("time", dimNamesRef[mar])
                        # Convert dates to daily:
                        nhours <- length(aux.factor) / nlevels(aux.factor)
                        aux.foreDatesList[[i]][[j]] <- aux.foreDatesList[[i]][[j]][seq(1, by = nhours, length.out = nlevels(aux.factor))]
                  }
                  # Monthly aggregator
                  if (foreTimePars$aggr.m != "none") {
                        mes <- (aux.foreDatesList[[i]][[j]])$mon
                        day <- (aux.foreDatesList[[i]][[j]])$mday
                        mar <- grep("^time", dimNamesRef, invert = TRUE)
                        aux.list1[[j]] <- apply(aux.list1[[j]], MARGIN = mar, FUN = function(x) {
                              tapply(x, INDEX = mes, FUN = foreTimePars$aggr.m)
                        })
                        dimNamesRef <- c("time", dimNamesRef[mar])
                        aux.foreDatesList[[i]][[j]] <- aux.foreDatesList[[i]][[j]][which(day == 1)]
                  }
            }
            aux.list[[i]] <- do.call("abind", c(aux.list1, along = grep("^time", dimNamesRef)))
            aux.list1 <- NULL
      }
      mdArray <- do.call("abind", c(aux.list, along = grep(gcs$getEnsembleAxis()$getDimensionsString(), dimNamesRef, fixed = TRUE)))
      aux.list <- NULL
      if (any(dim(mdArray) == 1)) {
            dimNames <- dimNamesRef[-which(dim(mdArray) == 1)]
            mdArray <- drop(mdArray)
      } else {
            dimNames <- dimNamesRef
      }
      dimNames <- gsub("^time.*", "time", dimNames)
      mdArray <- unname(mdArray)
      attr(mdArray, "dimensions") <- dimNames
      # Date adjustment
      foreTimePars$forecastDates <- aux.foreDatesList
      foreTimePars$forecastDates <- adjustDates.forecast(foreTimePars)
      return(list("mdArray" = mdArray, "foreTimePars" = foreTimePars))
}
# End