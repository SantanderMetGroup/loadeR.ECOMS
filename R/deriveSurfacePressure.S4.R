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
      # Constants
      Rd <- 287.058 # dry air constant J/(K kg)
      GammaST <- 0.0065 #(dT/dz)^st standard atmosphere vertical gradient of the temperature in the troposphere (0.0065 (K/m^-1))
      g <- 9.80665 # (m/s^2)
      # grid = tas (K)
      grid.zs <- gds$findGridByName("zsfc") # surface geopotential (STATIC!) (m^2/s^2)
      grid.mslp <- gds$findGridByName("mslp") # sea level pressure (Pa)
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
                  static.range <- .jnew("ucar/ma2/Range", 0L, 0L)
                  for (k in 1:length(latLon$llRanges)) {
                        subSet <- grid$makeSubset(rt, ens, ft, z, latLon$llRanges[[k]]$get(0L), latLon$llRanges[[k]]$get(1L))
                        subSet.zs <- grid.zs$makeSubset(static.range, static.range, static.range, z, latLon$llRanges[[k]]$get(0L), latLon$llRanges[[k]]$get(1L))
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
                        To <- tas + GammaST * zs / g
                        ind <- which(abs(zs) >= 0.001)
                        ind1 <- intersect(intersect(which(To > 290.5), which(tas <= 290.5)), ind)
                        mslp <- subSet.mslp$readDataSlice(-1L, -1L, -1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray()
                        auxGamma <- mslp
                        ps <- mslp
                        auxGamma[ind1]<-g*(290.5-tas[ind1])/zs[ind1]
                        ind <- setdiff(ind, ind1)
                        ind1 <- intersect(intersect(which(To > 290.5), which(tas > 290.5)), ind)
                        auxGamma[ind1] <- 0
                        tas[ind1] <- 0.5 * (255 + tas[ind1])
                        ind <- setdiff(ind, ind1)
                        ind1 <- intersect(which(tas < 255), ind)
                        auxGamma[ind1] <- GammaST
                        tas[ind1] <- 0.5 * (255 + tas[ind1])
                        ind <- setdiff(ind, ind1)
                        auxGamma[ind] <- GammaST
                        ind <- which(abs(zs) >= 0.001)
                        ps[ind] <- mslp[ind] * exp((-zs[ind] / (Rd * tas[ind])) * (1 - 0.5 * (auxGamma[ind] * zs[ind]) / (g * tas[ind]) + (1 / 3) * ((auxGamma[ind] * zs[ind]) / (g * tas[ind])) ^ 2))
                        tas <- NULL
                        zs <- NULL
                        mslp <- NULL
                        auxGamma <- NULL
                        aux.list2[[k]] <- array(ps, dim = shapeArray)
                        ps <- NULL
                  }
                  # Sub-routine for daily aggregation from 6h data
                  if (!is.na(foreTimePars$dailyAggr)) {
                        aux.list1[[j]] <- toDD(do.call("abind", c(aux.list2, along = 1)), dimNamesRef, foreTimePars$dailyAggr)
                        dimNamesRef <- attr(aux.list1[[j]], "dimensions")
                  } else {
                        aux.list1[[j]] <- do.call("abind", c(aux.list2, along = 1))
                  }
                  aux.list2 <- NULL
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
      return(mdArray)
}
# End