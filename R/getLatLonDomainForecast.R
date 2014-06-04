#' Define spatial domain of the query
#' 
#' @param grid a java \sQuote{GeoGrid}
#' @param lonLim longitudinal range definition (see \code{\link{loadSeasonalForecast}})
#' @param latLim latitudinal range definition (see \code{\link{loadSeasonalForecast}})
#' @return A list of parameters passed to the loading function:
#' \begin{itemize}{}
#' \item{llRanges}{A list of length 2 or 1 (selection crosses or not dateline) of 
#' java \sQuote{LatLonRect} class elements}
#' \item{pointXYindex}{A vector of length 2 with the index positions for the xy
#'  coordinates in case of single point selections. See details}
#' \item{xyCoords}{A list of x and y coordinates (in ascending order)}
#' \item{revLat}{Logical. Should latitude values be reversed? Sometimes needed for having latitudes
#'  in ascending order for consistent mapping}
#' \end{itemize}
#' @details In order to deal with the problem of dateline crossing, the selection is
#' partitioned into two, and the part of the domain with negative eastings is put in first
#' place for consistent spatial mapping.
#' The index position of lon and lat in the corresponding axes is returned 
#' by \code{pointXYindex}, and is passed to the \sQuote{readDataSlice} method in
#'  \code{\link{makeSubset.S4} and \code{\link{makeSubset.CFS}. For single point locations, 
#'  this is a integer vector of length two defining these positions, while in the case of rectangular
#'   domains its value is c(-1L,-1L).
#' @note The function assumes that datasets have degrees-east and degress-north as units
#' of the corresponding X and Y axes.
#' @references \url{https://www.unidata.ucar.edu/software/thredds/current/netcdf-java/v4.0/javadocAll/ucar/nc2/dt/GridCoordSystem.html#getRangesFromLatLonRect%28ucar.unidata.geoloc.LatLonRect%29}
#' @author J. Bedia \email{joaquin.bedia@@gmail.com} and A. Cofin\~no

getLatLonDomainForecast <- function(grid, lonLim, latLim) {
      gcs <- grid$getCoordinateSystem()
      bboxDataset <- gcs$getLatLonBoundingBox()
      pointXYindex <- c(-1L, -1L)
      if (length(lonLim) == 1 | length(latLim) == 1) {
            if (any(lonLim < 0) & bboxDataset$getLonMin() >= 0) {
                  lon.aux <- sort(lonLim[which(lonLim < 0)] + 360)
            } else {
                  if (!is.null(lonLim)) {
                        lon.aux <- lonLim
                  } else {
                        lon.aux <- bboxDataset$getCenterLon()
                  }
            }
            if (length(lonLim) == 1) {
                  if (is.null(latLim)) {
                        lat.aux <- bboxDataset$getLatMin()
                  } else {
                        lat.aux <- latLim[1]
                  }
                  pointXYindex[1] <- gcs$findXYindexFromCoord(lon.aux, lat.aux, .jnull())[1]
                  if (pointXYindex[1] < 0) {
                        stop("Selected X point coordinate is out of range")
                  }
                  lonLim <- NULL   
            }
            if (length(latLim) == 1) {
                  pointXYindex[2] <- gcs$findXYindexFromCoord(lon.aux[1], latLim, .jnull())[2]
                  if (pointXYindex[2] < 0) {
                        stop("Selected Y point coordinate is out of range")
                  }
                  latLim <- NULL
            }
      }
      if (is.null(lonLim)) {
            lonLim <- c(bboxDataset$getLonMin(), bboxDataset$getLonMax())
            if (all(pointXYindex < 0)) {
                  lonLim <- lonLim - 180
            }
      }
      if (is.null(latLim)) {
            latLim <- c(bboxDataset$getLatMin(), bboxDataset$getLatMax())
      }
      deltaLat <- latLim[2] - latLim[1]
      deltaLon <- lonLim[2] - lonLim[1]
      spec <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, deltaLon, sep = ", "))
      bboxRequest <- .jnew("ucar/unidata/geoloc/LatLonRect", spec)
      llRanges <- list()
      if (bboxRequest$getLonMin() < 0 & bboxRequest$getLonMax() >= 0 & bboxDataset$crossDateline()) {
            spec1 <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, 0 - lonLim[1], sep = ", "))
            spec2 <- .jnew("java/lang/String", paste(latLim[1], 0, deltaLat, lonLim[2], sep = ", "))
            llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec1))
            llRanges[[2]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec2))
      } else {
            llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec))
      }
      # Coordinates
      if (pointXYindex[1] >= 0) {
            aux <- grid$makeSubset(.jnull(), .jnull(), .jnull(), 1L, 1L, 1L)
            lonSlice <- aux$getCoordinateSystem()$getLonAxis()$getCoordValue(pointXYindex[1])
      } else {
            lonAux <- list()
            for (k in 1:length(llRanges)) {
                  aux <- grid$makeSubset(.jnull(), .jnull(), .jnull(), .jnull(), llRanges[[k]]$get(0L), llRanges[[k]]$get(1L))
                  lonAxisShape <- aux$getCoordinateSystem()$getXHorizAxis()$getRank()
                  lonAux[[k]] <- aux$getCoordinateSystem()$getXHorizAxis()$getCoordValues()
                  if (lonAxisShape > 1) {
                        lonAux[[k]] <- apply(t(matrix(lonAux[[k]], ncol = lonAxisShape[1])), 2, min)
                  } 
            }
            lonSlice <- do.call("c", lonAux)      
      }
      lonSlice[which(lonSlice > 180)] <- lonSlice[which(lonSlice > 180)] - 360
      lonSlice <- sort(lonSlice)
      aux <- grid$makeSubset(.jnull(), .jnull(), .jnull(), .jnull(), llRanges[[1]]$get(0L), llRanges[[1]]$get(1L))
      revLat <- FALSE
      if (pointXYindex[2] >= 0) {
            latSlice <- aux$getCoordinateSystem()$getYHorizAxis()$getCoordValue(pointXYindex[2])
      } else {
            latSlice <- aux$getCoordinateSystem()$getYHorizAxis()$getCoordValues()
            latAxisShape <- aux$getCoordinateSystem()$getYHorizAxis()$getRank()
            if (latAxisShape > 1) {
                  latSlice <- apply(t(matrix(latSlice, ncol = latAxisShape[1])), 1, min)
            }
            if (diff(latSlice)[1] < 0) {
                  latSlice <- rev(latSlice)
                  revLat <- TRUE
            }
      }
      return(list("llRanges" = llRanges, "pointXYindex" = pointXYindex, "xyCoords" = list("x" = lonSlice, "y" = latSlice), "revLat" = revLat))            
}
# End