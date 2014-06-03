getLatLonDomainForecast <- function(grid, lonLim, latLim) {
      gcs <- grid$getCoordinateSystem()
      bboxDataset <- gcs$getLatLonBoundingBox()
      pointXYindex <- c(-1L, -1L)
      if (length(lonLim) == 1 | length(latLim) == 1) {
            if (bboxDataset$getLonMin() >= 0) {
                  lon.aux <- lonLim[which(lonLim < 0)] + 360
            }
            if (length(lonLim) == 1) {
                  pointXYindex[1] <- gcs$findXYindexFromCoord(lon.aux, latLim[1], .jnull())[1]
                  if (pointXYindex[1] < 0) {
                        stop("Selected X point coordinate is out of range")
                  }
            }
            if (length(latLim) == 1) {
                  pointXYindex[2] <- gcs$findXYindexFromCoord(lon.aux[1], latLim, .jnull())[2]
                  if (pointXYindex[2] < 0) {
                        stop("Selected Y point coordinate is out of range")
                  }
            }
            lonLim <- NULL   
            latLim <- NULL
      }
      if (is.null(lonLim)) {
            lonLim <- c(bboxDataset$getLonMin(), bboxDataset$getLonMax())
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
#             if (bboxDataset$getLonMin() < 0 & bboxDataset$getLonMax() >= 0 & bboxRequest$crossDateline()) {
#                   .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, 180 - lonLim[1], sep = ", "))
#                   .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, lonLim[1] - 180, sep = ", "))
#                   llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec1))
#                   llRanges[[2]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec2))
#             } else {
            llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec))
#             }
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
      aux <- grid$makeSubset(.jnull(), .jnull(), .jnull(), .jnull(), llRanges[[1]]$get(0L), llRanges[[1]]$get(1L))
      revLat <- FALSE
      if (pointXYindex[2] >= 0) {
            latSlice <- aux$getCoordinateSystem()$getYHorizAxis()$getCoordValue(pointXYindex[2])
      } else {
            latSlice <- aux$getCoordinateSystem()$getYHorizAxis()$getCoordValues()
            if (lonAxisShape > 1) {
                  latSlice <- apply(t(matrix(latSlice, ncol = lonAxisShape[1])), 1, min)
            }
            if (diff(latSlice)[1] < 0) {
                  latSlice <- rev(latSlice)
                  revLat <- TRUE
            }
      }
      return(list("llRanges" = llRanges, "pointXYindex" = pointXYindex, "xyCoords" = list("x" = lonSlice, "y" = latSlice), "revLat" = revLat))            
}
# End

