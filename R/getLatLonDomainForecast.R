#' Define spatial domain of the query
#' 
#' @param grid a java \sQuote{GeoGrid}
#' @param lonLim longitudinal range definition (see \code{\link{loadSeasonalForecast}})
#' @param latLim latitudinal range definition (see \code{\link{loadSeasonalForecast}})
#' @return A list of parameters passed to the loading function:
#' \begin{itemize}
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
#'  \code{\link{makeSubset.S4}} and \code{\link{makeSubset.CFS}}. For single point locations, 
#'  this is a integer vector of length two defining these positions, while in the case of rectangular
#'   domains its value is c(-1L,-1L).
#' @note The function assumes that datasets have degrees-east and degress-north as units
#' of the corresponding X and Y axes.
#' @references \url{https://www.unidata.ucar.edu/software/thredds/current/netcdf-java/v4.0/javadocAll/ucar/nc2/dt/GridCoordSystem.html}
#' @author J. Bedia and A. Cofin\~no
#' @keywords internal

getLatLonDomainForecast <- function(grid, lonLim, latLim) {
      if (any(lonLim > 180) | any(lonLim < -180) | any(latLim > 90) | any(latLim < -90)) {
            stop("Invalid geographical coordinates. Check 'lonLim' and/or 'latLim' argument values", call. = FALSE)
      }
      message("[", Sys.time(), "] Defining geo-location parameters")
      gcs <- grid$getCoordinateSystem()
      bboxDataset <- gcs$getLatLonBoundingBox()
      if (length(lonLim) == 1 | length(latLim) == 1) {
            pointXYpars <- findPointXYindex(lonLim, latLim, gcs)
            lonLim <- pointXYpars$lonLim
            latLim <- pointXYpars$latLim
            pointXYindex <- pointXYpars$pointXYindex
      } else {
            pointXYindex <- c(-1L, -1L)
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
      coordPars <- getXYcoordinatesForecast(grid, pointXYindex, llRanges)
      return(list("llRanges" = llRanges, "pointXYindex" = pointXYindex, "xyCoords" = list("x" = coordPars$x, "y" = coordPars$y), "revLat" = coordPars$revLat))            
}
# End