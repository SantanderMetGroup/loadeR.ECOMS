#' Get geographical coordinates
#' 
#' Retrieves the geographical coordinates of the data subset, taking care of its adequate ordering
#'  for data retrieval and mapping
#'  
#' @param grid A java \sQuote{GeoGrid}
#' @param pointXYindex An integer vector of length 2 with specifying the point selection (if any).
#' @param llRanges A list of length 2 or 1 (depending whether the subset crosses or not the dateline),
#' with the java \sQuote{LatLonRect} objects defining the spatial ranges.
#' @return A list of the following components:
#' \begin{itemize}
#' \item{x}{x coordinates in ascending order} 
#' \item{y}{y coordinates in ascending order}
#' \item{revLat}{A logical indicating if the ordering of latitudes should be reversed or not}
#' \end{itemize}
#' @author J Bedia \email{joaquin.bedia@@gmail.com}

getXYcoordinatesForecast <- function(grid, pointXYindex, llRanges) {
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
      return(list("x" = lonSlice, "y" = latSlice, "revLat" = revLat))
}
# End