#' Deaccumulation routine
#' 
#' Used for deaccumulation of precipitation in certain model data (e.g. System4). 
#' 
#' @param x a vector of (accumulated) data.
#' @param dff Logical. Is deaccumulation performed from the first value of the time series?.
#' @return a vector of deaccumulated data
#' @details When leadMonth equals 0, there is not a previous day for starting deaccumulation, and therefore the
#'  first value of the time series is taken 'as is'. Otherwise, one value before the start has to be taken
#'  to preserve time series length (this is previously done by getForecastTimeDomain.S4).
#' @author J. Bedia 
#' @keywords internal

deaccumulate <- function(x, dff) {
      if (isTRUE(dff)) {
            c(x[1], diff(x))
      } else {
            diff(x)
      }
}