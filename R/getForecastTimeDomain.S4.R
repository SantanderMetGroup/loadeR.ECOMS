#' Calculates parameters for forecast time determination along the runtime axis
#' 
#' This is a subroutine of \code{\link{loadSeasonalForecast.S4}}
#' 
#' @param grid a java \sQuote{GeoGrid}
#' @param dataset character string of the dataset
#' @param dic dictionary information
#' @param runTimePars A list of elements as returned by \code{\link{getRunTimeDomain.ECOMS}}
#' @param time Verification time.
#' @param aggr.d Character string. Function of aggregation of sub-daily data for daily data calculation. 
#' Currently accepted values are \code{"none"}, \code{"mean"}, \code{"min"}, \code{"max"} and \code{"sum"}.
#' @param aggr.m Same as \code{aggr.d}, bun indicating the aggregation function to compute monthly from daily data.
#' If \code{aggr.m = "none"} (the default), no monthly aggregation is undertaken.
#' @return A list with the following elements:
#' \itemize{
#' \item{forecastDates}{A list with POSIXlt dates defining the start and end of the 
#' representative verification time. If start and end are identical, the variable is instantaneous
#' and therefore the representative time interval is 0}
#' \item{foreTimeRangesList}{A list of length \emph{i} containing the java ranges defining the
#' forecast times selected along the \emph{i-th} run time axis.}
#' \item{foreTimeShift}{Integer value (java format) giving the shift to start reading in the time axis}
#' \item{foreTimeStride}{Integer value (java format) giving the stride for reading in the time axis}
#' \item{deaccumFromFirst}{NULL if no deaccumulation is performed. TRUE or FALSE if deaccumulation is performed from
#' the first time of the runtime axis or not respectively. If FALSE, an additional runtime is added at the beginning
#' of each element of the runTimeList to avoid losing the first day when performing deaccumulation.}
#' \item{doDailyMean}{Logical. Are the forecast time values going to be used for data aggregation?. This argument is passed
#' to \code{\link{makeSubset.S4}} to undertake the pertinent aggregation if TRUE.}
#' }
#' @author J. Bedia 

getForecastTimeDomain.S4 <- function(grid, dataset, dic, runTimePars, time, aggr.d, aggr.m) {
      gcs <- grid$getCoordinateSystem()
      deaccum <- FALSE
      #deaccumFromFirst <- NULL
      if (dic$time_step == "static") {
            foreDates <- "static field"
            foreDatesList <- list(NULL)
            foreTimeRangesList <- list(.jnew("ucar/ma2/Range", 0L, 0L))
      } else {
            timeResInSeconds <- gcs$getTimeAxisForRun(runTimePars$runTimeRanges[[1]]$element(0L))$getTimeResolution()$getValueInSeconds()
            if ((aggr.d == "none") & (time == "DD") & ((timeResInSeconds / 3600) < 24)) {
                  stop("Data is sub-daily:\nA daily aggregation function must be indicated to perform daily aggregation", call. = FALSE)
            }
            # Si es MM hay que asegurarse de que se calcula sobre dato diario
            if ((aggr.m != "none") & ((timeResInSeconds / 3600) < 24) & (time == "none")) {
                  stop("Data is sub-daily:\nA daily aggregation function must be indicated before performing a monthly aggregation", call. = FALSE)
            }
            if ((timeResInSeconds / 3600) == 24) {
                  time <- "DD"
                  if (aggr.d != "none") {
                        aggr.d <- "none"
                        message("NOTE: The original data is daily: argument 'aggr.d' ignored")
                  }
            } else if ((timeResInSeconds / 3600) > 100) { # Monthly, typically 700something
                  aggr.d <- aggr.m <- "none"
            }
            if (aggr.d != "none") message("NOTE: Daily aggregation will be computed from ", timeResInSeconds / 3600, "-hourly data")
            if (aggr.m != "none") message("NOTE: Daily data will be monthly aggregated")
            foreTimesList <- rep(list(bquote()), length(runTimePars$runTimeRanges))
            foreDatesList <- foreTimesList
            for (i in 1:length(runTimePars$runTimeRanges)) {
                  auxDates <- javaCalendarDate2rPOSIXlt(gcs$getTimeAxisForRun(runTimePars$runTimeRanges[[i]]$element(0L))$getCalendarDates())
                  ind <- which((auxDates$mon + 1) %in% runTimePars$season)
                  if (grepl("annual", dataset)) {
                        if (!is.null(runTimePars$year.cross)) {
                              rm.ind <- which((auxDates$mon + 1) == runTimePars$season[runTimePars$year.cross] & (auxDates$year + 1900) == (runTimePars$years[i] + 1))
                        } else {
                              if (runTimePars$season[1] < runTimePars$validMonth) {
                                    rm.ind <- which((auxDates$mon + 1) %in% runTimePars$season & (auxDates$year + 1900) != (runTimePars$years[i] + 1))
                              } else {
                                    rm.ind <- which((auxDates$mon + 1) %in% runTimePars$season & (auxDates$year + 1900) != (runTimePars$years[i]))
                              }
                        }
                        if (length(rm.ind) > 0) {
                              foreTimesList[[i]] <- ind[-match(rm.ind, ind)]
                        } else {
                              foreTimesList[[i]] <- ind
                        }
                  } else {
                        foreTimesList[[i]] <- ind
                  }
                  foreDatesList[[i]] <- auxDates[foreTimesList[[i]]]
                  auxDates <- NULL
            }
            if (time == "DD" | time == "none") {
                  foreTimeStride <- 1L
                  foreTimeShift <- 0L
            } else {
                  time <- as.integer(time)
                  timeIndList <- lapply(1:length(foreDatesList), function(x) {
                        which(foreDatesList[[x]]$hour == time)
                  })
                  if (length(timeIndList[[1]]) == 0) {
                        stop("Non-existing verification time selected.\nCheck value of argument 'time'", call. = FALSE)
                  }
                  foreDatesList <- lapply(1:length(foreDatesList), function(x) {
                        foreDatesList[[x]][timeIndList[[x]]]
                  })
                  foreTimeStride <- as.integer(diff(timeIndList[[1]])[1])
                  foreTimeShift <- as.integer(-(timeIndList[[1]][1] - 1))
                  timeIndList <- NULL
            }
            # Sub-routine for adjusting times in case of deaccumulation
            if (!is.null(dic)) {
                  if (dic$deaccum == 1) {
                        deaccum <- TRUE
                        foreTimesList <- lapply(1:length(foreTimesList), function(x) {
                              append(foreTimesList[[x]], tail(foreTimesList[[x]], 1) + 1)
                        })
                  }
            }
            foreTimeRangesList <- lapply(1:length(foreTimesList), function(x) {
                  .jnew("ucar/ma2/Range", as.integer(foreTimesList[[x]][1] - 1), 
                        as.integer(tail(foreTimesList[[x]], 1L) - 1),
                        foreTimeStride)$shiftOrigin(foreTimeShift)
            })
      }
      return(list("forecastDates" = foreDatesList, "ForeTimeRangesList" = foreTimeRangesList, "deaccum" = deaccum,
                  "aggr.d" = aggr.d, "aggr.m" = aggr.m))
}
# End