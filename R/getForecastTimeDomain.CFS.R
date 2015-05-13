#' Calculates parameters for forecast time determination along the runtime axes in CFS
#' 
#' This is a subroutine of \code{\link{loadSeasonalForecast.CFS}}
#' 
#' @param gcs a java \sQuote{GridCoordinateSystem}
#' @param runTimePars A list of elements as returned by \code{\link{getRunTimeDomain}}
#' @return A list with the following elements:
#' \begin{itemize}
#' \item{forecastDates}{A list with POSIXlt dates defining the start and end of the 
#' representative verification time. If start and end are identical, the variable is instantaneous
#' and therefore the representative time interval is 0}
#' \item{foreTimeRangesList}{A list of length \emph{i} containing the java ranges defining the
#' forecast times selected along the \emph{i-th} run time axis.}
#' \item{deaccumFromFirst}{NULL if no deaccumulation is performed. TRUE or FALSE if deaccumulation is performed from
#' the first time of the runtime axis or not respectively. If FALSE, an additional runtime is added at the beginning
#' of each element of the runTimeList to avoid losing the first day when performing deaccumulation.}
#' \item{dailyAggr}{Logical. Are the forecast time values going to be used for data aggregation?. This argument is passed
#' to \code{\link{makeSubset.CFS}} to undertake the pertinent aggregation if TRUE.}
#' \end{itemize}
#' @author J. Bedia \email{joaquin.bedia@@gmail.com} 
#' 
getForecastTimeDomain.CFS <- function (grid, dic, runTimePars, time, aggr.d, aggr.m) {
      gcs <- grid$getCoordinateSystem()
      foreTimesList <- rep(list(bquote()), length(runTimePars$runTimeRanges)) 
      foreDatesList <- foreTimesList
      timeResInSeconds <- gcs$getTimeAxisForRun(runTimePars$runTimeRanges[[1]][[1]]$element(0L))$getTimeResolution()$getValueInSeconds()
      if ((aggr.d == "none") & (time == "DD") & ((timeResInSeconds / 3600) < 24)) {
            stop("Data is sub-daily:\nA daily aggregation function must be indicated to perform daily aggregation")
      }
      # Si es MM hay que asegurarse de que se calcula sobre dato diario
      if ((aggr.m != "none") & ((timeResInSeconds / 3600) < 24) & (time == "none")) {
            stop("Data is sub-daily:\nA daily aggregation function must be indicated first to perform monthly aggregation")
      }
      if ((timeResInSeconds / 3600) == 24) {
            time <- "DD"
            if (aggr.d != "none") {
                  aggr.d <- "none"
                  message("NOTE: The original data is daily: argument 'aggr.d' ignored")
            }
      }
      if (aggr.d != "none") message("NOTE: Daily aggregation will be computed from ", timeResInSeconds / 3600, "-hourly data")
      if (aggr.m != "none") message("NOTE: Daily data will be monthly aggregated")
      for (x in 1:length(runTimePars$runTimeRanges)) {
            aux.foreTimesList <- rep(list(bquote()), length(runTimePars$runTimeRanges[[x]]))
            aux.foreDatesList <- aux.foreTimesList
            for (i in 1:length(runTimePars$runTimeRanges[[x]])) {
                  auxDates <- javaCalendarDate2rPOSIXlt(gcs$getTimeAxisForRun(runTimePars$runTimeRanges[[x]][[i]]$element(0L))$getCalendarDates())
                  ind <- which((auxDates$mon + 1) %in% runTimePars$season)
                  aux.foreTimesList[[i]] <- ind
                  aux.foreDatesList[[i]] <- auxDates[aux.foreTimesList[[i]]]
                  auxDates <- NULL
            }
            foreTimesList[[x]] <- aux.foreTimesList
            foreDatesList[[x]] <- aux.foreDatesList
      }
      aux <- rep(NA, length(foreTimesList))
      for (i in 1:length(foreTimesList)) {
            aux[i] <- length(foreTimesList[[i]][[1]])
      }
      if (length(unique(aux)) > 1) {
            aux.dates <- rep(list(bquote()), length(foreTimesList))
            for (i in 1:length(foreTimesList)) {
                  aux.dates[[i]] <- head(foreDatesList[[i]][[1]], 1)
            }
            init <- do.call("max", aux.dates)
            for (i in 1:length(foreTimesList)) {
                  if(head(foreDatesList[[i]][[1]], 1) < init) {
                        message("NOTE: some forecast times at the beginning of the initialization may be lost so all members have equal length")
                        for (j in 1:length(foreTimesList[[i]])) {
                              retain <- which(foreDatesList[[i]][[1]] >= init)
                              foreTimesList[[i]][[j]] <- foreTimesList[[i]][[j]][retain] 
                              foreDatesList[[i]][[j]] <- foreDatesList[[i]][[j]][retain] 
                        }
                  }
            }
      }
      aux <- aux.dates <- NULL
      # Sub-routine for setting stride and shift along time dimension    
#       if (is.null(dic) & time != "none") {
#             stop("Time resolution especification incompatible with non-standard variable requests\nUse the dictionary or set the 'time' argument to NULL")
#       }
#       if (is.null(dic) | isTRUE(dic$doDailyMean)) {
#             foreTimeStride <- 1L
#             foreTimeShift <- 0L
#       } else {
            if (time == "DD" | time == "none") {
                  foreTimeStride <- 1L
                  foreTimeShift <- 0L
            } else {
                  time <- as.integer(time)
                  timeInd <- which(foreDatesList[[1]][[1]]$hour == time)
                  if (length(timeInd) == 0) {
                        stop("Non-existing verification time selected.\nCheck value of argument 'time'")
                  }
                  for (i in 1:length(foreDatesList)) {
                        for (j in 1:length(foreDatesList[[i]])) {
                              foreDatesList[[i]][[j]] <- foreDatesList[[i]][[j]][timeInd]      
                        }
                  }
                  foreTimeStride <- as.integer(diff(timeInd)[1])
                  foreTimeShift <- as.integer(-(timeInd[1]-1))
                  timeInd <- NULL
            }
#       }
      # Ensure matching for daily aggregation (full days only)
#       if(!is.na(dic$dailyAggr)) {
#             for (i in 1:length(foreDatesList)) {
#                   for (j in 1:length(foreDatesList[[i]])) {
#                         # Ensure foredates start at 00:00
#                         if (foreDatesList[[i]][[j]][1]$hour != 0) {
#                               start.ind <- which(foreDatesList[[i]][[j]]$hour == 0)[1]
#                               foreDatesList[[i]][[j]] <- foreDatesList[[i]][[j]][start:length(foreDatesList[[i]][[j]])]
#                         }
#                         # Ensure full days and remove uncomplete days if any at the end
#                         len <- length(foreDatesList[[i]][[j]]) 
#                         if (len %% 4 != 0) {
#                               ntail <- len %% 4
#                               foreDatesList[[i]][[j]] <- head(foreDatesList[[i]][[j]], n = len - ntail)
#                         }
#                   }
#             }
#       }                 
#       foreDates <- do.call("c", foreDatesList[[1]])
#       foreDatesList <- NULL
      # Sub-routine for adjusting times in case of deaccumulation (unused so far in CFS)
      deaccumFromFirst <- NULL
      if (!is.null(dic)) {
            if (dic$deaccum == 1) {
                  if (foreTimesList[[1]][[1]][1] > 1) {
                        deaccumFromFirst <- FALSE
                        for (i in 1:length(foreTimesList)) {
                              for (j in 1:length(foreTimesList[[i]])) {
                                    foreTimesList[[i]][[j]] <- c(foreTimesList[[i]][[j]][1] - 1, foreTimesList[[i]][[j]])
                              }
                        }
                  } else {
                        deaccumFromFirst <- TRUE
                  }
            }
      }
      # Sub-routine for calculation of time bounds
#      foreDates <- timeBounds(dic, foreDates)
      # Java forecast time ranges list along rt axes
      for (i in 1:length(foreTimesList)) {
            for (j in 1:length(foreTimesList[[i]])) {
                  start <- as.integer(foreTimesList[[i]][[j]][1] - 1)
                  end <- as.integer(foreTimesList[[i]][[j]][length(foreTimesList[[i]][[j]])] - 1)
                  foreTimesList[[i]][[j]] <- .jnew("ucar/ma2/Range", start, end, foreTimeStride)$shiftOrigin(foreTimeShift)
            }
      }
      return(list("forecastDates" = foreDatesList, "ForeTimeRangesList" = foreTimesList, "deaccumFromFirst" = deaccumFromFirst,
                  "aggr.d" = aggr.d, "aggr.m" = aggr.m))
}
# End