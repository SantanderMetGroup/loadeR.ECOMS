#' Calculates parameters for forecast time determination along the runtime axis
#' 
#' This is a subroutine of \code{\link{loadSeasonalForecast.S4}}
#' 
#' @param gcs a java \sQuote{GridCoordinateSystem}
#' @param runTimePars A list of elements as returned by \code{\link{getRunTimeDomain}}
#' @return A list with the following elements:
#' \begin{itemize}
#' \item{forecastDates}{A list with POSIXlt dates defining the start and end of the 
#' representative verification time. If start and end are identical, the variable is instantaneous
#' and therefore the representative time interval is 0}
#' \item{foreTimeRangesList}{A list of length \emph{i} containing the java ranges defining the
#' forecast times selected along the \emph{i-th} run time axis.
#' \item{foreTimeShift}{Integer value (java format) giving the shift to start reading in the time axis}
#' \item{foreTimeStride}{Integer value (java format) giving the stride for reading in the time axis}
#' \item{deaccumFromFirst}{NULL if no deaccumulation is performed. TRUE or FALSE if deaccumulation is performed from
#' the first time of the runtime axis or not respectively. If FALSE, an additional runtime is added at the beginning
#' of each element of the runTimeList to avoid losing the first day when performing deaccumulation.}
#' \end{itemize}
#' @author J. Bedia \email{joaquin.bedia@@gmail.com} 

getForecastTimeDomain.S4 <- function (grid, runTimePars, verifTime) {
      gcs <- grid$getCoordinateSystem()
      foreTimesList <- rep(list(bquote()), length(runTimePars$runTimeRanges))
      foreDatesList <- foreTimesList
      for (i in 1:length(runTimePars$runTimeRanges)) {
            auxDates <- javaCalendarDate2rPOSIXlt(gcs$getTimeAxisForRun(runTimePars$runTimeRanges[[i]]$element(0L))$getCalendarDates())
            ind <- which((auxDates$mon + 1) %in% runTimePars$season)
            if (grepl("51$", dataset)) {
                  if (!is.null(runTimePars$year.cross)) {
                        rm.ind <- which((auxDates$mon + 1) == runTimePars$season[timePars$year.cross] & (auxDates$year + 1900) == (timePars$years[i] + 1))
                  } else {
                        if (timePars$season[1] < runTimePars$validMonth) {
                              rm.ind <- which((auxDates$mon + 1) %in% runTimePars$season & (auxDates$year + 1900) != (timePars$years[i] + 1))
                        } else {
                              rm.ind <- which((auxDates$mon + 1) %in% runTimePars$season & (auxDates$year + 1900) != (timePars$years[i]))
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
            deaccumFromFirst <- FALSE
            rm(auxDates)
      }
      # Sub-routine for setting stride and shift along time dimension    
      if (is.null(verifTime)) { 
            foreTimeStride <- 1L
            foreTimeShift <- 0L
      } else {
            verifTimeIndList <- lapply(1:length(foreDatesList), function(x) {
                  which(foreDatesList[[x]]$hour == verifTime)
            })
            if (length(verifTimeIndList[[1]]) == 0) {
                  stop("Non-existing verification time selected.\nCheck value of argument 'verifTime'")
            }
            foreDatesList <- lapply(1:length(foreDatesList), function(x) {
                  foreDatesList[[x]][verifTimeIndList[[x]]]
            })
            foreTimeStride <- as.integer(diff(verifTimeIndList[[1]])[1])
            foreTimeShift <- as.integer(-(verifTimeIndList[[1]][1] - 1))
            rm(verifTimeIndList)
      }
      foreDates <- do.call("c", foreDatesList)
      rm(foreDatesList)
      # Sub-routine for adjusting times in case of deaccumulation
      deaccumFromFirst <- NULL
      if (!is.null(dic)) {
            if (dic$deaccum == 1) {
                  if (foreTimesList[[1]][1] > 1) {
                        deaccumFromFirst <- FALSE
                        foreTimesList <- lapply(1:length(foreTimesList), function(x) {
                              c(foreTimesList[[x]][1] - 1, foreTimesList[[x]])
                        })
                  } else {
                        deaccumFromFirst <- TRUE
                  }
            }
      }
      # Sub-routine for calculation of time bounds
      foreDates <- timeBounds(dic, foreDates)
      # Java forecast time ranges list along rt axes
      foreTimeRangesList <- lapply(1:length(foreTimesList), function(x) {
            .jnew("ucar/ma2/Range", as.integer(foreTimesList[[x]][1] - 1), as.integer(foreTimesList[[x]][length(foreTimesList[[x]])] - 1), foreTimeStride)$shiftOrigin(foreTimeShift)
            
      })
      return(list("forecastDates" = foreDates, "ForeTimeRangesList" = foreTimeRangesList, "deaccumFromFirst" = deaccumFromFirst))
}
# End