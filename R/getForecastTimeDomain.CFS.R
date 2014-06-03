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
#' forecast times selected along the \emph{i-th} run time axis.
#' \item{deaccumFromFirst}{NULL if no deaccumulation is performed. TRUE or FALSE if deaccumulation is performed from
#' the first time of the runtime axis or not respectively. If FALSE, an additional runtime is added at the beginning
#' of each element of the runTimeList to avoid losing the first day when performing deaccumulation.}
#' \end{itemize}
#' @author J. Bedia \email{joaquin.bedia@@gmail.com} 
#' 
getForecastTimeDomain.CFS <- function (grid, dic, runTimePars, verifTime) {
      gcs <- grid$getCoordinateSystem()
      foreTimesList <- rep(list(bquote()), length(runTimePars$runTimeRanges)) 
      foreDatesList <- foreTimesList
      for (x in 1:length(runTimePars$runTimeRanges)) {
            aux.foreTimesList <- rep(list(bquote()), length(runTimePars$runTimeRanges[[x]]))
            aux.foreDatesList <- aux.foreTimesList
            for (i in 1:length(runTimePars$runTimeRanges[[x]])) {
                  auxDates <- javaCalendarDate2rPOSIXlt(gcs$getTimeAxisForRun(runTimePars$runTimeRanges[[x]][[i]]$element(0L))$getCalendarDates())
                  ind <- which((auxDates$mon + 1) %in% runTimePars$season)
                  aux.foreTimesList[[i]] <- ind
                  aux.foreDatesList[[i]] <- auxDates[aux.foreTimesList[[i]]]
                  rm(auxDates)
            }
            foreTimesList[[x]] <- aux.foreTimesList
            foreDatesList[[x]] <- aux.foreDatesList
      }
      deaccumFromFirst <- FALSE
      # Sub-routine for setting stride and shift along time dimension    
      if (is.null(verifTime)) { 
            foreTimeStride <- 1L
            foreTimeShift <- 0L
      } else {
            verifTimeInd <- which(foreDatesList[[1]][[1]]$hour == verifTime)
            if (length(verifTimeInd) == 0) {
                  stop("Non-existing verification time selected.\nCheck value of argument 'verifTime'")
            }
            for (i in 1:length(foreDatesList)) {
                  for (j in 1:length(foreDatesList[[i]])) {
                        foreDatesList[[i]][[j]] <- foreDatesList[[i]][[j]][verifTimeInd]      
                  }
            }
            foreTimeStride <- as.integer(diff(verifTimeInd)[1])
            foreTimeShift <- as.integer(-(verifTimeInd[1]-1))
            rm(verifTimeInd)
      }
      foreDates <- do.call("c", foreDatesList[[1]])
      rm(foreDatesList)
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
      foreDates <- timeBounds(dic, foreDates)
      # Java forecast time ranges list along rt axes
      for (i in 1:length(foreTimesList)) {
            for (j in 1:length(foreTimesList[[i]])) {
                  start <- as.integer(foreTimesList[[i]][[j]][1] - 1)
                  end <- as.integer(foreTimesList[[i]][[j]][length(foreTimesList[[i]][[j]])] - 1)
                  foreTimesList[[i]][[j]] <- .jnew("ucar/ma2/Range", start, end, foreTimeStride)$shiftOrigin(foreTimeShift)
            }
      }
      return(list("forecastDates" = foreDates, "ForeTimeRangesList" = foreTimesList, "deaccumFromFirst" = deaccumFromFirst))
}
# End