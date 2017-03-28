#' Provides run time definition for ECMWF's S4 models
#' 
#' Output is passed to \code{getRunTimeDomain.ECOMS} for used-defined subsets.
#' 
#' @param runDatesAll A POSIXlt vector with all runtime dates in the dataset    
#' @param validMonth Integer. Initialization month.
#' @param years Numeric vector of years selected
#' @return A list with two elements:
#' \begin{itemize}
#' \item runDates a POSIXlt vector of initialization dates
#' \item runTimeRanges a list of of initialization times, in the form of java ranges, as required by makeSubset.
#' \end{itemize}
#' @details The input/outoputs are slightly different than getRuntimeDomain.CFS because of the 
#' different ensemble member configuration
#' @author J. Bedia 
#' @keywords internal

getRunTimeDomain.S4 <- function(runDatesAll, validMonth, years) {
      if (!is.null(runDatesAll)) {
            runTimesAll <- which(runDatesAll$mon == (validMonth - 1))
            if (length(runTimesAll) == 0) {
                  stop(paste("Incompatible 'leadMonth' and 'season' argument values.\nInitializations in", 
                             paste(month.name[unique(runDatesAll$mon + 1)], collapse = ", ")),
                       call. = FALSE)
            }
            runDatesValidMonth <- runDatesAll[runTimesAll]
            runTimes <- runTimesAll[which((runDatesValidMonth$year + 1900) %in% years)]
            runDatesValidMonth <- runTimesAll <- NULL
            runDates <- runDatesAll[runTimes]
            # java ranges
            runTimeRanges <- lapply(1:length(runTimes), function(x) {
                  .jnew("ucar/ma2/Range", as.integer(runTimes[x] - 1), as.integer(runTimes[x] - 1))
            })
      } else { ## Static variables
            runDates <- NULL
            runTimeRanges <- list(.jnull())
      }
      return(list("runDates" = runDates, "runTimeRanges" = runTimeRanges))
}
# End
