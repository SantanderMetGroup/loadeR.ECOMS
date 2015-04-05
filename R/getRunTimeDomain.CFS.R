#' Provides run time definition for the CFSv2 model
#' 
#' Output is passed to \code{getRunTimeDomain} for used-defined subsets.
#' 
#' @param runDatesAll A POSIXlt vector with all runtime dates in the dataset    
#' @param validMonth Integer. Initialization month.
#' @param members A vector defining members to select, as passed by \code{\link{loadSeasonlForecast}}
#' @return A list with two elements:
#' \begin{itemize}
#' \item runDatesEnsList a list of the same length as the number of members selected, with the initialization dates
#' \item runTimesEnsList a list of the same length as the number of members selected, with a list of initialization
#' times, in the form of java ranges, as required by makeSubset.
#' \item years The years vector, updated if necessary. See details.
#' \end{itemize}
#' @details The lagged runtime configuration of CFSv2 means that ensemble members are defined by different
#'  initializations, so that the dimension ensemble does not exist \emph{per se}, but is defined by
#'  different initializations.
#'  
#'  It may happen that there are not enough initializations for the last year to retrieve
#'  the data for all the initializations requested. In this case, the last year in selection
#'  is suppressed with a warning. If the selection corresponds to one single year, the query
#'  is automatically updated to the maximum number of available members, with a warning.
#' @references \url{http://meteo.unican.es/trac/wiki/udg/ecoms/dataserver/datasets/CFSv2}      
#' @author J. Bedia \email{joaquin.bedia@@gmail.com}

getRunTimeDomain.CFS  <- function (runDatesAll, validMonth, members, years) {
      if (is.null(members)) {
            members <- 1:16      
      }
      d <- runDatesAll$mday
      m <- runDatesAll$mon + 1
      y <- runDatesAll$year + 1900
      jan.inits <- which(((d == 12 | d == 17 | d == 22 | d == 27) & m == 12) | ((d == 1 | d == 6) & m == 1))
      feb.inits <- which(((d == 11 | d == 16 | d == 21 | d == 26 | d == 31) & m == 1) | (d == 5 & m == 2))
      mar.inits <- which(((d == 10 | d == 15 | d == 20 | d == 25) & m == 2) | ((d == 2 | d == 7) & m == 3))
      apr.inits <- which(((d == 12 | d == 17 | d == 22 | d == 27) & m == 3) | ((d == 1 | d == 6) & m == 4))
      may.inits <- which(((d == 11 | d == 16 | d == 21 | d == 26) & m == 4) | ((d == 1 | d == 6) & m == 5))
      jun.inits <- which(((d == 11 | d == 16 | d == 21 | d == 26 | d == 31) & m == 5) | (d == 5 & m == 6))
      jul.inits <- which(((d == 10 | d == 15 | d == 20 | d == 25 | d == 30) & m == 6) | (d == 5 & m == 7))
      aug.inits <- which(((d == 10 | d == 15 | d == 20 | d == 25 | d == 30) & m == 7) | (d == 4 & m == 8))
      sep.inits <- which(((d == 9 | d == 14 | d == 19 | d == 24 | d == 29) & m == 8) | (d == 3 & m == 9))
      oct.inits <- which(((d == 8 | d == 13 | d == 18 | d == 23 | d == 28) & m == 9) | (d == 3 & m == 10))
      nov.inits <- which(((d == 8 | d == 13 | d == 18 | d == 23 | d == 28) & m == 10) | ((d == 2 | d == 7) & m == 11))
      dec.inits <- which(((d == 12 | d == 17 | d == 22 | d == 27) & m == 11) | ((d == 2 | d == 7) & m == 12))
      init.list <- lapply(ls(pattern = "\\.inits$")[pmatch(tolower(month.abb), ls(pattern = "\\.inits$"))], function(x) get(x))
      rm(list = c("d", "m", "y", ls(pattern = "\\.inits$")))
      runTimesAll <- init.list[[validMonth]]
      init.list <- NULL
      runDatesValidMonth <- runDatesAll[runTimesAll]
      runTimes <- runTimesAll[which((runDatesValidMonth$year + 1900) %in% years)]
      runDates <- runDatesAll[runTimes]
      runDatesValidMonth <- runTimesAll <- runDatesAll <- NULL
      if (validMonth == 11 & (length(members) > 28 | any(members > 28))) {
            stop("Maximum number of members in this initialization is 28")
      }
      if (validMonth != 11 & (length(members) > 24 | any(members > 24))) {
            stop("Maximum number of members in this initialization is 24")
      }
      rt.mem <- sapply(unique(runDates$year + 1900), function(x) {
            length(runTimes[which((runDates$year + 1900) == x)])
      })
      if (any(members > min(rt.mem))) {
            if (length(rt.mem) == 1) {
                  members <- 1:min(rt.mem)
                  warning("Unavailable initializations for the requested members\nSelection modified to members ", members[1], " to ", length(members))
            } else {
                  year.out <- unique(runDates$year + 1900)[which.min(rt.mem)]
                  years <- years[-match(year.out, years)]
                  warning("Last year in selection removed (not enough initializations available in hindcast)\nMaximum possible choice to include last year is members 1 to ", min(rt.mem))
            }
      }
      runDates.aux <- unlist(lapply(years, function(x) {
            format(as.POSIXct(runDates[which(runDates$year + 1900 == x)[members]], tz = "GMT"), format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
      }))
      runDatesEnsList <- rep(list(bquote()), length(members))
      names(runDatesEnsList) <- paste("Member", members, sep = "_")
      runTimesEnsList <- runDatesEnsList
      for (i in 1:length(members)) {
            ind <- seq.int(i, by = length(members), length.out = length(years))
            runTimesEnsList[[i]] <- runTimes[ind]
            runDatesEnsList[[i]] <- runDates.aux[ind]
      }
      ind <- NULL
      for (i in 1:length(runTimesEnsList)) {
            runTimesEnsList[[i]] <- lapply(1:length(runTimesEnsList[[i]]), function (j) {
                  rt <- as.integer(runTimesEnsList[[i]][j] - 1)
                  .jnew("ucar/ma2/Range", rt, rt)
            })
      }
      return(list("runDates" = runDatesEnsList, "runTimeRanges" = runTimesEnsList, "years" = years))
}
# End 
