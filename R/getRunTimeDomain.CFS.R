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
      h <- runDatesAll$hour
      jan.inits <- which(((d == 12 | d == 17 | d == 22 | d == 27) & (m == 12 & h != 6)) | ((d == 1 | d == 6) & m == 1))
      feb.inits <- which(((d == 11 | d == 16 | d == 21 | d == 26 | d == 31) & m == 1) | (d == 5 & m == 2))
      mar.inits <- which(((d == 10 | d == 15 | d == 20 | d == 25) & m == 2) | (((d == 2 & h != 0) | d == 7) & (m == 3)))
      apr.inits <- which(((d == 12 | d == 17 | d == 22 | d == 27) & m == 3) | ((d == 1 | d == 6) & m == 4))
      may.inits <- which(((d == 11 | d == 16 | d == 21 | d == 26) & m == 4) | ((d == 1 | d == 6) & m == 5))
      jun.inits <- which(((d == 11 | d == 16 | d == 21 | d == 26 | d == 31) & m == 5) | (d == 5 & m == 6))
      jul.inits <- which(((d == 10 | d == 15 | d == 20 | d == 25 | d == 30) & m == 6) | (d == 5 & m == 7))
      aug.inits <- which(((d == 10 | d == 15 | d == 20 | d == 25 | d == 30) & m == 7) | (d == 4 & m == 8))
      sep.inits <- which(((d == 9 | d == 14 | d == 19 | d == 24 | d == 29) & m == 8) | (d == 3 & m == 9))
      oct.inits <- which(((d == 8 | d == 13 | d == 18 | d == 23 | d == 28) & m == 9) | (d == 3 & m == 10))
      nov.inits <- which(((d == 8 | d == 13 | d == 18 | d == 23 | d == 28) & m == 10) | ((d == 2 | d == 7) & (m == 11 & h != 0)))
      dec.inits <- which(((d == 12 | d == 17 | d == 22 | d == 27) & (m == 11 & h != 0)) | ((d == 2 | d == 7) & (m == 12 & h != 6)))
      init.list <- lapply(ls(pattern = "\\.inits$")[pmatch(tolower(month.abb), ls(pattern = "\\.inits$"))], function(x) get(x))
      rm(list = c("d", "m", "y", ls(pattern = "\\.inits$")))
      runTimesValidMonth <- init.list[[validMonth]]
      runDatesValidMonth <- runDatesAll[runTimesValidMonth]
      nmem <- length(init.list[[validMonth]] ) / 28
      init.list <- NULL
      if (length(members) > nmem | any(members > nmem)) {
            stop("Maximum number of members for this initialization is ", nmem, "\nSee details in <http://meteo.unican.es/trac/wiki/udg/ecoms/dataserver/datasets/CFSv2>")
      }
      # Excepcion cuando valid month = 1 toma inicializaciones del a\~no anterior
      if (validMonth == 1) {
            yr.ind <- which(unique(runDatesValidMonth$year + 1900) %in% (years -1))
      } else {
            yr.ind <- which(unique(runDatesValidMonth$year + 1900) %in% (years))
      }
      aux.ind <- findInterval(1:length(runDatesValidMonth), vec = seq(1, length(runDatesValidMonth), nmem))
      runTimes.aux <- unlist(lapply(yr.ind, function(x) runTimesValidMonth[aux.ind == x][members]))
      runDates.aux <- do.call("c", lapply(yr.ind, function(x) runDatesValidMonth[aux.ind == x][members]))
      runTimesValidMonth <- runDatesValidMonth <- aux.ind <- NULL
      runTimesEnsList <- lapply(1:length(members), function(x) {
            ind <- seq(x, by = length(members), length.out = length(years))
            return(runTimes.aux[ind])
      })
      runDatesEnsList <- lapply(1:length(members), function(x) {
            ind <- seq(x, by = length(members), length.out = length(years))
            return(format(as.POSIXct(runDates.aux[ind]), format = "%Y-%m-%d %H:%M:%S", tz = "GMT", usetz = TRUE))
      })
      runTimes.aux <- runDates.aux <- NULL
      names(runTimesEnsList) <- names(runDatesEnsList) <- paste("Member", members, sep = "_")
      for (i in 1:length(runTimesEnsList)) {
            runTimesEnsList[[i]] <- lapply(1:length(runTimesEnsList[[i]]), function (j) {
                  rt <- as.integer(runTimesEnsList[[i]][j] - 1)
                  .jnew("ucar.ma2.Range", rt, rt)
            })
      }
      return(list("runDates" = runDatesEnsList, "runTimeRanges" = runTimesEnsList, "years" = years))
}
# End 


