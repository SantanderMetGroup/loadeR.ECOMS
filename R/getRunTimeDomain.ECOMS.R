#' Computes run time definition according to the time specifications
#' 
#' This is a sub-routine of \code{\link{loadECOMS}}. Its output is passed to the loading functions
#' (the \code{makeSubset\\.*} and \code{derive*} interfaces).
#' 
#' @param dataset Target hindcast dataset (see \code{\link{loadECOMS}})
#' @param grid A java \dQuote{GeoGrid} containing the target(leading) variable
#' @param members Member selection (see \code{\link{loadECOMS}})
#' @param season Season selection (see \code{\link{loadECOMS}})
#' @param years Years selected (see \code{\link{loadECOMS}})
#' @param leadMonth Lead month (see \code{\link{loadECOMS}})
#' @return A list of parameters:
#' \itemize{
#' \item validMonth An integer in the range [1,12] indicating the month to take the initialization
#' \item years A vector of years selected
#' \item season Season
#' \item year.cross An auxiliary integer value thet indicates the position of year-crossing
#'  within the season vector. NULL if no year-crossing season has been chosen.
#' \item runDates a POSIXlt vector of initialization dates
#' \item runTimeRanges a list of of initialization times of the java class \dQuote{ucar.ma2.ranges}.
#' }
#' @details The function calls to specific subroutines for CFS or System4 requests, given their different
#' runtime configurations. The function also takes care of selecting the appropriate initialization
#' in the case of year-crossing seasons 
#' @author J Bedia 
#' @keywords internal

getRunTimeDomain.ECOMS <- function(dataset, grid, members, season, years, leadMonth) {
      message("[", Sys.time(), "] Defining initialization time parameters")
      gcs <- grid$getCoordinateSystem()
      if (is.null(season)) {
            season <- unique(javaCalendarDate2rPOSIXlt(gcs$getTimeAxisForRun(0L)$getCalendarDates())$mon + 1)
      }
      rt.axis <- gcs$getRunTimeAxis()
      runDatesAll <- javaCalendarDate2rPOSIXlt(rt.axis$getCalendarDates())
      startDay <- javaCalendarDate2rPOSIXlt(rt.axis$getCalendarDateRange()$getStart())
      endDay <- javaCalendarDate2rPOSIXlt(rt.axis$getCalendarDateRange()$getEnd())
      startYear <- startDay$year + 1900
      endYear <- endDay$year + 1900
      allYears <- startYear:endYear
      if (is.null(years)) {
            if (grepl("CFSv2_seasonal_operative", dataset)) {
                  years <- 2015:as.numeric(format(Sys.time(),"%Y"))
            }else if (grepl("CFSv2", dataset)) {
                  years <- 1983:2009
            } else {
                  years <- allYears
            }
      } 
      if (grepl("CFSv2", dataset)) {
            if (grepl("CFSv2_seasonal_operative", dataset)) {
                  aux <- intersect(years, 2015:as.numeric(format(Sys.time(),"%Y")))
                  if(length(aux)>1){
                    stop('Multiple year requests are not allowed for CFSv2 operative')
                  }
                  if(length(aux)<1){
                    stop('Requested year not available')
                  }
            }else{
                  aux <- intersect(years, 1983:2009)
                  if (!identical(as.integer(aux), as.integer(years))) {
                        warning("Available years in dataset: 1983-2009\nSome years were removed")
                  }
            }
            years <- aux
            aux <- NULL
      }
      if (years[1] < startYear & years[length(years)] > endYear) {
            warning("Year selection out of dataset range. Only available years will be returned")
            years <- allYears
      }
      if (years[1] < startYear) {
            warning("First year in dataset: ", startYear,". Only available years will be returned")
            years <- startYear:years[length(years)]
      }
      if (years[length(years)] > endYear) {
            warning("Last initialization in the dataset in year: ", endYear,". Only available years will be returned")
            years <- years[which(years <= endYear + 1)]
      }
      # Month to take the initialization 
      validMonth <- season[1] - leadMonth 
      if ((season[1] - leadMonth) < 1) {
            validMonth <- validMonth + 12
            years <- years - 1 
      }
      # Year-crossing seasons - year to take the initialization
      if (!identical(season, sort(season))) {
            year.cross.ind <- which(diff(season) < 0) # indicates the position of year-crossing within season
            if (years[1] == startYear) { 
                  warning(paste0("First forecast date in dataset: ", startDay, ".\nRequested seasonal data for ", startYear," not available"))
                  years <- years[-length(years)]
            } else {
                  years <- years - 1      
            }
      } else {
            year.cross.ind <- NULL
      }
      # runtime parameters depending on model
      if (grepl("CFSv2", dataset)) {
            rtPars <- getRunTimeDomain.CFS(runDatesAll, validMonth, members, years, dataset)
            # years <- rtPars$years
      } else if (grepl("^System4|SMHI-EC-EARTH_EUPORIAS", dataset)) {
            rtPars <- getRunTimeDomain.S4(runDatesAll, validMonth, years)  
      } else if (grepl("^Glosea5", dataset)) {
            rtPars <- getRunTimeDomain.GS5(dataset, season, leadMonth, runDatesAll, validMonth, members, years) 
      }
      return(list("validMonth" = validMonth, "years" = years, "season" = season, "year.cross" = year.cross.ind, "memberRangeList" = rtPars$memberRangeList, "runDates" = rtPars$runDates, "runTimeRanges" = rtPars$runTimeRanges))
}
# End
