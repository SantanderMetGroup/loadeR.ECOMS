getRunTimeDomain <- function(dataset, grid, members, season, years, leadMonth) {
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
      allYears <- startYear : endYear
      if (is.null(years)) {
            years <- allYears
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
            warning("Last year in dataset: ", endYear,". Only available years will be returned")
            years <- years[1]:endYear
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
                  warning(paste("First forecast date in dataset: ", startDay, ".\nRequested seasonal data for ", startYear," not available", sep=""))
                  years <- years[-length(years)]
            } else {
                  years <- years - 1      
            }
      } else {
            year.cross.ind <- NULL
      }
      # runtime parameters depending on model
      if (grepl("CFSv2", dataset)) {
            rtPars <- getRunTimeDomain.CFS(runDatesAll, validMonth, members, years)  
      }
      if (grepl("^System4", dataset)) {
            rtPars <- getRunTimeDomain.S4(runDatesAll, validMonth, years)  
      }
      return(list("validMonth" = validMonth, "years" = years, "season" = season, "year.cross" = year.cross.ind, "runDates" = rtPars$runDates, "runTimeRanges" = rtPars$runTimeRanges))
}
# End