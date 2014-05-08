getTimeDomainForecast <- function(gcs, season, years, leadMonth) {
      if (is.null(season)) {
            auxDateString <- gsub("\\[|]", "", unlist(strsplit(gcs$getTimeAxisForRun(0L)$getCalendarDates()$toString(), ", ")))
            season <- 1:length(unique(as.POSIXlt(strptime(auxDateString, format = "%Y-%m-%dT%H:%M:%SZ"))$mon))
            rm(auxDateString)
      }
      charRunTimes <- gsub("\\[|]|\\s", "", unlist(strsplit(gcs$getRunTimeAxis()$getCalendarDates()$toString(), ","),""))
      runDatesAll <- strptime(gsub("\\[|]|\\s", "", unlist(strsplit(charRunTimes, ","))), format = "%Y-%m-%dT%H:%M:%SZ")
      rm(charRunTimes)
      startDay <- runDatesAll[1]
      startYear <- startDay$year + 1900
      endYear <- runDatesAll[length(runDatesAll)]$year + 1900
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
      if (season[1] - leadMonth < 1) {
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
      return(list("validMonth" = validMonth, "years" = years, "season" = season, "year.cross" = year.cross.ind, "runDatesAll" = runDatesAll))
}
# End