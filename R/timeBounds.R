# Internal function to compute the time bounds of each verification time
timeBounds <- function(dic, foreDates) { 
      if (!is.null(dic)) {
            ltb <- as.difftime(dic$lower_time_bound, format = "%H", units = "hours")
            foreDates <- foreDates - ltb
            foreDates <- as.POSIXlt(foreDates)
            varTimeStep <- strptime(dic$time_step, format = "%Hh")$hour
            foreDatesEnd <- foreDates + varTimeStep * 3600
      } else {
            varTimeStep <- difftime(foreDates[2], foreDates[1])
            foreDatesEnd <- foreDates + varTimeStep
      }
      foreDatesEnd <- as.POSIXlt(foreDatesEnd)
      return(list("start" = foreDates, "end" = foreDatesEnd))
}
# End