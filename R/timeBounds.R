# Internal function to compute the time bounds of each verification time
timeBounds <- function(dic, foreDates) { 
      if (!is.null(dic)) {
            ltb <- as.difftime(dic$lower_time_bound, format = "%H", units = "hours")
            utb <- as.difftime(dic$upper_time_bound, format = "%H", units = "hours")
            foreDates <- as.POSIXlt(foreDates - ltb)
            foreDatesEnd <- as.POSIXlt(foreDates + utb)
      } else {
            varTimeStep <- difftime(foreDates[2], foreDates[1])
            foreDatesEnd <- foreDates + varTimeStep
      }
      foreDatesEnd <- as.POSIXlt(foreDatesEnd)
      return(list("start" = foreDates, "end" = foreDatesEnd))
}
# End