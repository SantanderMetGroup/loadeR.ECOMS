#' Java calendar dates to R POSIXlt dates
#' 
#' Converts objects of the Java class \sQuote{CalendarDate} to \link[base]{POSIXlt} in R
#' 
#' @param javaCalendarDate A \sQuote{ucar.nc2.time.CalendarDate} object or array 
#'  containing N of them.
#' @return A vector of \code{POSIXlt} dates in R
#' @author J. Bedia \email{joaquin.bedia@@gmail.com}
#' @references \url{https://www.unidata.ucar.edu/software/thredds/current/netcdf-java/v4.3/javadoc/ucar/nc2/time/CalendarDate.html}

javaCalendarDate2rPOSIXlt <- function(javaCalendarDate) {
      r.string <- gsub("T|Z", "", javaString2rChar(javaCalendarDate$toString()))
      r.posix <- as.POSIXlt(r.string, tz = "GMT", format = "%Y-%m-%d%H:%M:%S")
      return(r.posix)
}

