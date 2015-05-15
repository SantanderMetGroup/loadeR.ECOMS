#' Searches variable string in the dictionary
#' 
#' Searches variable string provided in the dictionary to map it in the vocabulary, in order to get
#' all the necessary information for variable homogenization. It also includes a new column specifying the
#' aggregation function to be applied (if any). This funciton is ECOMS-UDG specific.
#' 
#' @param dicPath Full path to the dictionary file (a csv file with extension \sQuote{.dic}).
#' @param derInterface A list as returned by \code{\link{deriveInterface}}
#' @param time Time specification.
#' @return A data.frame of 1 row with the mapping information
#' @details The function is somewhat tricky when dealing with derived variables. It applies the
#' time adjustment and homogenization/transformation steps indicated in the \sQuote{original variable},
#' (i.e., the variable to be derived) but at the end replaces the \code{shortName} by that of the
#'  \sQuote{leading variable} (see \code{\link{deriveInterface}} for a description of this concept),
#'  in order to open an existing \dQuote{GeoGrid}.
#' @references \url{http://meteo.unican.es/ecoms-udg/RPackage/Homogeneization}
#' @author J. Bedia \email{joaquin.bedia@@gmail.com}

dictionaryLookup.ECOMS <- function(dicPath, derInterface, time) {
      message("[", Sys.time(), "] Defining homogeneization parameters for variable \"", derInterface$origVar, "\"")
      dictionary <- tryCatch({read.csv(dicPath, stringsAsFactors = FALSE)}, error = function(e) stop("Dictionary not found"))
      dicRow <- grep(paste("^", findVerticalLevel(derInterface$leadVar)$var, "$", sep = ""), dictionary$identifier) 
      if (length(dicRow) == 0) {
            stop("Variable requested not found\nCheck available variables at https://meteo.unican.es/trac/wiki/udg/ecoms/dataserver/listofvariables")
      }
      if (length(dicRow) > 1) {
            if (time == "DD" & derInterface$deriveInterface == "none") {
                  dicRow <- dicRow[dictionary$time_step[dicRow] == "24h"]
                  if (length(dicRow) == 0) {
                        dicRow <- grep(paste("^", derInterface$origVar, "$", sep = ""), dictionary$identifier)                  
                        dicRow <- dicRow[dictionary$time_step[dicRow] == "6h" | dictionary$time_step[dicRow] == "3h"]
                  }
            } else {
                  dicRow <- dicRow[dictionary$time_step[dicRow] == "6h" | dictionary$time_step[dicRow] == "3h"]
            }
      } else {
            if (dictionary$time_step[dicRow] == "12h" & time == "DD") {
                  warning("Daily mean was calculated from 12-h data")
            }
            if ((time %in% c("03","06","09","15","18","21")) & dictionary$time_step[dicRow] == "12h") {
                  stop("Requested 'time' value (\"", time, "\") not available for 12-h data")
            }
            if ((time != "none" & time != "DD") & (dictionary$time_step[dicRow] == "24h")) {
                  stop("Subdaily data not available for variable \"", var, "\". Check value of argument 'time'")
            }
            if (time == "DD" & dictionary$time_step[dicRow] == "24h") {
                  time <- "none"
            }
      }
      if (derInterface$deriveInterface == "none") {
            dic <- dictionary[dicRow, ]
      } else {
            dicRow2 <- grep(paste("^", derInterface$origVar, "$", sep = ""), dictionary$identifier)
            dic <- dictionary[dicRow2, ]
            dic$short_name <- dictionary$short_name[dicRow]
      }
      return(dic)
}
# End

 