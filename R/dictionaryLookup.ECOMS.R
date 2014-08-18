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
            stop("Variable requested does not match any identifier in the dictionary")
      }
      dailyAggr <- NA
      if (length(dicRow) > 1) {
            morethanone <- TRUE
            if (time == "DD" & derInterface$deriveInterface == "none") {
                  dicRow <- dicRow[dictionary$time_step[dicRow] == "24h"]
                  time <- "none"
                  if (length(dicRow) == 0) {
                        dicRow <- grep(paste("^", derInterface$origVar, "$", sep = ""), dictionary$identifier)                  
                        dicRow <- dicRow[dictionary$time_step[dicRow] == "6h"]
                        time <- "DD"
                  }
            } else {
                  dicRow <- dicRow[dictionary$time_step[dicRow] == "6h"]
                  morethanone <- FALSE
            }
      } else {
            morethanone <- FALSE
            if (dictionary$time_step[dicRow] == "12h" & time == "DD") {
                  stop("Cannot compute daily mean from 12-h data")
            }
            if ((time == "06" | time == "18") & dictionary$time_step[dicRow] == "12h") {
                  stop("Requested 'time' value (\"", time, "\") not available for 12-h data")
            }
            if ((time != "none" & time != "DD") & (dictionary$time_step[dicRow] == "24h")) {
                  stop("Subdaily data not available for variable \"", derInterface$origVar, "\". Check value of argument 'time'")
            }
            if (time == "DD" & dictionary$time_step[dicRow] == "24h") {
                  time <- "none"
            }
      }
      if (time == "DD" & !isTRUE(morethanone)) {
            dailyAggr <- "mean"
            if (derInterface$origVar == "tp" | derInterface$origVar == "rlds" | derInterface$origVar == "rsds") {
                  dailyAggr <- "sum"
                  message("NOTE: daily accumulated will be calculated from the 6-h model output")
            } else if (grepl(".*max$", derInterface$origVar)) {
                  dailyAggr <- "max"
                  message("NOTE: daily maximum will be calculated from the 6-h model output")
            } else if (grepl(".*min$", derInterface$origVar)) {
                  dailyAggr <- "min"
                  message("NOTE: daily minimum will be calculated from the 6-h model output")
            } else {
                  message("NOTE: daily mean will be calculated from the 6-h model output")
            }
      }
      if (derInterface$deriveInterface == "none") {
            dic <- cbind.data.frame(dictionary[dicRow, ], "dailyAggr" = I(dailyAggr))
      } else {
            dicRow2 <- grep(paste("^", derInterface$origVar, "$", sep = ""), dictionary$identifier)
            dic <- cbind.data.frame(dictionary[dicRow2, ], "dailyAggr" = I(dailyAggr))
            dic$short_name <- dictionary$short_name[dicRow]
      }
      return(dic)
}
# End

 