#' Searches variable string in the dictionary
#' 
#' Searches variable string provided in the dictionary to map it in the vocabulary, in order to get
#' all the necessary information for variable homogenization.
#' 
#' @param dicPath Full path to the dictionary file (a csv file with extension \sQuote{.dic}).
#' @param var Character string with the (standard) name of the variable
#' @return A data.frame of 1 row with the mapping information
#' @references \url{http://meteo.unican.es/ecoms-udg/RPackage/Homogeneization}
#' @author J. Bedia \email{joaquin.bedia@@gmail.com}

dictionaryLookup <- function(dicPath, var) {
      dictionary <- tryCatch({read.csv(dicPath, stringsAsFactors = FALSE)}, error = function(e) stop("Dictionary not found"))
      dicRow <- grep(paste("^", var, "$", sep = ""), dictionary$identifier)
      if (length(dicRow) == 0) {
            stop("Variable requested does not match any identifier in the dictionary")
      }
      return(as.data.frame(dictionary[dicRow, ]))
}
# End
