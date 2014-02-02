dictionaryLookup <- function(dictionary, var) {
      dictionary <- tryCatch({read.csv(dictionary, stringsAsFactors = FALSE)}, error = function(e) stop("Dictionary not found"))
      dicRow <- grep(paste("^", var, "$", sep = ""), dictionary$identifier)
      if (length(dicRow) == 0) {
            stop("Variable requested does not match any identifier in the dictionary")
      }
      return(as.data.frame(dictionary[dicRow, ]))
}
# End
