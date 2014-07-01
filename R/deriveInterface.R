#' Identification of interface for derived variables
#' 
#' Determines whether a variable is derived or not and returns the interface name
#'  if applicable, building on the information retrieved from the dictionary.
#'  This is a subroutine of \code{\link{loadECOMS}}
#' 
#' @param dataset character string indicating the dataset. See details.
#' @param var character string defining the target variable. See details.
#' @param dictionary Logical indicating if a dictionary is used.
#' @return A list of length two:
#' \begin{itemize}
#' \item{deriveInterface}{Either a character string indicating the interface to derive the variable,
#'  or NULL if the variable is not derived}
#' \item{leadVar}{First variable that will be loaded and whose subsetting parameters
#' and metadata will be used as reference for the derived variable. See details.}
#' \item{origVar}{Name of the variable requested. In case of applying an approximation function,
#' this is the requested variable}
#' \end{itemize}
#' @details Currently in ECOMS there is only a reduced set of derived variables,
#' corresponding to the S4-seasonal-15-member model, all at surface level, but the
#'  function is intended to handle other eventualities (derived 3D vars and/or
#'  derived variables from other datasets/models). The underlying idea of the function is
#'  first arbitrarily loading one of the input variables (the \code{leadVar}), whose
#'  geolocation and time subsetting definition are used subsequently for all other input
#'  variables. For instance, if one needs the \sQuote(original var) surface wind speed (\code{"wss"}),
#'  the first variable being loaded (i.e., the \sQuote{leading var}) is the eastward 
#'  wind component (\code{"uas"}), and then all parameters are passed to the subsequent
#'  functions for subsetting and are recicled for loading also the northward component \code{"vas"}
#'   needed to compute the velocity module.
#' @author J Bedia \email{joaquin.bedia@@gmail.com}

deriveInterface <- function(dataset, var, dictionary) {
      if (!dictionary) {
            stop("The requested variable is derived. The dictionary must be used for homogenization and conversion of input variables\nGo to http://meteo.unican.es/ecoms-udg/ListOfVariables for details")
      }
      # dicPath <- file.path(find.package("ecomsUDG.Raccess"), "dictionaries", paste(dataset,".dic", sep = ""))
      # devel (comment before package building)
      dicPath <- file.path("./inst/dictionaries", paste(dataset,".dic", sep = ""))
      dictionary <- tryCatch({read.csv(dicPath, stringsAsFactors = FALSE)}, error = function(e) stop("Dictionary not found"))
      dicRow <- grep(paste("^", var, "$", sep = ""), dictionary$identifier) 
      if (dictionary$derived[dicRow] == 1) {
            message("NOTE: The requested variable is not originally stored in model's database\nIt will be derived on-the-fly using an approximation\nGo to http://meteo.unican.es/ecoms-udg/ListOfVariables for details")
            deriveInterface <- dictionary$interface[dicRow]
            lev <- findVerticalLevel(var)$level
            leadVar <- switch(deriveInterface, 
                                    "deriveSurfacePressure" = "psl",
                                    "deriveSurfaceRelativeHumidity" = "tas",
                                    "deriveSurfaceSpecificHumidity" = "tdps",
                                    "deriveSurfaceWindSpeed" = "uas")
            if (!is.null(lev)) {
                  leadVar <- paste(leadVar, lev, sep = "@")
            }       
      } else {
            deriveInterface <- NULL
            leadVar <- var
      }
      return(list("deriveInterface" = deriveInterface, "leadVar" = leadVar, "origVar" = var))
}
# End
