#' Definition of vertical dimension slices
#' 
#' Returns the selected level value (if any) and a suitable java structure. This is a subroutine
#' of \code{loadGridDataset}, whose output is passed to \code{makeSubset}. 
#'  
#' @param gcs An object of the java class \sQuote{GeoGrid})
#' @param level Vertical level. Passed by \code{loadGridDataset}, obtained via \code{findVerticalLevel}
#' @param dataset dataset name
#' @return A list with the level value and either a java Range or a java null reference
#' defining the index of the vertical axis (null if no vertical levels exist)
#' @author J. Bedia 

getVerticalLevelPars.ECOMS <- function(grid, dataset, level) {
    gcs <- grid$getCoordinateSystem()
    if (gcs$hasVerticalAxis()) {
        if (is.null(level)) {
            stop("Variable with vertical levels: '@level' following the variable name is required.\nGo to <http://meteo.unican.es/trac/wiki/udg/ecoms/dataserver/catalog> for details", call. = FALSE)
        }
        levelInd <- gcs$getVerticalAxis()$findCoordElement(level)
        if (grepl("System4", dataset)) {
            levelInd <- 0L      
        } else if (levelInd < 0) {
            stop("Vertical level not found.\nGo to <http://meteo.unican.es/trac/wiki/udg/ecoms/dataserver/catalog> for valid vertical level values", call. = FALSE)
        }
        zRange <- .jnew("ucar/ma2/Range", levelInd, levelInd)
    } else {
        if (!is.null(level)) {
            warning("The variable selected is 2D: the '@level' specification was ignored", call. = FALSE)
            level <- NULL
        }
        zRange <- .jnull()    
    }
    return(list("level" = level, "zRange" = zRange))
}
# End