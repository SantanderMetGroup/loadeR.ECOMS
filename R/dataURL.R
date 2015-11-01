#' Map dataset argument to URL
#' 
#' Map dataset name argument to the remote URL
#' 
#' @param dataset Character string indicating the dataset name. Passed by \code{\link{loadSeasonalForecast}}
#' @return A list of length two:
#' \itemize{
#' \item \code{URL}. URL path to the dataset
#' \item \code{excludeVars}. Character vector or NULL, indicating the variables in the dataset to be ignored
#' }
#' @author J. Bedia \email{joaquin.bedia@@gmail.com}
#' @keywords internal

dataURL <- function(dataset) {
      x <- switch(dataset, 
            "System4_seasonal_15" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Seasonal_15Members.ncml", "excludeVars" = NULL),
            "System4_seasonal_51" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Seasonal_51Members.ncml", "excludeVars" = NULL),
            "System4_annual_15" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Annual_15Members.ncml", "excludeVars" = NULL),
            "CFSv2_seasonal" = list("URL" = "http://meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml", "excludeVars" = NULL),
            "SMHI_EC_EARTH_EUPORIAS" = list("URL" = "http://meteo.unican.es/tds5/dodsC/ec_earth_v3/smhi_ec_earth_4xDaily.ncml", "excludeVars" = NULL),
            "WFDEI" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/wfdei/wfdei_daily.ncml", "excludeVars" = NULL),
            "NCEP_reanalysis1" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/ncepReanalysis1/ncepReanalysis1_4xDaily.ncml", "excludeVars" = NULL),
            "ERA_interim" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/interim/daily/interim20_daily.ncml", "excludeVars" = NULL))
      return(x)
}
# End
