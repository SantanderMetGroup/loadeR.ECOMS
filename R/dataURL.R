#' Map dataset argument to URL
#' 
#' Map dataset name argument to the remote URL
#' 
#' @param dataset Character string indicating the dataset name. Passed by \code{\link{loadSeasonalForecast}}
#' @return A list of length two:
#' \begin{itemize}
#' \item{URL}{URL path to the dataset}
#' \item{excludeVars}{Character vector or NULL, indicating the variables in the dataset to be ignored}
#' @author J. Bedia \email{joaquin.bedia@@gmail.com}

dataURL <- function(dataset) {
      x <- switch(dataset, 
            "System4_seasonal_15" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Seasonal_15Members.ncml", "excludeVars" = NULL),
            "System4_seasonal_51" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Seasonal_51Members.ncml", "excludeVars" = NULL),
            "System4_annual_15" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Annual_15Members.ncml", "excludeVars" = NULL),
            "CFSv2_seasonal_16" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/cfs/agg/cfsAgg_fmrc.ncd", "excludeVars" = c("Pressure_reduced_to_MSL_msl", "Temperature_height_above_ground")),
            "WFDEI" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/wfdei/wfdei_daily.ncml", "excludeVars" = NULL),
            "NCEP" = list("URL" = "http://meteo.unican.es/tds5/dodsC/ncepReanalysis1/ncepReanalysis1_4xDaily.ncml", "excludeVars" = NULL))
      return(x)
}
# End
