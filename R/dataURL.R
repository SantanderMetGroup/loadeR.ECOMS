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
      dataset <- match.arg(dataset, c("System4_seasonal_15", "System4_seasonal_51", "System4_annual_15", "CFSv2_seasonal_16"))
      switch(dataset, 
            "System4_seasonal_15" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Seasonal_15Members.ncml", "excludeVars" = NULL),
            "System4_seasonal_51" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Seasonal_51Members.ncml", "excludeVars" = NULL),
            "System4_annual_15" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Annual_15Members.ncml", "excludeVars" = NULL),
            "CFSv2_seasonal_16" = list("URL" = "http://www.meteo.unican.es/tds5/dodsC/cfs/agg/cfsAgg_fmrc.ncd", "excludeVars" = "Pressure_reduced_to_MSL_msl"))
}
# End
