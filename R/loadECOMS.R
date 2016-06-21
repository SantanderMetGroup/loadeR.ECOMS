# loadECOMS.R Load a user-defined spatio-temporal slice from ECOMS-UDG datasets
#
#     Copyright (C) 2015 Santander Meteorology Group (http://www.meteo.unican.es)
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Load a field from decadal forecasts
#' @description Load a user-defined spatio-temporal slice from decadal forecasts
#' @import rJava
#' 
#' @template templateParams
#' @param members Vector of integers indicating the members to be loaded.
#' @param time A character vector indicating the temporal filtering/aggregation 
#' of the output data. Default to \code{"none"}, which returns the original time 
#' series as stored in the dataset. For sub-daily variables, instantantaneous data at 
#' selected verification times can be filtered using one of the character strings 
#' \code{"00"}, \code{"03"}, \code{"06"}, \code{"09"}, \code{"12"}, \code{"15"},
#'  \code{"18"}, \code{"21"},and \code{"00"} when applicable. If daily aggregated data are 
#' required use \code{"DD"}. If the requested variable is static (e.g. orography) it will be ignored. 
#' See the next arguments for time aggregation options.
#' @param aggr.d Character string. Function of aggregation of sub-daily data for daily data calculation. 
#' Currently accepted values are \code{"none"}, \code{"mean"}, \code{"min"}, \code{"max"} and \code{"sum"}.
#' @param aggr.m Same as \code{aggr.d}, bun indicating the aggregation function to compute monthly from daily data.
#' If \code{aggr.m = "none"} (the default), no monthly aggregation is undertaken.
#' 
#' @template templateReturnGridData
#' @template templateDicDetails  
#' @template templateGeolocation
#' @export
#' @author J. Bedia, S. Herrera, M. Iturbide, J.M. Gutierrez 
#' @family loading.grid


loadECOMS <- function(dataset, var, dictionary = TRUE, 
                     members = NULL, lonLim = NULL, latLim = NULL, season = NULL,
                     years = NULL, leadMonth = 1, time = "none",
                     aggr.d = "none", aggr.m = "none") {
      dataset <- match.arg(dataset, c("System4_seasonal_15",
                                      "System4_seasonal_51",
                                      "System4_annual_15",
                                      "CFSv2_seasonal",
                                      "SMHI-EC-EARTH_EUPORIAS",
                                      "Glosea5_seasonal_12",
                                      "Glosea5_seasonal_24",
                                      "WFDEI",
                                      "NCEP_reanalysis1",
                                      "ERA_interim"))
      time <- match.arg(time, choices = c("none","00","03","06","09","12","15","18","21","DD"))
      aggr.d <- match.arg(aggr.d, choices = c("none", "mean", "min", "max", "sum"))
      if (time != "DD" & aggr.d != "none") {
            aggr.d <- "none"
            message("NOTE: Argument 'aggr.d' ignored as 'time' was set to ", time)
      }
      aggr.m <- match.arg(aggr.m, choices = c("none", "mean", "min", "max", "sum"))
      derInterface <- deriveInterface(dataset, var, dictionary, time)
      var <- derInterface$leadVar
      aux.level <- findVerticalLevel(derInterface$leadVar)
      var <- aux.level$var
      level <- aux.level$level
      url <- dataURL(dataset)
      # Dictionary/shortName search
      if (isTRUE(dictionary)) {
            dicPath <- file.path(find.package("loadeR.ECOMS"), "dictionaries", paste0(dataset, ".dic"))
            # for devel only 
            # dicPath <- file.path("./inst/dictionaries", paste(dataset, ".dic", sep = ""))
            dic <- dictionaryLookup.ECOMS(dicPath, derInterface, time)
            shortName <- dic$short_name
            if (grepl("System4\\_seasonal\\_15", dataset) & grepl("^u$|^v$|^z$|^t$|^q$", shortName)) {
                  shortName <- paste0(dic$short_name, level, "mb")
            } else if (grepl("ERA\\_interim", dataset) & grepl("^U$|^V$|^Z$|^T$|^Q$", shortName)) {
                  shortName <- paste0(dic$short_name, level)
            }
      } else {
            dic <- NULL
            shortName <- var
      }
      # Static variable requests
      if (dic$time_step == "static") {
            message("NOTE: The requested variable is static. All time-related arguments will be ignored")
            season <- 1
            years <- 2000
            time <- "none"
      } else {
            if (is.null(season)) {
                  stop("Argument 'season' must be provided")
            }
            if (min(season) < 1 | max(season) > 12) {
                  stop("Invalid season definition")
            }
      }
      # Exception in NCEP due to different grids within the same dataset
      if (dataset == "NCEP_reanalysis1") {
            if (length(lonLim) == 1 || length(latLim) == 1) {
                  stop("Single-point selections are invalid for the NCEP_reanalysis1 dataset\nConsider using a small rectangular domain", call. = FALSE)
            }
      }
      # Note when loading gridded datasets
      if (grepl("WFDEI|NCEP_reanalysis1|ERA_interim", dataset) & !is.null(members)) {
            message("NOTE: The dataset is not a forecast. Argument 'members' will be ignored")      
      }
      # Discover dataset and open grid
      gds <- openDataset(url$URL)
      grid <- gds$findGridByShortName(shortName)
      if (is.null(grid)) {
            stop("Variable requested not found\nCheck available variables at http://meteo.unican.es/trac/wiki/udg/ecoms/dataserver/catalog", call. = FALSE)
      }
      latLon <- getLatLonDomain(grid, lonLim, latLim)
      proj <- grid$getCoordinateSystem()$getProjection()
      if (!proj$isLatLon()) latLon <- adjustRCMgrid(gds, latLon, lonLim, latLim) 
      # Grid datasets
      if (grepl("WFDEI|NCEP_reanalysis1|ERA_interim", dataset)) {
           out <- loadGridDataset(var, grid, dic, level, season, years, time, latLon, aggr.d, aggr.m)
      # Forecasts
      } else {
            if (dic$time_step == "static") {
                  members <- 1
            }
            if (!is.null(members)) {
                  members <- sort(members)
            }
            if (is.null(leadMonth) & dic$time_step != "static") {
                  stop("A lead month for forecast initialization must be specified", call. = FALSE)
            }
            if (leadMonth < 0 & dic$time_step != "static") {
                  stop("Invalid lead time definition", call. = FALSE)
            }
            if (leadMonth == 0 & dic$time_step != "static") {
                  message("NOTE: 'leadMonth = 0' selected")
            }
            # Season range constraints
            if (dic$time_step != "static") {
                  if (grepl("CFSv2", dataset) & (length(season) + leadMonth) > 9) {
                        stop("Max. forecast extent is 9 months. Reduce season length or lead month value accordingly", call. = FALSE)            
                  }
                  if (grepl("System4_seasonal", dataset) & (length(season) + leadMonth) > 7) {
                        stop("Max. forecast extent is 7 months. Reduce season length or lead month value accordingly", call. = FALSE)            
                  }
                  if (grepl("Glosea5.*12", dataset) & (length(season) + leadMonth) > 4) {
                        stop("Max. forecast extent is 130 days. Reduce season length or lead month value accordingly", call. = FALSE)            
                  }
                  if (grepl("Glosea5.*24", dataset) & (length(season) + leadMonth) > 4) {
                        stop("Max. forecast extent is 120 days. Reduce season length or lead month value accordingly", call. = FALSE)            
                  }
                  if (grepl("System4_annual", dataset) & (length(season) + leadMonth) > 13) {
                        stop("Max. forecast extent is 13 months. Reduce season length or lead month value accordingly", call. = FALSE)            
                  }
                  if (grepl("SMHI-EC-EARTH_EUPORIAS", dataset) & (length(season) + leadMonth) > 4) {
                        stop("Max. forecast extent is 4 months. Reduce season length or lead month value accordingly", call. = FALSE)            
                  }
            }
            leadMonth <- as.integer(leadMonth)
            runTimePars <- getRunTimeDomain.ECOMS(dataset, grid, members, season, years, leadMonth)
            if (grepl("^System4|SMHI-EC-EARTH_EUPORIAS", dataset)) {
                  out <- loadSeasonalForecast.S4(dataset, gds, var, grid, dic, members, latLon, runTimePars, time, level, aggr.d, aggr.m, derInterface)
            } else if (grepl("CFSv2", dataset)) {
                  out <- loadSeasonalForecast.CFS(var, gds, grid, dic, latLon, runTimePars, time, level, aggr.d, aggr.m, derInterface)
            } else if (grepl("Glosea", dataset)) {
                  out <- loadSeasonalForecast.GS5(var, gds, grid, dic, latLon, runTimePars, time, level, aggr.d, aggr.m, derInterface)                                    
            }
            if (derInterface$deriveInterface != "none") {
                  out$Variable$varName <- derInterface$origVar
            }
      }
      gds$close()
      message("[", Sys.time(), "]", " Done")
      attr(out$xyCoords, "projection") <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
      # Dimension ordering
      x <- attr(out$Data, "dimensions")
      if (length(x) > 1) {
            tab <- c("member", "time", "level", "lat", "lon")
            b <- na.exclude(match(tab, x))
            dimNames <- attr(out$Data, "dimensions")[b]
            out$Data <- aperm(out$Data, perm = b)    
            attr(out$Data, "dimensions")  <- dimNames
      }
      # Source Dataset and other metadata 
      attr(out, "dataset") <- dataset
      attr(out, "source") <- "ECOMS User Data Gateway" 
      attr(out, "URL") <- "<http://meteo.unican.es/trac/wiki/udg/ecoms>"
      return(out)
}      
# End


