datasetInventory <- function(dataset = c("System4_seasonal_15", "System4_seasonal_51", "System4_annual_15", "CFSv2_seasonal_16")) {
      dataset <- match.arg(dataset)
      if (dataset == "System4_seasonal_15") {
            data.url <- "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Seasonal_15Members.ncml"
      }
      if (dataset == "System4_seasonal_51") {
            data.url <- "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Seasonal_51Members.ncml"
      }
      if (dataset == "System4_annual_15") {
            data.url <- "http://www.meteo.unican.es/tds5/dodsC/system4/System4_Annual_15Members.ncml"
      }
      if (dataset == "CFSv2_seasonal_16") {
            data.url <- "http://www.meteo.unican.es/tds5/dodsC/cfs/agg/cfsAgg_fmrc.ncd"
      }
      gds <- J("ucar.nc2.dt.grid.GridDataset")$open(data.url)
      varNames <- unlist(strsplit(gsub("\\[|]|\\s", "", gds$getGrids()$toString()), ","))
      var.list <- list()
      for (i in 1:length(varNames)) {
            dataVar <- gds$getDataVariable(varNames[i])
            description <- dataVar$getDescription()
            varName <- dataVar$getShortName()
            dataType <- dataVar$getDataType()$toString()
            dimensions <- unlist(strsplit(dataVar$getDimensionsString(), "\\s"))
            units <- dataVar$getUnitsString()
            grid <- gds$findGridByName(varName)
            gridLevels <- as.numeric(unlist(strsplit(gsub("\\[|]|\\s","", grid$getLevels()$toString()), split=",")))
            levelDimIndex <- grep("^lev|^height", dimensions)
            if (length(gridLevels) > 0) {
                  v <- c()
                  for (k in 1:length(gridLevels) - 1) {
                        start <- rep(1, length(dimensions))
                        stride <- start
                        start[levelDimIndex] <- k
                        res <- tryCatch({ 
                              grid$getVariable()$read(as.integer(start), as.integer(stride))$getStorage()
                              }, error = function(er) { 
                                    err <- NA
                                    return(err)
                              })
                        v <- c(v, res)
                  }
                  noLevelInd <- which(is.na(v))
            }     
            gcs <- grid$getCoordinateSystem()
            dim.list <- list()
            for (j in 1:length(dimensions)) {
                  if(dimensions[j] == "member") {
                        axis <- gcs$getEnsembleAxis()
                  }
                  if(grepl("^run", dimensions[j])) {
                        axis <- gcs$getRunTimeAxis()
                  }
                  if (grepl("^time", dimensions[j])) {
                        axis <- gcs$getTimeAxis()
                  }
                  if(grepl("^lat", dimensions[j])) {
                        axis <- gcs$getLatAxis()
                  }
                  if(grepl("^lon", dimensions[j])) {
                        axis <- gcs$getLonAxis()
                  }
                  if(grepl("^lev|zeta|^height", dimensions[j])) {
                        axis <- gcs$getVerticalAxis()
                  }
                  axisType <- axis$getAxisType()$toString()
                  lonAxisShape <- gcs$getLonAxis()$getShape()
                  if (!grepl("^time|run|^lev", dimensions[j])) {
                        values <- axis$getCoordValues()
                        if (grepl("^lon", dimensions[j])) {
                              values[which(values > 180)] <- values[which(values > 180)] - 360
                              if (length(lonAxisShape) == 2) {
                                    values <- apply(t(matrix(values, ncol = lonAxisShape[1])), 2, min)
                              }
                        }
                        if (grepl("^lat", dimensions[j])) {
                              latAxisShape <- gcs$getLatAxis()$getShape()
                              if (length(latAxisShape) == 2) {
                                    values <- apply(t(matrix(values, ncol = lonAxisShape[1])), 1, min)
                              }
                        }
                  }
                  if(grepl("^lev|^height", dimensions[j])) {
                        if (length(noLevelInd) > 0) {
                              values <- axis$getCoordValues()[-noLevelInd]
                        } else {
                              values <- axis$getCoordValues()
                        }
                  }
                  if (grepl("^time", dimensions[j])) {
                        charDates <- tryCatch({unlist(strsplit(gsub("\\[|]|\\s", "", gcs$getTimes()$toString()), split = ","))},
                                              error = function(err) {
                                                    rt.ax <- gcs$getRunTimeAxis()
                                                    er <- rep("0000-00-00T00:00:00Z", 2)
                                                      return(er)
                                              })
                        values <- strptime(charDates, format = "%Y-%m-%dT%H:%M:%SZ")
                        time.agg <- difftime(values[2], values[1], units = "hours")
                        values <- paste(range(values), collapse = " to ")
                  }
                  if(grepl("^run", dimensions[j])) {
                        values <- strptime(gsub("\\[|]|\\s", "", unlist(strsplit(axis$getCalendarDates()$toString(), ","))), format = "%Y-%m-%dT%H:%M:%SZ")
                  }
                  dim.list[[j]] <- list("Type" = axisType, "Units" = axis$getUnitsString(), "Values" = values)
            }
            names(dim.list) <- dimensions
            dimShape <- rep(NA, length(dim.list))
            for (h in 1:length(dim.list)) {
                  dimShape[h] <- length(dim.list[[h]]$Values)
            }
            var.list[[i]] <- list("Description" = description, "DataType" = dataType, "Units" = units, "TimeStep" = time.agg, "Dimensions" = dim.list)
      }
      names(var.list) <- varNames
      gds$close()
      return(var.list)
}
# End