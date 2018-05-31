 
deriveTotalPrecipitation <- function(gds, grid, dic, level, season, years, time, latLon, aggr.d, aggr.m) {
      timePars <- getTimeDomain(grid, dic, season, years, time, aggr.d, aggr.m)
      levelPars <- getVerticalLevelPars(grid, level)
      message("[", Sys.time(), "] Retrieving data subset ..." )
      # grid = prsn (snowfall flux)
      grid1 <- gds$findGridByName("Rainf") # grid1 = tp (in the case of WFDEI, this is rainfall flux)
      gcs <- grid$getCoordinateSystem()
      dimNames <- rev(names(scanVarDimensions(grid)))
      aux.list <- rep(list(bquote()), length(timePars$tRanges))
      do.aggr <- ifelse((timePars$aggr.d != "none") | (timePars$aggr.m != "none"), TRUE, FALSE)
      proj <- gcs$getProjection()
      for (i in 1:length(aux.list)) {
            dimNamesRef <- dimNames
            aux.list2 <- rep(list(bquote()), length(latLon$llRanges))
            for (j in 1:length(aux.list2)) {
                  subSet <- grid$makeSubset(levelPars$zRange, levelPars$zRange, timePars$tRanges[[i]], levelPars$zRange, latLon$llRanges[[j]]$get(0L), latLon$llRanges[[j]]$get(1L))
                  subSet1 <- grid1$makeSubset(levelPars$zRange, levelPars$zRange, timePars$tRanges[[i]], levelPars$zRange, latLon$llRanges[[j]]$get(0L), latLon$llRanges[[j]]$get(1L))
                  shapeArray <- rev(subSet$getShape()) # Reversed!!
                  # shape of the output depending on spatial selection
                  if (latLon$pointXYindex[1] >= 0) {
                        rm.dim <- grep(gcs$getXHorizAxis()$getDimensionsString(), dimNamesRef, fixed = TRUE)
                        shapeArray <- shapeArray[-rm.dim]
                        dimNamesRef <- dimNamesRef[-rm.dim]
                  }
                  if (latLon$pointXYindex[2] >= 0) {
                        rm.dim <- grep(gcs$getYHorizAxis()$getDimensionsString(), dimNamesRef, fixed = TRUE)
                        shapeArray <- shapeArray[-rm.dim]
                        dimNamesRef <- dimNamesRef[-rm.dim]
                  }
                  # Calculate total precipitation
                  snow <- array(subSet$readDataSlice(-1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray(), dim = shapeArray)
                  rain <- array(subSet1$readDataSlice(-1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray(), dim = shapeArray)
                  prec <- snow + rain
                  snow <- rain <- NULL
                  aux.list2[[j]] <- array(prec, dim = shapeArray)
                  prec <- NULL
            }
            aux.list[[i]] <- do.call("abind", c(aux.list2, along = 1))
            aux.list2 <- NULL
            # Daily aggregator 
            if (timePars$aggr.d != "none") {
                  aux.string <- paste(substr(timePars$dateSliceList[[i]],6,7), 
                                      substr(timePars$dateSliceList[[i]],9,10), sep = "-")
                  aux.factor <- factor(aux.string, levels = unique(aux.string), ordered = TRUE)
                  mar <- grep("^time", dimNamesRef, invert = TRUE)
                  aux.list[[i]] <- apply(aux.list[[i]], MARGIN = mar, FUN = function(x) {
                        tapply(x, INDEX = aux.factor, FUN = timePars$aggr.d, na.rm = TRUE)
                  })
                  dimNamesRef <- c("time", dimNamesRef[mar])
                  # Convert dates to daily:
                  nhours <- length(aux.factor) / nlevels(aux.factor)
                  timePars$dateSliceList[[i]] <- timePars$dateSliceList[[i]][seq(1, by = nhours, length.out = nlevels(aux.factor))]
            }
            # Monthly aggregator
            if (timePars$aggr.m != "none") {
                  mes <- as.numeric(substr(timePars$dateSliceList[[i]],6,7))
                  mes <- factor(mes, levels = unique(mes), ordered = TRUE)
                  day <- as.POSIXlt(timePars$dateSliceList[[i]])$mday
                  mar <- grep("^time", dimNamesRef, invert = TRUE)
                  aux.list[[i]] <- apply(aux.list[[i]], MARGIN = mar, FUN = function(x) {
                        tapply(x, INDEX = mes, FUN = timePars$aggr.m)
                  })
                  dimNamesRef <- if (length(unique(mes)) > 1) {
                        c("time", dimNamesRef[mar])
                  } else {
                        dimNamesRef[mar]
                  }
                  timePars$dateSliceList[[i]] <- timePars$dateSliceList[[i]][which(day == 1)]
            }
      }
      if (timePars$aggr.m != "none") {
            if (length(unique(mes)) > 1) {
                  mdArray <- do.call("abind", c(aux.list, along = grep("^time", dimNamesRef)))
            } else {
                  mdArray <- do.call("abind", c(aux.list, along = -1L))
                  dimNamesRef <- c("time", dimNamesRef)
            }
      } else {
            mdArray <- do.call("abind", c(aux.list, along = grep("^time", dimNamesRef)))
      }
      aux.list <- timePars$tRanges <- NULL
      if (any(dim(mdArray) == 1)) {
            dimNames <- dimNamesRef[-which(dim(mdArray) == 1)]    
            mdArray <- drop(mdArray)
      } else {
            dimNames <- dimNamesRef
      }
      mdArray <- unname(mdArray)
      attr(mdArray, "dimensions") <- dimNames
      timePars$dateSliceList <- as.POSIXct(do.call("c", timePars$dateSliceList), tz = "GMT")      
      # Next steps are needed to match the structure returned by loadeR::loadGridDataset
      cube <- list("timePars" = timePars, "mdArray" = mdArray)
      if (!is.null(dic)) {
            isStandard <- TRUE
            cube$mdArray <- dictionaryTransformGrid(dic, cube$timePars, cube$mdArray)
      } else {
            isStandard <- FALSE
      }
      if (isTRUE(latLon$revLat)) {
            cube$mdArray <- revArrayLatDim(cube$mdArray)
      }
      Variable <- list("varName" = "pr", "level" = levelPars$level)
      attr(Variable, "use_dictionary") <- isStandard
      attr(Variable, "description") <- "total precipitation amount (rain + snow)"
      if (isStandard) {
            vocabulary <- C4R.vocabulary()
            attr(Variable, "units") <- as.character(vocabulary[grep("^pr$", vocabulary$identifier), 3])
            attr(Variable, "longname") <- as.character(vocabulary[grep("^pr$", vocabulary$identifier), 2])
      } else {
            attr(Variable, "units") <- grid$getUnitsString()
            attr(Variable, "longname") <- grid$getFullName()
      }
      attr(Variable, "daily_agg_cellfun") <- cube$timePars$aggr.d
      attr(Variable, "monthly_agg_cellfun") <- cube$timePars$aggr.m
      attr(Variable, "verification_time") <- time
      out <- list("Variable" = Variable, "Data" = cube$mdArray, "xyCoords" = latLon$xyCoords, "Dates" = adjustDates(cube$timePars))
      return(out)
}
# End
