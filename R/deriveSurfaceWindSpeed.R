
deriveSurfaceWindSpeed.S4 <- function(gds, grid, latLon, runTimePars, memberRangeList, foreTimePars) {
      message("[", Sys.time(), "] Retrieving data subset ..." )
      # grid = uas
      grid1 <- gds$findGridByName("v10m") # grid1 = vas
      gcs <- grid$getCoordinateSystem()
      dimNames <- rev(names(scanVarDimensions(grid))) # reversed!
      z <- .jnew("ucar/ma2/Range", 0L, 0L)
      aux.list <- rep(list(bquote()), length(memberRangeList))
      for (i in 1:length(memberRangeList)) {
            ens <- memberRangeList[[i]]
            aux.list1 <- rep(list(bquote()), length(runTimePars$runTimeRanges))
            for (j in 1:length(runTimePars$runTimeRanges)) {
                  rt <- runTimePars$runTimeRanges[[j]]
                  ft <- foreTimePars$ForeTimeRangesList[[j]]
                  aux.list2 <- rep(list(bquote()), length(latLon$llRanges))
                        for (k in 1:length(latLon$llRanges)) {
                        subSet <- grid$makeSubset(rt, ens, ft, z, latLon$llRanges[[k]]$get(0L), latLon$llRanges[[k]]$get(1L))
                        subSet1 <- grid1$makeSubset(rt, ens, ft, z, latLon$llRanges[[k]]$get(0L), latLon$llRanges[[k]]$get(1L))
                        shapeArray <- rev(subSet$getShape())
                        dimNamesRef <- dimNames              
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
                        # Computation of derived wss from uas and vas
                        uas <- subSet$readDataSlice(-1L, -1L, -1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray()
                        vas <- subSet1$readDataSlice(-1L, -1L, -1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray()
                        wss <- sqrt(uas^2 + vas^2)
                        uas <- NULL
                        vas <- NULL
                        aux.list2[[k]] <- array(wss, dim = shapeArray)
                  }
                  # Sub-routine for daily aggregation from 6h data
                  if (!is.na(foreTimePars$dailyAggr)) {
                        aux.list1[[j]] <- toDD(do.call("abind", c(aux.list2, along = 1)), dimNamesRef, foreTimePars$dailyAggr)
                        dimNamesRef <- attr(aux.list1[[j]], "dimensions")
                  } else {
                        aux.list1[[j]] <- do.call("abind", c(aux.list2, along = 1))
                  }
                  aux.list2 <- NULL
            }
            aux.list[[i]] <- do.call("abind", c(aux.list1, along = grep("^time", dimNamesRef)))
            aux.list1 <- NULL
      }
      mdArray <- do.call("abind", c(aux.list, along = grep(gcs$getEnsembleAxis()$getDimensionsString(), dimNamesRef, fixed = TRUE)))
      aux.list <- NULL
      if (any(dim(mdArray) == 1)) {
            dimNames <- dimNamesRef[-which(dim(mdArray) == 1)]
            mdArray <- drop(mdArray)
      } else {
            dimNames <- dimNamesRef
      }
      dimNames <- gsub("^time.*", "time", dimNames)
      mdArray <- unname(mdArray)
      attr(mdArray, "dimensions") <- dimNames
      return(mdArray)
}
# End

