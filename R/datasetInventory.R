datasetInventory <- function(dataset = c("System4_seasonal_15", "System4_seasonal_51", "System4_annual_15", "CFSv2_seasonal_16")) {
      url <- dataURL(dataset)
      gds <- J("ucar.nc2.dt.grid.GridDataset")$open(url$URL)
      varNames <- javaString2rChar(gds$getGrids()$toString())
      if (!is.null(url$excludeVars)) {
            varNames <- varNames[-match(url$excludeVars, varNames)]
      }
      message("[",Sys.time(),"] The dataset contains ", length(varNames), " variables")
      var.list <- list()
      for (i in 1:length(varNames)) {
            message("[", Sys.time(), "] Retrieving info for \'", varNames[i], "\' (", length(varNames) - i, " vars remaining)")
            description <- gds$getDataVariable(varNames[i])$getDescription()
            varName <- gds$getDataVariable(varNames[i])$getShortName()
            dataType <- gds$getDataVariable(varNames[i])$getDataType()$toString()
            units <- gds$getDataVariable(varNames[i])$getUnitsString()
            grid <- gds$findGridByShortName(varName)
            dim.list <- scanVarDimensions(grid)
            var.list[[i]] <- list("Description" = description, "DataType" = dataType, "Units" = units, "Dimensions" = dim.list)
      }
      names(var.list) <- varNames
      gds$close()
      return(var.list)
}
# End