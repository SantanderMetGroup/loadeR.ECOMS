getRunTimeDomain.GS5 <- function(dataset, runDatesAll, validMonth, members, years) {
      if (grepl("Glosea5.*12$", dataset)) {
            csv <- "GS5_12.csv"
            if (!is.null(members)) {
                  if (length(members) > 12 | any(members) > 12 | any(members) < 1) {
                        stop("Invalid member definition")
                  }
            } else {
                  members <- 1:12
            }
      } else if (grepl("Glosea5.*24$", dataset)) {
            csv <- "GS5_24.csv"
            if (!is.null(members)) {
                  if (length(members) > 24 | any(members) > 24 | any(members) < 1) {
                        stop("Invalid member definition")
                  }
            } else {
                  members <- 1:24
            }
      }
      # Lookup reference table for member definition
      # mem.ref <- read.csv("inst/memdefs/GS5_12.csv")[members, ]
      mem.ref <- read.csv(file.path(find.package("ecomsUDG.Raccess"), "memdefs", csv))[members, ]
      # mem.ref <- mem.ref[order(mem.ref[ ,1]),]
      # Define Members
      memberRangeList <- lapply(as.integer(mem.ref$mem - 1), function(x) .jnew("ucar/ma2/Range", x, x))
      # Define runtimes
      rt.ref <- mem.ref$rt
      rt.ind <- which((runDatesAll$mon + 1) %in% c(validMonth-1, validMonth))
      # Valid month inits
      # runTimesValidMonth <- rt.axis$getCoordValues()[rt.ind]
      runTimesValidMonth <- (1:length(rt.axis$getCoordValues()))[rt.ind]
      runDatesValidMonth <- runDatesAll[rt.ind] 
      # Valid year inits
      rt.ind <- which((runDatesValidMonth$year + 1900) %in% years)
      runDatesValid <- runDatesValidMonth[rt.ind]
      runTimesValid <- runTimesValidMonth[rt.ind]
      aux.ind <- c(seq(1, length(runTimesValid), by = 3), length(runTimesValid) + 1)
      rt.list <- lapply(1:(length(aux.ind)-1), function(x) runTimesValid[aux.ind[x]:(aux.ind[x+1]-1)])
      runDatesList <- lapply(1:(length(aux.ind)-1), function(x) runDatesValid[aux.ind[x]:(aux.ind[x+1]-1)])
      runDatesValidMonth <- runTimesValid <- runDatesValid <- runDatesAll <- NULL
      runTimesEnsList <- lapply(1:length(rt.ref), function(x) lapply(rt.list, "[", rt.ref[x]))
      for (i in 1:length(runTimesEnsList)) {
            runTimesEnsList[[i]] <- lapply(1:length(runTimesEnsList[[i]]), function (j) {
                  rt <- as.integer(runTimesEnsList[[i]][[j]] - 1)
                  .jnew("ucar.ma2.Range", rt, rt)
            })
      }
      runDatesList <- lapply(runDatesList, "as.POSIXct", format = "%Y-%m-%d %H:%M:%S", tz = "GMT", usetz = TRUE)
      runDatesEnsList <- rep(list(runDatesList), length(rt.ref))
      runDatesEnsList <- lapply(1:length(rt.ref), function(x) lapply(runDatesEnsList[[x]], "[", rt.ref[x]))
      names(runTimesEnsList) <- names(runDatesEnsList) <- paste0("Member_", members)
      runDatesEnsList <- lapply(runDatesEnsList, function(x) do.call(c, x))
      return(list("memberRangeList" = memberRangeList, "runDates" = runDatesEnsList, "runTimeRanges" = runTimesEnsList))
}
