.onAttach <- function(...) {
      pkgname <- "loadeR.ECOMS"
      ver <- packageDescription(pkgname)$Version
      builddate <- packageDescription(pkgname)$Date
      mess <- paste(pkgname, " version ", ver, " (", builddate,") is loaded", sep = "")                        
      packageStartupMessage(mess)
      url <- "https://raw.githubusercontent.com/SantanderMetGroup/loadeR.ECOMS/master/DESCRIPTION"
      con <- tryCatch(getURL(url, ssl.verifypeer = FALSE), error = function(er) {
            er <- NULL
            return(er)
      })
      if (!is.null(con)) {
            b <- readLines(textConnection(con))
            latest.ver <- package_version(gsub("Version: ", "", b[grep("Version", b)]))
            if (ver < latest.ver) {
                  ver.mess1 <- "WARNING: Your current version of loadeR.ECOMS is not up-to-date"
                  ver.mess <- paste("Get the latest stable version", latest.ver, "typing on your R console:\ndevtools::install_github(\"SantanderMetGroup/loadeR.ECOMS\")")      
                  packageStartupMessage(ver.mess1)
                  packageStartupMessage(ver.mess)
            }
      }
} 
# End

