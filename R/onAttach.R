.onAttach <- function(...) {
      pkgname <- "ecomsUDG.Raccess"
      lib <- system.file(package = pkgname)
      ver <- packageVersion(pkgname)
      builddate <- packageDescription(pkgname)$Date
      mess <- paste(pkgname, " version ", ver, " (", builddate,") is loaded", sep = "")                        
      packageStartupMessage(mess)
      url <- "https://raw.githubusercontent.com/SantanderMetGroup/ecomsUDG.Raccess/stable/DESCRIPTION"
      a <- getURL(url)
      b <- readLines(textConnection(a))
      latest.ver <- package_version(gsub("Version: ", "", b[grep("Version", b)]))
      if (ver < latest.ver) {
            ver.mess1 <- "WARNING: Your current version of ecomsUDG.Raccess is not up-to-date"
            ver.mess <- paste("Get the latest stable version", latest.ver, "at http://meteo.unican.es/ecoms-udg/RPackage#Versions")      
            packageStartupMessage(ver.mess1)
            packageStartupMessage(ver.mess)
      }
} 
# End
