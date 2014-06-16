.onAttach <- function(...) {
      mylib <- dirname(system.file(package = "ecomsUDG.Raccess"))
      ver <- packageVersion("ecomsUDG.Raccess")
      builddate <- packageDescription("ecomsUDG.Raccess")$Date
      mess <- paste("ecomsUDG.Raccess version ", ver ," (", builddate,") is loaded", sep = "")                        
      packageStartupMessage(mess)
} 
# End
