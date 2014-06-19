.onAttach <- function(...) {
      pkgname <- "ecomsUDG.Raccess"
      lib <- system.file(package = pkgname)
      ver <- packageVersion("ecomsUDG.Raccess")
      builddate <- packageDescription("ecomsUDG.Raccess")$Date
      mess <- paste(pkgname, " version ", ver, " (", builddate,") is loaded", sep = "")                        
      packageStartupMessage(mess)
#       jar <- "netcdfAll-4.3.jar"
#       if (!file.exists(file.path(lib, "java", jar))) {
#             packageStartupMessage("NetCDF-Java not available. Attempting download...")
#             destdir <- file.path(lib, "java")
#             dir.create(path = destdir)
#             download.file("ftp://ftp.unidata.ucar.edu/pub/netcdf-java/v4.3/netcdfAll-4.3.jar", destfile = file.path(destdir, jar))
#       }
} 
# End
