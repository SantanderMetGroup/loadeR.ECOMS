.onLoad <- function(libname, pkgname = "netcdfAll-4.3.jar") {
      .jpackage(pkgname, lib.loc = libname)
      options(java.parameters = "-Xmx2g")
      cat("ecomsUDG.Raccess version 2.0-0 (2014-06-16) is loaded")
}

