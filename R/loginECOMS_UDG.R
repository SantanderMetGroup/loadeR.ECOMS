loginECOMS_UDG <- function(username, password, proxy.host = NULL, proxy.port = NULL) {
      proxy.port <- as.integer(proxy.port)
      if (!is.character(username) | !is.character(password)) {
            stop("\'username\' and \'password\' must be character strings")
      }
      if (!is.null(proxy.host)) {
            J("ucar.nc2.util.net.HTTPSession")$setGlobalProxy(proxy.host, proxy.port)
      }
      J("ucar.httpservices.MyHTTPFactory")$setCredentials(username, password)
}
# End
