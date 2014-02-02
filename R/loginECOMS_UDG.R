loginECOMS_UDG <- function(username, password, proxy.host = NULL, proxy.port = NULL) {
      proxy.port <- as.integer(proxy.port)
      if (!is.character(username) | !is.character(password)) {
            stop("\'username\' and \'password\' must be character strings")
      }
      if (!is.null(proxy.host)) {
            J("ucar.nc2.util.net.HTTPSession")$setGlobalProxy(proxy.host, proxy.port)
      }
      aux <- .jnew("ucar/nc2/util/net/HTTPBasicProvider", username, password)
      J("ucar.nc2.util.net.HTTPSession")$setGlobalCredentialsProvider(aux)
}
# End
