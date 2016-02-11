loginECOMS_UDG <- function(username, password, proxy.host = NULL, proxy.port = NULL) {
      .Deprecated("loginUDG", package = "loadeR", msg = "'loginECOMS_UDG' is deprecated.\nUse 'loginUDG' from package loadeR instead.")      
      loginUDG(username, password, proxy.host, proxy.port)
}

