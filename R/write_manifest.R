
# Niet uitvoeren op Posit Connect
if ( !stringr::str_detect(Sys.getenv("QUARTO_PROFILE"), "connect") ) {
  rsconnect::writeManifest()
  }
