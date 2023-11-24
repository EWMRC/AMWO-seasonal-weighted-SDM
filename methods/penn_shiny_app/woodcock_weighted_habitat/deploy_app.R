source("write_shiny_credentials.R")
rsconnect::deployApp(appName = "W-PAST", forceUpdate = TRUE)
