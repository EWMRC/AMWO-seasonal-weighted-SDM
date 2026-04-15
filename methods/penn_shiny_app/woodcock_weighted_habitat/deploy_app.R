library(here)
source(here("methods", "penn_shiny_app", "woodcock_weighted_habitat", "write_shiny_credentials.R"))
rsconnect::deployApp(appDir = here("methods", "penn_shiny_app", "woodcock_weighted_habitat"),
                     appName = "W-PAST2", forceUpdate = TRUE)
