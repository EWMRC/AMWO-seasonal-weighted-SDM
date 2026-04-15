library(here)
source(here("methods", "penn_shiny_app", "prerender_PA_app", "write_shiny_credentials.R"))
rsconnect::deployApp(appDir = here("methods", "penn_shiny_app", "prerender_PA_app"),
                     appName = "prerender_PA_app", forceUpdate = TRUE)
