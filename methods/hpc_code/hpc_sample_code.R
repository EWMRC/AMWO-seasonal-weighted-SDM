# install.packages("SDMtune")
log_file <- "/test.log"

# Start logging
sink(log_file, append = TRUE)          # capture console output
sink(log_file, append = TRUE, type = "message")  # capture warnings/messages

cat("Script started:", Sys.time(), "\n")

install.packages("here")

library(tidyverse)
library(here) #"/home/aublab001"

# files_to_run <- here("Penn_residential_model", "batch_files") %>%
#   list.files()

print("log me")

cat("Script finished:", Sys.time(), "\n")

# Stop logging
sink(type = "message")
sink()



