library(tidyverse)
library(here)
library(parallel)
library(furrr)
library(SDMtune) 
library(terra)
library(sp)
library(sf)
library(randomForest)

nCores <- parallelly::availableCores()
plan(multisession, workers = nCores)

model_1 <- readRDS('/home/aublab001/Penn_residential_model/model_residential_1.rds')
model_2 <- readRDS('/home/aublab001/Penn_residential_model/model_residential_2.rds')
model_3 <- readRDS('/home/aublab001/Penn_residential_model/model_residential_3.rds')
model_4 <- readRDS('/home/aublab001/Penn_residential_model/model_residential_4.rds')
model_5 <- readRDS('/home/aublab001/Penn_residential_model/model_residential_5.rds')

hpc_predict <- function(index){
  library(SDMtune) 
  library(randomForest)
  
  predictor_stack_iter <- rast(paste0("/home/aublab001/Penn_residential_model/predictor_stack/predictor_stack_30m_", index,".grd"))

  pred_1 <- predict(model_1, data = predictor_stack_iter)
  pred_2 <- predict(model_2, data = predictor_stack_iter)
  pred_3 <- predict(model_3, data = predictor_stack_iter)
  pred_4 <- predict(model_4, data = predictor_stack_iter)
  pred_5 <- predict(model_5, data = predictor_stack_iter)
  
  pred_stack <- c(pred_1, pred_2, pred_3, pred_4, pred_5)
  
  app(pred_stack, fun = mean, filename = paste0("/home/aublab001/Penn_residential_model/predict_output/predict_", index,".tif"))
}

print("beginning parallel computation")

future_walk(1:305, hpc_predict)

print("done")
