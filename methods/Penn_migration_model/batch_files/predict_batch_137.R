
library(tidyverse)
library(SDMtune) # Needs version 1.1.6 to run: https://cran.r-project.org/src/contrib/Archive/SDMtune/SDMtune_1.1.6.tar.gz
library(raster)
library(sp)
library(sf)
library(randomForest)

print('reading models')

model_1 <- readRDS('/home/lberigan/SDM_mig/model_migratory_1.rds')
model_2 <- readRDS('/home/lberigan/SDM_mig/model_migratory_2.rds')
model_3 <- readRDS('/home/lberigan/SDM_mig/model_migratory_3.rds')
model_4 <- readRDS('/home/lberigan/SDM_mig/model_migratory_4.rds')
model_5 <- readRDS('/home/lberigan/SDM_mig/model_migratory_5.rds')

print('reading raster')

predictor_stack_iter <- brick('/home/lberigan/SDM_mig/predictor_stack_3/predictor_stack_3_30m_137.grd')
predictor_stack_iter <- stack(predictor_stack_iter)

print('predicting variables')

f <- list(sclass = c(1,2,3,4,5,6,7,111,120,132,180))

pred_1 <- predict(model_1, data = predictor_stack_iter, factors = f)
pred_2 <- predict(model_2, data = predictor_stack_iter, factors = f)
pred_3 <- predict(model_3, data = predictor_stack_iter, factors = f)
pred_4 <- predict(model_4, data = predictor_stack_iter, factors = f)
pred_5 <- predict(model_5, data = predictor_stack_iter, factors = f)

print('stacking')

pred_stack <- stack(pred_1, pred_2, pred_3, pred_4, pred_5)

print('averaging')

calc(pred_stack, fun = mean, filename = '/home/lberigan/SDM_mig/predict_output/predict_137.tif')

print('done')
             