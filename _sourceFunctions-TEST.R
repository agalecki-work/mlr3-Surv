#3 source("_sourceFunctions-TEST.R")
library(mlr3)
library(mlr3data)
library(mlr3proba)
library(data.table)
library(survival)
library(dplyr)

rm(list=ls())
source("./R/_sourceFunctions.R")
lapply(FUNscriptNames, FUN= function(src) source(paste0( "./R/", src)))

source("calc_survconcordance_tibble-TEST.R")
source("createBackendData-TEST.R")
source("createTaskSurv-TEST.R")
source("onehotencodeTask-TEST.R")
source("task_cv.glmnet_add_extras-TEST.R")
source("truncate_survival_time-TEST.R")



