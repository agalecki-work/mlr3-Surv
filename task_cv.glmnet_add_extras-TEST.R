# source("task_cv.glmnet_add_extras-TEST.R")

library(mlr3)
library(mlr3data)
library(mlr3proba)
library(data.table)
library(survival)
library(dplyr)

rm(list=ls())

message("=== task_cv.glmnet_add_extras-TEST  STARTS") 

# Load data for testing
load("./Rdata/05CreateData.Rdata") # `CRIC_dt` `DataInfo` loaded

# Manually source necessary R functions

source("./R/traceit.R")
source("./R/apply_time_horizon.R")
source("./R/createTaskSurv.R")

traceit("-- task1 ------", NULL, TRUE)    
tinfo_mtx = DataInfo$targets_info_mtx
target_info= tinfo_mtx[6,]
print(target_info)
task = createTaskSurv(target_info, "CRIC", CRIC_dt)
print(task)

source("./R/task_cv.glmnet_add_extras.R") # TESTED script


# Example usage

task1 <- task_cv.glmnet_add_extras(task = task, nfolds = 5, seed = 123)
extra_args = task1$extra_args
print(names(extra_args))

