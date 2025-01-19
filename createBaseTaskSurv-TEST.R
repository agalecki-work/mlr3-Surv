# source("createBaseTaskSurv-TEST.R")
library(mlr3)
library(mlr3data)
library(mlr3proba)
library(data.table)
library(survival)
library(dplyr)

rm(list=ls())

message("=== createBaseTaskSurv-TEST.R STARTS") 

# Load data for testing
load("./Rdata/05CreateData.Rdata") # `CRIC_dt` `DataInfo` loaded

# Manually source necessary R functions

# Manually source necessary R functions

source("./R/traceit.R")
source("./R/apply_time_horizon.R")
source("./R/createBaseTaskSurv.R")

tinfo_mtx = DataInfo$targets_info_mtx
target_info= tinfo_mtx[1,]#-----------
formals(createBaseTaskSurv)

base_task1 = createBaseTaskSurv(target_info, "CRIC", CRIC_dt)
print(base_task1)

base_task2 = createBaseTaskSurv(target_info, "CRIC", CRIC_dt, time_horizon =5)
print(base_task2)
