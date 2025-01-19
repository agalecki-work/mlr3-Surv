# source("createTaskSurv-TEST.R")
library(mlr3)
library(mlr3data)
library(mlr3proba)
library(data.table)
library(survival)
library(dplyr)

rm(list=ls())

message("=== createTaskSurv-TEST.R  STARTS") 


# Load data for testing
load("./Rdata/05CreateData.Rdata") # `CRIC_dt` `DataInfo` loaded

# Manually source necessary R functions

# Manually source necessary R functions

source("./R/traceit.R")
source("./R/apply_time_horizon.R")
source("./R/createTaskSurv.R")

tinfo_mtx = DataInfo$targets_info_mtx

#-----------


#cols =  c("PID", "TIME_LOSS50", "TIME_DEATH", "DEADx", "LOSS50", target_info[c("time", "event")])

#dtx = CRIC_dt[,cols]
#colnames(dtx)



traceit("-- TEST1 : subset_col is 'BMI30_idx'", NULL, TRUE)    

target_info= tinfo_mtx[6,]
print(target_info)


task1 = createTaskSurv(target_info, "CRIC", CRIC_dt, subset_col = "BMI30_idx")
print(task1)

traceit("-- TEST2 : `createTaskSurv()` feature_cols argument ", NULL, TRUE)


task2 = createTaskSurv(target_info, "CRIC", CRIC_dt,feature_cols  = "BMI")
print(task2)

traceit("-- TEST3 : feature_cols and subset_col arguments ", NULL, TRUE)

task3 = createTaskSurv(target_info,"CRIC", CRIC_dt, subset_col = "BMI30_idx", feature_cols ="BMI")
print(task3)


traceit("-- TEST4:  add_to_strata_cols argument ", NULL, TRUE) 

task4 = createTaskSurv(target_info,"CRIC", CRIC_dt,  add_to_strata_cols = "CHF", feature_cols ="BMI")
print(task4)
print(task4$strata)



