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
load("./CRIC_prj/out/05CreateData.Rdata") # `CRIC_dt` `DataInfo` loaded

# Manually source necessary R functions

source("./R/traceit.R")

source("./R/createTaskSurv.R") # TESTED



traceit("-- TEST1 : subset_col is 'BMI30_idx'", NULL, TRUE)    


tmtx1     = DataInfo$tmtx1
task1 = createTaskSurv(2, tmtx1, "CRIC", CRIC_dt, subset_col = "BMI30_idx")
print(task1)

traceit("-- TEST2 : `createTaskSurv()` feature_cols argument ", NULL, TRUE)

tmtx2     = DataInfo$tmtx2
task2 = createTaskSurv(2, tmtx2, "CRIC", CRIC_dt,feature_cols  = "BMI")
print(task2)

traceit("-- TEST3 : feature_cols and subset_col arguments ", NULL, TRUE)

task3 = createTaskSurv(2, tmtx2,"CRIC", CRIC_dt, subset_col = "BMI30_idx", feature_cols ="BMI")
print(task3)


traceit("-- TEST4:  add_to_strata_cols argument ", NULL, TRUE) 

task4 = createTaskSurv(3, tmtx2,"CRIC", CRIC_dt,  add_to_strata_cols = "CHF", feature_cols ="BMI")
print(task4)
print(task4$strata)



