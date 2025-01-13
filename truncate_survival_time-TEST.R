# source("truncate_survival_time-TEST.R")
library(mlr3)
library(mlr3data)
library(mlr3proba)
library(data.table)
library(survival)
library(dplyr)

rm(list=ls())


# Load data for testing
load("./CRIC_prj/out/05CreateData.Rdata") # `CRIC_dt` `DataInfo` loaded

# Manually source necessary R functions

source("./R/traceit.R")
source("./R/truncate_survival_time.R")
source("./R/createTaskSurv.R")

traceit("-- task1 ------", NULL, TRUE)    
tmtx1 =DataInfo$tmtx1
task = createTaskSurv(2, tmtx1, "CRIC", CRIC_dt, feature_cols ="BMI")
print(task)


#-----------

task = truncate_survival_time(task, time_horizon= 5)
print(range(task$data(cols=task$target_names["time"])))
