# source("calc_survconcordance_tibble-TEST.R")
library(mlr3)
library(mlr3data)
library(mlr3proba)
library(data.table)
library(survival)
library(dplyr)
rm(list=ls())


message("=== calc_survconcordance_tibble-TEST.R  STARTS") 


# Load data for testing
load("./Rdata/05CreateData.Rdata") # `CRIC_dt` `DataInfo` loaded

# Manually source necessary R functions

source("./R/traceit.R")
source("./R/apply_time_horizon.R")
source("./R/createTaskSurv.R")
source ("./R/calc_survconcordance_tibble.R") # Script TESTED here


traceit("-- TEST1 : `createTaskSurv()`", NULL, TRUE)    

tinfo_mtx = DataInfo$targets_info_mtx

#-----------

target_info= tinfo_mtx[3,]
print(target_info)
task = createTaskSurv(target_info, "CRIC", CRIC_dt, feature_cols ="BMI")
print(task)



cox = lrn("surv.coxph")
part = partition(task)

prediction = cox$train(task, part$train)$predict(task,part$test)



#--- Execute
res = calc_survconcordance_tibble(prediction)
print(res)
