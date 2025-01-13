# source("calc_survconcordance_tibble-TEST.R")
library(mlr3)
library(mlr3data)
library(mlr3proba)
library(data.table)
library(survival)
library(dplyr)
rm(list=ls())


message("=== 10TaskCreate-TEST.R  STARTS") 
outPath ="./out/"

# Load data for testing
load("./CRIC_prj/out/05CreateData.Rdata") # `CRIC_dt` `DataInfo` loaded

# Manually source necessary R functions

source("./R/traceit.R")

source("./R/createTaskSurv.R")
source ("./R/calc_survconcordance_tibble.R") # Script TESTED here


traceit("-- TEST1 : `createTaskSurv()`", NULL, TRUE)    

tmtx1     = DataInfo$tmtx1

task = createTaskSurv(2, tmtx1, "CRIC", CRIC_dt, feature_cols ="BMI")
print(task)



cox = lrn("surv.coxph")
part = partition(task)

prediction = cox$train(task, part$train)$predict(task,part$test)



#--- Execute
res = calc_survconcordance_tibble(prediction)
print(res)
