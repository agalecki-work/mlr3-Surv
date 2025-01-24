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

source("./R/traceit.R")
source("./R/apply_time_horizon.R")
source("./R/createTaskSurv.R")

tinfo_mtx = DataInfo$targets_info_mtx

#-----------

cat("===== TEST1 \n")

target_info= tinfo_mtx[1,]
print(target_info)

backendSRS_BMI = list(
       option = "SRS",
       id     = "CRIC",
       primary_key  = "SSID_num",
       feature_cols =c("BMI", "SEXf"),
       time_horizon = 5,
       filter = "BMI<35",
       CCH_subcohort = "CHF", #
       weight = c(SRS= "wt1", CCH = "wt2", CCH1 = NA), # tentative, weight extracted depending on option value
       add_to_strata_cols = "SEXf"
)



task1 = createTaskSurv(CRIC_dt, target_info, backendSRS_BMI, traceon=TRUE)

# Retrieve and print the entire backend data. Use task$row_ids directly.
task1_data <- task1$data()

backend1_data <- task1$backend$data(
  rows = task1$row_ids,  # Directly use the ID column
  cols = task1$backend$colnames
)
print(task1)
print(task1_data)
# print(backend1_data)

#========== TEST2
cat("======== TEST2 : `createTaskSurv()` feature_cols \n ")

backend_info2 = backendSRS_BMI
backend_info2$feature_cols  = "BMI" 
task2 = createTaskSurv(CRIC_dt, target_info, backend_info2, traceon=TRUE)
print(task2)


#======= TEST3

cat("====== TEST3 : feature_cols and filter arguments \n")
backend_info3 = backendSRS_BMI
backend_info3$feature_cols  = "BMI" 
backend_info3$filter  = "BMI <25" 


task3 = createTaskSurv(CRIC_dt,target_info,backend_info3, traceon=FALSE)
#print(task3)


#====== TEST4


traceit("-- TEST4:  add_to_strata_cols argument ", NULL, TRUE) 
cat("======== TEST4 : feature_cols and filter arguments \n")
backend_info4 = backendSRS_BMI
backend_info4$feature_cols  = "BMI" 
backend_info4$add_to_strata_cols  = "RACE_CAT_1" 

task4 = createTaskSurv(CRIC_dt, target_info, backend_info4, traceon = FALSE)
#print(task4)
#print(task4$strata)
# Retrieve and print the entire backend data. Use task$row_ids directly.
task4_data <- task4$data()

backend4_data <- task4$backend$data(
  rows = task1$row_ids,  # Directly use the ID column
  cols = task1$backend$colnames
)


#print(task4)
#print(task4_data)
#print(backend4_data)


