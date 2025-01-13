# source("cv.glmnet_stratified_folds-TEST.R")
library(mlr3)
library(mlr3data)
library(mlr3proba)
library(data.table)
library(survival)
library(dplyr)

rm(list=ls())

message("=== cv.glmnet_stratified_folds-TEST.R  STARTS") 
outPath ="./out/"

# Load data for testing
load("./CRIC_prj/out/05CreateData.Rdata") # `CRIC_dt` `DataInfo` loaded

# Manually source necessary R functions

source("./R/traceit.R")
source("./R/createTaskSurv.R")

traceit("-- task1 ------", NULL, TRUE)    
tmtx1 =DataInfo$tmtx1
task1 = createTaskSurv(2, tmtx1, "CRIC", CRIC_dt)
print(task1)

source("./R/cv.glmnet_stratified_folds.R") # TESTED script


# Example usage

foldid <- cv.glmnet_stratified_folds(task = task1, nfolds = 5, seed = 123)
print(head(foldid))
