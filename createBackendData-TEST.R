# source("createBackendData-TEST.R")
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
source("./R/createBackendData.R") # TESTED 

dt = createBackendData(CRIC_dt)
print(dim(dt))
print(colnames(dt))
