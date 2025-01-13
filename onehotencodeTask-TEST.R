# source("onehotencodeTask-TEST.R")

# Load required libraries
library(mlr3)           # Core 'mlr3' library for tasks and learners
library(mlr3proba)      # Extensions for survival analysis
library(mlr3learners)   # Collection of additional learners
library(mlr3pipelines)  # Tools for data preprocessing and pipeline construction

rm(list=ls())

source("./R/onehotencodeTask.R")
# Create the 'rats' survival task
task = tsk("rats")
print(task)  

task = onehotencodeTask(task)
print(task)
