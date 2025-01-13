# Load required libraries
library(mlr3)           # Core 'mlr3' library for tasks and learners
library(mlr3proba)      # Extensions for survival analysis
library(mlr3learners)   # Collection of additional learners
library(mlr3pipelines)  # Tools for data preprocessing and pipeline construction

rm(list=ls())

# Create the 'rats' survival task
task = tsk("rats")
print(task)  

onehotencodeTask <- function(task){
  # Define the pipeline for one-hot encoding
  # This pipeline will convert factor variables into dummy variables (one-hot encoding)
  po_onehotencode = po("encode", method = "one-hot", affect_columns = selector_type("factor"))

  # Train the pipeline on the task data
  po_onehotencode$train(list(task))

  # Apply the trained pipeline to the task and obtain the transformed task with dummy variables
  task_tr1 = po_onehotencode$predict(list(task))[[1]]
  return(task_tr1)
}

task1 = onehotencodeTask(task)

print(task1)

# Display the first few rows of the transformed task
print(head(task1$data()))

