# Load required libraries
library(mlr3)           # Core 'mlr3' library for tasks and learners
library(mlr3proba)      # Extensions for survival analysis
library(mlr3learners)   # Collection of additional learners
library(mlr3pipelines)  # Tools for data preprocessing and pipeline construction

rm(list=ls())

# Create the 'rats' survival task
task = tsk("rats")
print(task)  

# Define the pipeline for one-hot encoding
# This pipeline will convert factor variables into dummy variables (one-hot encoding)
po_onehotencode = po("encode", method = "one-hot", affect_columns = selector_type("factor"))

# Train the pipeline on the task data
po_onehotencode$train(list(task))

# Apply the trained pipeline to the task and obtain the transformed task with dummy variables

task_tr1 = po_onehotencode$predict(list(task))[[1]]
print(task_tr1)

# Display the first few rows of the transformed task
print(head(task_tr1$data()))

# Define the learner - Cox Proportional Hazards Model
learner = lrn("surv.coxph")

# Perform a training/test split, stratified on `status` by default
part = partition(task_tr1)  # Train/test split balanced by status

# Train the learner using the training subset of the data
learner$train(task_tr1, row_ids = part$train)

# Make predictions on the test subset
prediction = learner$predict(task_tr1, row_ids = part$test)

