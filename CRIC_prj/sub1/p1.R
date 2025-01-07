# Load required libraries
library(mlr3)           # Core 'mlr3' library for tasks and learners
library(mlr3proba)      # Extensions for survival analysis
library(mlr3learners)   # Collection of additional learners
library(mlr3extralearners)   # Collection of additional learners
library(mlr3pipelines)  # Tools for data preprocessing and pipeline construction

rm(list=ls())

# Load tasks
loaded_objects =load("./out/11tm_lists.Rdata")
print(loaded_objects)
names(tm_lists)

# Select task
task_init = tm_lists$tm_test$tm01

# Define the pipeline for one-hot encoding
# This pipeline will convert factor variables into dummy variables (one-hot encoding)
po_onehotencode = po("encode", method = "one-hot", affect_columns = selector_type("factor"))

# Train the pipeline on the task data
po_onehotencode$train(list(task_init))

# Apply the trained pipeline to the task and obtain the transformed task with dummy variables

task = po_onehotencode$predict(list(task_init))[[1]]
print(task)

# Perform a training/test split
part = partition(task)  # Train/test split balanced by status
print(sapply(part, length))

# Define the learner - Cox Proportional Hazards Model

lrn_coxph = lrn("surv.coxph")

# Train the learner using the training subset of the data
lrn_coxph$train(task, row_ids = part$train)

# Make predictions on the test subset
prediction = lrn_coxph$predict(task, row_ids = part$test)

# survival predictions on test data
p= prediction$distr$survival(c(0, 1, 3, 5, 7)) 
dim(p)
head(t(p))

# Model
lrn_coxph$model


