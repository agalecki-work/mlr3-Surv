# source("010-cv.glmnet-predict_risk_at_time.R")

# Load necessary libraries
rm(list=ls())


library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(mlr3extralearners) 
library(mlr3verse)
library(glmnet)
library(data.table)

# Load dataset
vet = as.data.table(survival::veteran)
veteran = within(vet, {
    celltyp_num = as.numeric(celltype)
    rm(celltype)
    }) # Factors are not supported 


#===== Create task
task = TaskSurv$new(id = "veteran", label= "Veteran", backend = veteran, time = "time", event =  "status")

# Define weights for the observations (example: give each instance equal weight)
weights = rep(1, task$nrow)
# Add weights to the task
task$cbind(data.table(weights = weights))

# Define the weights column role
task$set_col_roles("weights", roles = "weight")

# Perform a training/test split
part = partition(task)
print(sapply(part, length))


#=====  Define and train the learner
cvglmnetSurv = lrn("surv.cv_glmnet")

# set parameters cv.glmnet arguments not defined in task
pen =task$n_features
penalty_factor = rep(1, pen)  # Example: applying equal penalties to all predictors

# Learner params

cvglmnetSurv$param_set$values = list(
     alpha = 0.9,
     nfolds = 10,
     penalty.factor = penalty_factor
     )

# Train the learner on the task using train dats
cvglmnetSurv$train(task, row_ids= part$train)

# Get predictions for test data
predictions = cvglmnetSurv$predict(task, row_ids= part$test)

# survival predictions at selected time points
p =predictions$distr$survival(c(0, 1, 3, 5, 7, 10))
dim(p)
print(head(t(p)))
