#source("../R/53cvglmnet_flow-extra_args.R")

library(mlr3)
library(mlr3proba)
library(mlr3extralearners)
library(mlr3pipelines)
library(paradox)
library(data.table)
rm(list=ls())
source("../R/traceit.R")
source("../R/create_cvglmnet_extra_args.R")

run_cvglmnet_with_partition <- function(traceon = TRUE) {
  task <- tsk("rats")
  
  # Configure task
  task$set_col_roles("sex", add_to = "stratum")
  task$filter(290:11)
  
  # Pipeline for encoding
  po_encode <- po("encode", method = "one-hot")
  encoded_task <- po_encode$train(list(task))[[1]]
  
  # Partition the task (use a seed for reproducibility)
  set.seed(123)
  partition_indices <- partition(encoded_task)

  # Obtain the training subset
  train_task <- encoded_task$filter(partition_indices$train)
  
  # Generate cross-validation arguments using only the training data
  cv_args <- create_cvglmnet_extra_args(train_task, traceon = traceon)
  
  # Modify penalty factors if needed
  penalty_factor <- cv_args$penalty.factor
  sex_vars <- grep("^sex\\.", names(penalty_factor), value = TRUE)
  if (length(sex_vars) > 0) {
    penalty_factor[sex_vars] <- 0.5
  }
  
  # Learner setup and model training
  learner <- lrn("surv.cv_glmnet", alpha = 0.8)
  learner$param_set$values <- list(
    foldid = cv_args$foldid,
    penalty.factor = penalty_factor
  )
  
  learner$train(train_task)
  traceit("Surv cv_glmnet model trained", learner, traceon = traceon)

  return(learner)
}

# Execute the process
learner <- run_cvglmnet_with_partition(traceon = TRUE)

