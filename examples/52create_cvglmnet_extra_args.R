#source("../R/52create_cvglmnet_extra_args.R")

library(mlr3)
library(mlr3proba)
library(mlr3extralearners)
library(mlr3pipelines)
library(paradox)
library(data.table)
library(dplyr)
traceit <- function(msg, variable = NULL, traceon = FALSE) {
  if (isTRUE(traceon)) {
    cat(msg, ":\n")
    if (!is.null(variable)) {
      print(str(variable))
    }
  }
}

create_cvglmnet_extra_args = function(task, nfolds = 5, seed = 123, traceon = FALSE) {
  traceit(sprintf("--- create_cvglmnet_extra_args() STARTS. nfolds: %d", nfolds), traceon = traceon)
  
  # Inspect the types of the task's features
  feature_types = task$feature_types
  # Define a vector of disallowed types for cv.glmnet
  disallowed_types = c("factor", "ordered", "character")
  
  # Check for disallowed feature types and gather feature names
  fnms = feature_types[feature_types$type %in% disallowed_types, "id", drop = FALSE]
  
  # If there are any disallowed features, stop and report them
  if (nrow(fnms) > 0) {
    stop("The task contains features with types not compatible with cv.glmnet: ",
         paste(fnms$id, collapse = ", "))
  }
  
  if (is.null(nfolds) || length(nfolds) == 0) {
    stop("'nfolds' parameter is not set or is invalid")
  }
  
  set.seed(seed)
  n = task$nrow
  foldid = integer(n)
  
  # By default penalty_factor is a vector of ones
  penalty_factor = rep(1, task$n_features)
  names(penalty_factor) = task$feature_names
  
  if ("stratum" %in% names(task$col_roles) && !is.null(task$strata)) {
    strata = task$strata
    filtered_indices = task$row_ids
    
    for (i in seq_len(nrow(strata))) {
      original_indices = unlist(strata$row_id[i])
      indices = which(filtered_indices %in% original_indices)
      fold_sizes = sample(rep(1:nfolds, length.out = length(indices)))
      foldid[indices] = fold_sizes
    }
  } else {
    foldid = sample(rep(1:nfolds, length.out = n))
  }
  
  res = list(foldid = foldid, penalty.factor = penalty_factor)
  traceit("--- create_cvglmnet_extra_args() ENDS with result", res, traceon = traceon)
  
  return(res)
}


run_cvglmnet_process <- function(traceon = TRUE) {
  # Log task creation and filtering
  task <- tsk("rats")
  traceit("Task after creation", task, traceon = traceon)
  
  task$set_col_roles("sex", add_to = "stratum")
  traceit("Task after setting col roles", task, traceon = traceon)

  task$filter(290:11)
  traceit("Task after filtering", task, traceon = traceon)
  traceit("Task's strata", task$strata, traceon = traceon)

  # One-hot encoding
  po_encode <- po("encode", method = "one-hot")
  
  # Log encoding transformation
  encoded_task <- po_encode$train(list(task))[[1]]
  traceit("Encoded Task", encoded_task, traceon = traceon)

  # Run function with encoded task
  cv_args <- create_cvglmnet_extra_args(encoded_task, traceon = traceon)

  # Log final arguments from function
  traceit("Result of create_cvglmnet_extra_args", cv_args, traceon = traceon)
  
  # Modify penalty.factor after function execution
  penalty_factor <- cv_args$penalty.factor
  # Identify sex variable adjustments
  sex_vars <- grep("^sex\\.", names(penalty_factor), value = TRUE)
    if (length(sex_vars) > 0) {
      penalty_factor[sex_vars] <- 0.5
    }
    
    # Setup learner and train the model
    learner <- lrn("surv.cv_glmnet", alpha = 0.8)
    learner$param_set$values <- list(
      foldid = cv_args$foldid,
      penalty.factor = penalty_factor
    )
  

  return(cv_args)
}

# Use the function with tracing enabled
cv_args = run_cvglmnet_process(traceon = TRUE)


# Partion
# part = partition(task)

# Verify the encoded task directly
encoded_task = po_encode$train(list(task))[[1]]
cv_args = create_cvglmnet_extra_args(task$train)
str(cv_args)


create_cvglmnet_extra_args

# Combine them into a pipeline using %>>% operator
pipeline = po_encode %>>% po_glmnet_args

# Train the pipeline on the task
processed_task = pipeline$train(list(task))[[1]]

# Print the structure of the processed task to verify changes
message("Processed Task:")
print(str(processed_task))

# Create new instance, specifying params
po_glmnet_args = PipeOpCVGlmnetExtraArgs$new()

# Ensure params are supplied properly
po_glmnet_args$param_set$values$nfolds = 5
po_glmnet_args$param_set$values$seed = 123


tryCatch(
  {
    processed_task = po_glmnet_args$train(list(encoded_task))[[1]]
    message("Updated Task with Extra Args:")
    print(str(processed_task$extra_args))
  },
  error = function(e) {
    message("Error during training: ", e$message)
  }
)






# Use the parameters in the existing surv.cv_glmnet learner
learner = lrn("surv.cv_glmnet")

# Pass the parameters directly to the learner's train method
learner$param_set$values$foldid = extra_args$foldid
learner$param_set$values$penalty.factor = extra_args$penalty.factor

# Train the learner
learner$train(processed_task)
predictions = learner$predict(processed_task)
print(predictions)