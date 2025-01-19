library(mlr3)
library(mlr3pipelines)
library(paradox)
library(data.table)

# Define the custom PipeOp
source("../R/PipeOpCVGlmnetExtraArgs.R")

# Register the new PipeOp
mlr_pipeops$add("cv_glmnet.extra_args", PipeOpCVGlmnetExtraArgs)

# Example usage with and without strata
task = tsk("iris")
task$filter(140:11)

# Set Species as stratum
task$set_col_roles(cols = "Species", add_to = "stratum")

# Initialize and train the PipeOp
cv_glmnet_extra_args_op = po("cv_glmnet.extra_args", nfolds = 5, seed = 123)
modified_task_with_strata = cv_glmnet_extra_args_op$train(list(task))[[1]]
print(modified_task_with_strata$extra_args)

# Remove strata and retry
task$set_col_roles(cols = "Species", remove_from = "stratum")
modified_task_without_strata = cv_glmnet_extra_args_op$train(list(task))[[1]]
print(modified_task_without_strata$extra_args)