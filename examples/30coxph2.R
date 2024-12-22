library(mlr3)
library(mlr3proba)

rm(list=ls())
# Load the rats dataset and create a survival task
task0 = tsk("rats")

# Define task for surv.coxph learner with case weighte
set.seed(123)
selected_features= c("litter", "rx", "sex")
task  = task0$clone()
task$cbind(data.frame(weights = runif(task$nrow, 1, 2)))
task$select(selected_features)
task$col_roles$weight = "weights"
task
## <TaskSurv:rats> (300 x 5): Rats
## * Target: time, status
## * Properties: weights
## * Features (3):
##  - int (2): litter, rx
##  - fct (1): sex
## * Weights: weights

# task$weights


# Define the learner - Cox Proportional Hazards Model (with case weights)
learner = lrn("surv.coxph")

# Perform a training/test split, stratified on `status` by default
part = partition(task)  # Train/test split balanced by status (default)

# Train the learner on the training split
learner$train(task, part$train)
learner

# Make predictions on the testing split
p = learner$predict(task, part$test)

## Error in model.frame.default(formula = Surv(time, status, type = "right") ~  : 
##   variable lengths differ (found for '(weights)')




