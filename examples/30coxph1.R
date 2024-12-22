#?LearnerSurvCoxPH
#?mlr_measures_surv.cindex

library(mlr3)
library(mlr3proba)

# Load the rats dataset and create a survival task
task = tsk("rats")
task

# Define the learner - Cox Proportional Hazards Model
learner = lrn("surv.coxph")

# Perform a training/test split, stratified on `status` by default
part = partition(task)  # Train/test split balanced by status

# Train the learner on the training split
learner$train(task, part$train)

# Make predictions on the testing split
p = learner$predict(task, part$test)

# Harrell's C-index
cindex_harrell = p$score(msr("surv.cindex"))
print(paste("Harrell's C-Index:", cindex_harrell))

# Uno's C-index
cindex_uno = p$score(msr("surv.cindex", weight_meth = "G2"), task = task, train_set = part$train)
print(paste("Uno's C-Index:", cindex_uno))


# Harrell's C-index evaluated up to a specific time horizon
p$score(msr("surv.cindex", t_max = 97))

# Harrell's C-index evaluated up to the time corresponding to 30% of censoring
p$score(msr("surv.cindex", p_max = 0.3))

