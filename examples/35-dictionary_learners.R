library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(mlr3extralearners)

library(mlr3misc)

# Create a dictionary to hold learners
learners_dict = Dictionary$new()

# Define a list of alpha values
alpha_values = seq(0lr, 1, by = 0.1)

# Loop through alpha values and create learners
for (alpha in alpha_values) {
  learner_id = paste0("surv.cv_glmnet.alpha_", alpha)
  learners_dict$add(learner_id, lrn("surv.cv_glmnet", alpha = alpha))
}

# Access a specific learner
learner = learners_dict$get("surv.cv_glmnet.alpha_0.5")
learner
