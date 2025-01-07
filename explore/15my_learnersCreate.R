library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(mlr3extralearners)

library(mlr3misc)

# Available surv mlr learners
available_learners = as.data.table(mlr_learners)[, c("key", "label","task_type","predict_types")]
keys      = unlist(available_learners[, "key"])
surv_learners = keys[startsWith(keys, "surv.")]
print(surv_learners)


# Create a named list to hold learners with specific information
my_learners <- list()

# Define a list of alpha values
alpha_values = c(0.1, 0.25, 0.5, 0.75, 1)

# Loop through alpha values and create learners
for (alpha in alpha_values) {
  learner_id = paste0("surv.glmnet.alpha_", alpha)
  learner <- lrn("surv.glmnet", alpha = alpha, mxit =101, id = learner_id)
    # Create a list to hold the specified information
    learner_info <- list(
      label = learner_id,
      task_type = learner$task_type,
      predict_types = learner$predict_types,
      learner = learner
    )
  my_learners[[learner_id]] = learner
}

nms = names(my_learners)
nm_lasso =nms[length(nms)] 
lasso_lrn = my_learners[[nm_lasso]]
names(lasso_lrn)
lasso_lrn$label
lasso_lrn$task_type
paste(lasso_lrn$predict_types, collapse = ", ")
colnames(lasso_lrn$param_set)
# Access a specific learner
as.data.table(my_learners_dict)[, "key"]
learner = learners_dict$get("surv.cv_glmnet.alpha_0.5")
learner
