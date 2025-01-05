# Load necessary libraries
library(mlr3)
library(mlr3proba)
library(riskRegression)
library(survival)
library(paradox)
library(R6)

rm(list =ls())
# source("50LearnerSurvFineGray.R")
LearnerSurvFineGray = R6::R6Class("LearnerSurvFineGray",
  inherit = LearnerSurv,

  public = list(
    initialize = function() {
      print ("--1 init")
      pset = ps(lambda = p_dbl(lower = 0, upper = Inf, default = 0.01))
      print(pset)
      print("--2")
      super$initialize(
        id = "surv.finegray",
        feature_types = c("numeric", "integer", "factor", "ordered"),
        predict_types = c("crank", "lp"),
        param_set = pset
      )
      print("--5")
      print(super)
      print("--6")
    },
    
    train_internal = function(task) {
      print("-11")
      data = task$data()
      print(task)
      print("---12")
      ftime = data[[task$target_names[1]]]
      fstatus = data[[task$target_names[2]]]
      covariates = data[, !names(data) %in% task$target_names, drop = FALSE]
      formula = as.formula(paste("Hist(ftime, fstatus) ~", paste(names(covariates), collapse = "+")))
      print("--- 20")
      self$model = FGR(
        formula = formula,
        data = data, 
        cause = 1, 
        lambda = self$param_set$values$lambda
      )
    },
    
    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)
      preds = predict(self$model, newdata = newdata)
      list(crank = preds$cif, lp = preds$cif)
    }
  )
)

# Register the new learner
mlr3::mlr_learners$add("surv.finegray", LearnerSurvFineGray)
# ===== Testing


# Prepare example data with competing risks
set.seed(123)
n <- 200
p <- 10
data <- data.frame(
  time = sample(1:50, n, replace = TRUE), 
  status = as.factor(sample(0:2, n, replace = TRUE)), 
  matrix(rnorm(n * p), ncol = p)
#  status = factor(statx)
)

# Define a survival task for competing risks
task = mlr3proba::as_task_surv(data, time = "time", event = "status", type="mstate")
print("---40")
print(task)
print("--41")
# Initialize the custom Fine-Gray learner
learner = lrns("surv.finegray", lambda = 0.01)
print("--49")
print(learner)
# Train the model
print("--50")
learner$train(task)
print("--- 51")

# Make predictions
preds = learner$predict(task)

# Display predictions
print(preds)
