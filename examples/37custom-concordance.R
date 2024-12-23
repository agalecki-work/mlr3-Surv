library(mlr3)
library(mlr3proba)
library(survival)
library(R6)
library(mlr3misc)

rm(list =ls())
MeasureSurvConcordanceDetails = R6::R6Class(
  "MeasureSurvConcordanceDetails",
  inherit = Measure,
  public = list(
    concordance_details = NULL,
    
    initialize = function() {
      super$initialize(
        id = "surv.concordance_details",
        task_type = "surv",
        range = c(0, 1),  # Concordance ranges from 0 to 1
        minimize = FALSE,
        predict_type = "crank"
      )
    },
    
    # Override the score method to calculate concordance
    score = function(prediction, ...) {
    
      Survx = prediction$truth   # 
      pred = prediction$crank   # `crank` prediction type

      # Calculate concordance using the `concordance` function from the survival package
      concordance_result = survival::concordance(Survx ~ pred, reverse =TRUE)
      # Store the full concordance detail object
      self$concordance_details = concordance_result
        print(concordance_result$concordance)
      # Return the concordance index
      return(concordance_result$concordance)
    },
    
    get_concordance_details = function() {
      return(self$concordance_details)
    }
  )
)

# Register the custom measure

mlr_measures$add("surv.concordance_details", MeasureSurvConcordanceDetails$new())
print(mlr_measures$keys())

#--------
# Create the survival task
task = tsk("veteran")

# Define a learner
learner = lrn("surv.coxph")

# Train the learner on the task
learner$train(task)

# Predict on the same task (or on a test set)
prediction = learner$predict(task)

# Evaluate using the custom concordance details measure
concordance_details_measure = msr("surv.concordance_details")
concordance_score = prediction$score(concordance_details_measure)

# Print the concordance index score
print(paste("Concordance Index:", concordance_score))

# Access the detailed concordance object
concordance_details = concordance_details_measure$get_concordance_details()
print(concordance_details)
