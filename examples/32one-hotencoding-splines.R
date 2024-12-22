# source
# Load required libraries
library(mlr3)           # Core 'mlr3' library for tasks and learners
library(mlr3proba)      # Extensions for survival analysis
library(mlr3learners)   # Collection of additional learners
library(mlr3pipelines)  # Tools for data preprocessing and pipeline construction
library(splines2)       # Library for spline transformations
library(paradox)        # For parameter management 

rm(list=ls())

# Create the 'veteran' survival task
task = tsk("veteran")
print(task)  

# Train the pipeline on the task data
po_onehotencode$train(list(task))


PipeOpSplineTransform = R6::R6Class(
  "PipeOpSplineTransform",
  inherit = mlr3pipelines::PipeOpTaskPreproc,
  public = list(
    initialize = function(features, id = "spline_transform", param_vals = list()) {
      super$initialize(
        id, 
        param_vals = param_vals,
        packages = "splines2", 
        param_set = ps(
          features = p_uty(custom_check = function(x) checkmate::checkCharacter(x))
        )
      )
      self$param_set$values$features = features
    }
  ),
  private = list(
    .transform = function(task) {
      features = self$param_set$values$features
      for (feature in features) {
        col = task$col_data()[[feature]]
        if (is.numeric(col)) {
          # Apply spline transformation
          spline_data = as.data.table(bSpline(col, df = 5, degree = 3, intercept = FALSE))
          colnames(spline_data) = paste0(feature, "_", seq_len(ncol(spline_data)))
          # Add spline columns to the task
          task$cbind(spline_data)
          # Remove the original column
          task$backend$remove(feature)
        }
      }
      task
    }
  )
)

# Instantiate the custom spline transformation for specified features
selector_features = c("age", "karno")
po_spline = PipeOpSplineTransform$new(features = selector_features)
print(po_spline)

# Combine pipelines: first one-hot encoding, then spline transformation
pipeline = po_onehotencode %>>% po_spline



# Apply the trained pipeline to the task and obtain the transformed task with dummy variables

task_tr1 = po_onehotencode$predict(list(task))[[1]]
print(task_tr1)

# Display the first few rows of the transformed task
print(head(task_tr1$data()))

# Define the learner - Cox Proportional Hazards Model
learner = lrn("surv.coxph")

# Perform a training/test split, stratified on `status` by default
part = partition(task_tr1)  # Train/test split balanced by status

# Train the learner using the training subset of the data
learner$train(task_tr1, row_ids = part$train)

# Make predictions on the test subset
prediction = learner$predict(task_tr1, row_ids = part$test)

