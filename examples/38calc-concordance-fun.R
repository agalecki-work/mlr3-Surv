library(mlr3)
library(mlr3proba)
library(survival)
library(tibble)
library(dplyr)

rm(list=ls())

# Function to calculate and assemble concordance indices into a tidy tibble
calc_survconcordance = function(prediction, timewt = c("n", "S", "S/G", "n/G2", "I"), conf_level = 0.95, weights = NULL, ...) {
  # Extract truth and prediction from the prediction object
  Survx = prediction$truth
  predx = prediction$crank

  # Get the learner id from the task used to create the prediction
  learner_id = prediction$learner$id

  # Warn and ignore weights if prediction is from surv.coxph learner
  if (learner_id == "surv.coxph" && !is.null(weights)) {
    warning("Weights are not supported for surv.coxph learner and will be ignored.")
    weights = NULL
  }

  # Function to calculate concordance
  calculate_concordance = function(Survx, predx, timewt = NULL, weights = NULL, ...) {
    tryCatch({
      survival::concordance(Survx ~ predx, timewt = timewt, weights = weights, ...)
    }, error = function(e) {
      return(NULL)
    })
  }

  # Initialize results tibble
  results = tibble(
    term = character(),
    estimate = numeric(),
    std.error = numeric(),
    statistic = numeric(),
    conf.low = numeric(),
    conf.high = numeric(),
    p.value = numeric()
  )

  # Loop over provided timewt values with informative names
  timewt_names = c("n" = "C-index (timewt = n)",
                   "S" = "C-index (timewt = S)",
                   "S/G" = "C-index (timewt = S/G)",
                   "n/G2" = "C-index (timewt = n/G2)",
                   "I" = "C-index (timewt = I)")

  for (wt in timewt) {
    concord = calculate_concordance(Survx, predx, timewt = wt, weights = weights, reverse = TRUE, ...)

    if (!is.null(concord)) {
      std.error = sqrt(concord$var)
      estimate = concord$concordance
      z_stat = (estimate - 0.5) / std.error
      alpha = 1 - conf_level
      z_critical = qnorm(1 - alpha / 2)
      ci_lower = estimate - z_critical * std.error
      ci_upper = estimate + z_critical * std.error
      p_val = 1 - pnorm(z_stat)

      # Append results to tibble
      results = results %>% add_row(
        term = timewt_names[wt],
        estimate = estimate,
        std.error = std.error,
        statistic = z_stat,
        conf.low = ci_lower,
        conf.high = ci_upper,
        p.value = p_val
      )
    }
  }

  return(results)
}

# Create the survival task
task = tsk("veteran")

# Define a learner
learner = lrn("surv.coxph")

# Train the learner on the task
learner$train(task)

# Predict on the same task (or on a test set)
prediction = learner$predict(task)

# Example weight vector or NULL if no weights
weights = NULL  # Ensure no weights for surv.coxph

# Calculate concordance indices using the function with optional weights
concordance_results1 = calc_survconcordance(prediction, weights = weights)

# Print the results1
print(concordance_results1)

# Calculate concordance indices using the function with optional weights
concordance_results2 = calc_survconcordance(prediction, weights = weights, ymin = 50, ymax = 100)

# Print the results1
print(concordance_results2)

