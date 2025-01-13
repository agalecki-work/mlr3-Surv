
# Function to calculate and assemble concordance indices into a tidy tibble
calc_survconcordance_tibble = function(prediction, timewt = c("n", "S", "S/G", "n/G2", "I"), conf_level = 0.95, weights = NULL, ...) {
  # Extract truth and prediction from the prediction object
  Survx = prediction$truth
  predx = prediction$crank

  # Get the learner id from the task used to create the prediction
  learner_id = prediction$learner$id

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

#--- Example
# cox = lrn("surv.coxph")
# part = partition(task)
# prediction = cox$train(task, part$train)$predict(task,part$test)
# calc_survconcordance_tibble(prediction)
