Descriptive_stats <- function(task, cols = NULL, group_by_cols = NULL) {
  # Extract data from the task
  task_data <- task$data()

  # Select specified columns or default to all numeric columns
  if (is.null(cols)) {
    data_to_summarize <- task_data %>% select(where(is.numeric))
  } else {
    data_to_summarize <- task_data %>% select(all_of(cols))
  }
  
  # If group_by_cols is specified, add those columns to the data_to_summarize
  if (!is.null(group_by_cols)) {
    data_to_summarize <- task_data %>%
      select(all_of(c(group_by_cols, names(data_to_summarize))))
  }

  # Calculate the descriptive statistics with optional grouping
  descriptive_stats <- data_to_summarize %>%
    { if (!is.null(group_by_cols)) group_by(., across(all_of(group_by_cols))) else . } %>%
    summarise(across(everything(), list(
      n = ~ sum(!is.na(.)),
      missing = ~ sum(is.na(.)),
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE),
      min = ~ min(., na.rm = TRUE),
      q1 = ~ quantile(., 0.25, na.rm = TRUE),
      median = ~ median(., na.rm = TRUE),
      q3 = ~ quantile(., 0.75, na.rm = TRUE),
      max = ~ max(., na.rm = TRUE)
    ), .names = "{.col}_{.fn}"), .groups = "drop") %>% 
    pivot_longer(
      cols = -all_of(group_by_cols), 
      names_to = c("variable", "statistic"), 
      names_pattern = "(.*)_(.*)") %>%
    pivot_wider(names_from = statistic, values_from = value)
  
  return(descriptive_stats)
}
  
#  Descriptive_stats(task)


