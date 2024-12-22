library(mlr3)
library(survival)
library(mlr3proba)      # Extensions for survival analysis
library(mlr3learners)   # Collection of additional learners

library(data.table)

rm(list=ls())

# Helper function for tracing
trace_print <- function(enabled, message, data = NULL) {
  if (enabled) {
    cat(message, "\n")
    if (!is.null(data)) {
      print(str(data))
    }
  }
}


# Create function to truncate survival times on a task
truncate_survival_time <- function(surv_task, time_horizon, trace = FALSE) {
  # Extract the data from the task
  data = as.data.frame(surv_task$data())
  trace_print(trace, "--1 Data extracted from task", data)
  
  # Extract the Surv object from the task
  temp_surv_obj = surv_task$truth()
  surv_col_names = colnames(temp_surv_obj)
  trace_print(trace, "--2 Names of Surv object columns", surv_col_names)
  trace_print(trace, "--3 Structure of Surv object", temp_surv_obj)
  
  # Use survSplit to truncate survival times
  truncated_data = survival::survSplit(
    formula = temp_surv_obj ~ .,
    data = data,
    cut = time_horizon,
    episode = "temp_episode",
    id = "temp_id"
  )
  trace_print(trace, "--4 Data after survSplit", truncated_data)
  
  # Drop the additional episodes and handle censoring
  truncated_data = truncated_data[truncated_data$temp_episode == 1, ]
  trace_print(trace, "--5 Data with temp_episode == 1", truncated_data)
  
  # Censor and truncate time
  truncated_data$status[truncated_data$time > time_horizon] = 0
  truncated_data$time = pmin(truncated_data$time, time_horizon)
  trace_print(trace, "--6 Data after censoring and truncation of time", truncated_data)
  
  # Remove the unnecessary columns
  columns_to_remove = c("temp_episode", "temp_surv_obj")
  truncated_data = truncated_data[, !names(truncated_data) %in% columns_to_remove]
  trace_print(trace, "--7 Data after removing unnecessary columns", truncated_data)
  
  # Convert the modified data back to a data.table
  modified_data = as.data.table(truncated_data)
  trace_print(trace, "--8 Converted to data.table", modified_data)
  
  # Create a new backend using the modified data.table
  new_backend = DataBackendDataTable$new(modified_data, primary_key = "temp_id")
  trace_print(trace, "--9 New backend created", new_backend)
  
  # Rebuild a new TaskSurv with the modified backend
  new_task = TaskSurv$new(
    id = surv_task$id, 
    backend = new_backend,
    time = surv_col_names[1],
    event = surv_col_names[2],
    label = paste0("Time horizon =", time_horizon)
  )
  
  return(new_task)
}

#### ---- Execution starts ---------

# Create the survival task
task = tsk("veteran")
print(task)

# Print original task for comparison
print(task$data())

# Truncate survival times at a time horizon of 300 days
time_horizon = 300
task = truncate_survival_time(task, time_horizon)

# Print to check data in the modified task
task
print(task$data())
