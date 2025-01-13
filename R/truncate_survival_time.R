truncate_survival_time <- function(surv_task, time_horizon=NULL, traceon = FALSE) {
  # Extract the data from the task
  if (is.null(time_horizon)) return(surv_task)
  data = as.data.frame(surv_task$data())
  traceit("--1 Data extracted from task", data, traceon=traceon)
  
  # Extract the Surv object from the task
  surv_obj = surv_task$truth()
  target_names =  surv_task$target_names
  #surv_col_names =  surv_task$target_names # colnames(temp_surv_obj)
 # traceit("--2 Names of Surv object columns", surv_col_names, traceon=traceon)
  traceit("--3 Structure of Surv object", surv_obj, traceon=traceon)
  
  # Use survSplit to truncate survival times
  truncated_data = survival::survSplit(
    formula = surv_obj ~ .,
    data = data,
    cut = time_horizon,
    episode = "temp_episode",
    id = "..row_id"
  )
  traceit("--4 Data after survSplit", truncated_data, traceon=traceon)
 
  # Drop the additional episodes and handle censoring
  truncated_data = truncated_data[truncated_data$temp_episode == 1, ]
  traceit("--5 Data with temp_episode == 1", truncated_data, traceon=traceon)
  truncated_data[, target_names] = truncated_data$surv_obj[, c("stop","status")]
   traceit("--6 Data after censoring and truncation of time", truncated_data, traceon=traceon)
  
  # Remove the unnecessary columns
  columns_to_remove = c("temp_episode", "surv_obj")
  truncated_data = truncated_data[, !names(truncated_data) %in% columns_to_remove]
  traceit("--7 Data after removing unnecessary columns", truncated_data, traceon=traceon)
  
  # Convert the modified data back to a data.table
  modified_data = as.data.table(truncated_data)
  traceit("--8 Converted to data.table", modified_data, traceon=traceon)
  
  # Create a new backend using the modified data.table
  new_backend = DataBackendDataTable$new(modified_data, primary_key = "..row_id")
  traceit("--9 New backend created", new_backend, traceon=traceon)
  
  # Rebuild a new TaskSurv with the modified backend
  new_task = TaskSurv$new(
    id = surv_task$id, 
    backend = new_backend,
    time = target_names[1],
    event = target_names[2],
    label = paste0(surv_task$label,": Time horizon =", time_horizon)
  )
  
  return(new_task)
}

#-----------

#> task = truncate_survival_time(task, time_horizon= 5)
#> print(range(task$data(cols=task$target_names["time"])))
