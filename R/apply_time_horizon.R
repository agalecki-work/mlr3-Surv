
apply_time_horizon <- function(data, target_info, id=NULL, time_horizon=NULL, traceon = FALSE) {
  # target_info is named vector: target_id, time, event, type

  if (is.null(time_horizon)) return(data)
  
  args = as.list(target_info)
  target_id = args[["target_id"]]
  time  =   args[["time"]]
  time2  =  args[["time2"]]
  event =   args[["event"]]
  type =    target_info[["type"]]
  
 
  # Extract the Surv object from the data
  surv_obj = with(data, Surv(get(time),get(event)))
  if (!is.null(time2)) surv_obj = with(data, Surv(get(time),get(time2), get(event)))
  target_names =  c(time, time2, event)
  traceit("--2 target_names:", target_names, traceon=traceon)
  traceit("--3 data[, target_names) before survSplit", head(data), traceon=traceon)

  # Use survSplit to truncate survival times
  truncated_data = survival::survSplit(
    formula = surv_obj ~ .,
    data = data,
    cut = time_horizon,
    episode = "temp_episode",
    id = "temp_id"
  )
  traceit("--4 Data after survSplit", truncated_data, traceon=traceon)
   # Drop the additional episodes and handle censoring
  truncated_data = truncated_data[truncated_data$temp_episode == 1, ]
  traceit("--5 Data with temp_episode == 1", truncated_data, traceon=traceon)
  surv_object = truncated_data$surv_obj
  traceit("--5.1 surv_object", surv_object, traceon=traceon)

  time_var= surv_object[, "stop"]
  traceit("--5.12  time_var",  time_var, traceon=traceon)

  if (type == "right") status_var = surv_object[, "status"]
  if (type == "mstate") {
     status_numeric <- surv_object[, 3]
     traceit("--5.2 status_numeric", status_numeric, traceon=traceon)

     factor_levels <- attr(surv_object, "inputAttributes")$event$levels
     traceit("--5.3 factor_levels", factor_levels, traceon=traceon)
     len1 = length(factor_levels)-1
     status_var <- factor(status_numeric, levels = 0:len1, labels = factor_levels)
     traceit("--5.4 status_var", status_var, traceon=traceon)
  
   }

   # Remove the unnecessary columns
  columns_to_remove = c("temp_episode", "temp_id", "surv_obj")
  dtout = truncated_data %>% select(- all_of(columns_to_remove))
  traceit("--6 Data after censoring and truncation of time", dtout, traceon=traceon)
    
  
  dtout[,time] = time_var
  dtout[,event] = status_var
  traceit("--7 Data after censoring and truncation of time", dtout, traceon=traceon)

  dtout = as.data.table(dtout)
  return(dtout)
}

#-----------
