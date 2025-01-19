createTaskSurv = function(
          target_info,
          id     ,            #    
          data   ,            #    Ex.  = CRIC_dt, 
          primary_key        = "..row_id",
          subset_col         = NULL,
          time_horizon       = NULL,
          feature_cols       = NULL,
	  weight_col         = NULL,
	  add_to_strata_cols = NULL,
          event_strata       = TRUE,
          traceon            = FALSE){
      
       # target_info is a named vector with elements: target_id, time, type, event
       # target_id is a string with values: "tm01", "TM11" etc.
       # id is task_id Ex. Ex. = "CRIC"
       # primary key needs to be included in `data`
         traceit("==== traceit: `createTaskSurv` function STARTS",NULL, traceon= traceon)
         # Create srcData table 
            srcData = as.data.table(data)
            srcData = apply_time_horizon(data = srcData, target_info= target_info, id =NULL,
                                      time_horizon = time_horizon, traceon=traceon)
         # Create the backend using srcData
            backend <- DataBackendDataTable$new(srcData, primary_key =  primary_key)
            set = list(rows=srcData[, ..subset_col] ==1)
            rows <- which(set$rows)
            if (length(rows) == 0) rows = srcData[, ..primary_key] %>% pull()
            backend_data = backend$data(rows = rows, cols = colnames(data))

      
      target_id   = target_info["target_id"]
      time        = target_info["time"]
      event       = target_info["event"]
      type        = target_info["type"]  
      traceit("---  target_info :", target_info, traceon=traceon)
      idx = paste0(id,":",target_id)
      add_cols = unique(c(feature_cols, weight_col, add_to_strata_cols))
      sel_cols = unique(c(time, event, add_cols))
      backend_data = backend_data[,..sel_cols]
      task = TaskSurv$new(id = idx, time = time, event= event, backend = backend_data,
                  type=type)
     
      # if (!is.null(add_cols)) task$cbind(backend_data[, ..add_cols])
      if (event_strata) task$set_col_roles(cols = event, add_to = "stratum")
      if (!is.null(add_to_strata_cols)) task$set_col_roles(cols = add_to_strata_cols, add_to = "stratum")
      if (!is.null(weight_col)) task$set_col_roles(cols = weight_col , roles = "weight")
      lbl = if (is.null(subset_col)) NULL else paste0("Subset of the main dataset:  ", subset_col, "=1")  
      task$label = lbl
         traceit("==== traceit: `createTaskSurv` function ENDS",NULL, traceon= traceon)

return(task)
}


