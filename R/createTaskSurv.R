createTaskSurv =function(
                 data,
                 id_num,  # EX: "row_no" corresponding column has to be numeric
                 task_id, 
                 target_info,
                 feature_cols =NULL
                 weight = NULL,
                 time_horizon = 15,
                 event_strata = TRUE,
                 add_to_strata_cols = NULL,
                 CCH_weight = NULL,
                 CCH_subcohort = NULL,  # "col_name"
                 filter = NULL,         # "BMI <= 30"
                 option= NULL,          # "SRS", "CCH", "CCH1"
                 traceon=TRUE){
    # no weight column
    # time_horizon applied (unless NULL)
    # event added to stratum, if event_strata = TRUE
    traceit("--- createTaskSurv_SRS STARTS ", data, traceon=traceon)
    
     # Set default to "SRS" if option is NULL or missing
      if (is.null(option)) {
        option <- "SRS"
      }
      
      if (option =="SRS") CCH_weight = CCH_subcohort = NULL
      if (option %in% c("CCH", "CCH1")) weight = NULL
      
    #--- Common execution/validation
    # Unpack target_info
        target_id   = target_info["target_id"]
        time        = target_info["time"]
        event       = target_info["event"]
        type        = target_info["type"]  
        traceit("---  target_info :", target_info, traceon=traceon)
        
    # time_horizon 
       subset_df = apply_time_horizon(data = data , target_info= target_info, id =NULL,
                                          time_horizon = time_horizon, traceon=traceon)
                                          
     # option =="CCH1": Select subcohort only 
    
     if (option =="CCH1"){
        tmpc = paste0(CCH_subcohort, " == 1")
        subset_df  =  subset_df[eval(parse(text = tmpc)), ]
     }                                   
    
    # Ensure the filter argument is a character string representing a valid R expression
      if (!is.null(filter) && !is.character(filter)) {
            stop("Filter must be NULL or a character string representing a valid R expression")
      }
      if (!is.null(filter)) subset_df <- subset_df[eval(parse(text = filter)), ]
      
    keep_cols = c(id_num, weight, CCH_weight, CCH_subcohort, feature_cols)
    print(keep_cols)
  return(subset_df)
 }
 
 

    
    
      idx = paste0(id,":", target_id)
    data_table = as.data.table(subset_df)    
    task = TaskSurv$new(id = idx, time = time, event= event, backend = backend_data,
                   type=type)
    if (event_strata) task$set_col_roles(cols = event, add_to = "stratum")
    traceit("--- createTaskSurv_SRS ends ", task, traceon=traceon)
   return(task)
 }
    