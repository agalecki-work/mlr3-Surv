
createSRS_TaskSurv = function(
     target_info,
     srcdata_name       ="CRIC_dt",
     feature_cols       = NULL,
     filter             = NULL,
     weight_col         = NULL,
     add_to_strata_cols = NULL,
     event.strata       = TRUE,
     traceon            = FALSE){
  # target_info is a named vector with elements: target_id, time, task_type, event 
  # traget_id is a string with values: "tm01", "TM11"
  # `filter` is a character string with an expression  Ex. "BMI < 30"
  traceit("-- createSRS_Task function STARTS: target_info", target_info, traceon= traceon) 
  id   = target_info["target_id"]
  type = target_info["task_type"]
  time = target_info["time"]
  event =target_info["event"] 
  target_cols = c(time, event)

  srcData = get(srcdata_name, envir = .GlobalEnv)
  traceit("--- Source data dim ", dim(srcData), traceon= traceon) 
  
  if (!is.null(filter)){
     traceit("-- filter", filter, traceon= traceon) 
     srcData = srcData %>% mutate(filter_condition= if_else(eval(parse(text = filter)),TRUE, FALSE)) %>%
       dplyr::filter(filter_condition) %>% select(- filter_condition)
     traceit("--- Source data dim after applying filter: ", dim(srcData), traceon= traceon) 
   }
   
  dfx =  as.data.frame(srcData[, target_cols])
  colnames(dfx) = target_cols
  idx = paste0(srcdata_name, "|", id, "|type=", type)  

 task = TaskSurv$new(id =idx, time =time, event = event, backend =dfx, type=type)
 cbind_cols = c(feature_cols, weight_col, add_to_strata_cols)
 traceit(" --- cbind_cols: ", cbind_cols, traceon=traceon)
 
 if (!is.null(cbind_cols)){
    dfx_cbind = as.data.frame(srcData[, cbind_cols])
    colnames(dfx_cbind) = cbind_cols
    traceit(" --- dfx_cbind.No of cols: ", str(dfx_cbind), traceon=traceon)
    task$cbind(dfx_cbind)
 }
 label = paste0(" task stratified by event: ", event.strata)
 task$label = if (!is.null(filter)) paste0(label, ", filter =", filter) else label  
 traceit("--- label", task$label, traceon=traceon) 
 if (event.strata) task$set_col_roles(cols = event, add_to = "stratum")
 if (!is.null(add_to_strata_cols)) task$set_col_roles(cols = add_to_strata_cols, add_to = "stratum")
 if (!is.null(weight_col)) task$set_col_roles(cols = weight_col , roles = "weight")
 return(task)
}
