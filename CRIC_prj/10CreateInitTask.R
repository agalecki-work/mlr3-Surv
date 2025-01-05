library(mlr3)
library(mlr3data)
library(mlr3tasks)
library(mlr3proba)
library(data.table)
library(survival)
library(dplyr)

rm(list=ls())

# Objects: "DataInfo" "CRIC_dt" created
source("05CreateData.R")
print(saved_objects)
print(names(DataInfo))
print(DataInfo$tmtx1)
print(DataInfo$tmtx2)

tmtx1 =  DataInfo$tmtx1
tmtx2 =  DataInfo$tmtx2
tmtX  = list(tmtx1 = tmtx1, tmtx2 = tmtx2)

targetInfo_helper =function(id,tmtX){
  # id = tm?? or TM?? corresponds to column named "target_id"
  tmtx       = if (substr(id,1,2) == "tm")  tmtX$tmtx1 else tmtX$tmtx2
  task_type  = if (substr(id,1,2) == "tm") "right"  else "mstate"
  names(task_type) ="task_type"
  # Extract elements from tmtx
  col_id = tmtx[, "target_id"]
  row_no = which(col_id == id)
  trgt_info0  = c(tmtx[row_no, ], task_type) 
  event      = if (task_type == "right") trgt_info0["event_num"]  else trgt_info0["event_factor"]
  names(event) ="event"
  trgt_info  = c(trgt_info0, event)
  trgt_info = trgt_info[c("target_id","time","task_type","event")] 

  return(trgt_info)
}

trgt_tm02 = targetInfo_helper("tm02", tmtX)
print(trgt_tm02)
trgt_TM12 = targetInfo_helper("TM12", tmtX)
print(trgt_TM12)

traceit = function(msg, object, traceon = FALSE){

  if (traceon){
     message(msg)
     str(object)
  } else {
     invisible(NULL)
}}


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

createSRS_TaskSurv(trgt_tm02)
createSRS_TaskSurv(trgt_tm02, event.strata = FALSE)
createSRS_TaskSurv(trgt_tm02, feature_cols ="BMI",  weight_col="wt1", filter = "BMI<30")


OLx = paste0("OL", 1:21)

test   = c("BMI", "OL1","CKD")
test_OLx    = c("BMI", OLx,"CKD")

tsk_tm02_test = createSRS_TaskSurv(trgt_tm02, feature_cols =test)
tsk_TM12_test = createSRS_TaskSurv(trgt_TM12, feature_cols =test_OLx)

my_tasks2 = list(
 tsk_tm02_test = createSRS_TaskSurv(trgt_tm02, feature_cols =test),
 tsk_TM12_test = createSRS_TaskSurv(trgt_TM12, feature_cols =test_OLx)
)

my_tasks = list(
       tsk_tm02_test =  tsk_tm02_test,
       tsk_TM12_test = tsk_TM12_test 
)


names(my_tasks)
lapply(my_tasks, typeof)
tx= lapply(my_tasks, print)
print(tx)  

save(my_tasks, file = "./out/10my_tasks.Rdata")

