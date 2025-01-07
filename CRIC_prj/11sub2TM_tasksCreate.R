# source("11sub2TM_tasksCreate.R")
library(mlr3)
library(mlr3data)
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

source("./R/traceit.R")
source("./R/tmtxInfo_helper.R")
source("./R/createSRS_TaskSurv.R")


tmtx1 = DataInfo$tmtx1
row1  = tmtx1[1, ]
tmtx2 = DataInfo$tmtx2
row2  = tmtx2[1,]
message("--- tmtx1")
tmtxInfo_helper(row1)
message("--- tmtx2")
tmtxInfo_helper(row2)


auxFun = function(i){
         info = tmtxInfo_helper(i)
         argsi = c(list(target_info = info), args)
         task = do.call(createSRS_TaskSurv, argsi)
         extra_args = c(timeStamp = timeStamp, task$extra_args)
         task$extra_args = extra_args
         return(task)
   }

#### Default list of `createSRS_TaskSurv` arguments 


args_default = list(
     # target_info is mandatoy (no default value)
     srcdata_name       ="CRIC_dt",
     feature_cols       = NULL,
     filter             = NULL,
     weight_col         = NULL,
     add_to_strata_cols = NULL,
     event.strata       = TRUE,
     traceon            = FALSE
)

# Create auxiliary vectors
timeStamp = strsplit(as.character(Sys.time()), split=".", fixed=TRUE)[[1]][1]

tmtx = DataInfo$tmtx2  ## !!! 


#================  Create `TM_test` list of tasks 
test_features        = c("BMI", "OL1","CKD")


args = args_default
args$feature_cols = test_features      # <-------

TM_test = apply(tmtx, 1, auxFun)
names(TM_test) =  tmtx[, "target_id"]
print(TM_test)


#================  Create `tm_test_OLx` list of tasks 

OLx = paste0("OL", 1:21)
testOLx_features    = c("BMI", OLx,"CKD")

args = args_default
args$feature_cols = testOLx_features
TM_testOLx = apply(tmtx, 1, auxFun)
names(TM_testOLx) =  tmtx[, "target_id"]
print(TM_testOLx)

#===== Combine tm lists and save

TM_lists = list(
   TM_test    = TM_test,
   TM_testOLx = TM_testOLx
)
save(TM_lists, file="./sub2/out/11TM_lists.Rdata")
message("--- Script: `11sub12TM_tasks_create.R` executed. ")










