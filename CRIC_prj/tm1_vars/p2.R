# Load required libraries
library(mlr3)           # Core 'mlr3' library for tasks and learners
library(mlr3proba)      # Extensions for survival analysis
library(mlr3learners)   # Collection of additional learners
library(mlr3extralearners)   # Collection of additional learners
library(mlr3pipelines)  # Tools for data preprocessing and pipeline construction
library(dplyr)

rm(list=ls())

outPath ="../out/"
load(paste0(outPath, "05CreateData.Rdata"))

source("../../R/_sourceFunctions.R")
fun =lapply(FUNscriptNames, FUN= function(src) source(paste0( "../../R/", src)))
print(FUNscriptNames)

# Set global default values using options
     options(            
      # ---- createTaskSurv() args 
         # target_info,
         # id                     = "CRIC"
         # data                   = CRIC_dt
         # primary_key            = "Row_no"
         T0.subset_col         = NULL,
         T0.feature_cols       = NULL,
         T0.weight_col         = NULL,
         T0.event_strata       = TRUE,
         T0.add_to_strata_cols = NULL,
         T0.traceon            = FALSE
 )
option_names = names(options())
option_names[startsWith(option_names, "T")]
tmtx1 =DataInfo$tmtx1



ti = tmtx1[1,]
task = createTaskSurv(ti, feature_cols =c("BMI", "OL1", "CHF", "ACRcat"))
task$target_names
head(task$truth()) # survival::Surv() object
range(task$times()) 
table(task$status()) 
task$prop_haz()
task$kaplan(strata ="ACRcat")
# proportion of censored observations across all dataset
task$cens_prop()
# proportion of variables that are significantly associated with the
# censoring status via a logistic regression model
task$dep_cens_prop() # 0 indicates independent censoring




cox = lrn("surv.coxph")

part = partition(task)  # Train/test split balanced by status (default)
# Train the learner using the training subset of the data and nake predictions on test data

prediction = cox$train(task, part$train)$predict(task, part$test)
pred_surv = prediction$distr$survival(c(0, 1, 3, 5, 7)) 
dim(pred_surv) 
head(t(pred_surv))
