# source("05CreateData.R")
rm(list=ls())

# Load dependencies
library(mlr3)
library(dplyr)
library(data.table)

# Set paths and filenames

DataSetupInfo = list(
    ScriptName       = "05CreateData",
    DataFolderPath   = "./data/",
    RdataName        = "02-cric_complete112023.Rdata",
    DataName         =  "df_complete",
    OutputFolderPath = "./out/",
    saved_objects    =  c("saved_objects", "DataSetupInfo", "DataInfo", "CRIC_dt")
    )
ScriptName       = DataSetupInfo$ScriptName
RdataName        = DataSetupInfo$RdataName
DataName         = DataSetupInfo$DataName
DataFolderPath   = DataSetupInfo$DataFolderPath
OutputFolderPath = "./out/"

RdataPath = paste0(DataFolderPath, RdataName)

# Start logging
#sink(paste0(OutputFolderPath, "05CreateMasterBackend.Rout"))

# Load and display data
cat("===== Rdata name:", RdataName, "\n")
loaded_objects = load(RdataPath)
cat("Object names: ", paste(loaded_objects, collapse = ', '), "\n")


# Create working copy of the data frame and remove original data
Data = get(DataName)
rm(list= DataName)

cat("\nNumber of rows:", nrow(Data), "\n")
cat("\nVariable names in downloaded data\n")
print(names(Data))

cols_original = colnames(Data)

# Remove unnecessary columns
cols_removed <- c(
  "TIME_ESRD7", "TIME_ESRD10", "TIME_LOSS50_ESRD7", "TIME_LOSS50_ESRD10",  
  "TIME_ESRD4", "TIME_LOSS50_ESRD4", "ESRD7", "ESRD10", "LOSS50_ESRD7", 
  "LOSS50_ESRD10", "ESRD4", "LOSS50_ESRD4"
)

Data <- Data %>% select(-all_of(cols_removed))

cols_after_rm = colnames(Data)

# Modify and create new variables
# Weight variables created for illustration
set.seed(123)

Data <- Data %>%
  mutate(
    ..row_id = 1:nrow(Data),  # Mandatory
    ACRcat = factor(ACRcat),
    CKD = factor(CKD),
    CHF_factor = factor(CHF),
    AFIB = factor(AFIB),
    RACE_CAT_1 = factor(RACE_CAT_1),
    log_OL1 = log(OL1),
    wt1 = runif(nrow(Data), 1, 2), 
    wt2 = runif(nrow(Data), 0.8, 1.2),
    BMI30_idx = if_else(BMI <30, 1, 0),
    CCH_idx = CHF# Auxiliary indicator variable
  )

# Create time/event variables for competing risks analysis
Data <- Data %>%
  mutate(
    eTIME_ESRD = if_else(ESRD == 0, TIME_DEATH, TIME_ESRD),
    event_ESRDx = if_else(ESRD == 0, 2 * DEAD, 1),
    event_ESRDf = factor(event_ESRDx, levels = 0:2, labels = c("censor", "ESRD", "death")),
    eTIME_LOSS50 = if_else(LOSS50 == 0, TIME_DEATH, TIME_LOSS50),
    event_LOSS50x = if_else(LOSS50 == 0, 2 * DEAD, 1),
    event_LOSS50f = factor(event_LOSS50x, levels = 0:2, labels = c("censor", "LOSS50", "death")),
    eTIME_LOSS50_ESRD = if_else(LOSS50_ESRD == 0, TIME_DEATH, TIME_LOSS50_ESRD),
    event_LOSS50_ESRDx = if_else(LOSS50_ESRD == 0, 2 * DEAD, 1),
    event_LOSS50_ESRDf = factor(event_LOSS50_ESRDx, levels = 0:2, labels = c("censor", "LOSS50_ESRD", "death"))
  )
  
# ===== Changes to loaded dataframe are _completed_ (No more changes below)

# `cols_removed` vector defined above
cols_after_create = colnames(Data)
cols_added   = setdiff(cols_after_create, cols_after_rm)

# Matrix `time_event1_mtx`; Time/event variables with 0/1 events
# Use TMxx format for `target_id` column            

# Define time/event matrices
time_event1_mtx <- matrix(c(
  "tm01", "TIME_ESRD", "ESRD",
  "tm02", "TIME_LOSS50", "LOSS50", 
  "tm03", "TIME_LOSS50_ESRD", "LOSS50_ESRD",
  "tm04", "TIME_DEATH", "DEAD"
), ncol = 3, byrow = TRUE)
colnames(time_event1_mtx) <- c("target_id", "time", "event_num")
print(time_event1_mtx)


# Matrix `time_event2_mtx`; Time/event variables  with 0/1/2  events for competing risks analysis
# Use TMxx format for `target_id` column            

time_event2_mtx <- matrix(c(
  "TM11", "eTIME_ESRD", "event_ESRDx", "event_ESRDf",
  "TM12", "eTIME_LOSS50", "event_LOSS50x", "event_LOSS50f", 
  "TM13", "eTIME_LOSS50_ESRD", "event_LOSS50_ESRDx", "event_LOSS50_ESRDf"
), ncol = 4, byrow = TRUE)
colnames(time_event2_mtx) <- c("target_id", "time", "event_num", "event_factor")
print(time_event2_mtx)


# Create vx vector: Define  variable groups
vx = rep("?", times = ncol(Data))
names(vx)=  colnames(Data)
vx[cols_added] = "new"
vx[c("wt1", "wt2")] = "WT"
vx[paste0("OL", 1:21)] = "OL"

tm1_nms <- as.vector(time_event1_mtx[, c("time", "event_num")])
tm2_nms <- as.vector(time_event2_mtx[, c("time", "event_num", "event_factor")])
vx[tm1_nms] <- "tm1"
vx[tm2_nms] <- "tm2"
vx["..row_id"] <- "ID"
vx[c("BMI30_idx", "CCH_idx")] <-  "AUX"

CRIC_dt = Data   #!!!
rm(Data)

# Design cols do not contain predictors
CRIC_design_cols = c("..row_id", tm1_nms, tm2_nms, "wt1", "wt2","BMI30_idx", "CCH_idx")  # Mandatory  


DataInfo <- list(
  RdataPath    = RdataPath,
  tmtx1        = time_event1_mtx,
  tmtx2        = time_event2_mtx,
  cols_removed = cols_removed,
  cols_added   = cols_added,
  col_grps     = vx,
  design_cols  = CRIC_design_cols
)



cat("\n ==== DataInfo component names \n")
print(names(DataInfo))

# Display `DataInfo` contents

cat("\n --- RdataPath \n")
print(DataInfo$RdataPath)

cat("\n --- Data Info \n")
print(DataInfo$Data_Info)


cat("\n --- Matrix for  0/1 events \n") 
print(DataInfo$tmtx1)

cat("\n --- Matrix for  0/1/2 events \n") 
print(DataInfo$tmtx2)

cat("\n --- Cols removed from the original data \n") 
print(DataInfo$cols_removed)

cat("\n --- Cols added to/modified in original data \n") 
print(DataInfo$cols_added)

grps = DataInfo$col_grps
tbl_grps  = table(col_groups = grps)
grp_nms = names(tbl_grps)

# Remaining vars
cat ("\n --- New variables without specified role \n") 
print(names(grps[grps == "new"]))

# Groups of variables
cat ("\n Groups of variables \n") 
res = sapply(grp_nms, FUN = function(nm) names(grps[grps == nm]))
print(res)
       
# Design variables
cat ("\n Design variables \n") 
#res = sapply(grp_nms, FUN = function(nm) names(grps[grps == nm]))
print(DataInfo$design_cols)

# Save objects
saved_objects = DataSetupInfo$saved_objects
save(list = saved_objects, file = paste0(OutputFolderPath, DataSetupInfo$ScriptName, ".RData"))
cat("\n --- Objects saved: ", paste(saved_objects, collapse=', '), "\n")

# Stop logging
#sink()
cat("\n --- Execution finished: Objects saved: ", paste(saved_objects, collapse=', '), "\n")

# Cleanup

lsAll <- ls()
rm_objects <- setdiff(lsAll, saved_objects)
rm(list = rm_objects)
rm(lsAll, rm_objects)


