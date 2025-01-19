# source("apply_time_horizon-TEST.R")
library(mlr3)
library(mlr3data)
library(mlr3proba)
library(data.table)
library(survival)
library(dplyr)

rm(list=ls())
# Load data for testing
load("./Rdata/05CreateData.Rdata") # `CRIC_dt` `DataInfo` loaded

# Manually source necessary R functions

source("./R/traceit.R")
source("./R/apply_time_horizon.R")
source("./R/createTaskSurv.R")

tinfo_mtx = DataInfo$targets_info_mtx

#-----------

target_info= tinfo_mtx[6,]
print(target_info)

cols =  c("PID", "TIME_LOSS50", "TIME_DEATH", "DEADx", "LOSS50", target_info[c("time", "event")])

dtx = CRIC_dt[,cols]
colnames(dtx)

dtout = apply_time_horizon(dtx,  id="PID",target_info,  time_horizon= 16.5, traceon=TRUE)
colnames(dtout)
nms = c("TIME_LOSS50", "TIME_DEATH", "DEADx", "LOSS50", "eTIME_LOSS50", "event_LOSS50f")
dt = dtout %>% select(all_of(nms))
head(dt, n=10)


#=====
#>    ..row_id TIME_LOSS50 TIME_DEATH DEADx LOSS50 eTIME_LOSS50 event_LOSS50f  |time_horizon =5		     thorizon =16.5
#> 1         1   15.964384  16.878516     1      0    16.878516         death  |     5        censor |	 16.5  censor
#> 2         2    4.000000   4.875470     0      0     4.875470        censor  |     4.87     censor |    4.87 censor
#> 3         3    4.046575   5.605479     0      0     5.605479        censor  |     5.       censor |	  5.6  censor
#> 4         4    4.910884  12.408152     0      1     4.910884        LOSS50  |     4.91     LOSS50 |    4.9  LOSS50
#> 5         5    3.726027  16.407283     1      1     3.726027        LOSS50  |     3.72     LOSS50 !	  3.72 LOSS50
#> 6         6   16.081967  16.327869     1      0    16.327869         death  |     5        censor |	 16.3  death
#> 7         7    6.947945  14.312329     0      1     6.947945        LOSS50  |     5        censor |	  6.9  LOSS50
#> 8         8   13.202800  16.566188     1      1    13.202800        LOSS50  |     5        censor |	 13.2 LOSS50
#> 9         9   15.994521  16.503174     1      0    16.503174         death  |     5        censor |	 16.5 censor
#> 10       10    4.525167  14.279452     0      1     4.525167        LOSS50  |     4.52     LOSS50 |    4.52 LOSS50
#> 

