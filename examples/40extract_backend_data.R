library(mlr3)
library(mlr3proba)
library(data.table)

rm(list=ls())
# Example data preparation
set.seed(123)
my_dt <- data.table(
  ryear = sample(1:5, 100, replace = TRUE),
  rfs = sample(0:1, 100, replace = TRUE),
  csize = runif(100, 1, 10),
  nodes2 = sample(0:1, 100, replace = TRUE),
  nodes3 = sample(0:1, 100, replace = TRUE),
  grade3 = sample(0:1, 100, replace = TRUE),
  pid = 1:100  # Additional column for patient IDs
)
colnames_all = names(my_dt)

# Ex.1 Create a TaskSurv

colnames_ignore = c("pid","ryear","rfs")   # Ignore cols from using as predictors
selected_features = setdiff(colnames_all, colnames_ignore) 
print(selected_features)

task = TaskSurv$new(
  id = "01",   #  ideentifier 
  backend = my_dt, 
  time = "ryear", 
  event = "rfs", 
  type = "right",
  label = "my_dt|ryear:rfs"
)

# Select only the desired features for modeling
task$select(selected_features)
print(task)

# Illustrates how to extract backend data table 
backend_colnames = task$backend$colnames #  colnames in backend
backend_dt = as.data.table(task$backend$data(rows=task$row_ids, cols =backend_colnames)) # 
print(head(backend_dt))


