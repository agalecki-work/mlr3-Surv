library(mlr3)
library(mlr3proba)
library(data.table)

rm(list=ls())

# Helper function to filter rows in a task
filter_task_rows <- function(task, condition) {
  # Extract all data from the backend
  backend_colnames = task$backend$colnames #  colnames in backend
  all_data <- task$backend$data(rows = task$row_ids, cols =  backend_colnames)
  
  # Apply the filtering condition
  filtered_row_ids <- all_data[eval(parse(text = condition)), which = TRUE]
  
  # Update the task to use only the filtered rows
  task$filter(rows = filtered_row_ids)

  return(task)
}

# Example data preparation
set.seed(4123)
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

colnames_ignore = c("pid","ryear","rfs","csize")   # Ignore cols from using as predictors
selected_features = setdiff(colnames_all, colnames_ignore) 
print(selected_features)

task = TaskSurv$new(
  id = "task_surv", 
  backend = my_dt, 
  time = "ryear", 
  event = "rfs", 
  type = "right",
  label = "task1:my_dt:ryear:rfs"
)

# Select only the desired features for modeling
task$select(selected_features)

# Define the condition for filtering (e.g., "csize > 5")
condition <- "csize > 5"

# Use the helper function to filter the task rows
filtered_task <- filter_task_rows(task, condition)
# Define a learner
learner <- lrn("surv.coxph")

# Train the learner on the filtered task
learner$train(filtered_task)

# Check the task data to confirm the filter has been applied
 be_colnames = task$backend$colnames #  colnames in backend

filtered_data <- filtered_task$backend$data(rows = filtered_task$row_ids,cols=be_colnames)
print(filtered_data)
print(task)

