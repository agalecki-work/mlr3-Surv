
rm(list=ls())

# Function to create a wide tibble with self-documented detailed info for a task
TaskSummary <- function(task) {
  # Function to convert col_roles to a tibble
  col_roles_to_tibble <- function(col_roles) {
    # Convert col_roles to a tibble with column names and their corresponding roles
    tibble(
      col_name = unlist(col_roles, use.names = FALSE),
      col_role = rep(names(col_roles), lengths(col_roles))
    )
  }
 # Function to convert row_roles to a tibble with row_id and row_role
   row_roles_to_tibble <- function(row_roles) {
    # Convert row_roles to a tibble with row IDs and their corresponding roles
   tibble(
    row_id = unlist(row_roles, use.names = FALSE),
    row_role = rep(names(row_roles), lengths(row_roles))
   )
}

  # Extract relevant information from the task
  task_glance <- tibble(
    task_id = task$id,
    task_type = task$task_type,
    task_label = task$label,
    target_names = paste(task$target_names, collapse = ", "),
    weights = task$weights,
    ncol = task$ncol,
    nrow = task$nrow,
    n_features = length(task$feature_names),
    positive = if (!is.null(task$positive)) task$positive else NA,
    hash = task$hash,
    properties = if (!is.null(task$properties)) paste(task$properties, collapse = ", ") else NA,
    mlr3_version = as.character(task$mlr3_version)
  )

  # Create col_dictionary tibble
  col_roles_tibble <- tibble(task_id = task$id, col_roles_to_tibble(task$col_roles))
  feature_types_tibble <- as_tibble(task$feature_types) %>%
    rename(col_name = id, col_type = type)
  task_labels_tibble <- tibble(col_name = names(task$labels), col_label = task$labels)
  
  
  task_dict_info <- col_roles_tibble %>%
    full_join(feature_types_tibble, by = "col_name") %>%
    full_join(task_labels_tibble, by = "col_name")
    
    # Create row_roles_summary tibble
    row_roles_tibble <- row_roles_to_tibble(task$row_roles)
    row_roles_summary_tibble = row_roles_tibble %>% group_by(row_role) %>% summarise(nper_role=n())
   
   be_glance = tibble(
          task_id = task$id,
          ncol = task$backend$ncol,
          nrow = task$backend$nrow,
          hash = task$backend$hash)

   # Create backend_colnames tibble
   backend_colnames <- tibble(colname = task$backend$colnames)

           extra_info= list(
           task_extra_args = task$extra_args,
           backend_glance = be_glance,
           backend_colnames = backend_colnames
          ) # extra_info

    # Return the wide tibble
    return(list(glance = task_glance, dict = task_dict_info, 
           row_roles =  row_roles_summary_tibble,
           extra_info = extra_info
    ))
  }

# =========== Example usage with the iris dataset

library(mlr3)
library(dplyr)
library(tidyr)
library(tibble)

task <- TaskClassif$new(id = "iris", backend = iris, target = "Species", label = "Iris Classification")

# Assign column labels
task$labels <- c(
  Sepal.Length = "Length of Sepal",
  Sepal.Width = "Width of Sepal",
  Petal.Length = "Length of Petal",
  Petal.Width = "Width of Petal",
  Species = "Species of Iris"
)

# Add extra arguments to the task
task$extra_args <- list(
  description = "This task uses the famous Iris dataset for classification.",
  source = "UCI Machine Learning Repository",
  preprocessing_steps = c("Normalization", "Encoding")
)

task_info <- TaskSummary(task)
print(names(task_info))
print(task_info$glance)
print(task_info$dict)
print(task_info$row_roles)

extra_info = task_info$extra_info
names(extra_info)
print(extra_info$task_extra_args)
print(extra_info$backend_glance)
print(extra_info$backend_colnames)


