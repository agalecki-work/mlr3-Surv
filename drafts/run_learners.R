# Define a function to create a task with a specific target
create_target_task <- function(base_task, task_id, target) {
  task_clone <- base_task$clone(deep = TRUE)
  
  # Set the target
  task_clone$set_target(target)
  task_clone$id <- task_id
  
  return(task_clone)
}

# Base task with all available features and a default target
base_task <- tsk("rats")

# Example: Create tasks with different targets
task_time_status <- create_target_task(base_task, "task_time_status", target = c("time", "status"))
task_rx <- create_target_task(base_task, "task_rx", target = "rx")

# Store tasks in a list with metadata for managing them
tasks <- list(
  task_time_status = task_time_status,
  task_rx = task_rx
)

# Task metadata to track target associations
task_metadata <- list(
  task_time_status = list(targets = c("time", "status")),
  task_rx = list(targets = "rx")
)

# Function to run learner with a specific task
run_learner <- function(task, preprocessing_pipeline, learner_id, learner_params, partition_seed, traceon = FALSE) {
  processed_task <- preprocessing_pipeline$train(list(task))[[1]]

  set.seed(partition_seed)
  partition_indices <- partition(processed_task)
  train_task <- processed_task$filter(partition_indices$train)
  
  learner <- lrn(learner_id, param_set = learner_params)
  learner$train(train_task)
  
  traceit("Model trained", learner, traceon = traceon)
  return(learner)
}

# Example: running a learner on a multitask setup
preprocessing_pipeline <- po("encode", method = "one-hot")
learner_params <- list(alpha = 0.8)

learner_time_status <- run_learner(
  task = tasks$task_time_status,
  preprocessing_pipeline = preprocessing_pipeline,
  learner_id = "surv.cv_glmnet",
  learner_params = learner_params,
  partition_seed = 123,
  traceon = TRUE
)

# Running another learner setup differently as needed
learner_rx <- run_learner(
  task = tasks$task_rx,
  preprocessing_pipeline = preprocessing_pipeline,
  learner_id = "surv.cv_glmnet",  # Hypothetical; adjust learner based on target type
  learner_params = learner_params,
  partition_seed = 123,
  traceon = TRUE
)