# Without JSON
# source("31logger-simple.R")
library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(data.table)
library(lgr)
rm(list=ls())


# Configure the logger
logger = lgr::get_logger("mlr3") # Create a logger
logger$set_threshold("info")    # Options: "trace", "debug", "info", "warn", "error", "fatal"

# Define a console appender
console_appender = lgr::AppenderConsole$new(layout = lgr::LayoutFormat$new("%t [%l]: %m"))


# Define a file appender
file_appender = lgr::AppenderFile$new(
  file = "31logger-simple.log",
  layout = lgr::LayoutFormat$new("%t [%l]: %m")
)

# Memory Appender
memory_appender = lgr::AppenderBuffer$new(buffer_size = 100, layout = lgr::LayoutFormat$new("%t [%l]: %m"))

# Custom appender writing to a list
CustomAppender = R6::R6Class("CustomAppender", inherit = lgr::Appender,
  public = list(
    logs = list(),
    append = function(event) {
      self$logs <- c(self$logs, list(event))
    })
)
custom_appender = CustomAppender$new()

# Add the console, file and custom appenders to the logger
logger$add_appender(console_appender)
logger$add_appender(file_appender)
logger$add_appender(memory_appender)
logger$add_appender(custom_appender)

cat("==== logging starts \n")


# Load a survival task
task = tsk("rats")
task

# Define the learner - Cox Proportional Hazards Model
learner = lrn("surv.coxph")

# Perform a training/test split, stratified on `status` by default
part = partition(task)  # Train/test split balanced by status


# Train the learner with logging enabled
logger$info("Starting training")
learner$train(task, part$train)
logger$info("Finished training")

# Make predictions
logger$info("Making predictions")
p = learner$predict(task, part$test)
logger$info("Predictions completed")

# Evaluate performance
logger$info("Evaluating performance")

# Uno's C-index
cindex_uno = p$score(msr("surv.cindex", weight_meth = "G2"), task = task, train_set = part$train)
print(paste("Uno's C-Index:", cindex_uno))
logger$info("Performance evaluation completed")

# Print performance
logger$info(sprintf("Uno's C-Index: %f", cindex_uno ))

# Retrieve logs from memory buffer
cat(" ==== Memory buffer ==== \n")
print(memory_appender$buffer_df)

# Retrieve and print the logs from the custom appender
cat(" ==== Custom appender printed ===== \n")
ctxt = sapply(custom_appender$logs, FUN= function(event) paste0(event$timestamp, event$msg))
cat(paste(ctxt, collapse= "\n"))
cat("\n")


