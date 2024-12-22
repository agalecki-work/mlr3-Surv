# source("31logger-advanced.R")
library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(data.table)
library(lgr)
library(jsonlite)
rm(list=ls())


# Configure two loggers
# Logger for short messages
short_logger = lgr::get_logger("short_logger")
short_logger$set_threshold("debug")

# Logger for long messages
long_logger = lgr::get_logger("long_logger")
long_logger$set_threshold("debug")

# Configure the console appender for short logger
short_console_appender = lgr::AppenderConsole$new(layout = lgr::LayoutFormat$new("%t [%l]: %m"))
short_logger$add_appender(short_console_appender)


# Optionally, add file appenders if needed
short_file_appender = lgr::AppenderFile$new(
  file = "31logger-advanced.log",
  layout = lgr::LayoutFormat$new("%t [%l]: %m")
)

# Custom appender writing to a list
CustomAppender = R6::R6Class("CustomAppender", inherit = lgr::Appender,
  public = list(
    logs = list(),
    append = function(event) {
      self$logs <- c(self$logs, list(event))
    })
)
long_custom_appender = CustomAppender$new()

# Add the console, file and custom appenders to the logger
short_logger$add_appender(short_console_appender) # short console
short_logger$add_appender(short_file_appender)    # short file
long_logger$add_appender(long_custom_appender)    # long custom

# Function to log the message
log_message = function(logger_short, logger_long, level, message, obj = NULL) {
  if (!is.null(obj)) {
    serialized_obj = tryCatch(
      toJSON(obj, pretty = TRUE),
      error = function(e) {
        warning_message = paste0("Warning: Object not serializable - ", conditionMessage(e))
        logger_short$warn(warning_message)
        return(NULL)
      }
    )
    if (!is.null(serialized_obj)) {
      full_message = paste0(message, "\n", serialized_obj)
      logger_long$log(level, full_message)
    } else {
      logger_short$log(level, message)
    }
  } else {
    logger_short$log(level, message)
  }
}

cat("==== logging starts \n")


# Load a survival task
task = tsk("rats")
task

# Define the learner - Cox Proportional Hazards Model
learner = lrn("surv.coxph")

# Perform a training/test split, stratified on `status` by default
part = partition(task)  # Train/test split balanced by status


# Train the learner with logging enabled
log_message(short_logger, long_logger, "info", "Starting training")
learner$train(task, part$train)
log_message(short_logger, long_logger, "info", "JSON coxph fit", learner$model)
log_message(short_logger, long_logger, "info", "Finishing training")

# Make predictions
log_message(short_logger, long_logger, "info", "Making predictions")
p = learner$predict(task, part$test)
log_message(short_logger, long_logger, "info", "Predictions completed")


# Evaluate performance
log_message(short_logger, long_logger, "info", "Evaluating performance")
# Uno's C-index
cindex_uno = p$score(msr("surv.cindex", weight_meth = "G2"), task = task, train_set = part$train)
print(paste("Uno's C-Index:", cindex_uno))
log_message(short_logger, long_logger, "info", "Performance evaluation completed")

# Print performance
log_message(short_logger, long_logger, "info", sprintf("Uno's C-Index: %f", cindex_uno ))

# Retrieve and print the logs from the custom appender
cat(" ==== Custom appender printed ===== \n")
ctxt = sapply(long_custom_appender$logs, FUN= function(event) paste0(event$timestamp, event$msg))
cat(paste(ctxt, collapse= "\n"))
cat("\n")


