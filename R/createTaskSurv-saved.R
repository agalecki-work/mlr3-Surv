createTaskSurv <- function(data, target_info, backend_info = NULL, event_strata = TRUE, traceon = FALSE) {
  # Helper for default values
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # Trace function
  traceit <- function(msg, variable = NULL) {
    if (isTRUE(traceon)) {
      cat(msg, "\n")
      if (!is.null(variable)) {
        print(variable)
      }
    }
  }
  
  # Start tracing
  traceit("--- createTaskSurv STARTS")
  traceit("target_info:", target_info)
  traceit("backend_info:", backend_info)

  # Unpack `backend_info` with defaults
  backend_id <- backend_info$id
  option <- backend_info$option %||% "SRS"
  primary_key <- backend_info$primary_key
  feature_cols <- unique(backend_info$feature_cols)
  filter <- backend_info$filter
  time_horizon <- backend_info$time_horizon
  CCH_subcohort <- backend_info$CCH_subcohort
  add_to_strata_cols <- backend_info$add_to_strata_cols
  
  # Determine weight column based on the option
  weight_col <- backend_info$weight[option]
  
  if (option == "SRS") CCH_subcohort <- NULL

  # Adjust strata columns for "CCH" option
  if (option == "CCH") {
    add_to_strata_cols <- c(add_to_strata_cols, CCH_subcohort)
  }

  traceit("backend settings:", c(id = backend_id, option = option, primary_key = primary_key))

  # Unpack `target_info`
  target_id <- target_info["target_id"]
  time <- target_info["time"]
  event <- target_info["event"]
  type <- target_info["type"]
  traceit("target_info:", target_info)

  # Apply time horizon filtering
  subset_df <- apply_time_horizon(data, target_info, id = NULL, time_horizon = time_horizon, traceon = FALSE)

  # "CCH1" filters for subcohort subjects only
  if (option == "CCH1") {
    subset_df <- subset_df[subset_df[[CCH_subcohort]] == 1, ]
    CCH_subcohort <- NULL
  }

  # Apply filter if present
  if (!is.null(filter)) {
    subset_df <- subset_df[eval(parse(text = filter)), ]
  }

  # Prepare columns to retain
  xtra_cols <- na.omit(c(CCH_subcohort, primary_key))
  keep_cols <- unique(na.omit(c(xtra_cols, event, feature_cols, weight_col, add_to_strata_cols)))
  if (type == "right") keep_cols <- c(keep_cols, time)
  
  subset_df <- subset_df[, ..keep_cols]
  traceit("Final subset columns:", keep_cols)

  # Configure backend and task
  backend <- DataBackendDataTable$new(subset_df, primary_key = primary_key)
  task_id <- paste0(option, ".", backend_id, ":", target_id)

  # Create the task
  task <- switch(
    type,
    "right" = TaskSurv$new(id = task_id, time = time, event = event, backend = backend, type = "right"),
    "mstate" = TaskClassif$new(id = task_id, backend = backend, target = event)
  )

  # Define roles for columns
  if (event_strata) add_to_strata_cols <- c(add_to_strata_cols, event)
  roles_list <- list(
    weight_col = weight_col,
    feature_cols = feature_cols,
    stratum_cols = add_to_strata_cols
  )
  traceit("Column roles:", roles_list)

  # Assign roles to the task
  column_roles <- list()

  if (length(roles_list$feature_cols) > 0) {
    for (col in roles_list$feature_cols) {
      column_roles[[col]] <- union(column_roles[[col]], "feature")
    }
  }

  if (length(roles_list$weight_col) > 0) {
    for (col in roles_list$weight_col) {
      column_roles[[col]] <- union(column_roles[[col]], "weight")
    }
  }

  if (length(roles_list$stratum_cols) > 0) {
    for (col in roles_list$stratum_cols) {
      column_roles[[col]] <- union(column_roles[[col]], "stratum")
    }
  }

  traceit("====3")
  traceit("Column roles before assignment:", roles_list)

  # Apply the roles
  if (length(names(column_roles)) > 0) {
    for (col in names(column_roles)) {
      task$set_col_roles(col, role = column_roles[[col]])
    }
  }

  # Add extra data if needed
  if (length(xtra_cols) > 0) {
    xtra_df <- subset_df[, ..xtra_cols]
    task$extra_args <- c(task$extra_args, list(extra_df = xtra_df))
  }

  # Label the task with metadata
  lblx <- c(
    if (!is.null(time_horizon)) paste0("T_horizon =", time_horizon),
    if (!is.null(filter)) paste0("filter=", filter)
  )
  task$label <- paste(lblx, collapse = ", ")

  # Finish
  traceit("--- Task creation finished")
  return(task)
}