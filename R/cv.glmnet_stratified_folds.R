
cv.glmnet_stratified_folds <- function(task, nfolds = 10, seed = NULL) {
  # Set the random seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Extract the strata information
  strata_info <- task$strata
  
  # Initialize an empty vector to store the fold ids
  foldids <- integer(sum(strata_info$N))
  
  # Counter for current index in foldids
  current_index <- 1

  # Generate fold IDs by stratum
  for (i in seq_along(strata_info$N)) {
    # Get row ids corresponding to the current stratum
    ids <- unlist(strata_info$row_id[[i]])
    
    # Generate fold ids for this stratum
    foldids_stratum <- sample(1:nfolds, size = length(ids), replace = TRUE)
    
    # Assign the fold ids to the corresponding positions in the foldid vector
    foldids[current_index:(current_index + length(ids) - 1)] <- foldids_stratum
    
    # Update the current index position
    current_index <- current_index + length(ids)
  }
  
  return(foldids)
}

# Example usage

# foldid <- cv.glmnet_stratified_folds(task = task1, nfolds = 5, seed = 123)
# print(head(foldid))
