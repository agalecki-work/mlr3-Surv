# See example #53, requires traceit()

create_cvglmnet_extra_args <- function(task, nfolds = 5, seed = 123, traceon = FALSE) {
  traceit(sprintf("--- create_cvglmnet_extra_args() STARTS. nfolds: %d", nfolds), traceon = traceon)
  
  set.seed(seed)
  nrows = task$nrow
  foldid = integer(nrows)
  
  # Get strata and assign folds
  if ("stratum" %in% names(task$col_roles) && !is.null(task$strata)) {
    strata = task$strata
    traceit("Strata details", strata, traceon = traceon)
    filtered_indices = task$row_ids
    
    for (i in seq_len(nrow(strata))) {
      original_indices = unlist(strata$row_id[i])
      indices = which(filtered_indices %in% original_indices)
      foldid[indices] = sample(rep(1:nfolds, length.out = length(indices)))
    }
  } else {
    foldid = sample(rep(1:nfolds, length.out = nrows))
  }

  penalty_factor = rep(1, task$n_features)
  names(penalty_factor) = task$feature_names
  
  res = list(foldid = foldid, penalty.factor = penalty_factor)
  traceit("--- create_cvglmnet_extra_args() ENDS with result", res, traceon = traceon)
  
  return(res)
}
