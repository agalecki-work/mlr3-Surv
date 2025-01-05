
prediction object needed

# does not work
msr_keys = mlr_measures$keys()              # Vector with available measure names
msr("surv.cindex")
mlr_measures$get("surv.cindex")
mlr_measures$get("surv.cindex")
# mlr_measures$get("surv.cindex")$help()

# Identify available survival measures
surv_msr_keys = msr_keys[startsWith(msr_keys, "surv.")] # Keys of surv mesures 
print(surv_msr_keys) # All available 'surv' measure keys

# Function to check if measure extraction from `prediction_train` object produces an error
check_measure_extraction <- function(key) {
  tryCatch({
    tt =  prediction$score(msrs(key))
    err_msg = "NA"
  }, error = function(e) {
    err_msg <<- e$message
  })
  tibble(key=key, err_msg =err_msg)
}
# Apply the function to all survival measures
surv_msr_errmsgs = lapply(surv_msr_keys, check_measure_extraction)


surv_msr_df = bind_rows(surv_msr_errmsgs)


surv_msr_select = surv_msr_df %>%  select(key) %>% pull()

print(surv_msr_select)