
createBackendData = function( 
     data,
     primary_key        = "..row_id",
     subset_col         = NULL,  
     traceon            = FALSE){
  traceit("==== `createBackendData` function STARTS: subset_col", subset_col, traceon= traceon)
  
   # Create srcData table 
      srcData = as.data.table(data)
   # Create the backend using srcData
      backend <- DataBackendDataTable$new(srcData, primary_key =  primary_key)
      set = list(rows=srcData[, ..subset_col] ==1)
      rows <- which(set$rows)
      if (length(rows) == 0) rows = srcData[, ..primary_key] %>% pull()
      return (backend$data(rows = rows, cols = colnames(data)))
}

# dt = createBackendData(CRIC_dt)
# print(dim(dt))
# print(colnames(dt))
