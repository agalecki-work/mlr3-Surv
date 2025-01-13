
DataPath ="../data/"
load(

# Source R functions
srcPath = paste0("../R/")
source(paste0(srcPath, "_sourceFunctions.R"))
FUNscriptNames
fun_nms =lapply(FUNscriptNames, FUN= function(src) source(paste0(srcPath, src)))
fun_nms
