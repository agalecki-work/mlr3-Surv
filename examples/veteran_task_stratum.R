
require(mlr3)
require(mlr3proba)

task1 = tsk("veteran")
task$set_col_roles(cols = event_col, add_to = "stratum")
