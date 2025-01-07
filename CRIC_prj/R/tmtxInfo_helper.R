
tmtxInfo_helper =function(row){
  # row is a row extracted from tmtx has elements: "target_id","time","task_type","event_num","event_factor" (optional)
  target_id  = row["target_id"]
  task_type  = if (substr(target_id,1,2) == "tm") "right" else "mstate"
  event      = if (task_type == "right")   row["event_num"] else row["event_factor"]
  res = c(target_id, row["time"], task_type, event)
  names(res) = c("target_id","time","task_type","event")
  return(res)
}
