traceit = function(msg, object = NULL, traceon = FALSE) {
  if (traceon) {
    message(msg)
    if (!is.null(object)) str(object)
  }
}


