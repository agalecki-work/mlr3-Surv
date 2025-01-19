traceit <- function(msg, variable = NULL, traceon = FALSE) {
  if (isTRUE(traceon)) {
    cat(msg, ":\n")
    if (!is.null(variable)) {
      print(str(variable))
    }
  }
}


