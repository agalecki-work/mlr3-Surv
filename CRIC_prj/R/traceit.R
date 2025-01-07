traceit = function(msg, object, traceon = FALSE){

  if (traceon){
     message(msg)
     str(object)
  } else {
     invisible(NULL)
}}
