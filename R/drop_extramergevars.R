#' Clean up data by dropping variables systematically
#'
#' This simple helper function will drop duplicated variable names created via data.table merges
#'
#' @param dt data.table or data.frame object
#' @param string_match a string match to identify the set of additional variables to be dropped. "i\\."
#' will drop all variables that contain duplicated variables from a data.table merge
#'

drop_extramergevars <- function(dt,
                                string_match = "i\\."){

  dt <- as.data.table(dt)

  names_drop <- colnames(dt)[grepl(string_match, colnames(dt))]
  dt[,(names_drop) := NULL]

  return(dt)
}
