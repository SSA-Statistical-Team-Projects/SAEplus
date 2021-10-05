#' Create dummy variables
#'
#' This function will create a set of dummy variables from the level of another variable
#'
#' @param dt an object of class data.table/data.frame
#' @param var a variable name within dt
#'

saeplus_dummify <- function(dt, var){

  dt <- as.data.table(dt)

  levels <- dt[,unique(get(var))]

  for (i in seq_along(levels)){

    dt[,c(levels[i]) := ifelse(get(var) == levels[i], 1, 0)]

  }

  return(dt)
}
