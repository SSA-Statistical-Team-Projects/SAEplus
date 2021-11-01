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


  create_dummy <- function(X){

    bin_col <- dt[,ifelse(get(var) == X, 1, 0)]
    return(bin_col)

  }

  results <- lapply(levels, create_dummy)
  results <- as.data.table(do.call(cbind, results))
  colnames(results) <- levels

  return(results)

}
