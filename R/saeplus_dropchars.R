#' Remove non-numeric characters
#'
#' This function removes non-numeric characters from one or more specified columns within data.frame/data.table/matrix
#' objects. Then the results will be converted to integers
#'
#' @param dt a data.table/data.frame/matrix object
#' @param vars one or more variables from which non-characters will be removed
#'

saeplus_dropchars <- function(dt, vars){

  dt <- as.data.table(dt)

  for (i in seq_along(vars)){

    dt[, (vars[i]) := gsub("[A-Za-z]", "", get(vars[i]))]
    dt[, (vars[i]) := gsub("[[:punct:]]", "", get(vars[i]))]

    dt[, (vars[i]) := as.integer(get(vars[i]))]

  }

  return(dt)


}
