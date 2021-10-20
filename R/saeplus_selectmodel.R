#' Select Model for Small Area Estimation
#'
#' This function performs model selection assuming data obtained from Open Street Maps, Google Earth Engine and other sources
#' from which the SAEplus package functions pull and process geospatial data
#'
#' @param dt object of class data.table or data.frame
#' @param outcomevar character vector of size 1 naming the outcome variable
#' @param xvars the set of explanatory variables to be selected from
#' @param drop_NA_tags if TRUE, variable names with "NA" in the names are dropped
#' @return a list summarizing the lasso regression output as well as the set of coefficients
#'
#' @export
#'
#' @import data.table hdm bestNormalize
#'

saeplus_selectmodel <- function(dt,
                                outcomevar,
                                xvars,
                                drop_NA_tags = TRUE){

  dt <- as.data.table(dt)

  # ## prepare the set of variables to used for analysis
  # mult_grepl <- function(ids = var_identifier,
  #                        dt.obj = dt){
  #
  #   vars <- colnames(dt.obj)[grepl(ids, colnames(dt.obj))]
  #
  #   return(vars)
  # }
  #
  # xset <- unlist(lapply(var_identifier, mult_grepl))
  # yvar <- dt[,get(outcomevar)]
  # select_variables <- function(tag){
  #
  #   res <- xset[grepl(tag, xset)]
  #   return(res)
  #
  # }
  #
  # xset <- unlist(lapply(var_identifier, select_variables))

  xset <- xvars
  yvar <- dt[, as.numeric(get(outcomevar))]

  ### drop variables with NA
  if (drop_NA_tags == TRUE){

    xset <- xset[!(grepl("NA", xset))]

  }

  ## Normalize y
  yvar <- bestNormalize::orderNorm(yvar)$x.t

  ## replace missing observations with 0
  replace_missings <- function(X){

    X[is.na(X)] <- 0

    return(X)
  }


  xset <- dt[, xset, with = F]
  xset <- xset[,apply(.SD, 2, replace_missings)]
  xset <- as.data.table(xset)

  dt <- cbind(yvar, xset)
  dt <- setDT(dt)



  # Lasso regression
  lasso.reg <-  hdm::rlasso(yvar ~ . , data = dt, post=TRUE)
  coefs <- lasso.reg$beta[lasso.reg$index==TRUE]


  return(lasso.reg)



}
