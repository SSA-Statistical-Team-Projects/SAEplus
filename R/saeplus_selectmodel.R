#' Select Model for Small Area Estimation
#'
#' This function performs model selection assuming data obtained from Open Street Maps, Google Earth Engine and other sources
#' from which the SAEplus package functions pull and process geospatial data
#'
#' @param dt object of class data.table or data.frame
#' @param outcomevar character vector of size 1 naming the outcome variable
#' @param var_identifier a character vector of common tags
#' @param drop_NA_tags if TRUE, variable names with "NA" in the names are dropped
#' @return a list summarizing the lasso regression output as well as the set of coefficients
#'
#' @export
#'
#' @import data.table sf haven bestNormalize hdm dplyr tidyr glmnet BBmisc
#'
#'

saeplus_selectmodel <- function(dt,
                                outcomevar = "pc_exp",
                                var_identifier = c("roaddensity", "count", "length",
                                                   "pointcount", "bld", "2018", "2019"),
                                drop_NA_tags = FALSE){

  dt <- setDT(dt)

  ## prepare the set of variables to used for analysis
  xset <- colnames(dt)[!(colnames(dt) %in% outcomevar)]

  select_variables <- function(tag){

    res <- xset[grepl(tag, xset)]
    return(res)

  }

  xset <- unlist(lapply(var_identifier, select_variables))

  ### drop variables with NA
  if (drop_NA_tags == TRUE){

    xset <- xset[!(grepl("NA", xset))]
  }

  ### select x and y variables
  xset <- dt[,xset,with=F]
  yvar <- dt[,outcomevar]
  dt <- cbind(yvar, xset)

  ### lasso regression
  lasso.reg <-  rlasso(yvar ~ . , data = dt, post=TRUE)
  coefs <- lasso.reg$beta[lasso.reg$index==TRUE]


  return(list(regression_results = summary(lasso.reg),
              coefficients = coefs))



}
