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
                                outcomevar = "pcexp",
                                var_identifier = c("roaddensity_", "count_", "length_",
                                                   "_pointcount", "bld_", "_2018", "_2019"),
                                drop_NA_tags = TRUE){

  dt <- setDT(dt)

  # Prepare the set of variables to used for analysis
  xset <- colnames(dt)

  ## Selection of dependent variables
  dset <- ""
  for (i in 1:7) {
    zset <- xset[!grepl(var_identifier[i], xset)]

    dset <- c(dset,xset[!(xset %in% unlist(zset))])
  }

  ### To ensure that there are no duplicates in the set of selected variables
  dset <- dset[!duplicated(dset)]

  xset <- dset[2:length(dset)]


  # Extract x and y variables from the master set

  xset <- dt[,xset,with=F]

  ## drop  variables whose values are all missing
  ## and replacement of missing values by zero for the remaining variables
  if (drop_NA_tags == TRUE){
    xset <- xset[,which(unlist(lapply(xset, function(x)!all(is.na(x))))),with=F]
    xset <- mutate(xset, across(everything(), ~replace_na(.x, 0)))

  }

  yvar <- dt[,outcomevar,with=F]

  ## Normalize y
  yvar <- orderNorm(yvar[[1]])$x.t

  dt <- cbind(yvar, xset)

  # Lasso regression
  lasso.reg <-  rlasso(yvar ~ . , data = dt, post=TRUE)
  coefs <- lasso.reg$beta[lasso.reg$index==TRUE]


  return(list(regression_results = summary(lasso.reg),
              coefficients = coefs))



}
