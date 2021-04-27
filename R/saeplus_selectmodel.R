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
                                var_identifier = c("roaddensity", "count_", "length",
                                                   "_pointcount", "2018", "2019"),
                                drop_NA_tags = TRUE){

  dt <- setDT(dt)

  ## prepare the set of variables to used for analysis
  #xset <- colnames(dt)[!(colnames(dt) %in% outcomevar]
  xset <- colnames(dt)

  # select_variables <- function(tag){
  #
  #   res <- xset[grepl(tag, xset)]
  #   return(res)
  #
  # }
  #
  # zset <- unlist(lapply(var_identifier, select_variables))


  dset <- ""
  for (i in 1:6) {
    zset <- xset[!grepl(var_identifier[i], xset)]

    dset <- c(dset,xset[!(xset %in% unlist(zset))])
  }

  xset <- c(dset[2:152],"bld_count", "bld_cvarea","bld_meanarea")


  ## select x and y variables
  xset <- dt[,xset,with=F]

  ### drop variables with NA
  if (drop_NA_tags == TRUE){

    #xset <- xset[!(grepl("NA", xset))]
    xset <- xset[,which(unlist(lapply(xset, function(x)!all(is.na(x))))),with=F]
    xset <- mutate(xset, across(everything(), ~replace_na(.x, 0)))

  }

  yvar <- dt[,outcomevar,with=F]
  yvar <- orderNorm(yvar[[1]])$x.t

  dt <- cbind(yvar, xset)

  ### lasso regression
  lasso.reg <-  rlasso(yvar ~ . , data = dt, post=TRUE)
  coefs <- lasso.reg$beta[lasso.reg$index==TRUE]


  return(list(regression_results = summary(lasso.reg),
              coefficients = coefs))



}
