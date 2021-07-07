#' Select Model for Small Area Estimation
#'
#' This function performs model selection assuming data obtained from Open Street Maps, Google Earth Engine and other sources
#' from which the SAEplus package functions pull and process geospatial data
#'
#' @param dt object of class data.table or data.frame
#' @param outcomevar character vector of size 1 naming the outcome variable
#' @param var_identifier a character vector of common tags
#' @param drop_NA_tags if TRUE, variable names with "NA" in the names are dropped
#' @param method two methods are available, "LASSO" or "GLMNET"
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
                                drop_NA_tags = TRUE,
                                method = "LASSO"){

  dt <- setDT(dt)

  ## prepare the set of variables to used for analysis
  mult_grepl <- function(ids = var_identifier,
                         dt.obj = dt){

    vars <- colnames(dt.obj)[grepl(ids, colnames(dt.obj))]

    return(vars)
  }

  xset <- unlist(lapply(var_identifier, mult_grepl))
  xset <- unique(xset)
  yvar <- dt[,get(outcomevar)]
  select_variables <- function(tag){

    res <- xset[grepl(tag, xset)]
    return(res)

  }

  xset <- unlist(lapply(var_identifier, select_variables))
  xset <- unique(xset)

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

  if(method == "LASSO"){
    lasso.reg <-  hdm::rlasso(yvar ~ . , data = dt, post=TRUE)
    coefs <- lasso.reg$beta[lasso.reg$index==TRUE]
  }

  if(method == "GLMNET"){
    lasso.reg <-  cv.glmnet(as.matrix(x=xset),y=yvar, data = dt, family="gaussian",relax=TRUE)
    coefs <- as.data.frame(as.matrix(coef(lasso.reg, s="lambda.min")))
    coefs$index <- coefs > 0
  }

  # # Lasso regression
  # #lasso.reg <-  hdm::rlasso(yvar ~ . , data = dt, post=TRUE)
  # lasso.reg <-  cv.glmnet(as.matrix(x=xset),y=yvar, data = dt, family="gaussian",relax=TRUE)
  # #coefs <- lasso.reg$beta[lasso.reg$index==TRUE]
  # coefs <- as.data.frame(as.matrix(coef(lasso.reg, s="lambda.min")))
  # coefs$index <- coefs > 0



  return(coefs)



}
