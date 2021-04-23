#' Select Model for Small Area Estimation
#'
#' A function to select the set of geospatial variables to be used for a household level small area estimation model
#'
#' @param dt object of class data.table or data.frame
#' @param outcomevar character vector of size 1 naming the outcome variable
#' @param dropvars a list of variables to be omitted by the selection algorithm, all other variables will be included
#'
#' @return a list summarizing the lasso regression output as well as the set of coefficients
#'
#' @export
#'
#' @import data.table sf haven bestNormalize hdm dplyr tidyr glmnet BBmisc


model_selection <- function(dt, outcomevar, dropvars) {

  # Preparing dt
  Y_t <- orderNorm(dt[ ,outcomevar])
  Y_t <- Y_t$x.t

  X <- dt[ ,as.factor(prefecture),waterperm_2018julsep:length_unclassified]
  X <- X[ ,-c("id.y")]
  X[is.na(X)] <- 0

  # lengths <- X %>% dplyr::select(starts_with("length"))
  # densities <- X %>% dplyr:: select(starts_with("roaddensity"))
  # counts <- X %>% dplyr:: select(starts_with("count"))
  # points <- X %>% dplyr:: select(ends_with("pointcount"))


  X <- X[ ,waterperm_2018julsep:bld_totallength]
  X <- dt.table(X,lengths,counts,points)


  # Lasso regression
  lasso.reg <-  rlasso(Y_t~ .,dt=X,post=TRUE)
  coefs <- lasso.reg$beta[lasso.reg$index==TRUE]



  return(list(regression_results = summary(lasso.reg),
              coefficients = coefs))


}
