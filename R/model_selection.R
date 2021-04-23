#'
#'
#' @param data Data source name
#' @param outcomevar Independent variable
#'
#' @return summary of the lasso regression and a list of the coefficients of each regressor
#'
#' @export
#'
#' @import data.table sf haven bestNormalize hdm dplyr tidyr glmnet BBmisc



model_selection(data,
                outcomevar){

  # Preparing data
  Y_t <- orderNorm(data[ ,outcomevar])
  Y_t <- Y_t$x.t

  X <- data[ ,as.factor(prefecture),waterperm_2018julsep:length_unclassified]
  X <- X[ ,-c("id.y")]
  X[is.na(X)] <- 0

  lengths <- X %>% dplyr::select(starts_with("length"))
  densities <- X %>% dplyr:: select(starts_with("roaddensity"))
  counts <- X %>% dplyr:: select(starts_with("count"))
  points <- X %>% dplyr:: select(ends_with("pointcount"))


  X <- X[ ,waterperm_2018julsep:bld_totallength]
  X <- data.table(X,lengths,counts,points)


  # Lasso regression
  lasso.reg <-  rlasso(Y_t~ .,data=X,post=TRUE)
  coefs <- lasso.reg$beta[lasso.reg$index==TRUE]



  return(list(regression_results = summary(lasso.reg),
              coefficients = br_poly))

}

