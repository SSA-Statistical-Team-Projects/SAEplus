#' Create report ready tables from EBP output
#'
#' This function takes the results of the EBP function and creates more outputs to be included in reports
#'
#' @param ebp_obj an object of class "emdi", "ebp"
#' @param hhsample_dt household level survey dataset containing grid ID
#' @param hhcensus_dt household level population dataset containing grid ID
#' @param samplegrid grid ID variable in hhsample_dt
#' @param censusgrid grid ID variable in hhcensus_dt
#'
#'
#'

saeplus_regtable <- function(ebp_obj = ginemdi_model2,
                             hhsample_dt = gin_master.dt,
                             hhcensus_dt = gin_mastercentroid.dt,
                             samplegrid = "id",
                             censusgrid = "id"){

  ## regression table
  dt <- data.table(Variable = names(ebp_obj$model$coefficients$fixed),
                   Coefficient = ebp_obj$model$coefficients$fixed,
                   StandardError = sqrt(diag(ebp_obj$model$varFix)))

  names <- c("# of Sample Households", "# of Populated Grids in Sample", "# of LGAs in Sample",
             "# of Census Households", "# of Populated Grids in Census", "# of LGAs in Census",
             "Variance of LGA random effect",
             "Variance of Household Residual",
             "Proportion of Variance of Residual due to LGA random effect")

  hhsample_dt <- as.data.table(hhsample_dt)
  hhcensus_dt <- as.data.table(hhcensus_dt)
  popgridsamplecount <- hhsample_dt[, length(unique(get(samplegrid)))]
  popgridcensuscount <- hhcensus_dt[, length(unique(get(censusgrid)))]
  lgasamplecount <- ebp_obj$framework$N_dom_smp
  lgapopcount <- ebp_obj$framework$N_dom_smp + ebp_obj$framework$N_dom_unobs
  varlgare <- ebp_obj$model$sigma^2

  ### create dataframe for estimating household residual and residual due to LGA random effect
  resid.dt <- as.data.table(ebp_obj$model$residuals)
  colnames(resid.dt) <- c("hh", "lga")

  varhhres <- sd(resid.dt$hh)^2

  propvar <- varlgare / varhhres

  diagnostics <- data.table(Stats = names,
                            Values = c(ebp_obj$framework$N_smp, popgridsamplecount, lgasamplecount,
                                       ebp_obj$framework$N_pop, popgridcensuscount, lgapopcount,
                                       varlgare, varhhres, propvar))
  ## create object of other model diagnostics
  return(list(regtable = dt,
              diagnostics = diagnostics))




}

