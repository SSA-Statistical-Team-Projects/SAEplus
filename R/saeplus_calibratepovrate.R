#' This function benchmarks the poverty rates to match survey estimates
#'
#' This function ensures that the estimated poverty rates obtained from aggregating the predicted small area estimated
#' matches with the overall poverty rates obtained from the general survey
#'
#' @param hh_dt a household data.frame/data.table object containing area level IDs for which poverty rates were small area
#' estimated
#' @param pop_dt a population level data.frame/data.table object containing geospatial polygons and areas level IDs used in hh_dt
#' for which population level estimates can be computed
#' @param ebp_obj an object of class "emdi" "ebp" obtained from small area estimation containing area level poverty rates.
#' Area Level IDs must be matching across pop_dt, hh_dt and ebp_obj
#'
#' @import data.table


saeplus_calibratepovrate <- function(hh_dt = hh.dt,
                                     pop_dt = gin_masterpoly.dt,
                                     ebp_obj = ginemdi_model2,
                                     area_id = "ADM3_CODE",
                                     pop_var = "population"){

  hh_dt <- as.data.table(hh_dt)
  pop_dt <- as.data.table(pop_dt)
  ## Generate population estimate for each SAE level ID by aggregating across grids
  pop_size <- pop_dt[,sum(get(pop_var)),by = get(area_id)]

  #2.	Use these population estimates for each sampled sub-prefecture as weights to generate small area estimates for
  #   each state. Call these the sae state estimates.
  #         a.	Take the sum of population*poverty rate / total population, by sub-prefecture



  return(pop_size)

}
