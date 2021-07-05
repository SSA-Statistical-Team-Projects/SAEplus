#' This function benchmarks the poverty rates to match survey estimates
#'
#' This function ensures that the estimated poverty rates obtained from aggregating the predicted small area estimated
#' matches with the overall poverty rates obtained from the general survey
#'
#' @param hh_dt a household data.frame/data.table object containing area level IDs for which poverty rates were small area
#' estimated
#' @param pop_dt a population level data.frame/data.table object containing geospatial polygons and areas level IDs used in hh_dt
#' for which population level estimates can be computed
#' @param fh_obj an object of class "fh" obtained from emdi::fh(). Area Level IDs must be matching across
#' pop_dt, hh_dt and ebp_obj
#' @param area_id the level at which the poverty areas were estimated
#' @param harea_id a higher level at which intermediate SAE estimates will be computed
#' @param povline national poverty line to be used with the hh_dt object to determine survey level poverty rates
#' @param weight household level weights within the hh_dt object
#' @param excl_outsample whether or not outsample grids should be excluded in the benchmarking exercise
#'
#' @import data.table



fh_calibratepovrate <- function(hh_dt = hh.dt,
                                pop_dt = gin_masterpoly.dt,
                                fh_obj = ginemdi_model2,
                                area_id = "ADM3_CODE",
                                pop_var = "population",
                                harea_id = "ADM1_CODE",
                                povline = 5006362,
                                welfare = "pcexp",
                                weight = "hhweight",
                                excl_outsample = TRUE){

  hh_dt <- as.data.table(hh_dt)
  pop_dt <- as.data.table(pop_dt)

  pop_doms <- unique(pop_dt[,get(area_id)])
  survey_doms <- unique(hh_dt[,get(area_id)])

  if(excl_outsample == TRUE){

    excl_doms <- pop_doms[!(pop_doms %in% survey_doms)]

    pop_dt <- pop_dt[!(get(area_id) %in% excl_doms),]
  }
  #1. Generate population estimate for each SAE level ID by aggregating across grids
  # pop_size <- pop_dt[,sum(get(pop_var)),by = get(area_id)]
  pop_size <- hh_dt[,sum(get(weight)), by = area_id]
  setnames(pop_size, colnames(pop_size), c(area_id, "population"))

  #### merge in h_area ID and poverty rates from EMDI object
  pop_size <- pop_dt[,c(area_id, harea_id),with=F][pop_size, on = area_id]
  pop_size <- unique(pop_size)
  povrate.dt <- as.data.table(fh_obj$ind)
  povrate.dt[,Domain := as.integer(as.character(Domain))]
  setnames(povrate.dt, "Domain", area_id)
  pop_size <- povrate.dt[,c(area_id, "FH"),with=F][pop_size, on = area_id]

  #2.	Use these population estimates for each sampled sub-prefecture as weights to generate small area estimates for
  #   each state. Call these the sae state estimates.
  #         a.	Take the sum of population*poverty rate / total population, by sub-prefecture
  #3. Estimate state level poverty rates
  pop_size[,harea_popsize := sum(population), by = harea_id]
  pop_size[,area_povrate_share := population * FH / harea_popsize]
  pop_size[,harea_povrate := sum(area_povrate_share), by = harea_id]

  hh_dt[,povline := ifelse(get(welfare) < povline, 1, 0)]
  harea_povline <- hh_dt[,weighted.mean(povline, get(weight)), by = harea_id]
  setnames(harea_povline, "V1", "survey_povrate")
  harea_povline <- harea_povline[!is.na(harea_id),]

  #4.	Divide the vector of state survey estimates by sae estimates to get the benchmarking ratio.
  #5.	Multiply the point estimates by the benchmarking ratio.

  pop_size <- harea_povline[pop_size, on = harea_id]

  pop_size[, bmratio := survey_povrate / harea_povrate]

  pop_size[, BM_Head_Count := FH * bmratio]

  return(pop_size)

}




