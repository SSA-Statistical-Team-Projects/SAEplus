#' Small Estimation of a Sub Area Level Model
#'
#' This function takes as inputs the geospatial census of remote sensing data, a geocoded household survey with welfare
#' aggregates and then estimates the empirical best predictor of the poverty rate.
#'
#' @param hhsurvey_dt an object of class "sf" and "data.table/data.frame" representing the household unit level data with
#' welfare variable
#' @param geopolycensus_dt an object of class sf, data.table and/or data.frame containing polygon/multipolygon
#' geometries and geospatial indicators
#' @param geopoly_id a string/character variable representing the polygon ID within geopolycensus_dt
#' @param geopopvar a character string for the population count variable name in the geopolycensus_dt
#' @param wgt_vartype a character string representing the weighting type in hhsurvey_dt. The options
#' could be "hh", "pop" i.e. households vs population weights.
#' @param weight a numeric/integer weight variable found within the hhsurvey_dt
#' @param cons_var the dependent variable for small area estimation (typically household per capita consumption)
#' @param pline the national poverty line (ensure the same units as cons_var)
#' @param cand_vars a character vector of candidate explanatory variables to be included in the model
#' selection process
#' @param target_id a character string representing an integer column vector for the admin level at which
#' small area estimates will be computed for the poverty map
#' @param ncpu the number of CPUs for parallelizing the small area estimation algorithm




saeplus_modelsubarea <- function(hhsurvey_dt,
                                 geopolycensus_dt,
                                 geopoly_id,
                                 geopopvar,
                                 wgt_vartype,
                                 weight,
                                 cons_var,
                                 pline,
                                 cand_vars){

  hhsurvey_dt <- st_join(hhsurvey_dt, geopolycensus_dt[,c(geopoly_id, geopopvar)])

  ### compute poverty areas by polygon ID
  hhsurvey_dt <- as.data.table(hhsurvey_dt)

  hhsurvey_dt[, poor := ifelse(get(cons_var) < pline, 1, 0)]

  ###### create population weights

  if (wgt_vartype == "hh") {

    hhsurvey_dt[, popweight := get(size_hh) * get(weight)]

  } else if (wgt_vartype == "pop") {

    hhsurvey_dt[, popweight := get(weight)]

  }

  hhsurvey_dt[, povrate := weighted.mean(x = poor, w = popweight), by = geopoly_id]
  hhsurvey_dt[, sumpopweight := sum(popweight, na.rm = TRUE), by = geopoly_id]

  add.dt <- unique(gin_master.dt[,c(geopoly_id, "povrate", "sumpopweight"),with=F])

  ## include poverty rates in the masterpolygon set and begin estimating models
  geopolycensus_dt <- add.dt[geopolycensus_dt, on = geopoly_id]

  ## model selection on the sub-area model
  selected.vars <- SAEplus::saeplus_selectmodel(dt = geopolycensus_dt[is.na(povrate) == FALSE,],
                                                xvars = cand_vars,
                                                outcomevar = "povrate")

  selected.vars <- names(selected.vars$index[selected.vars$index == TRUE])

  ## model estimation
  ## prepare model for estimation
  subarea_model <- paste(selected.vars, collapse = " + ")
  subarea_model <- as.formula(paste("povrate", subarea_model, sep = " ~ "))


  smp_data <-
    geopolycensus_dt[is.na(povrate) == FALSE,
                          c(selected.vars, "povrate",
                            target_id, "sumpopweight"), with = F]

  pop_data <-
    geopolycensus_dt[!(get(geopopvar) == 0), c(selected.vars, target_id), with = F]


  subarea_model <- emdi::ebp(fixed = subarea_model,
                             pop_data = as.data.frame(pop_data),
                             pop_domains = target_id,
                             smp_data = as.data.frame(smp_data),
                             smp_domains = target_id,
                             threshold = 0,
                             L = 100,
                             transformation = "no",
                             na.rm = TRUE,
                             weights = "sumpopweight",
                             B = 100,
                             cpus = ncpu,
                             MSE = TRUE)


  return(subarea_model)

}
