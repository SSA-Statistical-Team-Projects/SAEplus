#' Compute the geospatial grid level estimates
#'
#' This function estimates geospatial grid level estimates from household surveys indicators
#'
#' @param geo_dt sf/st dataframe object including the polygon geometry included
#' @param hh_dt household survey data with indicator(s) to be estimated at the grid level
#' @param admin_var a variable within hh_dt for the level at which the indicator will be estimated
#' @param ind_var the indicator variable of interest within hh_dt
#' @param weight_var the weight variable of interest within hh_dt
#' @param FUN the aggregation function to be applied (currently MUST take ind_var and weight_var)
#' @param geo_dt_var existing geospatial indicator (such as population) to be used in estimating grid level estimates
#'
#' @return data.table/data.frame object with grid level estimates
#'
#' @import stats tidyr
#'


saeplus_hhestpoly <- function(geo_dt,
                              hh_dt,
                              admin_var = "ADM3_CODE",
                              ind_var = "hhsize",
                              weight_var = "hhweight",
                              FUN = "weighted.mean",
                              geo_dt_var = "population"){

  ## first aggregate variable at the admin_var level within hh_dt
  hh_dt <- setDT(hh_dt)
  hh_dt[, ind_estimate := do.call(FUN, list(get(ind_var), get(weight_var))), by = get(admin_var)]

  ## merge hh_dt with geo_dt
  ### first make sure both geospatial objects are the correct crs specifications

  hh_dt <- st_as_sf(hh_dt, crs = 4326, agr = "constant")
  geo_dt <- st_as_sf(geo_dt, crs = 4326, agr = "constant")


  geohh_dt <- st_join(geo_dt, hh_dt[,c(admin_var, ind_var, weight_var, "ind_estimate")])

  geohh_dt <- setDT(geohh_dt)
  geohh_dt[, polygon_est := get(geo_dt_var)/ind_estimate]

  ids <- colnames(geo_dt)

  ## re-estimate/re-calibrate polygons
  geohh_dt[,polygon_est := weighted.mean(x = polygon_est, w = get(weight_var), na.rm = TRUE), by = get(ids)]

  ids <- ids[!grepl("geometry", ids)]
  geohh_dt <- unique(geohh_dt[is.na(get(admin_var)) == FALSE, c(ids, "polygon_est"), with = F])

  return(geohh_dt)

}
