#' Compute the geospatial grid level estimates
#'
#' This function estimates geospatial grid level estimates from household surveys indicators
#'
#' @param geo_dt sf/st dataframe object including the polygon geometry included
#' @param hh_dt household survey data with indicator(s)
#' @param shp_dt shapefile with desired administrative level of aggregation
#' @param admin_var a variable within hh_dt for the level at which the indicator will be estimated
#' @param ind_var the indicator variable of interest within hh_dt
#' @param weight_var the weight variable of interest within hh_dt
#' @param FUN the aggregation function to be applied (currently MUST take ind_var and weight_var)
#' @param geo_dt_var existing geospatial indicator (such as population) to be used in estimating grid level estimates
#'
#' @return data.table/data.frame object with grid level estimates
#'
#' @import stats
#'
#' @export


saeplus_hhestpoly <- function(geo_dt,
                              hh_dt,
                              shp_dt,
                              admin_var = "ADM3_CODE",
                              ind_var = "hhsize",
                              weight_var = "hhweight",
                              FUN = "weighted.mean"){

  ## first aggregate variable at the admin_var level within hh_dt
  hh_dt <- setDT(hh_dt)
  shp_dt <- setDT(shp_dt)
  ind_estimate_dt <- hh_dt[, do.call(FUN, list(get(ind_var), get(weight_var))), by = get(admin_var)]

  colnames(ind_estimate_dt) <- c(admin_var, "ind_estimate")

  ind_estimate_dt <- ind_estimate_dt[shp_dt, on = admin_var]
  est_value <- hh_dt[, do.call(FUN, list(get(ind_var), get(weight_var)))]

  ind_estimate_dt[is.na(ind_estimate) == TRUE, ind_estimate := est_value]

  ### first make sure both geospatial objects are the correct crs specifications

  ind_estimate_dt <- st_as_sf(ind_estimate_dt, crs = 4326, agr = "constant")
  geo_dt <- st_as_sf(geo_dt, crs = 4326, agr = "constant")

  centroid_dt <- st_centroid(geo_dt)

  centroid_dt <- st_join(ind_estimate_dt, centroid_dt)

  return(centroid_dt)

}
