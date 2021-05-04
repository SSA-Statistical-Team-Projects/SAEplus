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
#' @import stats tidyr hutilscpp
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

  ### find the centroid in each polygon of geo_dt
  geo_dt$centroid <- st_centroid(geo_dt)

  #### separate both geometries into lat and lon
  geo_dt$lat <- st_coordinates(geo_dt$centroid)[,1]
  geo_dt$lon <- st_coordinates(geo_dt$centroid)[,2]

  hh_dt$lat <- st_coordinates(hh_dt)[,1]
  hh_dt$lon <- st_coordinates(hh_dt)[,2]

  ### find the point in hh_dt that is closest to the points in geo_dt
  geo_dt$matches <- hutilscpp::match_nrst_haversine(lat = geo_dt$lat,
                                                    lon = geo_dt$lon,
                                                    addresses_lat = hh_dt$lat,
                                                    addresses_lon = hh_dt$lon)[,1]





  return(geohh_dt)

}
