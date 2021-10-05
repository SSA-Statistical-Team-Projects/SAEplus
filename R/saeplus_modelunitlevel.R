#' Estimate the unit level model
#'
#' This function prepares remote sensing data for imputation by checking the admin level shapefiles for geometric
#' consistency. Prior to the imputation, an algorithm is used to select an appropriate set of variables that best
#' predicts the outcome variable. The household survey data is used to create a synthetic census of households.
#' The empirical best predictor is estimated using the household survey data and is used as the basis for imputation
#' into the synthetic census.
#'
#' @param hhsurvey_dt object of class data.frame/data.table corresponding to household survey data (unit level data)
#' @param hhsurvey_lat the latitude variable within the hh_dt object
#' @param hhsurvey_lon the longitude variable within the hh_dt object
#' @param hhid_var the household ID variable from the hhsurvey_dt object
#' @param hhsize a household size variable within the hhsurvey_dt object
#' @param adminshp_dt an object of class sf, data.table and/or data.frame containing administrative
#' level boundaries with multipolygons/polygons geometries
#' @param admin_id a character string representing a column vector in hhsurvey_dt for the admin level at which
#' small area estimates will be computed for the poverty map
#' @param geopolycensus_dt an object of class sf, data.table and/or data.frame containing polygon/multipolygon
#' geometries and geospatial indicators
#' @param geopoly_id a string/character variable representing the polygon ID within geopolycensus_dt
#' @param geopopvar a character string for the population count variable name in the geopolycensus_dt
#' @param crs_set an integer list of the coordinate reference systems for of the aforementioned objects
#' i.e. the CRS for hh_dt, adminshp_dt and geopolycensus_dt in this order
#' @param agr_set a character/string list representing attribute-geometry-relationships specified for each
#' non-geometry attribute column and how it relates to the geometry, and can have one of following values
#' "constant", "aggregate" and "identity". The default is constant. See details for more. The AGR will listed
#' in the same order as the crs_set.
#' @param cand_vars a character vector of candidate explanatory variables to be included in the model
#' selection process
#' @param outcome_var the dependent variable for small area estimation (typically household per capita expenditure)
#' @param wgt_vartype a character string representing the weighting type. The options could be "hh", "pop" i.e.
#' households vs population weights.
#' @param weight the weight variable
#' @param create_dummy if TRUE, a dummy variable will be created if dummy_var is specified.
#' @param dummy_var a list of variables from which a dummies will be created for each level.


saeplus_modelunitlevel <- function(hhsurvey_dt,
                                   hhsurvey_lat,
                                   hhsurvey_lon,
                                   hhid_var,
                                   hhsize,
                                   adminshp_dt,
                                   hhgeocodes_dt = NULL,
                                   geopolycensus_dt,
                                   geopopvar,
                                   crs_set = rep(4326, 3),
                                   agr_set = rep("constant", 3),
                                   cand_vars,
                                   outcome_var,
                                   wgt_vartype = "hh",
                                   weight,
                                   create_dummy = TRUE,
                                   dummy_var){

  ## some basic cleaning to start off
  ### drop any extra geometry variables created in the merging process

  drop_extramergevars(dt = geopolycensus_dt)

  hhsurvey_dt <- sf::st_as_sf(hhsurvey_dt,
                              agr = agr_set[1],
                              coords = c(hhsurvey_lat, hhsurvey_lon),
                              crs = crs_set[1])

  geopolycensus_dt <- sf::st_as_sf(geopolycensus_dt, crs = crs_set[3], agr = agr_set[3])

  hhsurvey_dt <- sf::st_join(hhsurvey_dt, geopolycensus_dt)

  #### make some quick additions to the hhsurvey_dt
  ###### including the adminshp_dt data
  hhsurvey_dt <- st_join(hhsurvey_dt, adminshp_dt)

  #### drop duplicated observations
  hhsurvey_dt <- hhsurvey_dt[!duplicated(hhsurvey_dt[,hhid_var]),]

  hhsurvey_dt <- as.data.table(hhsurvey_dt)


  ### create admin one level dummies
  hhsurvey_dt <- saeplus_dummify(dt = hhsurvey_dt, var = dummy_var)


  ### now we are ready create a synthetic census
  ##### first compute average number of households in each grid
  ##### it is important that hh_dt and shp_dt below are the exact same variable type
  grid_hhcount.dt <- saeplus_hhestpoly(geo_dt = geopolycensus_dt[,c(geopoly_id, geopopvar, "geometry")],
                                       hh_dt = hhsurvey_dt,
                                       shp_dt = adminshp_dt) ##ind_estimate is the household size estimated

  grid_hhcount.dt <- grid_hhcount.dt[!duplicated(grid_hhcount.dt[,geopoly_id]),] ##drop duplicated IDs

  grid_hhcount.dt <- as.data.table(grid_hhcount.dt)

  geopolycensus_dt <- grid_hhcount.dt[,c(geopoly_id, "ind_estimate")][geopolycensus_dt, on = "id"]

  rm(grid_hhcount.dt) ##clean up the environment a little by removing the grid_hhcount.dt since we dont need it anymore

  hhcensus_dt <- saeplus_gencensus(poly_dt = geopolycensus_dt)

  ## finally, let's include admin area information into the polygon census data
  geocentcensus_dt <- sf::st_centroid(sf::st_as_sf(geopolycensus_dt[,c(geopoly_id, "geometry")],
                                                   agr = agr_set[3],
                                                   crs = crs_set[3]))

  adminshp_dt <- sf::st_as_sf(adminshp_dt, agr = agr_set[2], crs = agr_set[2])

  geocentcensus_dt <- st_join(geocentcensus_dt, adminshp_dt)
  geocentcensus_dt <- geocentcensus_dt[!duplicated(geocentcensus_dt[,geopoly_id]),]

  geocentcensus_dt <- as.data.table(geocentcensus_dt)

  hhcensus_dt <- geocentcensus_dt[hhcensus_dt, on = geopoly_id]

  ###### ALL THE DATA IS PREPPED NOW WE ARE READY FOR MODEL SELECTION
  if (create_dummy == TRUE) {

    cand_vars <- c(cand_vars, unique(hhsurvey_dt[, get(dummy_var), with = F]))

  }
  selected_vars <- SAEplus::saeplus_selectmodel(dt = hhsurvey_dt,
                                                xvars = cand_vars,
                                                outcomevar = outcome_var)

  selected_vars <- names(selected_vars$index[selected_vars$index == TRUE])

  ###### create population weights
  if (wgt_vartype == "hh") {

    hhsurvey_dt[, popweight := hhsize * weight]

  } else if (wgt_vartype == "pop") {

    hhsurvey_dt[, popweight := weight]

  }

  ###### now put together the EMDI model
  unit_model <- paste(selected_vars, collapse = " + ")
  unit_model <- as.formula(paste(outcome_var, unit_model, sep = " ~ "))

  ###### transform the outcome variable to the order norm
  pline <- saeplus_ordernormpl(pcexp = hhsurvey_dt[,pcexp])
  hhsurvey_dt[,pcexp := orderNorm(pcexp)$x.t]

  ###### build the unit level model
  vars <- c(selected_vars, outcome_var, admin_id)
  emdi_model <- emdi::ebp(fixed = unit_model,
                          pop_data = as.data.frame(gin_hhcensus.dt[,vars, with = F]),
                          pop_domains = admin_id,
                          smp_data = as.data.frame(gin_hhsurvey.dt[,vars, with = F]),
                          smp_domains = admin_id,
                          threshold = pline,
                          L = 100,
                          transformation = "no",
                          na.rm = TRUE,
                          weights = "popweight",
                          B = 100,
                          cpus = 30,
                          MSE = TRUE)

}
