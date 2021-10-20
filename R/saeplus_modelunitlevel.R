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
#' @param size_hh an integer/numeric for household size variable within the hhsurvey_dt object
#' @param adminshp_dt an object of class sf, data.table and/or data.frame containing administrative
#' level boundaries with multipolygons/polygons geometries
#' @param target_id a character string representing an integer column vector for the admin level at which
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
#' @param cons_var the dependent variable for small area estimation (typically household per capita consumption)
#' @param wgt_vartype a character string representing the weighting type. The options could be "hh", "pop" i.e.
#' households vs population weights.
#' @param weight a numeric/integer weight variable
#' @param create_dummy if TRUE, a dummy variable will be created if dummy_var is specified.
#' @param dummy_var a list of variables from which a dummies will be created for each level.
#' @param ... include any set of arguments available within emdi::ebp() function as you see fit.
#' @param ncpu the number of CPUs for parallelizing the small area estimation algorithm
#' @param pline the national poverty line
#' @param pline_transform select an order norm transformation method. There are three options. The default
#' inclusion_line" option adds the poverty line value stipulated to the vector of welfare vector
#' provided. The order norm value obtained from the normalization process is the converted value.
#' The "interpolation_line" option applies a linear interpolation to outcome_var to estimate a conversion
#' The "limsup_line" option takes the converted value of the greatest welfare value below the poverty line
#' @param result_dir local folder directory where all tables and charts will be stored. If result_dir is not
#' specified, the default is the result of getwd()


saeplus_modelunitlevel <- function(hhsurvey_dt,
                                   hhid_var,
                                   size_hh,
                                   adminshp_dt,
                                   target_id,
                                   geopolycensus_dt,
                                   geopopvar,
                                   geopoly_id = "id",
                                   crs_set = rep(4326, 3),
                                   agr_set = rep("constant", 3),
                                   cand_vars,
                                   cons_var,
                                   wgt_vartype = "hh",
                                   weight,
                                   create_dummy = TRUE,
                                   dummy_var,
                                   ...,
                                   ncpu = 30,
                                   pline = 5006362,
                                   pline_transform = "inclusion_line",
                                   result_dir){

  ### now we are ready create a synthetic census
  ##### first compute average number of households in each grid
  ##### it is important that hh_dt and shp_dt below are the exact same variable type
  grid_hhcount.dt <- saeplus_hhestpoly(geo_dt = geopolycensus_dt[,c(geopoly_id, geopopvar, "geometry")],
                                       hh_dt = hhsurvey_dt,
                                       shp_dt = adminshp_dt) ##ind_estimate is the household size estimated

  ##drop duplicated IDs
  grid_hhcount.dt <- as.data.table(grid_hhcount.dt)
  grid_hhcount.dt <- grid_hhcount.dt[!(duplicated(get(geopoly_id))),]

  geopolycensus_dt <- grid_hhcount.dt[,c(geopoly_id, "ind_estimate"),with=F][geopolycensus_dt, on = geopoly_id]

  rm(grid_hhcount.dt) ##clean up the environment a little by removing the grid_hhcount.dt since we dont need it anymore

  hhcensus_dt <- saeplus_gencensus(poly_dt = geopolycensus_dt)

  ## finally, let's include admin area information into the polygon census data
  geocentcensus_dt <- sf::st_centroid(sf::st_as_sf(geopolycensus_dt[,c(geopoly_id, "geometry"),with = F],
                                                   agr = agr_set[3],
                                                   crs = crs_set[3]))

  adminshp_dt <- sf::st_as_sf(adminshp_dt, agr = agr_set[2], crs = agr_set[2])

  geocentcensus_dt <- st_join(geocentcensus_dt, adminshp_dt)

  geocentcensus_dt <- as.data.table(geocentcensus_dt)
  geocentcensus_dt <- geocentcensus_dt[!(duplicated(get(geopoly_id))),]

  hhcensus_dt <- geocentcensus_dt[hhcensus_dt, on = geopoly_id]

  ###### ALL THE DATA IS PREPPED NOW WE ARE READY FOR MODEL SELECTION
  if (create_dummy == TRUE) {

    cand_vars <- c(cand_vars, unique(hhsurvey_dt[, get(dummy_var), with = F]))

  }
  selected_vars <- SAEplus::saeplus_selectmodel(dt = hhsurvey_dt,
                                                xvars = cand_vars,
                                                outcomevar = cons_var)

  selected_vars <- names(selected_vars$index[selected_vars$index == TRUE])

  ###### create population weights

  if (wgt_vartype == "hh") {

    hhsurvey_dt[, popweight := get(size_hh) * get(weight)]

  } else if (wgt_vartype == "pop") {

    hhsurvey_dt[, popweight := get(weight)]

  }

  ###### now put together the EMDI model
  unit_model <- paste(selected_vars, collapse = " + ")
  unit_model <- as.formula(paste(cons_var, unit_model, sep = " ~ "))

  ###### transform the outcome variable to the order norm
  pline <- saeplus_ordernormpl(pcexp = hhsurvey_dt[,get(cons_var)],
                               npl_value = pline)
  hhsurvey_dt[,pcexp := orderNorm(get(cons_var))$x.t]

  ###### build the unit level model
  vars <- c(selected_vars, "pcexp", target_id)
  census_vars <- vars[!(vars %in% "pcexp")]

  message("Data Prep and Model Selection complete. \n\n")

  message("The imputation process is being initiated ...\n\n")

  emdi_model <- emdi::ebp(fixed = unit_model,
                          pop_data = as.data.frame(hhcensus_dt[,census_vars, with = F]),
                          pop_domains = target_id,
                          smp_data = as.data.frame(hhsurvey_dt[,vars, with = F]),
                          smp_domains = target_id,
                          threshold = pline[[pline_transform]],
                          L = 100,
                          transformation = "no",
                          na.rm = TRUE,
                          weights = "popweight",
                          B = 100,
                          cpus = ncpu,
                          MSE = TRUE)


  write.excel(emdi_model,
              file = paste(result_dir, "emdi_basicmodel.xlsx", sep = "/"),
              indicator = "all",
              MSE = TRUE,
              CV = TRUE)


  #### benchmark poverty estimates
  gin_benchmark <- saeplus_calibratepovrate(pop_dt = hhcensus_dt,
                                            hh_dt = hhsurvey_dt,
                                            weight = "popweight",
                                            povline = pline,
                                            pop_var = "ind_estimate")
  #### create actual poverty map
  ## include the benchmarked results
  replace.dt <- gin_benchmark[,c(target_id, "BM_Head_Count")]

  setnames(replace.dt, colnames(replace.dt), c("Domain", "BM_Head_Count"))

  replace.dt[, Domain := as.factor(Domain)]

  emdi_model$ind <- left_join(emdi_model$ind, replace.dt, by = "Domain")

  emdi_model$ind$BM_Head_Count[is.na(emdi_model$ind$BM_Head_Count)] <-
    emdi_model$ind$Head_Count[is.na(emdi_model$ind$BM_Head_Count)] ##replacing NAs in BM_Head_Count with Head_Count

  emdi_model$ind$Head_Count <- NULL
  colnames(emdi_model$ind)[colnames(emdi_model$ind) %in% "BM_Head_Count"] <- "Head_Count"

  #### create the file with the EMDI estimates
  write.excel(emdi_model,
              file = paste(result_dir, "emdi_bmmodel.xlsx"),
              indicator = "all",
              MSE = TRUE,
              CV = TRUE)

  ### create the plot for the poverty grid
  geo.dt <- as.data.table(adminshp_dt)

  povgrid.dt <- as.data.table(emdi_model$ind)
  setnames(povgrid.dt, "Domain", target_id)
  povgrid.dt[,get(target_id) := as.integer(as.character(get(target_id)))]

  povgrid.dt <- geo.dt[povgrid.dt, on = target_id]
  povgrid.dt <- povgrid.dt[is.na(Head_Count) == "FALSE",]


  povgrid.dt <- st_as_sf(povgrid.dt, agr = "constant", crs = 4326)

  figure1 <-
    tm_shape(povgrid.dt) +
    tm_polygons("Head_Count", title = "Headcount Rates") +
    tm_layout(title = "Headcount Poverty Rates",
              title.size = 1.1,
              title.position = c("center", "top"))

  tmap_save(tm = figure1, filename = paste(result_dir, "povmap1.pdf", sep = "/"))
}
