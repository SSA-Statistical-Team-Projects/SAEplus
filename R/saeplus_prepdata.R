
#' Prepare geospatial data for small area estimation
#'
#' This function checks for consistency of the shapefiles, boundary level files, survey data and geospatial data
#' prior to the small area estimation modelling
#'
#' @param hhsurvey_dt geocoded household survey data (use st_as_sf() to create geometry column if not available needed
#' @param hhid_var household ID variable within the hhsurvey_dt object
#' @param adminshp_dt data.frame/data.table/sf object with administrative boundaries defined
#' @param hhgeo_dt an sf/data.table/data.frame containing household latitude and longitude
#' @param hhcoords a character string of length 2 for the longitude and latitude variables (respectively) in
#' @param adminvars list of every admin name and admin code pair in order
#' (e.g c("ADM1_NAME", "ADM1_CODE", "ADM2_NAME", "ADM2_CODE", ...))
#' @param geopolycensus_dt an object of class sf, data.table and/or data.frame containing polygon/multipolygon
#' geometries and geospatial indicators
#' @param target_id a character string representing a column vector in hhsurvey_dt for the admin level at which
#' small area estimates will be computed for the poverty map
#' @param crs_set an integer list of the coordinate reference systems for of the aforementioned objects
#' i.e. the CRS for hh_dt, adminshp_dt and geopolycensus_dt in this order
#' @param agr_set a character/string list representing attribute-geometry-relationships specified for each
#' non-geometry attribute column and how it relates to the geometry, and can have one of following values
#' "constant", "aggregate" and "identity". The default is constant. See details for more. The AGR will listed
#' in the same order as the crs_set.
#' @param drop_chars if TRUE, characters (usually country ISO3) in the admin level names and codes will be dropped.
#' The admin level codes must be integers for the emdi::ebp function to work.
#'
#' @return a list of geospatial objects for household, synthetic census
#' and a check of the administrative level info



saeplus_prepdata <- function(hhsurvey_dt,
                             hhid_var,
                             adminshp_dt,
                             hhgeo_dt,
                             hhcoords,
                             geosurvey_mergevars,
                             adminvars = c("ADM1_NAME", "ADM1_CODE",
                                           "ADM2_NAME", "ADM2_CODE",
                                           "ADM3_NAME", "ADM3_CODE"),
                             geopolycensus_dt,
                             target_id = "ADM3_CODE",
                             crs_set = rep(4326, 4),
                             agr_set = rep("constant", 4),
                             drop_chars = TRUE){

  hhsurvey_dt <- as.data.table(hhsurvey_dt)


  if ((is.null(hhgeo_dt) == FALSE)) {

    hhgeo_dt <- as.data.table(hhgeo_dt)
    hhsurvey_dt <- hhgeo_dt[hhsurvey_dt, on = geosurvey_mergevars]

    hhsurvey_dt <- sf::st_as_sf(hhsurvey_dt,
                                agr = agr_set[1],
                                coords = hhcoords,
                                crs = crs_set[1])

  }



  ## some basic cleaning to start off
  ### drop any extra geometry variables created in the merging process

  geopolycensus_dt <- drop_extramergevars(dt = geopolycensus_dt)
  adminshp_dt <- as.data.table(adminshp_dt)
  hhsurvey_dt <- as.data.table(hhsurvey_dt)
  ## check the name and code combinations at each admin level

  adm_check <- function(x){

    x <- 2*x
    res <- as.data.table(adminshp_dt[,table(get(adminvars[x-1]), get(adminvars[x]))])
    res <- res[N > 0,]
    res <- res[,c("V1", "V2")]
    colnames(res) <- c(adminvars[x-1], adminvars[x])
    return(res)

  }

  adm_namecode_check <- lapply(1:(length(adminvars)/2), adm_check)

  ### remove "." from column names
  remove_dots <- function(dt){

    removal_cols <- colnames(dt)[grepl("\\.", colnames(dt))]
    new_cols <- gsub("[[:punct:]]", "_", removal_cols)
    setnames(dt, removal_cols, new_cols)

    return(dt)

  }

  geopolycensus_dt <- remove_dots(geopolycensus_dt)
  hhsurvey_dt <- remove_dots(hhsurvey_dt)
  adminshp_dt <- remove_dots(adminshp_dt)

  geopolycensus_dt <- st_as_sf(geopolycensus_dt, crs = crs_set[3], agr = agr_set[3])

  hhsurvey_dt <- st_as_sf(hhsurvey_dt,
                          crs = crs_set[1],
                          agr = agr_set[1])

  hhsurvey_dt <- st_join(hhsurvey_dt, geopolycensus_dt)

  #### make some quick additions to the hhsurvey_dt
  ###### including the adminshp_dt data
  adminshp_dt <- st_as_sf(adminshp_dt,
                          crs = crs_set[2],
                          agr = agr_set[2])

  ### drop the characters to create the integer codes
  even_names <- length(adminvars)
  even_names <- seq(from = 0, to = even_names, by = 2)

  even_names <- adminvars[even_names]

  if (drop_chars == TRUE){
    adminshp_dt <- saeplus_dropchars(dt = adminshp_dt, vars = even_names)
  }


  ### ensure admin codes are integers
  int_codes <- adminshp_dt[, apply(.SD, 2, as.integer), .SDcols = even_names]
  int_codes <- as.data.table(int_codes)
  adminshp_dt <- adminshp_dt[,(even_names) := NULL]
  adminshp_dt <- cbind(adminshp_dt, int_codes)

  adminshp_dt <- st_as_sf(adminshp_dt,
                          crs = crs_set[2],
                          agr = agr_set[2])

  hhsurvey_dt <- st_join(hhsurvey_dt, adminshp_dt)

  #### drop duplicated observations
  hhsurvey_dt <- hhsurvey_dt[!duplicated(hhsurvey_dt[,hhid_var]),]

  hhsurvey_dt <- as.data.table(hhsurvey_dt)

  ### create admin one level dummies
  add_dt <- saeplus_dummify(dt = hhsurvey_dt, var = adminvars[1])
  hhsurvey_dt <- cbind(hhsurvey_dt, add_dt)

  results <- list(hhsurvey_dt,
                  geopolycensus_dt,
                  adminshp_dt,
                  adm_namecode_check
                  )
  names(results) <- c("survey_data", "geopolygon_census",
                      "adminshp_data","adminboundarychecks")

  return(results)

}































