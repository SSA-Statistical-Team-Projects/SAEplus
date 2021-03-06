#' Pull spatial data from google earth engine (GEE) for a given region on any feature in the google earth engine database
#'
#' @param email A GEE authenticated gmail address
#' @param gee_boundary A shapefile with the boundaries for a specific region/country for which data is to be collected
#' @param gee_polygons A polygon shapefile spanning the region in the gee_boundary file for features will be computed.
#' forms the unit of analysis
#' @param gee_dataname The specific google earth engine collection dataset name (use default as example)
#' see (https://developers.google.com/earth-engine/datasets/) for full name
#' @param gee_datestart The starting date for the specific feature of interest
#' @param gee_dateend The ending date for the specific feature of interest
#' @param gee_band The bandname within the gee_dataname selected (see https://developers.google.com/earth-engine/datasets/)
#' @param scale Used in mean region reduction for zonal statistics
#' @param gee_desc The name to be used to name output in google drive as well as local drive
#' @param gdrive_folder Google Drive folder name to be created or uses if already existing to store shapefile output
#' @param ldrive_dsn Full file path (including shapefile name) for local storage of resulting shapefile




gee_datapull <- function(email = "ifeanyi.edochie@gmail.com",
                         gee_boundary = "users/ifeanyiedochie/gadm36_CMR_0",
                         gee_polygons = "users/ifeanyiedochie/cmr_polypop_boundary",
                         gee_dataname = "NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG",
                         gee_datestart = "2018-01-01",
                         gee_dateend = "2018-01-31",
                         gee_band = "avg_rad",
                         scale = 100,
                         gee_desc = "nighttimelight_cmr",
                         gee_stat = "mean",
                         gdrive_folder = "/SAEplus",
                         ldrive_dsn = "data/cmr_nighttimelight"){

  ## below are the packages needed for the function to run
  usepkgs <- c("rgee", "reticulate")

  missing <- usepkgs[!(usepkgs %in% installed.packages()[,"Package"])]

  if(is.null(missing) == FALSE){
    install.packages(missing,
                     dependencies = TRUE,
                     repos = "http://cran.us.r-project.org")
  }

  invisible(sapply(usepkgs, library, character.only = TRUE))

  ee_Initialize(email = email)

  agebs <- ee$FeatureCollection(gee_polygons)
  agebs_boundary <- ee$FeatureCollection(gee_boundary)

  s5p_collect <- ee$ImageCollection(gee_dataname)$
    filterBounds(agebs_boundary)$
    filterDate("2018-01-01", "2018-01-31")$
    select(gee_band)

  ## compute zonal stats function
  if(gee_stat == "mean"){
    s5p_mean <- s5p_collect$mean()

    s5p_agebs <- s5p_mean$reduceRegions(
      collection =  agebs,
      reducer = ee$Reducer$mean(),
      scale = scale
    )
  } else if(gee_stat == "sum"){
    s5p_mean <- s5p_collect$mean()

    s5p_agebs <- s5p_mean$reduceRegions(
      collection = agebs,
      reducer = ee$Reducer$sum(),
      scale = scale
    )
  } else {return("gee_stat must be specified as sum or mean, please specify accordingly")}


  task <- ee_table_to_drive(
    collection = s5p_agebs,
    description = gee_desc,
    folder = gdrive_folder,
    fileFormat = "SHP"
  )

  task$start()

  ee_monitoring(task)

  return(ee_drive_to_local(task = task, dsn = ldrive_dsn))

}
