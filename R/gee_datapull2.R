#' Pull spatial data from google earth engine (GEE) for a given region on any feature in the google earth engine database
#'
#' @param email A GEE authenticated gmail address
#' @param gee_boundary A shapefile with the boundaries for a specific region/country for which data is to be collected
#' @param gee_dataname The specific google earth engine collection dataset name (use default as example)
#' see (https://developers.google.com/earth-engine/datasets/) for full name
#' @param gee_image if TRUE, an image rather than an image collection is expected (FALSE by default)
#' @param gee_datestart The starting date for the specific feature of interest
#' @param gee_dateend The ending date for the specific feature of interest
#' @param gee_band The bandname within the gee_dataname selected (see https://developers.google.com/earth-engine/datasets/)
#' @param scale Used in mean region reduction for zonal statistics
#' @param gee_desc The name to be used to name output in google drive as well as local drive
#' @param gdrive_folder Google Drive folder name to be created or uses if already existing to store shapefile output
#' @param ldrive_dsn Full file path (including shapefile name) for local storage of resulting shapefile
#' @param gee_crs Set CRS
#'
#' @return shapefiles to local drive
#'
#' @import rgee reticulate
#'
#' @export


gee_datapull2 <- function(email = "ifeanyi.edochie@gmail.com",
                          gee_polygons = "users/ifeanyiedochie/cmr_polypop_boundary",
                          gee_dataname = "NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG",
                          gee_datestart = "2018-01-01",
                          gee_dateend = "2018-01-31",
                          gee_band = "avg_rad",
                          gee_scale = 100,
                          gee_desc = "nighttimelight_cmr",
                          gee_stat = "mean",
                          gdrive_folder = "/SAEplus",
                          ldrive_dsn = "data/cmr_nighttimelight",
                          gee_crs = "EPSG:4326"){

  ### set up the appropriate feature collection
  feature_geom <- ee$FeatureCollection(gee_polygons)$geometry()

  area_map <- ee$ImageCollection(gee_dataname)$
    filterBounds(gee_polygons)$
    filterDate(gee_datestart, gee_dateend)$
    select(gee_band)

  ### compute statistics
  stat <- area_map$reduceRegion(

    reducer = ee$Reducer$mean(),
    geometry = feature_geom,
    scale = scale,
    crs = gee_crs

  )

  ## run file and store in local drive as well as online
  task_drive <- ee_table_to_drive(
    collection = stat,
    description = gee_desc,
    folder = gdrive_folder,
    fileFormat = "SHP"
  )

  task_drive$start()

  ee_monitoring(task_drive)

  task_local <- ee_drive_to_local(
    task = task_drive,
    dsn = ldrive_dsn
  )




}
