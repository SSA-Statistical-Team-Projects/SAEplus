#' Pull Images from Google Earth Engine
#'
#' This function is a slight variation from gee_datapull() which pulls image collections. This function specifically pulls
#' images
#'
#' @param email A GEE authenticated gmail address
#' @param gee_polygons A polygon shapefile spanning the region in the gee_boundary file for features will be computed.
#' forms the unit of analysis
#' @param gee_dataname The specific google earth engine collection dataset name (use default as example)
#' see (https://developers.google.com/earth-engine/datasets/) for full name
#' @param gee_band The bandname within the gee_dataname selected (see https://developers.google.com/earth-engine/datasets/)
#' @param scale Used in mean region reduction for zonal statistics
#' @param gee_desc The name to be used to name output in google drive as well as local drive
#' @param gdrive_folder Google Drive folder name to be created or uses if already existing to store shapefile output
#' @param ldrive_dsn Full file path (including shapefile name) for local storage of resulting shapefile
#'
#' @return shapefiles to local drive
#'
#' @import rgee reticulate
#' @importFrom googledrive drive_deauth
#'
#' @export


gee_pullimage <- function(email = "ifeanyi.edochie@gmail.com",
                          gee_polygons = "users/ifeanyiedochie/cmr_polypop_boundary",
                          gee_dataname = "Tsinghua/FROM-GLC/GAIA/v10",
                          gee_band = "change_year_index",
                          scale = 30,
                          gee_desc = "impervious_cmr",
                          gee_stat = "mean",
                          gdrive_folder = "/SAEplus",
                          ldrive_dsn = "data/cmr_impervious"){

  ee_Initialize(email = email)
  drive_deauth()

  agebs <- ee$FeatureCollection(gee_polygons)

  s5p_collect <- ee$Image(gee_dataname)$
    select(gee_band)

  s5p_agebs <- s5p_collect$reduceRegions(
    collection = agebs,
    reducer = ee$Reducer$mean(),
    scale = 30
  )

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
