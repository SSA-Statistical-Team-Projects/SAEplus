#' Pull Image Data from Google Earth Engine
#'
#' This function is also sends data pull requests to the GEE server specifically for Image tags as
#' different from 'SAEplus::gee_datapull()' which pulls Image Collection tags specifically
#'


gee_pullimage <- function(email = "ifeanyi.edochie@gmail.com",
                          gee_polygons = "users/ifeanyiedochie/cmr_polypop_boundary",
                          gee_dataname = "Tsinghua/FROM-GLC/GAIA/v10",
                          gee_band = "change_year_index",
                          scale = 30,
                          gee_desc = "impervious_cmr",
                          gee_stat = "mean",
                          gdrive_folder = "/SAEplus",
                          ldrive_dsn = "data/cmr_impervious"){

  requireNamespace(c("rgee", "reticulate", "googledrive"), quietly = TRUE)

  ee_Initialize(email = email)
  googledrive::drive_deauth()

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
