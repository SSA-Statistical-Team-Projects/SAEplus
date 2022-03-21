#' Pull Large Images from Google Earth Engine's Image Collections for further extraction
#'
#' This function is a slight variation from gee_datapull() which pulls image collections. This function specifically pulls
#' images
#'
#' @param email A GEE authenticated gmail address
#' @param gee_polygon A polygon shapefile spanning the region in the gee_boundary file for features will be computed.
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
#'
#' @export


gee_downloadimage <- function(gee_country,
                              gee_dataname,
                              gee_band,
                              gee_scale = NULL,
                              gdrive_folder = "rgee",
                              ldrive_dsn,
                              gee_datestart,
                              gee_dateend,
                              gee_desc,
                              gee_crs = NULL){

  ee_Initialize() ###initialize the google earth engine server

  # World FeatureCollection dataset
  countries <- ee$FeatureCollection("USDOS/LSIB_SIMPLE/2017")

  # Select Cameroon
  country_bounds <- countries %>%
    ee$FeatureCollection$filter(
      ee$Filter$eq('country_na', gee_country)
    ) %>%
    ee$FeatureCollection$first() %>%
    ee$Feature() %>%
    ee$Feature$geometry()

  country_bounds <- country_bounds %>% ee$Geometry$bounds()

  # gee_map <-
  #   ee$ImageCollection(gee_dataname)$
  #   filterDate(gee_datestart, gee_dateend)$
  #   filterBounds(country_bounds)$
  #   map(function(x) x$select(gee_band))
  #
  # date_list <- ee$List(gee_map$aggregate_array('system:time_start'))
  # date_list <- date_list$distinct()

  ##create list of dates
  date_list <- as.character(seq(as.Date(gee_datestart), as.Date(gee_dateend), "days"))

  date_list <- date_list %>% ee$List()

  ###filter the image collection to be pulled


  image_worker <- function(day){

    day <- day %>% ee$Date()
    delta_advance <- day$advance(1, "hour") ##add small amount of time needed for filtering
    gee_map <-
      ee$ImageCollection(gee_dataname)$
      filterDate(day, delta_advance)$
      filterBounds(country_bounds)$
      map(function(x) x$select(gee_band))$
      toBands()

    task_drive <- ee_image_to_drive(image = gee_map,
                                    description = gee_desc,
                                    folder = gdrive_folder,
                                    scale = gee_scale,
                                    crs = gee_crs,
                                    maxPixels = 1e13)

    task_drive$start()

    ee_monitoring(task_drive)

    task_drive <- ee_drive_to_local(task = task_drive,
                                    dsn = ldrive_dsn)


  }


  date_list$map(image_worker)

}





