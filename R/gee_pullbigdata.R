#' Another pulling function for pulling Google Earth Engine data for larger maps
#'
#' @param email A GEE authenticated gmail address
#' @param shp_dt an sf/dataframe object
#' @param gee_name character of length 1; The specific google earth engine collection dataset name
#' @param gee_band the name of GEE bandname for the specific dataset of interest
#' @param gee_datestart The starting date for the specific feature of interest
#' @param gee_dateend The ending date for the specific feature of interest
#' @param gee_chunksize an integer; the size of each chunk which the shapefile should be divided into for faster
#' pulls (GEE collection query aborts after accumulating a chunk size with over 5000 elements i.e.
#' rows and columns combined)
#' @param gee_stat the zonal statistic to be estimated. The current options are mean, median, min, max and stdDev
#' @param gee_scale if set to NULL the native resolution of the dataset is used, otherwise GEE will resample
#' according to the resolution set
#'
#'
#' @return an object of class data.frame
#'
#' @import rgee reticulate
#'
#' @export



gee_pullbigdata <- function(email = "ifeanyi.edochie@gmail.com",
                            shp_dt,
                            gee_name = 'NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG',
                            gee_datestart = "2019-01-01",
                            gee_dateend = "2019-12-31",
                            gee_band = "avg_rad",
                            gee_chunksize = 4000,
                            gee_stat = "mean",
                            gee_scale = NULL){

  ee_Initialize()


  gee_map <-
    ee$ImageCollection(gee_name)$
    filterDate(gee_datestart, gee_dateend)$
    map(function(x) x$select(gee_band))$
    toBands()

  ## cut the dataset into multiple parts
  shp_list <- split(shp_dt, (as.numeric(rownames(shp_dt))-1) %/% gee_chunksize)

  ## a simple function to extracting data into gee_map chunk

  counter <- 0
  #### compute mean
  if (gee_stat %in% "mean"){
    extract_chunk <- function(X){

      specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) ### round to decimal place

      counter <<- counter + 1
      print(paste0("GEE Collection query ",counter, " of ", length(shp_list), " initiated"))

      y <- ee_extract(x = gee_map,
                      y = X,
                      fun = ee$Reducer$mean(),
                      scale = gee_scale)
      print(paste0("Query complete, GEE job ",
                   specify_decimal((counter * 100)/length(shp_list), 2), "% completed!"))

      return(y)
    }

    dt <- lapply(X = shp_list,
                 FUN = extract_chunk)

    dt <- rbindlist(dt)

  }

  #### compute min
  if (gee_stat %in% "min"){
    extract_chunk <- function(X){

      specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) ### round to decimal place

      counter <<- counter + 1
      print(paste0("GEE Collection query ",counter, " of ", length(shp_list), " initiated"))
      y <- ee_extract(x = gee_map,
                      y = X,
                      fun = ee$Reducer$min(),
                      scale = gee_scale)
      print(paste0("Query complete, GEE job ",
                   specify_decimal((counter * 100)/length(shp_list), 2), "% completed!"))

      return(y)
    }

    dt <- lapply(X = shp_list,
                 FUN = extract_chunk)

    dt <- rbindlist(dt)

  }

  #### compute max
  if (gee_stat %in% "max"){
    extract_chunk <- function(X){

      specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) ### round to decimal place

      counter <<- counter + 1
      print(paste0("GEE Collection query ",counter, " of ", length(shp_list), " initiated"))
      y <- ee_extract(x = gee_map,
                      y = X,
                      fun = ee$Reducer$max(),
                      scale = gee_scale)
      print(paste0("Query complete, GEE job ",
                   specify_decimal((counter * 100)/length(shp_list), 2), "% completed!"))

      return(y)
    }

    dt <- lapply(X = shp_list,
                 FUN = extract_chunk)

    dt <- rbindlist(dt)

  }

  #### compute median
  if (gee_stat %in% "median"){
    extract_chunk <- function(X){

      specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) ### round to decimal place

      counter <<- counter + 1
      print(paste0("GEE Collection query ",counter, " of ", length(shp_list), " initiated"))

      y <- ee_extract(x = gee_map,
                      y = X,
                      fun = ee$Reducer$median(),
                      scale = gee_scale)
      print(paste0("Query complete, GEE job ",
                   specify_decimal((counter * 100)/length(shp_list), 2), "% completed!"))

      return(y)
    }

    dt <- lapply(X = shp_list,
                 FUN = extract_chunk)

    dt <- rbindlist(dt)

  }

  #### compute StdDev
  if (gee_stat %in% "stdDev"){

    specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) ### round to decimal place

    extract_chunk <- function(X){
      counter <<- counter + 1
      print(paste0("GEE Collection query ",counter, " of ", length(shp_list), " initiated"))

      y <- ee_extract(x = gee_map,
                      y = X,
                      fun = ee$Reducer$stdDev(),
                      scale = gee_scale)
      print(paste0("Query complete, GEE job ",
                   specify_decimal((counter * 100)/length(shp_list), 2), "% completed!"))

      return(y)
    }

    dt <- lapply(X = shp_list,
                 FUN = extract_chunk)

    dt <- rbindlist(dt)

  }

  return(dt)



}

# Another function to pull GEE data by parallel mapping the chunk extraction process
gee_parallelpullbigdata <- function(email = "ifeanyi.edochie@gmail.com",
                                    shp_dt,
                                    gee_name = 'NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG',
                                    gee_datestart = "2019-01-01",
                                    gee_dateend = "2019-12-31",
                                    gee_band = "avg_rad",
                                    gee_scale = NULL,
                                    gee_desc,
                                    gdrive_folder,
                                    local_drive){

  ee_Initialize() ##initialize credentials on the GEE server

  shp_json <- geojson::as.geojson(shp_dt) ##convert sf to geojson format
  shp_fc <- ee$FeatureCollection(shp_json) ##turn client object to server-side object feature collection

  ##select and filter image collection with the boundaries of the shapefile
  gee_map <-
    ee$ImageCollection(gee_name)$
    filterDate(gee_datestart, gee_dateend)$
    map(function(x) x$select(gee_band))$
    toBands()$
    map(function(x) x$FilterBounds(shp_fc))

  ##set mapping function for reduction
  compute_mean <- function(X){

    gee_map$reduceRegion(

      reducer   = ee$Reducer$mean(),
      geometry  = X,
      scale     = gee_scale,
      maxPixels = 1e13

    )
  }

  ##run the function
  results <- gee_map$map(compute_mean)

  ##return results back to drive and to console
  task_drive <- ee_table_to_drive(
    collection  = results,
    description = gee_desc,
    folder      = gdrive_folder,
    fileFormat  = "SHP"
  )

  task_drive$start()

  ee_monitoring(task_drive)

  task_local <- ee_drive_to_local(task = gee_desc,
                                  dsn  = local_drive)


}




































