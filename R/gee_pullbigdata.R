#' Another pulling function for pulling Google Earth Engine data for larger maps
#'
#' @param email A GEE authenticated gmail address
#' @param shp_dsn character of length 1; the local folder location of the shapefile
#' @param shp_layer character of length 1; the name of the file
#' @param gee_dataname character of length 1; The specific google earth engine collection dataset name
#' @param gee_datestart The starting date for the specific feature of interest
#' @param gee_dateend The ending date for the specific feature of interest
#' @param gee_chunksize an integer; the size of each chunk which the shapefile should be divided into for faster
#' pulls (GEE collection query aborts after accumulating a chunk size with over 5000 elements i.e.
#' rows and columns combined)
#' @param gee_stat the zonal statistic to be estimated. The current options are mean, median, min, max and stdDev
#'
#' @return an object of class data.frame
#'
#' @import rgee reticulate
#'
#' @export



gee_pullbigdata <- function(email = "ifeanyi.edochie@gmail.com",
                            shp_dsn,
                            shp_layer,
                            gee_name = 'NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG',
                            gee_datestart = "2019-01-01",
                            gee_dateend = "2019-12-31",
                            gee_band = "avg_rad",
                            gee_chunksize = 4000,
                            gee_stat = "mean",
                            gee_scale = 1000){

  ee_Initialize()

  shp_dt <- st_read(dsn = shp_dsn,
                    layer = shp_layer)

  shp_names <- colnames(shp_dt)

  gee_map <-
    ee$ImageCollection(gee_name)$
    filterDate(gee_datestart, gee_dateend)$
    map(function(x) x$select(gee_band))

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

  dt <- as.data.table(dt)

  return(dt)



}























