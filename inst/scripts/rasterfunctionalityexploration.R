### a script to test features in raster analysis
library(tidyverse)

raster_list <- list.files(path = "//esapov/esapov/ALL/Energy/NTL",
                          pattern = "tif")

date_range <- seq(as.Date("2018-11-01"), as.Date("2019-10-31"), by = "days")

date_range <- str_replace_all(date_range, "-", "")

mult_grepl <- function(X){

  y <- raster_list[grepl(X, raster_list)]

  return(y)

}

raster_list <- unlist(lapply(date_range, mult_grepl))


### read a bunch of rasters
mult_raster <- function(X){

  y <- raster(paste0("//esapov/esapov/ALL/Energy/NTL/", X))

  return(y)

}
raster_list <- lapply(raster_list, mult_raster)



shp_dt <- read_sf(dsn = "//esapov/esapov/MOZ/GEO/Population/poppoly",
                  layer = "moz_poppoly_gridded")

shp_dt <- parallel_extract(shp_dt = shp_dt,
                           raster_list = raster_list,
                           fun_list = rep("mean", length(raster_list)),
                           numCores = 30)
