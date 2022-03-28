### a quick script to crop the night time lights data to the bounding box for SSA

##### read an sample raster
#
# worldntl_raster <- raster::raster("//esapov/esapov/ALL/Energy/SVDNB_npp_d20181101.rade9d.tif")
#
## load africa shapefile
library(dplyr)

sf::sf_use_s2(FALSE)
africa_shp <- sf::st_read(dsn = "//esapov/esapov/ALL/Boundaries",
                          layer = "Africa_Boundaries")

africa_shp <- africa_shp %>% sf::st_bbox() %>% sf::st_as_sfc()
africa_shp <- africa_shp %>% sf::st_buffer(dist = 1)
africa_shp <- sf::as_Spatial(africa_shp)


# ### crop raster to the extent of the shapefile
# africantl_raster <- raster::crop(x = worldntl_raster, y = africa_shp)
#
# raster::writeRaster(africantl_raster, filename = "//esapov/esapov/ALL/test.tif")


##### create a function to crop raster to africa's extent and save to folder


ntl_files <- list.files(path = "//esapov/esapov/ALL/Energy",
                        pattern = ".rade9d")
### parallelize the function
multraster_cropsave <- function(numCores,
                                raster_list){

  raster_cropsave <- function(rastertocrop,
                              shp_obj,
                              storagefolder){

    raster_obj <- raster::raster(rastertocrop) ##read raster in

    cropped_raster <- raster::crop(x = raster_obj, y = shp_obj) ##crop the raster to the extent of the shapefile

    raster::writeRaster(cropped_raster, filename = paste0(storagefolder, "afr_", names(raster_obj), ".tif"))

  }


  numCores <- min(numCores, parallel::detectCores())
  parallelMap::parallelLibrary("foreach")
  parallelMap::parallelLibrary("raster")
  parallelMap::parallelLibrary("sf")

  doParallel::registerDoParallel(cores = numCores) ##initiate the number of cores to be used


    foreach(i = 1:length(raster_list)) %dopar% {


      raster_cropsave(rastertocrop = paste0("//esapov/esapov/ALL/Energy/", raster_list[i]),
                      shp_obj = africa_shp,
                      storagefolder = "//esapov/esapov/ALL/Energy/NTL")

    }

}

multraster_cropsave(numCores = 15, raster_list = ntl_files)
















