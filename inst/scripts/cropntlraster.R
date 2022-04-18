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


















