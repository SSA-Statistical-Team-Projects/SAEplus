library(sf)


## quick tutorial to show how to grid a shapefile


shp_dt <- sf::st_read(dsn = "//cwapov/cwapov/MLI/GEO/Boundaries",
                      layer = "MLI_ADM0_fromCCDR")

crs_dt <- rgdal::make_EPSG()

##compute area of the shapefile to check
shp_dt$area <- sf::st_area(shp_dt)
shp_dt$area <- units::set_units(shp_dt$area, "km^2")

test <- sf::st_transform(shp_dt, ## test to see if it transforms into a CRS that works
                     crs = crs_dt$prj4[crs_dt$code == 3974])

test$area <- units::set_units(sf::st_area(test), "km^2")


##grid the shapefile
raster_obj <- raster::raster("//cwapov/cwapov/MLI/GEO/Population/WorldPop/mli_ppp_2020_UNadj_constrained.tif")

grid_dt <- SAEplus::gengrid2(shp_dt = test,
                             grid_size = 1000,
                             pop_raster = raster_obj,
                             extract_name = "population")
