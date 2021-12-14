### trying to fix the functions quickly using the MOZ data

# ### read in the data
# gmb_shp <- sf::st_read(dsn = "//cwapov/cwapov/GMB/GEO/Boundaries", layer = "District_Boundary")
#
# gmb_grid <- gengrid2(shp_dt = gmb_shp,
#                      grid_size = 200,
#                      sqr = TRUE,
#                      raster_path = "georaw/gmb_ppp_2020_UNadj.tif",
#                      extract_name = "population")
#
# gmb_raster <- raster::raster("//cwapov/cwapov/GMB/GEO/Population/gmb_ppp_2020_UNadj.tif")

## read in MOZ shapefile
moz_shp <- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population/moz_censusshapefile2017",
                       layer = "BASE_COMPLETA_DE_AE_CENSO_2017")

moz_raster <- raster::raster("//esapov/esapov/MOZ/GEO/Population/moz_censusshapefile2017/MOZ_population_v1_1_gridded.tif")


test <- exactextractr::exact_extract(moz_raster,
                                     moz_shp,
                                     fun = "sum")

### make sure the raster and shapefiles are in a metric crs
crs_dt <- rgdal::make_EPSG()  ##list all the coordinate reference systems available in R via the rgdal package
moz_shp <- st_transform(moz_shp, crs = 4328)

# moz_shp <- st_transform(moz_shp, crs = crs_dt$prj4[crs_dt$code == 4328])

moz_raster <- raster::projectRaster(moz_raster,
                                    crs = moz_shp)

test2 <- exactextractr::exact_extract(moz_raster,
                                      moz_shp,
                                      fun = "sum")

mozgrid <- gengrid2(shp_dt = moz_shp,
                    grid_size = 1000,
                    raster_path = "//esapov/esapov/MOZ/GEO/Population/moz_censusshapefile2017/MOZ_population_v1_1_gridded.tif",
                    extract_name = "population",
                    raster_function = "sum")


