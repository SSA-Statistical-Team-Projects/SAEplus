sf_use_s2(FALSE)
## read in MOZ shapefile
moz_shp <- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population/moz_censusshapefile2017",
                       layer = "BASE_COMPLETA_DE_AE_CENSO_2017")

moz_raster <- raster::raster("//esapov/esapov/MOZ/GEO/Population/moz_censusshapefile2017/MOZ_population_v1_1_gridded.tif")

## check the shapefile
moz_shp$area <- st_area(moz_shp)
moz_shp$area <- set_units(moz_shp$area, "km^2")

### make sure the raster and shapefiles are in a metric crs
crs_dt <- rgdal::make_EPSG()  ##list all the coordinate reference systems available in R via the rgdal package
moz_shp <- st_transform(moz_shp, crs_dt$prj4[crs_dt$code == 3974])

moz_shp$area <- st_area(moz_shp)
moz_shp$area <- set_units(moz_shp$area, "km^2")



moz_grid <- gengrid2(shp_dt = moz_shp,
                     grid_size = 1000,
                     pop_raster = moz_raster,
                     extract_name = "population")

#### testing to see if the changes to gee_datapull works
precip_dt <- gee_datapull(gee_boundary = "users/ifeanyiedochie/moz_poppoly",
                          gee_polygons = "users/ifeanyiedochie/moz_poppoly",
                          gee_datestart = "2018-11-01",
                          gee_dateend = "2019-11-30",
                          gee_dataname = "UCSB-CHG/CHIRPS/DAILY",
                          gee_band = "precipitation",
                          scale = 5000,
                          gee_desc = "precip",
                          ldrive_dsn = "data/moz",
                          tileScale = 16)

#### testing to see if the changes to gee_datapull works
precip_dt <- st_read(dsn = "inst/extdata",
                     layer = "maputo_grid")

precip_dt <- precip_dt[,c("poly_id", "geometry")]

precip_dt <- gee_datapull2(shp_dt = precip_dt,
                           gee_chunksize = 1000)


precip_dt <- gee_pullbigdata(shp_dt = precip_dt,
                             gee_name = "UCSB-CHG/CHIRPS/DAILY",
                             gee_chunksize = 20,
                             gee_scale = 5566,
                             gee_band = "precipitation")


precip_dt <- gee_pullhighres(shp_dt = precip_dt)


### calculating on a raster is much faster than doing so on the shapefile



















