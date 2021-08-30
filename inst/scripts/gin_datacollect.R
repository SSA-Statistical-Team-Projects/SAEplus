### pull in base country-level boundary files from remote source
###### starting with GUINEA

## first carve up guinea country shapefile provided by PE into polygon grids
gin.grid <- gengrid(dsn = "tests/testdata",
                    layer = "sous_prefectures",
                    raster_tif = "gin_ppp_2020_UNadj_constrained.tif",
                    grid_shp=T,
                    featname="population",
                    drop_Zero=F)

## load and clean up the shapefile a little
sf::st_write(obj = gin.grid$polygon_dt, dsn = "tests/testdata", layer = "gin_poppoly",
             driver = "ESRI Shapefile")

gin.base <- sf::st_read(dsn = "tests/testdata",
                        layer = "sous_prefectures")

gin.base <- sf::st_make_valid(gin.base) #making shapefile valid for use on google earth engine
gin.base <- gin.base[sf::st_geometry_type(gin.base$geometry) == "MULTIPOLYGON",] #we want all multipolygons
                                                                                 #in this case
#gin.base <- rmapshaper::ms_simplify(gin.base)
gin.base <- sf::st_make_valid(gin.base)

## save the cleaned up shapefile
sf::st_write(gin.base, dsn = "tests/testdata", layer = "sous_prefectures_valid",
             driver = "ESRI Shapefile",
             append = FALSE)


gin.ntl <- SAEplus::gee_datapull(email = "ifeanyi.edochie@gmail.com",
                                 gee_boundary = "users/ifeanyiedochie/sous_prefectures_valid",
                                 gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                                 gee_datestart = "2018-07-01",
                                 gee_dateend = "2018-09-30",
                                 gee_desc = "GIN_NTL_2018JulSep",
                                 ldrive_dsn = "//cwapov/cwapov/GIN/GEO/GEE")
