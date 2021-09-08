### pull in base country-level boundary files from remote source
###### starting with GUINEA

## first carve up guinea country shapefile provided by PE into polygon grids
gin.grid <- gengrid(dsn = "tests/testdata",
                    layer = "sous_prefectures",
                    raster_tif = "gin_ppp_2020_UNadj_constrained.tif",
                    grid_shp=T,
                    featname="population",
                    drop_Zero=F)


sf::st_write(obj = gin.grid$polygon_dt, dsn = "tests/testdata", layer = "gin_poppoly",
             driver = "ESRI Shapefile", append = FALSE)

## load and clean up the shapefile a little
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

## pull nighttime lights
gin.ntl <- SAEplus::gee_datapull(email = "ifeanyi.edochie@gmail.com",
                                 gee_boundary = "users/ifeanyiedochie/sous_prefectures_valid",
                                 gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                                 gee_datestart = "2018-07-01",
                                 gee_dateend = "2018-09-30",
                                 gee_desc = "GIN_NTL_2018JulSep",
                                 ldrive_dsn = "//cwapov/cwapov/GIN/GEO/GEE")

gin.ntl2 <- SAEplus::gee_datapull(email = "ifeanyi.edochie@gmail.com",
                                  gee_boundary = "users/ifeanyiedochie/sous_prefectures_valid",
                                  gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                                  gee_datestart = "2019-04-01",
                                  gee_dateend = "2019-06-30",
                                  gee_desc = "GIN_NTL_2019AprJun",
                                  ldrive_dsn = "//cwapov/cwapov/GIN/GEO/GEE")

gin.no2 <- gee_datapull(email = "ifeanyi.edochie@gmail.com",
                        gee_boundary = "users/ifeanyiedochie/sous_prefectures_valid",
                        gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                        gee_band = "tropospheric_NO2_column_number_density",
                        gee_dataname = "COPERNICUS/S5P/NRTI/L3_NO2",
                        gee_datestart = "2018-07-01",
                        gee_dateend = "2018-09-30",
                        gee_desc = "GIN_NO2_2018JulSep",
                        ldrive_dsn = "//cwapov/cwapov/GIN/GEO/GEE")

gin.no21 <- gee_datapull(email = "ifeanyi.edochie@gmail.com",
                         gee_boundary = "users/ifeanyiedochie/sous_prefectures_valid",
                         gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                         gee_band = "tropospheric_NO2_column_number_density",
                         gee_dataname = "COPERNICUS/S5P/NRTI/L3_NO2",
                         gee_datestart = "2019-04-01",
                         gee_dateend = "2019-06-30",
                         gee_desc = "GIN_NO2_2019AprJun",
                         ldrive_dsn = "//cwapov/cwapov/GIN/GEO/GEE")

##pull in the landcover data
# pull the data on Landcover
SAEplus::gee_datapull(email = "ifeanyi.edochie@gmail.com",
                      gee_boundary = "users/ifeanyiedochie/sous_prefectures_valid",
                      gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                      gee_band = c("tree-coverfraction","urban-coverfraction","grass-coverfraction",
                                    "shrub-coverfraction","crops-coverfraction","bare-coverfraction",
                                    "water-permanent-coverfraction","water-seasonal-coverfraction",
                                    "moss-coverfraction"),
                      gee_dataname = "COPERNICUS/Landcover/100m/Proba-V-C3/Global",
                      gee_desc = "GIN_LC_2018JulSep",
                      ldrive_dsn = "tests/testdata")

SAEplus::gee_datapull(email = "ifeanyi.edochie@gmail.com",
                      gee_boundary = "users/ifeanyiedochie/sous_prefectures_valid",
                      gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                      gee_band = c("tree-coverfraction","urban-coverfraction","grass-coverfraction",
                                   "shrub-coverfraction","crops-coverfraction","bare-coverfraction",
                                   "water-permanent-coverfraction","water-seasonal-coverfraction",
                                   "moss-coverfraction"),
                      gee_dataname = "COPERNICUS/Landcover/100m/Proba-V-C3/Global",
                      gee_datestart = "2019-04-01",
                      gee_dateend = "2019-06-30",
                      gee_desc = "GIN_LC_2019AprJun",
                      ldrive_dsn = "tests/testdata")



## pull the data on impervious surface
SAEplus::gee_pullimage(email = "dasalm20@gmail.com",
                       gee_polygons = "users/dasalm20/gin_poppoly",
                       gee_band = "change_year_index",
                       gee_dataname = "Tsinghua/FROM-GLC/GAIA/v10",
                       gee_desc = "GIN_IS_2018JulSep",
                       ldrive_dsn = "InputData/GIN_IS_2018JulSep")






# include other GEE data on CO, global human modification, gridmet drought data
SAEplus::gee_datapull(gee_boundary = "users/ifeanyiedochie/sous_prefectures_valid",
                      gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                      gee_band = c("CO_column_number_density", "H2O_column_number_density",
                                   "cloud_height"),
                      gee_dataname = "COPERNICUS/S5P/NRTI/L3_CO",
                      gee_datestart = "2018-07-01",
                      gee_dateend = "2018-09-30",
                      gee_desc = "GIN_CO_2018JulSep",
                      ldrive_dsn = "GIN_2021/GIN_CO_2018JulSep",
                      gee_crs = "WGS84")



##name the ntl indicator real quick
ginntl_julsep.dt <- sf::st_read(dsn = "InputData", layer = "GIN_NTL_2018JulSep_2021_04_08_12_56_31")
ginntl_aprjun.dt <- sf::st_read(dsn = "InputData", layer = "GIN_NTL_2019AprJun_2021_04_08_12_50_58")

names(ginntl_aprjun.dt)[names(ginntl_aprjun.dt) == "mean"] <- "mean_ntlaprjun19"
names(ginntl_julsep.dt)[names(ginntl_julsep.dt) == "mean"] <- "mean_ntljulsep18"

ginno2_julsep.dt <- sf::st_read(dsn = "InputData", layer = "GIN_NO2_2018JulSep")
names(ginno2_julsep.dt)[names(ginno2_julsep.dt) == "mean"] <- "mean_no2julsep18"

