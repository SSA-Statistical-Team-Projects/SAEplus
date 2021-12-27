library(foreach)
library(doParallel)



### pull in base country-level boundary files from remote source
###### starting with GUINEA

## first carve up guinea country shapefile provided by PE into polygon grids
# gin.grid <- gengrid(dsn = "tests/testdata",
#                     layer = "sous_prefectures",
#                     raster_tif = "gin_ppp_2020_UNadj_constrained.tif",
#                     grid_shp=T,
#                     featname="population",
#                     drop_Zero=F)

gin_shp <- sf::st_read(dsn = "//cwapov/cwapov/GIN/GEO/Boundaries",
                       layer = "sous_prefectures")

sf_use_s2(FALSE)
gin_shp$area <- st_area(gin_shp)
gin_shp$area <- set_units(gin_shp$area, "km^2")


crs_dt <- rgdal::make_EPSG()
gin_shp <- st_transform(gin_shp,
                        crs = crs_dt$prj4[crs_dt$code == 3974])

gin_shp$area <- st_area(gin_shp)
gin_shp$area <- set_units(gin_shp$area, "km^2")


gin_raster <- raster("//cwapov/cwapov/GIN/GEO/Population/gin_ppp_2020_UNadj_constrained.tif")

gin_grid <- gengrid2(shp_dt = gin_shp,
                     grid_size = 1000,
                     pop_raster = gin_raster,
                     extract_name = "population")

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
                      ldrive_dsn = "//cwapov/cwapov/GIN/GEO/GEE")

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
                      ldrive_dsn = "//cwapov/cwapov/GIN/GEO/GEE")



## pull the data on impervious surface
SAEplus::gee_pullimage(email = "ifeanyi.edochie@gmail.com",
                       gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                       gee_band = "change_year_index",
                       gee_dataname = "Tsinghua/FROM-GLC/GAIA/v10",
                       gee_desc = "GIN_IS_2018JulSep",
                       ldrive_dsn = "//cwapov/cwapov/GIN/GEO/GEE")






# include other GEE data on CO, global human modification, gridmet drought data
SAEplus::gee_datapull(gee_boundary = "users/ifeanyiedochie/sous_prefectures_valid",
                      gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                      gee_band = c("CO_column_number_density", "H2O_column_number_density",
                                   "cloud_height"),
                      gee_dataname = "COPERNICUS/S5P/NRTI/L3_CO",
                      gee_datestart = "2019-04-01",
                      gee_dateend = "2019-06-30",
                      gee_desc = "GIN_CO_2019AprJun",
                      ldrive_dsn = "//cwapov/cwapov/GIN/GEO/GEE",
                      gee_crs = "EPSG:4326")

# pull some precipitation data as well
SAEplus::gee_datapull(gee_boundary = "users/ifeanyiedochie/sous_prefectures_valid",
                      gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                      gee_band = c("total_precipitation"),
                      gee_dataname = "ECMWF/ERA5/DAILY",
                      gee_datestart = "2018-07-01",
                      gee_dateend = "2018-09-30",
                      gee_desc = "GIN_PP_2018JulSep",
                      ldrive_dsn = "//cwapov/cwapov/GIN/GEO/GEE",
                      gee_crs = "EPSG:4326")

SAEplus::gee_datapull(gee_boundary = "users/ifeanyiedochie/sous_prefectures_valid",
                      gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                      gee_band = c("total_precipitation"),
                      gee_dataname = "ECMWF/ERA5/DAILY",
                      gee_datestart = "2019-04-01",
                      gee_dateend = "2019-06-30",
                      gee_desc = "GIN_PP_2019AprJun",
                      ldrive_dsn = "//cwapov/cwapov/GIN/GEO/GEE",
                      gee_crs = "EPSG:4326")

## add some drought data as well
SAEplus::gee_datapull(gee_boundary = "users/ifeanyiedochie/sous_prefectures_valid",
                      gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                      gee_band = c("pdsi", "z"),
                      gee_dataname = "GRIDMET/DROUGHT",
                      gee_datestart = "2018-07-01",
                      gee_dateend = "2018-09-30",
                      gee_desc = "GIN_drought_2018JulSep",
                      ldrive_dsn = "//cwapov/cwapov/GIN/GEO/GEE",
                      gee_crs = "EPSG:4326")

SAEplus::gee_datapull(gee_boundary = "users/ifeanyiedochie/sous_prefectures_valid",
                      gee_polygons = "users/ifeanyiedochie/gin_poppoly",
                      gee_band = c("pdsi", "z"),
                      gee_dataname = "GRIDMET/DROUGHT",
                      gee_datestart = "2019-04-01",
                      gee_dateend = "2019-06-30",
                      gee_desc = "GIN_drought_2019AprJun",
                      ldrive_dsn = "//cwapov/cwapov/GIN/GEO/GEE",
                      gee_crs = "EPSG:4326")


#######################################################################################################

## pull in the building data

wpopbuilding_pull(iso = "GIN", wpversion = "v2.0",
                  ldrive_dsn = "D:/Ify/Guinea_SAE")

## pull in the electricity data
gin.elect <- gengrid(dsn = "tests/testdata",
                     layer = "gin_poppoly",
                     raster_tif = "GIN_Electricity_2018.tif",
                     grid_shp = F,
                     featname="elect_cons",
                     drop_Zero=F)

## pull building data
#### writing a simple function to pull data from a set of tif files using gengrid() and then combining
#### the columns

gin_buildingtifs <- list.files(path = "tests/testdata", pattern = "GIN_buildings")

gin_building.dt <- mult_gengrid(tif_namelist = gin_buildingtifs,
                                location = "tests/testdata",
                                feature_name = c("count", "cv_length",
                                                 "imagery_year", "mean_length",
                                                 "total_length", "cv_area",
                                                 "density", "mean_area",
                                                 "total_area", "urban"),
                                parallel = T,
                                numCores = length(gin_buildingtifs))


## combine all extracted columns into one data.frame/data.table object
multi_merge_DT <- function(sf1, sf2){

  obj <- sf1[sf2, on = c("id", "population")]

  return(obj)

}

bld_polygon.dt <- list()

for (i in seq_along(gin_building.dt)){

  bld_polygon.dt[i] <- gin_building.dt[[i]]["polygon_dt"]
}

###remove geometry column from all but one polygon_dt
remove_varfromdtlist <- function(list_dt = bld_polygon.dt,
                                 varname = "geometry"){


  for (i in 1:(length(list_dt)-1)){

    selected_names <- colnames(list_dt[[i]])[!(colnames(list_dt[[i]]) %in% varname)]
    list_dt[[i]] <- list_dt[[i]][,selected_names, with = F]
  }

  return(list_dt)
}

bld_polygon.dt <- remove_varfromdtlist()

rm(gin_building.dt) ##remove the gin_building.dt object

bld_polygon.dt <- Reduce(multi_merge_DT, bld_polygon.dt) ##combine all building data

geopolycensus_dt <- bld_polygon.dt ## rename bld_polygon.dt to a name that will hold all geospatial data

rm(bld_polygon.dt) ## remove the bld_polygon.dt object after renaming it

## include the GEE data and electricty data into the geopolycensus.dt
geopolycensus_dt <- gin.elect$polygon_dt[,c("id", "elect_cons")][geopolycensus_dt, on = "id"] ## add electricity

#### function to read the set of GEE shapefiles and return a list of datasets


readmerge_gee <- function(folder = "//cwapov/cwapov/GIN/GEO/GEE", ##folder location
                          identifier = "GIN_"){ ##identifiable

  gee_dt <- list.files(path = folder, pattern = identifier)
  drop_chr <- function(x){
    x <- substr(x, start = 1, stop = nchar(x) - 4)
    return(x)
  }

  gee_dt <- unique(unlist(lapply(gee_dt, drop_chr)))

  st_readlist <- function(X){

    obj <- sf::st_read(dsn = folder, layer = X)
    obj <- as.data.table(obj)
    colnames(obj)[colnames(obj) %in% "mean"] <- X
    return(obj)

  }

  gee_dt <- lapply(gee_dt, st_readlist)

  return(gee_dt)

}

gee_dt <- readmerge_gee() ##run readmerge_gee to combine the pulled GEE data

##remove geometry from the gee_dt list for all but one so that we dont have duplicates after merge
gee_dt <- remove_varfromdtlist(list_dt = gee_dt)

#join gee data using rbindlist
gee_dt <- do.call(cbind, gee_dt)

gee_dt <- gee_dt[ , which( !duplicated( t( gee_dt ) ) ), with = FALSE ]

geopolycensus_dt <- gee_dt[geopolycensus_dt, on = c("id", "population")]

## include electricity data to geopolycensus
geopolycensus_dt <- gin.elect$polygon_dt[geopolycensus_dt, on = c("id", "population")]

## clean up the global environment
rm(gee_dt)
rm(gin.elect)

## write the data to file
saveRDS(geopolycensus_dt, file = "tests/testdata/gin_geopolycensus.RDS")

