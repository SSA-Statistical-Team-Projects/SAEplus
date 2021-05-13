
###########################################################################################
##############################FIRST PULL THE DATA TOGETHER#################################
###########################################################################################

### we need to drop the code that pulls together all the data
##### create the gridded polygon data for GIN
gingrid <- SAEplus::gengrid(dsn = "data",
                            layer = "sous_prefectures",
                            raster_tif = "gin_ppp_2020_UNadj_constrained.tif",
                            drop_Zero = FALSE)

#### load the shapefile for the poverty economist for GIN
ginshp <- sf::st_read(dsn = "data", layer = "sous_prefectures")


### using the model selection algorithm code on the data

##### first read in the GIN_master.RDS file


hh.dt <- readRDS("data/GIN_master.RDS")

selected.vars <- saeplus_selectmodel(dt = hh.dt)
selected.vars <- selected.vars$coefficients[selected.vars$index == TRUE]

selected.vars <- names(selected.vars)


##### create synthetic census with household and polygon level data
### estimate household size

hh.dt <- st_join(st_as_sf(hh.dt), st_as_sf(ginshp))

gin_poly.dt <- saeplus_hhestpoly(geo_dt = gingrid$polygon_dt, hh_dt = hh.dt, shp_dt = ginshp)

## drop the duplicates
gin_poly.dt <- gin_poly.dt[!duplicated(gin_poly.dt$id),]

#### merge in household dataset
hh.dt <- st_as_sf(hh.dt, crs = 4326, agr = "constant")
gingrid$polygon_dt <- st_as_sf(gingrid$polygon_dt, crs = 4326, agr = "constant")

##### create the combined GEE data for GIN
gin_geepoly.dt <- st_read(dsn = "data", layer = "GIN_gee_combined")



###### read in all the OSM data
## combine all building stats
### list all building tif data in the GIN folder
### take the sums of count, area, total length and then averages for density, urban, mean area, cv_area, mean length,
### cv length,

##### first pull in the building data
# buildinglist <- wpopbuilding_vcheck()
# ginbuilding_pull <- wpopbuilding_pull(iso = "GIN")
#
unzip("data-raw/GIN_buildings_v2_0.zip", exdir = "data")


dt <- SAEplus::gengrid(dsn = "data",
                       layer = "sous_prefectures",
                       raster_tif = "GIN_buildings_v2_0_count.tif",
                       grid_shp = T,
                       featname = "bld_count",
                       drop_Zero = F)
gin.bld.dt <- as.data.table(dt$polygon_dt)
dt <- SAEplus::gengrid(dsn = "data",
                       layer = "sous_prefectures",
                       raster_tif = "GIN_buildings_v2_0_cv_area.tif",
                       stats = "mean",
                       grid_shp = T,
                       featname = "bld_cvarea",
                       drop_Zero = F)
add.dt <- as.data.table(dt$polygon_dt)
gin.bld.dt <- cbind(gin.bld.dt, add.dt[,"bld_cvarea"])
dt <- gengrid(dsn = "data",
              layer = "sous_prefectures",
              raster_tif = "GIN_buildings_v2_0_cv_length.tif",
              stats = "mean",
              grid_shp = T,
              featname = "bld_cvlength",
              drop_Zero = F)
add.dt <- as.data.table(dt$polygon_dt)
gin.bld.dt <- cbind(gin.bld.dt, add.dt[,"bld_cvlength"])
dt <- gengrid(dsn = "data",
              layer = "sous_prefectures",
              raster_tif = "GIN_buildings_v2_0_density.tif",
              stats = "mean",
              grid_shp = T,
              featname = "bld_density",
              drop_Zero = F)
add.dt <- as.data.table(dt$polygon_dt)
gin.bld.dt <- cbind(gin.bld.dt, add.dt[,"bld_density"])
dt <- gengrid(dsn = "data",
              layer = "sous_prefectures",
              raster_tif = "GIN_buildings_v2_0_mean_area.tif",
              stats = "mean",
              grid_shp = T,
              featname = "bld_meanarea",
              drop_Zero = F)
add.dt <- as.data.table(dt$polygon_dt)
gin.bld.dt <- cbind(gin.bld.dt, add.dt[,"bld_meanarea"])
dt <- gengrid(dsn = "data",
              layer = "sous_prefectures",
              raster_tif = "GIN_buildings_v2_0_total_length.tif",
              grid_shp = T,
              stats = "mean",
              featname = "bld_totallength",
              drop_Zero = F)
add.dt <- as.data.table(dt$polygon_dt)
gin.bld.dt <- cbind(gin.bld.dt, add.dt[,"bld_totallength"])
saveRDS(gin.bld.dt, "data/GIN_allbuilding.RDS")


## write the data into a shapefile format that can be used to do other work
sf::st_write(st_as_sf(gin.bld.dt, crs = "WGS84", agr = "constant"),
             layer = "GIN_allbuilding", dsn = "data",
             driver = "ESRI Shapefile")

# pull process and join the osm data
gin.osm <- osm_datapull(country = "Guinea",
                        ldrive = "data-raw")

gin.lines <- SAEplus::osm_processlines(shapefile_path = "data/GIN_allbuilding.shp",
                                       geoid_var = "id",
                                       osm_path = "data-raw/Guinea_osmlines")
saveRDS(gin.lines, file = "data/GIN_lines_obj.RDS")


gin.mp <- SAEplus::osm_processmp(shapefile_path = "data/GIN_allbuilding.shp",
                                 geoid_var = "id",
                                 osm_path = "data-raw/Guinea_osmmp",
                                 feature_var = "amenity")

saveRDS(gin.mp, file = "data/GIN_mp_obj")

gin.points <- SAEplus::osm_processpoints(shapefile_path = "data/GIN_allbuilding.shp",
                                         geoid_var = "id",
                                         osm_path = "data-raw/Guinea_osmpoints")

saveRDS(gin.points, file = "data/GIN_points_obj") #save the object as RData


#transform objects from long to wide first
ginline.dt <-
  data.table::dcast(gin.lines[[1]], id ~ highway,
                    value.var = c("roaddensity", "count", "length"),
                    fun.aggregate = mean)

ginmp.dt <-
  data.table::dcast(gin.mp[[1]],
                    id ~ amenity,
                    value.var = "count",
                    fun.aggregate = mean)

### relabel variable names
labs <- colnames(ginmp.dt)[!(colnames(ginmp.dt) %in% "id")]



paste_tolist <- function(X, tag = "pointcount"){
  paste(X, tag, sep = "_")
}

varrelabs <- unlist(lapply(labs, paste_tolist))

data.table::setnames(ginmp.dt, labs, varrelabs)
data.table::setnames(ginmp.dt, "NA_pointcount", "unclassified_pointcount")


ginosm.dt <- ginmp.dt[ginline.dt, on = c("id")]

ginosm.dt <- gin.bld.dt[ginosm.dt, on = "id"] ### all open street maps data merged

#### merge this with the household data
# merge with household data
test <- data.table::as.data.table(readstata13::read.dta13("data/GIN-Grappe_GPS_2018.dta"))
test2 <- data.table::as.data.table(readstata13::read.dta13("data/ehcvm_welfare_GIN2018.dta"))
ginhhgeo.dt <- test[test2, on = c("grappe", "vague")]

### include geospatial data into the household data
ginhhgeo.dt <- sf::st_as_sf(ginhhgeo.dt, coords = c("coordonnes_gps__Longitude", "coordonnes_gps__Latitude"),
                            crs = 4326, agr = "constant")


### load in the google earth engine datasets
gin_geepoly.dt <- list.files(path = "./../S2S-Imputation-WAEMU/InputData", pattern = "GIN")

drop_chr <- function(x){
  x <- substr(x, start = 1, stop = nchar(x) - 4)
  return(x)
}

gin_geepoly.dt <- unique(unlist(lapply(gin_geepoly.dt, drop_chr)))

st_readlist <- function(X){

  obj <- sf::st_read(dsn = "./../S2S-Imputation-WAEMU/InputData", layer = X)
  colnames(obj)[colnames(obj) %in% "mean"] <- X
  return(obj)

}

gin_geepoly.dt <- lapply(gin_geepoly.dt, st_readlist)

### merge the list of spatial objects
convert_merge_DT <- function(sf1, sf2){

  sf1 <- data.table::as.data.table(sf1)
  sf2 <- data.table::as.data.table(sf2)

  obj <- sf1[sf2, on = c("id", "population")]

  return(obj)

}

gin_geepoly.dt <- Reduce(convert_merge_DT, gin_geepoly.dt)

#drop extra geometry columns
geovars <- colnames(gin_geepoly.dt)[grepl("geometry", colnames(gin_geepoly.dt))]
geovars <- geovars[!(geovars %in% "geometry")]

gin_geepoly.dt[,(geovars) := NULL]

names(gin_geepoly.dt) <- tolower(names(gin_geepoly.dt))
gin_geepoly.dt <- sf::st_as_sf(gin_geepoly.dt, crs = 4326, agr = "constant")

### implement join with all data to the household survey
ginosm.dt <- sf::st_as_sf(ginosm.dt, crs = 4326, agr = "constant")
gin_master.dt <- sf::st_join(ginhhgeo.dt, gin_geepoly.dt)
gin_master.dt <- sf::st_join(gin_master.dt, ginosm.dt)

gin_master.dt <- as.data.table(gin_master.dt)


##### also combine all the remote sensing geospatial data
ginosm.dt <- data.table::as.data.table(ginosm.dt)
gin_geepoly.dt <- data.table::as.data.table(gin_geepoly.dt)

gin_masterpoly.dt <- ginosm.dt[gin_geepoly.dt, on = "id"]


saveRDS(gin_master.dt, file = "data/GIN_masterhh.RDS")
saveRDS(gin_masterpoly.dt, file = "data/GIN_masterpoly.RDS")

###########################################################################################
##############################PREPARE FOR S2S IMPUTATION###################################
###########################################################################################

### run the model selection code
selected.vars <- SAEplus::saeplus_selectmodel(dt = gin_master.dt)







