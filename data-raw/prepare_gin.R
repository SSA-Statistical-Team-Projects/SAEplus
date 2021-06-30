#devtools::load_all()

#' #' @import emdi openxlsx nlme tmap
#' #' @importFrom pysch dummy.code
#'
#'
#' ###########################################################################################
#' ##############################FIRST PULL THE DATA TOGETHER#################################
#' ###########################################################################################
#'
#' ### we need to drop the code that pulls together all the data
#' ##### create the gridded polygon data for GIN
#' gingrid <- SAEplus::gengrid(dsn = "data",
#'                             layer = "sous_prefectures",
#'                             raster_tif = "gin_ppp_2020_UNadj_constrained.tif",
#'                             drop_Zero = FALSE)
#'
#' #### load the shapefile for the poverty economist for GIN
#' ginshp <- sf::st_read(dsn = "data", layer = "sous_prefectures")
#'
#'
#' ### using the model selection algorithm code on the data
#'
#' ##### first read in the GIN_master.RDS file
#'
#'
#' hh.dt <- readRDS("data/GIN_master.RDS")
#'
#' selected.vars <- saeplus_selectmodel(dt = hh.dt)
#' selected.vars <- selected.vars$coefficients[selected.vars$index == TRUE]
#'
#' selected.vars <- names(selected.vars)
#'
#'
#' ##### create synthetic census with household and polygon level data
#' ### estimate household size
#'
#' hh.dt <- st_join(st_as_sf(hh.dt), st_as_sf(ginshp))
#'
#' gin_poly.dt <- saeplus_hhestpoly(geo_dt = gingrid$polygon_dt, hh_dt = hh.dt, shp_dt = ginshp)
#'
#' ## drop the duplicates
#' gin_poly.dt <- gin_poly.dt[!duplicated(gin_poly.dt$id),]
#'
#' #### merge in household dataset
#' hh.dt <- st_as_sf(hh.dt, crs = 4326, agr = "constant")
#' gingrid$polygon_dt <- st_as_sf(gingrid$polygon_dt, crs = 4326, agr = "constant")
#'
#' ##### create the combined GEE data for GIN
#' gin_geepoly.dt <- st_read(dsn = "data", layer = "GIN_gee_combined")
#'
#'
#'
#' ###### read in all the OSM data
#' ## combine all building stats
#' ### list all building tif data in the GIN folder
#' ### take the sums of count, area, total length and then averages for density, urban, mean area, cv_area, mean length,
#' ### cv length,
#'
#' ##### first pull in the building data
#' # buildinglist <- wpopbuilding_vcheck()
#' # ginbuilding_pull <- wpopbuilding_pull(iso = "GIN")
#' #
#' unzip("data-raw/GIN_buildings_v2_0.zip", exdir = "data")
#'
#'
#' dt <- SAEplus::gengrid(dsn = "data",
#'                        layer = "sous_prefectures",
#'                        raster_tif = "GIN_buildings_v2_0_count.tif",
#'                        grid_shp = T,
#'                        featname = "bld_count",
#'                        drop_Zero = F)
#' gin.bld.dt <- as.data.table(dt$polygon_dt)
#' dt <- SAEplus::gengrid(dsn = "data",
#'                        layer = "sous_prefectures",
#'                        raster_tif = "GIN_buildings_v2_0_cv_area.tif",
#'                        stats = "mean",
#'                        grid_shp = T,
#'                        featname = "bld_cvarea",
#'                        drop_Zero = F)
#' add.dt <- as.data.table(dt$polygon_dt)
#' gin.bld.dt <- cbind(gin.bld.dt, add.dt[,"bld_cvarea"])
#' dt <- gengrid(dsn = "data",
#'               layer = "sous_prefectures",
#'               raster_tif = "GIN_buildings_v2_0_cv_length.tif",
#'               stats = "mean",
#'               grid_shp = T,
#'               featname = "bld_cvlength",
#'               drop_Zero = F)
#' add.dt <- as.data.table(dt$polygon_dt)
#' gin.bld.dt <- cbind(gin.bld.dt, add.dt[,"bld_cvlength"])
#' dt <- gengrid(dsn = "data",
#'               layer = "sous_prefectures",
#'               raster_tif = "GIN_buildings_v2_0_density.tif",
#'               stats = "mean",
#'               grid_shp = T,
#'               featname = "bld_density",
#'               drop_Zero = F)
#' add.dt <- as.data.table(dt$polygon_dt)
#' gin.bld.dt <- cbind(gin.bld.dt, add.dt[,"bld_density"])
#' dt <- gengrid(dsn = "data",
#'               layer = "sous_prefectures",
#'               raster_tif = "GIN_buildings_v2_0_mean_area.tif",
#'               stats = "mean",
#'               grid_shp = T,
#'               featname = "bld_meanarea",
#'               drop_Zero = F)
#' add.dt <- as.data.table(dt$polygon_dt)
#' gin.bld.dt <- cbind(gin.bld.dt, add.dt[,"bld_meanarea"])
#' dt <- gengrid(dsn = "data",
#'               layer = "sous_prefectures",
#'               raster_tif = "GIN_buildings_v2_0_total_length.tif",
#'               grid_shp = T,
#'               stats = "mean",
#'               featname = "bld_totallength",
#'               drop_Zero = F)
#' add.dt <- as.data.table(dt$polygon_dt)
#' gin.bld.dt <- cbind(gin.bld.dt, add.dt[,"bld_totallength"])
#' saveRDS(gin.bld.dt, "data/GIN_allbuilding.RDS")
#'
#'
#' ## write the data into a shapefile format that can be used to do other work
#' sf::st_write(st_as_sf(gin.bld.dt, crs = "WGS84", agr = "constant"),
#'              layer = "GIN_allbuilding", dsn = "data",
#'              driver = "ESRI Shapefile")
#'
#' # pull process and join the osm data
#' gin.osm <- osm_datapull(country = "Guinea",
#'                         ldrive = "data-raw")
#'
#' gin.lines <- SAEplus::osm_processlines(shapefile_path = "data/GIN_allbuilding.shp",
#'                                        geoid_var = "id",
#'                                        osm_path = "data-raw/Guinea_osmlines")
#' saveRDS(gin.lines, file = "data/GIN_lines_obj.RDS")
#'
#'
#' gin.mp <- SAEplus::osm_processmp(shapefile_path = "data/GIN_allbuilding.shp",
#'                                  geoid_var = "id",
#'                                  osm_path = "data-raw/Guinea_osmmp",
#'                                  feature_var = "amenity")
#'
#' saveRDS(gin.mp, file = "data/GIN_mp_obj")
#'
#' gin.points <- SAEplus::osm_processpoints(shapefile_path = "data/GIN_allbuilding.shp",
#'                                          geoid_var = "id",
#'                                          osm_path = "data-raw/Guinea_osmpoints")
#'
#' saveRDS(gin.points, file = "data/GIN_points_obj") #save the object as RData
#'
#'
#' #transform objects from long to wide first
#' ginline.dt <-
#'   data.table::dcast(gin.lines[[1]], id ~ highway,
#'                     value.var = c("roaddensity", "count", "length"),
#'                     fun.aggregate = mean)
#'
#' ginmp.dt <-
#'   data.table::dcast(gin.mp[[1]],
#'                     id ~ amenity,
#'                     value.var = "count",
#'                     fun.aggregate = mean)
#'
#' ### relabel variable names
#' labs <- colnames(ginmp.dt)[!(colnames(ginmp.dt) %in% "id")]
#'
#'
#'
#' paste_tolist <- function(X, tag = "pointcount"){
#'   paste(X, tag, sep = "_")
#' }
#'
#' varrelabs <- unlist(lapply(labs, paste_tolist))
#'
#' data.table::setnames(ginmp.dt, labs, varrelabs)
#' data.table::setnames(ginmp.dt, "NA_pointcount", "unclassified_pointcount")

####GINenvironment.RData is saved at this point
load("data/GINenvironment.RData")

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

gin_geepoly.dt <- gin_geepoly.dt[!(gin_geepoly.dt %in% c("GIN_Electricity_2018", "GIN_LC_2018JanDec"))]

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

gin_lc.dt <- data.table::fread("data-raw/LC_mean_bygrid_guinea.csv")
gin_lc.dt <- gin_lc.dt[,c("id","bare-coverfraction", "crops-coverfraction","moss-coverfraction",
                          "shrub-coverfraction", "tree-coverfraction", "urban-coverfraction",
                          "water-permanent-coverfraction", "water-seasonal-coverfraction"),with=F]

gin_geepoly.dt <- gin_lc.dt[gin_geepoly.dt, on = "id"]

## include electricity data
# dt <- SAEplus::gengrid(dsn = "data",
#                        layer = "sous_prefectures",
#                        raster_tif = "GIN_Electricity_2018.tif",
#                        drop_Zero = FALSE,
#                        grid_shp = T,
#                        featname = "electricity",
#                        stats = "mean")


#drop extra geometry columns
geovars <- colnames(gin_geepoly.dt)[grepl("geometry", colnames(gin_geepoly.dt))]
geovars <- geovars[!(geovars %in% "geometry")]

#gin_geepoly.dt[,(geovars) := NULL]

names(gin_geepoly.dt) <- tolower(names(gin_geepoly.dt))
gin_geepoly.dt <- sf::st_as_sf(gin_geepoly.dt, crs = 4326, agr = "constant")


### implement join with all data to the household survey
ginosm.dt <- sf::st_as_sf(ginosm.dt, crs = 4326, agr = "constant")
gin_master.dt <- sf::st_join(ginhhgeo.dt, gin_geepoly.dt)
gin_master.dt <- sf::st_as_sf(gin_master.dt, crs = 4326, agr = "constant")
gin_master.dt <- sf::st_join(gin_master.dt, ginosm.dt)

gin_master.dt <- data.table::as.data.table(gin_master.dt)


##### also combine all the remote sensing geospatial data
ginosm.dt <- data.table::as.data.table(ginosm.dt)
gin_geepoly.dt <- data.table::as.data.table(gin_geepoly.dt)

gin_masterpoly.dt <- ginosm.dt[gin_geepoly.dt, on = "id"]


### include the HDX data as well
gin_hdx.dt <- hdx_pull(iso = "GIN")
gin_hdx.dt <- sf::st_as_sf(gin_hdx.dt, coords = c("longitude", "latitude"),crs = 4326, agr = "constant")

gin_masterpoly.dt <- sf::st_as_sf(gin_masterpoly.dt, crs = 4326, agr = "constant")
gin_masterpoly.dt <- sf::st_join(gin_masterpoly.dt, gin_hdx.dt)
gin_master.dt <- sf::st_as_sf(gin_master.dt, crs = 4326, agr = "constant")
gin_master.dt <- sf::st_join(gin_master.dt, gin_masterpoly.dt[,c("rwi", "geometry")])

#### add the data to LC data to gin masterpoly.dt
# lcnames <- colnames(gin_geepoly.dt)[grepl("coverfraction", colnames(gin_geepoly.dt))]
# gin_masterpoly.dt <- gin_geepoly.dt[,c("id", lcnames),with=F][as.data.table(gin_masterpoly.dt), on = "id"]
#
# gin_master.dt <- sf::st_join(gin_master.dt, gin_masterpoly.dt[,c(lcnames, "geometry")])
# saveRDS(gin_master.dt, file = "data/GIN_masterhh.RDS")
# saveRDS(gin_masterpoly.dt, file = "data/GIN_masterpoly.RDS")


#### create community level variables
gin_mastercentroid.dt <- sf::st_centroid(gin_masterpoly.dt)
gin_mastercentroid.dt <- st_as_sf(gin_mastercentroid.dt, agr = "constant", crs = 4326)
ginshp <- st_as_sf(ginshp, agr = "constant", crs = 4326)
gin_mastercentroid.dt <- sf::st_join(gin_mastercentroid.dt, ginshp)



gin_mastercentroid.dt <- as.data.table(gin_mastercentroid.dt)
gin_mastercentroid.dt <- gin_mastercentroid.dt[!duplicated(id),]
### create the aggregate indicators at the adm3 level
gin_mastercentroid.dt <- as.data.table(gin_mastercentroid.dt)

gin_mastercentroid.dt[, Kankan := ifelse(ADM1_NAME == "Kankan", 1, 0)]
gin_mastercentroid.dt[, Kindia := ifelse(ADM1_NAME == "Kindia", 1, 0)]
gin_mastercentroid.dt[, Conakry := ifelse(ADM1_NAME == "Conakry", 1, 0)]
gin_mastercentroid.dt[, Nzerekore := ifelse(ADM1_NAME == "Nzerekore", 1, 0)]
gin_mastercentroid.dt[, Boke := ifelse(ADM1_NAME == "Boke", 1, 0)]
gin_mastercentroid.dt[, Labe := ifelse(ADM1_NAME == "Labe", 1, 0)]
gin_mastercentroid.dt[, Faranah := ifelse(ADM1_NAME == "Faranah", 1, 0)]
gin_mastercentroid.dt[, Mamou := ifelse(ADM1_NAME == "Mamou", 1, 0)]

idvars <- c("bld_", "_2018", "_2019",
            "rwi", "Conakry", "Kankan", "Nzerekore",
            "Faranah", "Labe", "Mamou", "Kindia", "Boke",
            "coverfraction")

mult_grepl <- function(ids = idvars,
                       dt = gin_mastercentroid.dt){

  vars <- colnames(dt)[grepl(ids, colnames(dt))]

  return(vars)
}

vars <- unlist(lapply(idvars, mult_grepl))


append_to_names <- function(X = vars){

  var <- paste(X, "adm", sep = "_")
  return(var)

}

new_vars <- unlist(lapply(vars, append_to_names))


gin_mastercentroid.dt[, (new_vars) := lapply(.SD, weighted.mean, w = population), by = "ADM3_CODE", .SDcols = vars]

gin_masterpoly.dt <- as.data.table(gin_masterpoly.dt)
gin_masterpoly.dt <- gin_mastercentroid.dt[, c(new_vars, "id"), with = FALSE][gin_masterpoly.dt, on = "id"]

### add gin_master.dt into the data


###########################################################################################
##############################PREPARE FOR S2S IMPUTATION###################################
###########################################################################################
## create the state level variables to be included in the model selection process
ginshp <- st_as_sf(ginshp, agr = "constant", crs = 4326)
gin_master.dt <- st_as_sf(gin_master.dt, crs = 4326, agr = "constant")

gin_master.dt <- st_join(gin_master.dt, ginshp)

gin_master.dt <- sf::st_as_sf(gin_master.dt, crs = 4326, agr = "constant")
gin_masterpoly.dt <- sf::st_as_sf(gin_masterpoly.dt, crs = 4326, agr = "constant")
gin_master.dt <- sf::st_join(gin_master.dt, gin_masterpoly.dt[,c(new_vars, "geometry"),with=F])


gin_master.dt <- as.data.table(gin_master.dt)

gin_master.dt[,ADM1_NUMBER := plyr::mapvalues(ADM1_NAME, from = unique(gin_master.dt$ADM1_NAME),
                                              to = 1:length(unique(gin_master.dt$ADM1_NAME)))]
gin_master.dt[,ADM1_NUMBER := as.numeric(ADM1_NUMBER)]

### create state level dummies
gin_master.dt[ADM1_NAME == "Labe\r\n", ADM1_NAME := "Labe"]
gin_master.dt <- cbind(gin_master.dt, gin_master.dt[,psych::dummy.code(ADM1_NAME)])
gin_master.dt[,missing_state := ifelse(Kankan == 0 && Faranah == 0 && Labe == 0 && Mamou == 0 && Kindia == 0 &&
                                         Boke == 0 && Conakry == 0 && Nzerekore == 0, 1, 0)]

## drop the multicollinear variables
gin_master.dt[,gin_lc_2018julsep := NULL]
gin_master.dt[,length_secondary_link := NULL]
gin_master.dt[,count_secondary_link := NULL]

## relabel some of the missing ADM3 areas
# hh.dt <- as.data.table(hh.dt)
# hh.dt[,id := 1:.N]
# hh.dt[id %in% 816:827,
#       c("ADM3_NAME", "ADM3_CODE", "ADM2_NAME", "ADM2_CODE", "ADM1_NAME", "ADM1_CODE") :=
#         list("Ratoma", 200105, "Conakry", 2001, "Conakry", 2)]
#
# hh.dt[id %in% 2591:2602,
#       c("ADM3_NAME", "ADM3_CODE", "ADM2_NAME", "ADM2_CODE", "ADM1_NAME", "ADM1_CODE") :=
#         list("Balandougouba", 400401, "Mandiana", 4004, "Kankan", 4)]
#
# hh.dt[id %in% 4948:4959,
#       c("ADM3_NAME", "ADM3_CODE", "ADM2_NAME", "ADM2_CODE", "ADM1_NAME", "ADM1_CODE") :=
#         list("Ratoma", 200105, "Conakry", 2001, "Conakry", 2)]


### run the model selection code
# selected.vars <- SAEplus::saeplus_selectmodel(dt = gin_master.dt,
#                                               var_identifier = c("roaddensity_", "count_", "length_",
#                                                                  "_pointcount", "bld_", "_2018", "_2019",
#                                                                  "rwi", "Conakry", "Kankan", "Nzerekore",
#                                                                  "Faranah", "Labe", "Mamou", "Kindia", "Boke",
#                                                                  "coverfraction"))



### compute adm3 level aggregates
gin_master.dt[,popweight := hhsize * hhweight]
gin_master.dt[,spopweight := mean(popweight, na.rm = TRUE), by = "ADM3_CODE"]
gin_master.dt[,spopweight := popweight / spopweight]



# gin_master.dt[, (new_vars) := lapply(.SD, weighted.mean, w = popweight), by = "ADM3_CODE", .SDcols = vars]

selected.vars <- SAEplus::saeplus_selectmodel(dt = gin_master.dt,
                                              var_identifier = c("bld_", "_2018", "_2019",
                                                                 "rwi", "Conakry", "Kankan", "Nzerekore",
                                                                 "Faranah", "Labe", "Mamou", "Kindia", "Boke",
                                                                 "coverfraction"))

selected.vars <- names(selected.vars$index[selected.vars$index == TRUE])

saveRDS(selected.vars, "data/gin_selectedvars.RDS")

gin_master.dt[, Kankan := ifelse(ADM1_NAME == "Kankan", 1, 0)]
gin_master.dt[, Kindia := ifelse(ADM1_NAME == "Kindia", 1, 0)]
gin_master.dt[, Conakry := ifelse(ADM1_NAME == "Conakry", 1, 0)]
gin_master.dt[, Nzerekore := ifelse(ADM1_NAME == "Nzerekore", 1, 0)]
gin_master.dt[, Boke := ifelse(ADM1_NAME == "Boke", 1, 0)]
gin_master.dt[, Labe := ifelse(ADM1_NAME == "Labe", 1, 0)]
gin_master.dt[, Faranah := ifelse(ADM1_NAME == "Faranah", 1, 0)]
gin_master.dt[, Mamou := ifelse(ADM1_NAME == "Mamou", 1, 0)]

#### census household dataset
## computing the number of households per grid estimates




gridhh_count.dt <- saeplus_hhestpoly(geo_dt = gin_mastercentroid.dt,
                                     hh_dt = gin_master.dt,
                                     shp_dt = ginshp)

setnames(gridhh_count.dt, c("ADM1_CODE.x", "ADM2_CODE.x", "ADM3_CODE.x", "ADM1_NAME.x", "ADM2_NAME.x", "ADM3_NAME.x"),
         c("ADM1_CODE", "ADM2_CODE", "ADM3_CODE", "ADM1_NAME", "ADM2_NAME", "ADM3_NAME"))
gridhh_count.dt <- as.data.table(gridhh_count.dt)
gridhh_count.dt <- gridhh_count.dt[!duplicated(id),]

gridhh_count.dt <- saeplus_gencensus(poly_dt = gridhh_count.dt)
gridhh_count.dt[ADM1_NAME == "Labe\r\n", ADM1_NAME := "Labe"]

gridhh_count.dt[, Kankan := ifelse(ADM1_NAME == "Kankan", 1, 0)]
gridhh_count.dt[, Kindia := ifelse(ADM1_NAME == "Kindia", 1, 0)]
gridhh_count.dt[, Conakry := ifelse(ADM1_NAME == "Conakry", 1, 0)]
gridhh_count.dt[, Nzerekore := ifelse(ADM1_NAME == "Nzerekore", 1, 0)]
gridhh_count.dt[, Boke := ifelse(ADM1_NAME == "Boke", 1, 0)]
gridhh_count.dt[, Labe := ifelse(ADM1_NAME == "Labe", 1, 0)]
gridhh_count.dt[, Faranah := ifelse(ADM1_NAME == "Faranah", 1, 0)]
gridhh_count.dt[, Mamou := ifelse(ADM1_NAME == "Mamou", 1, 0)]

#### the datasets
selected.vars <- gsub("`", "", selected.vars)



## relabel the missing ADM3 codes
# gin_master.dt <- as.data.table(gin_master.dt)
# gin_master.dt[,id := 1:.N]
# gin_master.dt[id %in% 816:827,
#       c("ADM3_NAME", "ADM3_CODE", "ADM2_NAME", "ADM2_CODE", "ADM1_NAME", "ADM1_CODE") :=
#         list("Ratoma", 200105, "Conakry", 2001, "Conakry", 2)]
#
# gin_master.dt[id %in% 2591:2602,
#       c("ADM3_NAME", "ADM3_CODE", "ADM2_NAME", "ADM2_CODE", "ADM1_NAME", "ADM1_CODE") :=
#         list("Balandougouba", 400401, "Mandiana", 4004, "Kankan", 4)]
#
# gin_master.dt[id %in% 4948:4959,
#       c("ADM3_NAME", "ADM3_CODE", "ADM2_NAME", "ADM2_CODE", "ADM1_NAME", "ADM1_CODE") :=
#         list("Ratoma", 200105, "Conakry", 2001, "Conakry", 2)]


### reassign ADM names and codes to missing observations
#selected.vars <- c(selected.vars, "Conakry", "Boke")
gin_hhsurvey.dt <- gin_master.dt[,c("ADM1_CODE", "ADM2_CODE", "ADM3_CODE", "hhweight", "pcexp", "hhsize", selected.vars),
                                 with=F]
gin_hhcensus.dt <- gridhh_count.dt[,c("ADM1_CODE", "ADM2_CODE", "ADM3_CODE", "ind_estimate",selected.vars),with=F]


#### perform EMDI
gin_model <- paste(selected.vars, collapse = " + ")
gin_model <- as.formula(paste("pcexp", gin_model, sep = " ~ "))

gin_hhsurvey.dt <- gin_hhsurvey.dt[is.na(gin_hhsurvey.dt$ADM3_CODE) == FALSE,]


replace_NA <- function(DT) {
  for (i in names(DT))
    DT[is.na(get(i)), (i):=0]

  return(DT)

}

gin_hhsurvey.dt <- replace_NA(gin_hhsurvey.dt)
gin_hhcensus.dt <- replace_NA(gin_hhcensus.dt)

#recode the adm codes to be numerics
gin_hhsurvey.dt[,ADM3_CODE := as.integer(substr(ADM3_CODE, 4, nchar(ADM3_CODE)))]
gin_hhcensus.dt[,ADM3_CODE := as.integer(substr(ADM3_CODE, 4, nchar(ADM3_CODE)))]

gin_hhsurvey.dt[,ADM2_CODE := as.integer(substr(ADM2_CODE, 4, nchar(ADM2_CODE)))]
gin_hhcensus.dt[,ADM2_CODE := as.integer(substr(ADM2_CODE, 4, nchar(ADM2_CODE)))]

gin_hhsurvey.dt[,ADM1_CODE := as.integer(substr(ADM1_CODE, 4, nchar(ADM1_CODE)))]
gin_hhcensus.dt[,ADM1_CODE := as.integer(substr(ADM1_CODE, 4, nchar(ADM1_CODE)))]

# gin_hhsurvey.dt <- gin_hhsurvey.dt[is.na(ADM3_CODE) == FALSE,]
# gin_hhsurvey.dt <- gin_hhsurvey.dt[is.na(ADM2_CODE) == FALSE,]
# gin_hhsurvey.dt <- gin_hhsurvey.dt[is.na(ADM1_CODE) == FALSE,]

gin_hhsurvey.dt[,pcexp := bestNormalize::orderNorm(pcexp)$x.t]







#save the datasets to have a new environment for EMDI
saveRDS(gin_hhcensus.dt, file = "data/gin_hhcensus.RDS")
saveRDS(gin_hhsurvey.dt, file = "data/gin_hhsurvey.RDS")




##############################################################################################################################

#### calibrate the poverty rates
#ginemdi_model2 <- readRDS("data/ginemdi_model2.RDS")

###### Below are the steps to be taken for the poverty rate calibration #####

#1.	Generate population estimate for each sub-prefecture by aggregating across grids
#2.	Use these population estimates for each sampled sub-prefecture as weights to generate small area estimates for
#   each state. Call these the sae state estimates.
#         a.	Take the sum of population*poverty rate / total population, by sub-prefecture
#3.	Calculate state estimates of poverty rates from the survey
#4.	Divide the vector of state survey estimates by sae estimates to get the benchmarking ratio.
#5.	Multiply the point estimates by the benchmarking ratio.

## combine master polygon data with the shpfiles
gin_masterpoly.dt <- sf::st_as_sf(gin_masterpoly.dt, agr = "constant", crs = 4326)
ginshp <- sf::st_as_sf(ginshp, agr = "constant", crs = 4326)


gin_mastercentroid.dt <- as.data.table(gin_mastercentroid.dt)
gin_mastercentroid.dt[,ADM3_CODE := as.integer(substr(ADM3_CODE, 4, nchar(ADM3_CODE)))]
gin_mastercentroid.dt[,ADM2_CODE := as.integer(substr(ADM2_CODE, 4, nchar(ADM2_CODE)))]
gin_mastercentroid.dt[,ADM1_CODE := as.integer(substr(ADM1_CODE, 4, nchar(ADM1_CODE)))]

hh.dt <- as.data.table(hh.dt)
hh.dt[,ADM3_CODE := as.integer(substr(ADM3_CODE, 4, nchar(ADM3_CODE)))]
hh.dt[,ADM2_CODE := as.integer(substr(ADM2_CODE, 4, nchar(ADM2_CODE)))]
hh.dt[,ADM1_CODE := as.integer(substr(ADM1_CODE, 4, nchar(ADM1_CODE)))]

