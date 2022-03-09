#### load libraries
library(sf)
library(rgdal)
library(SAEplus)
library(raster)
library(units)
library(ggplot2)

sf_use_s2(FALSE)
### load shapefile/boundaries and survey data
shp_dt <- st_read(dsn = "inst/extdata", ##load shapefile
                  layer = "maputo")

survey_dt <- st_read(dsn = "inst/extdata",
                     layer = "maputosurvey_dt")


crs_dt <- rgdal::make_EPSG() ##the database of recognized coordinate reference systems in R

popn_raster <- raster::raster("//esapov/esapov/MOZ/GEO/Population/moz_ppp_2020_1km_Aggregated_UNadj.tiff")

shp_dt$area <- st_area(x = shp_dt)
shp_dt$area <- set_units(x = shp_dt$area, "km^2")

shp_dt  <- st_transform(x = shp_dt, ##transform shapefile into a metric projection to preserve area and distance
                      crs = crs_dt$prj4[crs_dt$code == 3974]) ##this will help you grid in km^2



### grid shapefiles
grid_dt <- gengrid2(shp_dt = shp_dt,
                    grid_size = 1000,
                    pop_raster = popn_raster,
                    extract_name = "population")

## save the shapefile
sf::st_write(obj = grid_dt[,c("poly_id", "geometry")],
             dsn = "inst/extdata",
             layer = "maputo_grid",
             driver = "ESRI Shapefile",
             append = FALSE)


## pulling data from google earth engine
shp_dt <- st_read(dsn = "inst/extdata", ##first read in the gridded shapefile
                  layer = "maputo_grid")

ntl_dt <- gee_pullbigdata(shp_dt = shp_dt,  ##pull night time lights (low resolution data)
                          gee_chunksize = 20,
                          gee_scale = 463)

rainfall_dt <- gee_pullbigdata(shp_dt = shp_dt,
                               gee_chunksize = 20,
                               gee_name = "UCSB-CHG/CHIRPS/DAILY",
                               gee_scale = 5566,
                               gee_band = "precipitation")

## write a quick function to compute mean, max, min, std, median
gee_ReduceFeatures <- function(dt,
                               var_id = "avg_rad",
                               poly_id = "poly_id"){

  dt <- as.data.table(dt) ##ensures that dt is a data.table

  cols_dt <- colnames(dt)[grepl(var_id, colnames(dt))] ##
  cols_dt <- dt[, cols_dt, with = FALSE]
  results_dt <- cols_dt[, list(apply(.SD, 1, mean, na.rm = TRUE),
                               apply(.SD, 1, max, na.rm = TRUE),
                               apply(.SD, 1, min, na.rm = TRUE),
                               apply(.SD, 1, sd, na.rm = TRUE),
                               apply(.SD, 1, median, na.rm = TRUE))]

  colnames(results_dt) <- c("mean", "max", "min", "sd", "median")

  results_dt <- cbind(dt[,poly_id,with = FALSE], results_dt)

  return(results_dt)


}

reducentl_dt <- gee_ReduceFeatures(dt = ntl_dt)
grid_dt <- as.data.table(grid_dt)
grid_dt <- reducentl_dt[grid_dt, on = "poly_id"]


#### estimate unit level model
###### load survey data
survey_dt <- st_read(dsn = "inst/extdata",
                     layer = "maputosurvey_dt")

grid_dt <- st_as_sf(grid_dt, crs = 4326, agr = "constant")
survey_dt <- st_join(survey_dt, grid_dt)

survey_dt$subnatid1 <- stringr::str_replace(survey_dt$subnatid1, "\\?", "\\-") #a bit of cleaning

survey_dt$subnatid2 <- stringr::str_replace(survey_dt$subnatid2, "\\?", "\\-") #a bit of cleaning


survey_dt$hsize <- as.numeric(survey_dt$hsize) ##convert hsize var to numeric



##### check to see if we can estimate ebp unit level model
census_dt <- readRDS("inst/extdata/maputo_census.RDS")

# #### incase you need to transform the variable
# survey_dt$welfare_trans <- orderNorm(survey_dt$welfare)$x.t
#
# ##find transformed poverty line
# povline <- saeplus_ordernormpl(npl_value = 50000, pcexp = survey_dt$welfare)
survey_dt$regioncode <- as.integer(survey_dt$regioncode)

###variable selection is possible also using SAEplus
###the function is called saeplus_selectmodel() and it uses LASSO
###there are other functions in standard R

unit_model <- emdi::ebp(fixed = welfare ~ age,
                        pop_data = as.data.frame(census_dt[,c("regioncode", "age")]),
                        pop_domains = "regioncode",
                        smp_data = as.data.frame(survey_dt[,c("welfare", "age",
                                                              "popweight",
                                                              "regioncode")]),
                        smp_domains = "regioncode",
                        L = 50,
                        threshold = 50000,
                        transformation = "log",
                        weights = "popweight",
                        B = 3)

emdi::write.excel(object = unit_model,
                  file = "inst/extdata/maputopovrates.xlsx")

### merge the results into shapefile


##read in the maputo shapefile
maputo_shp <- st_read(dsn = "inst/extdata", ##load shapefile
                  layer = "maputo")

maputo_shp$regioncode <- unique(survey_dt$regioncode)

colnames(maputo_shp)[colnames(maputo_shp) == "regioncode"] <- "Domain"
maputo_shp$Domain <- as.factor(maputo_shp$Domain)

maputo_shp <- merge(maputo_shp, unit_model$ind, by = "Domain")

##what does this look like on a map?
##R has loads of plotting options

## in ggplot
ggplot(data = maputo_shp) +
  geom_sf(aes(fill = Head_Count), alpha = 0.8) +
  #scale_fill_viridis_c() +
  ggtitle(label = "Poverty Map for Maputo Cidade", subtitle = "Head Count Ratio")

tm_shape(maputo_shp) +
  tm_polygons("Head_Count")


