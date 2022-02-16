#### load libraries
library(sf)
library(rgdal)
library(SAEplus)
library(raster)
library(units)

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
ntl_dt <- gee_pullbigdata(shp_dsn = "inst/extdata",
                          shp_layer = "maputo_grid",
                          gee_chunksize = 20)

#### estimate unit level model
###### load survey data
survey_dt <- st_read(dsn = "inst/extdata",
                     layer = "maputosurvey_dt")

survey_dt <- st_join(survey_dt, grid_dt)

survey_dt$subnatid1 <- stringr::str_replace(survey_dt$subnatid1, "\\?", "\\-") #a bit of cleaning

survey_dt$subnatid2 <- stringr::str_replace(survey_dt$subnatid2, "\\?", "\\-") #a bit of cleaning

##### generate a synthetic census
survey_dt$hsize <- as.numeric(survey_dt$hsize)



##### check to see if we can estimate ebp unit level model
census_dt <- readRDS("inst/extdata/maputo_census.RDS")

survey_dt$welfare_trans <- orderNorm(survey_dt$welfare)$x.t

##find transformed poverty line
povline <- saeplus_ordernormpl(npl_value = 50000, pcexp = survey_dt$welfare)
survey_dt$regioncode <- as.integer(survey_dt$regioncode)

unit_model <- ebp(fixed = welfare ~ age,
                  pop_data = as.data.frame(census_dt[,c("regioncode", "age")]),
                  pop_domains = "regioncode",
                  smp_data = as.data.frame(survey_dt[,c("welfare", "age",
                                                        "popweight", "regioncode")]),
                  smp_domains = "regioncode",
                  L = 50,
                  threshold = 50000,
                  transformation = "log",
                  B = 2,
                  weights = "popweight")


### merge the results into shapefile
shp_dt$regioncode <- unique(survey_dt$regioncode)
setnames(shp_dt, "regioncode", "Domain")
shp_dt$Domain <- as.factor(shp_dt$Domain)

shp_dt <- merge(shp_dt, unit_model$ind, by = "Domain")


tm_shape(shp_dt) +
  tm_polygons("Head_Count")




