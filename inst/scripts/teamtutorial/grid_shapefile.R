### reading in the data

shp_dt <- readRDS("inst/extdata/maputoshp.RDS") ##load the shapefile


pop_raster <- raster("//esapov/esapov/MOZ/GEO/Population/moz_ppp_2020_1km_Aggregated_UNadj.tiff")

### create 1sqkm tesselation

#### first ensure the grid units match the units of the shapefile coordinate reference system
sf_use_s2(FALSE)

shp_dt$area <- st_area(shp_dt) ##lets compute the area of the shapefiles
shp_dt$area <- units::set_units(shp_dt$area, "km^2")

crs_dt <- rgdal::make_EPSG() ##the database of recognized coordinate reference systems in R
test <- st_transform(shp_dt, ## test to see if it transforms into a CRS that works
                       crs = crs_dt$prj4[crs_dt$code == 3974])

test$area <- st_area(test) ##compute the area and lets see if it sums to a number near equal to the original shp_dt
test$area <- units::set_units(test$area, "km^2") ##change area to km2 and then we can plot

plot(st_geometry(test))
plot(st_geometry(shp_dt))

shp_dt <- st_transform(shp_dt, ## test to see if it transforms into a CRS that works
                       crs = crs_dt$prj4[crs_dt$code == 3974])

## create the grid
grid_dt <- gengrid2(shp_dt = shp_dt,
                    grid_size = 1000,
                    pop_raster = pop_raster,
                    extract_name = "population")


## save the shapefile
sf::st_write(obj = grid_dt[,c("poly_id", "geometry")],
             dsn = "inst/extdata",
             layer = "maputo_grid",
             driver = "ESRI Shapefile",
             append = FALSE)


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








