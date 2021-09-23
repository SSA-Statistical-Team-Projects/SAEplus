## this script will be used to put together the data required to build the household/unit level model
## and then run it as well
require(readstata13)
require(data.table)

#### load the geopolycensus data i.e. geospatial census of remote sensing data
#### as well hh survey and adm level boundary file from poverty economist

geopolycensus_dt <- readRDS("tests/testdata/gin_geopolycensus.RDS")

boundary_dt <- as.data.table(readstata13::read.dta13("tests/testdata/GIN-Grappe_GPS_2018.dta"))
hhsurvey_dt <- as.data.table(readstata13::read.dta13("tests/testdata/ehcvm_welfare_GIN2018.dta"))
hhsurvey_dt <- boundary_dt[hhsurvey_dt, on = c("grappe", "vague")]

## convert boundary file to geospatial set
boundary_dt <- sf::st_as_sf(boundary_dt,
                            agr = "constant",
                            coords = c("coordonnes_gps__Longitude",
                                       "coordonnes_gps__Latitude"),
                            crs = 4326)


## quickly drop extra geometry columns (always important to keep the dataset tractable)
geopolycensus_dt <- drop_extramergevars(dt = geopolycensus_dt)

#######################################################################################################
## The basic cleaning is done! It is time to create the geospatial household sample and a synthetic
## census

#### the geospatial household sample combines hhsurvey_dt with the geospatial variables
hhsurvey_dt <- sf::st_as_sf(hhsurvey_dt,
                            agr = "constant",
                            coords = c("coordonnes_gps__Longitude",
                                       "coordonnes_gps__Latitude"),
                            crs = 4326)

geopolycensus_dt <- sf::st_as_sf(geopolycensus_dt, crs = 4326, agr = "constant")

hhsurvey_dt <- sf::st_join(hhsurvey_dt, geopolycensus_dt)



