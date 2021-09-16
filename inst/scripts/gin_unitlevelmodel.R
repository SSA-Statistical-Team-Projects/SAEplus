## this script will be used to put together the data required to build the household/unit level model
## and then run it as well
require(readstata13)

#### load the geopolycensus data i.e. geospatial census of remote sensing data
#### as well hh survey and adm level boundary file from poverty economist

geopolycensus_dt <- readRDS("tests/testdata/gin_geopolycensus.RDS")

boundary_dt <- data.table::as.data.table(readstata13::read.dta13("tests/testdata/GIN-Grappe_GPS_2018.dta"))
hhsurvey_dt <- data.table::as.data.table(readstata13::read.dta13("tests/testdata/ehcvm_welfare_GIN2018.dta"))
hhsurvey_dt <- boundary_dt[hhsurvey_dt, on = c("grappe", "vague")]

## convert boundary file to geospatial set
boundary_dt <- sf::st_as_sf(boundary_dt, agr = "constant",
                            coords = c("coordonnes_gps__Latitude",
                                       "coordonnes_gps__Longitude"))


