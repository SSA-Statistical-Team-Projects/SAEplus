## this script will be used to put together the data required to build the household/unit level model
## and then run it as well
require(readstata13)
require(data.table)
require(sf)
require(spatstat)
#### load the geopolycensus data i.e. geospatial census of remote sensing data
#### as well hh survey and adm level boundary file from poverty economist

geopolycensus_dt <- readRDS("tests/testdata/gin_geopolycensus.RDS") ##read in geospatial data

## read household locations and survey data
hhlocation_dt <- as.data.table(readstata13::read.dta13("tests/testdata/GIN-Grappe_GPS_2018.dta"))
hhsurvey_dt <- as.data.table(readstata13::read.dta13("tests/testdata/ehcvm_welfare_GIN2018.dta"))

## include the shapefile with admin level definitions
shapefile_dt <- sf::st_read(dsn = "tests/testdata",
                            layer = "sous_prefectures_valid")

## now we use the saeplus_prepdata() to clean up the data before they can be used to prep the unit model
clean_obj <-
  saeplus_prepdata(hhsurvey_dt = hhsurvey_dt,
                   hhid_var = "hhid",
                   adminshp_dt = shapefile_dt,
                   hhgeo_dt = hhlocation_dt,
                   hhcoords = c("coordonnes_gps__Longitude",
                                "coordonnes_gps__Latitude"),
                   geosurvey_mergevars = c("vague", "grappe"),
                   geopolycensus_dt = geopolycensus_dt)

clean_obj$survey_data[ADM1_NAME == "Labe\r\n", ADM1_NAME := "Labe"]

clean_obj$survey_data[is.na(ADM1_NAME) == TRUE & prefecture == "CONAKRY",
            c("ADM1_NAME", "ADM1_CODE", "ADM2_NAME", "ADM2_CODE", "ADM3_NAME", "ADM3_CODE") :=
              list("Conakry", 2, "Conakry", 2001, "Ratoma", 200105)]

clean_obj$survey_data[is.na(ADM1_NAME) == TRUE & prefecture == "MANDIANA",
            c("ADM1_NAME", "ADM1_CODE", "ADM2_NAME", "ADM2_CODE", "ADM3_NAME", "ADM3_CODE") :=
              list("Kankan", 4, "Mandiana", 4004, "Koundianakoro", 400407)]


clean_obj$survey_data <- saeplus_dummify(dt = clean_obj$survey_data, var = "ADM1_NAME")

## now we are ready to go into the saeplus_modelunitlevel()
modelvars <-
colnames(clean_obj$geopolygon_census)[!(colnames(clean_obj$geopolygon_census) %in%
                                          c("id", "imagery_year", "population", "geometry"))]

gin_modelresults <-
  saeplus_modelunitlevel(hhsurvey_dt = clean_obj$survey_data,
                         hhid_var = "hhid", hhsize = "hhsize",
                         adminshp_dt = clean_obj$adminshp_data,
                         target_id = "ADM3_CODE",
                         geopolycensus_dt = clean_obj$geopolygon_census,
                         geopopvar = "population",
                         crs_set = rep(4326, 3),
                         agr_set = rep("constant", 3),
                         cand_vars = modelvars,
                         outcome_var = "pcexp",
                         wgt_vartype = "hh",
                         weight = "hhweight",
                         create_dummy = FALSE)


