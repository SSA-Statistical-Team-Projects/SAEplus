## this script will be used to put together the data required to build the household/unit level model
## and then run it as well
require(readstata13)
require(data.table)
require(sf)
require(spatstat)
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
## include the shapefile with admin level definitions
shapefile_dt <- sf::st_read(dsn = "tests/testdata",
                            layer = "sous_prefectures_valid")

## check the shapefile to see if we have duplicates
shp_test <- saeplus_checkshpfiles(shp_dt = shapefile_dt,
                                  lowest_admin = "ADM3_NAME",
                                  file = "D:/Ify/guineachecks.html")




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

#### make some quick additions to the hhsurvey_dt
###### including the shapefile_dt data
hhsurvey_dt <- st_join(hhsurvey_dt, shapefile_dt)

names_to_chg <- colnames(hhsurvey_dt)[grepl("GIN_", colnames(hhsurvey_dt))]
changed_names <- c("impervious_surface", "no2_julsep18", "no2_aprjun19",
                   "ntl", "precip_julsep18", "precip_aprjun19")
setnames(hhsurvey_dt, names_to_chg, changed_names)

#### drop duplicated observations
hhsurvey_dt <- hhsurvey_dt[!duplicated(hhsurvey_dt$hhid),]

hhsurvey_dt <- as.data.table(hhsurvey_dt)

hhsurvey_dt[ADM1_NAME == "Labe\r\n", ADM1_NAME := "Labe"]

### we also have several missing observations (lets view this!)
tmap_mode("view") +
  tm_basemap() +
  tm_shape(sf::st_as_sf(hhsurvey_dt[is.na(ADM3_NAME) == TRUE,])) + tm_bubbles(col = "red", size = 0.0001)

hhsurvey_dt[is.na(ADM1_NAME) == TRUE & prefecture == "CONAKRY",
            c("ADM1_NAME", "ADM1_CODE", "ADM2_NAME", "ADM2_CODE", "ADM3_NAME", "ADM3_CODE") :=
              list("Conakry", "GIN002", "Conakry", "GIN002001", "Ratoma", "GIN00200105")]

hhsurvey_dt[is.na(ADM1_NAME) == TRUE & prefecture == "MANDIANA",
            c("ADM1_NAME", "ADM1_CODE", "ADM2_NAME", "ADM2_CODE", "ADM3_NAME", "ADM3_CODE") :=
              list("Kankan", "GIN004", "Mandiana", "GIN004004", "Koundianakoro", "GIN00400407")]

### create admin one level dummies
hhsurvey_dt <- saeplus_dummify(dt = hhsurvey_dt, var = "ADM1_NAME")

### convert the admin code variables to integer values (we cannot run emdi::ebp with character variable admin areas)
hhsurvey_dt <- saeplus_dropchars(dt = hhsurvey_dt,
                                 vars = c("ADM1_CODE", "ADM2_CODE", "ADM3_CODE"))
shapefile_dt <- saeplus_dropchars(dt = shapefile_dt,
                                  vars = c("ADM1_CODE", "ADM2_CODE", "ADM3_CODE"))

### now we are ready create a synethetic census
##### first compute average number of households in each grid
##### it is important that hh_dt and shp_dt below are the exact same variable type
grid_hhcount.dt <- saeplus_hhestpoly(geo_dt = geopolycensus_dt[,c("id", "population", "geometry")],
                                     hh_dt = hhsurvey_dt,
                                     shp_dt = shapefile_dt) ##ind_estimate is the household size estimated

grid_hhcount.dt <- grid_hhcount.dt[!duplicated(grid_hhcount.dt$id),] ##drop duplicated IDs

grid_hhcount.dt <- as.data.table(grid_hhcount.dt)

geopolycensus_dt <- grid_hhcount.dt[,c("id", "ind_estimate")][geopolycensus_dt, on = "id"]

rm(grid_hhcount.dt) ##clean up the environment a little by removing the grid_hhcount.dt since we dont need it anymore

hhcensus_dt <- saeplus_gencensus(poly_dt = geopolycensus_dt)

## finally, let's include admin area information into the polygon census data
geocentcensus_dt <- sf::st_centroid(sf::st_as_sf(geopolycensus_dt[,c("id", "geometry")],
                                                 agr = "constant",
                                                 crs = 4326))

shapefile_dt <- sf::st_as_sf(shapefile_dt, agr = "constant", crs = 4326)

geocentcensus_dt <- st_join(geocentcensus_dt, shapefile_dt)
geocentcensus_dt <- geocentcensus_dt[!duplicated(geocentcensus_dt$id),]

geocentcensus_dt <- as.data.table(geocentcensus_dt)

hhcensus_dt <- geocentcensus_dt[hhcensus_dt, on = "id"]

## quickly relabel landcover variables
lc_vars <- c("water.perm", "urban.cove", "shrub.cove", "bare.cover", "tree.cover",
             "crops.cove", "grass.cove", "moss.cover", "water.seas")
new_lc <- c("waterperm_cf", "urban_cf", "shrub_cf", "bare_cf", "tree_cf", "crops_cf",
            "grass_cf", "moss_cf", "waterseasonal_cf")

setnames(geopolycensus_dt, lc_vars, new_lc)
setnames(hhcensus_dt, lc_vars, new_lc)
setnames(hhsurvey_dt, lc_vars, new_lc)

###### ALL THE DATA IS PREPPED NOW WE ARE READY FOR MODEL SELECTION
selection_set <- c("elect_cons", "H2O_column", "cloud_heig", "CO_column_", new_lc, "no2_julsep18", "no2_aprjun19",
                   "ntl", "precip_julsep18", "precip_aprjun19", "count", "cv_length", "mean_length", "total_length",
                   "cv_area", "density", "mean_area", "total_area", "urban", "Boke", "Kindia", "Conakry", "Mamou",
                   "Faranah", "Nzerekore", "Kankan", "Labe")
selected_vars <- SAEplus::saeplus_selectmodel(dt = hhsurvey_dt,
                                              xvars = selection_set,
                                              outcomevar = "pcexp")

selected_vars <- names(selected_vars$index[selected_vars$index == TRUE])

###### create population weights
hhsurvey_dt[, popweight := hhsize * hhweight]


###### now put together the EMDI model
unit_model <- paste(selected_vars, collapse = " + ")
unit_model <- as.formula(paste("pcexp", unit_model, sep = " ~ "))

###### transform the outcome variable to the order norm
pline <- saeplus_ordernormpl(pcexp = hhsurvey_dt[,pcexp])
hhsurvey_dt[,pcexp := orderNorm(pcexp)$x.t]


###### build the unit level model
vars <- c(selected_vars, "pcexp", "ADM3_CODE")
emdi_model <- emdi::ebp(fixed = unit_model,
                        pop_data = as.data.frame(gin_hhcensus.dt[,vars, with = F]),
                        pop_domains = "ADM3_CODE",
                        smp_data = as.data.frame(gin_hhsurvey.dt[,vars, with = F]),
                        smp_domains = "ADM3_CODE",
                        threshold = -0.448955,
                        L = 100,
                        transformation = "no",
                        na.rm = TRUE,
                        weights = "popweight",
                        B = 100,
                        cpus = 30, MSE = TRUE)



















