## environment set up
remove(list = objects()) ## clear all objects in R workspace

options(
  stringsAsFactors = F, ## tell R to treat text as text, not factors
  width = 80, ## set the maximum width of outputs as 80 characters
  scipen = 6, ## discourage R from displaying numbers in scientific notation
  mc.cores = 6, ## set number of (mac) cores used in parallel processing
  start.time= Sys.time()
)

### we need to drop the code that pulls together all the data
##### create the gridded polygon data for GIN
gingrid <- SAEplus::gengrid(dsn = "data",
                            layer = "sous_prefectures",
                            raster_tif = "gin_ppp_2020_UNadj_constrained.tif",
                            drop_Zero = FALSE)

#### load the shapefile for the poverty economist for GIN
ginshp <- st_read(dsn = "data", layer = "sous_prefectures")


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

x <- st_join(gingrid$polygon_dt, hh.dt)









