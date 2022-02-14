set.seed(123)
library(tidyverse)
sf::sf_use_s2(FALSE)
### simple script to create package data for testing using the Mozambique shapefile

moz_dt <- readstata13::read.dta13(file = "../MOZpovmap/inst/data/MOZ_2019_IOF_v01_M_v01_A_SSAPOV_GMD.dta")

moz_dt <- moz_dt[,c("hhid", "subnatid1", "subnatid2", "age", "hsize", "welfare", "whours", "rooms")]

maputo_dt <- moz_dt[moz_dt$subnatid1 %in% "11 – Maputo Cidade",] ##select only those in the capital maputo

##ensure all variables are numerics
### regenerating the household IDs

maputo_dt$hhid <- 1:nrow(maputo_dt)

maputo_dt <- data.table::as.data.table(maputo_dt)

### use shapefile to load in fake geospatial locations
shp_dt <- sf::st_read(dsn = "//esapov/esapov/MOZ/GEO/Population/moz_censusshapefile2017",
                      layer = "BASE_COMPLETA_DE_AE_CENSO_2017")


maputo_dt <- maputo_dt[subnatid2 == "", subnatid2 := "100 - AEROPORTO INTERNACIONAL DE MAPUTO"]

maputo_dt[,regioncode := stringr::str_extract(subnatid2, "\\d+")] ##create digit subnatid2 as regioncode

maputo_dt[,district := stringr::str_extract(subnatid2, "[A-Za-z]+")]
maputo_dt[district == "AEROPORTO", district := "AEROPORTO INTERNACIONAL DE MAPUTO"]

union_dt <- shp_dt %>%
            group_by(Distrito) %>%
            summarize()

### now to randomize hh locations within each district
dist_count <- maputo_dt[,length(hhid), by = district]

union_dt <- union_dt[union_dt$Distrito %in% dist_count$district,]

spatial_sampler <- function(boundary, count){

  y <- st_sample(boundary, count, type = "random")

  return(y)
}

simhh_point <- mapply(spatial_sampler, union_dt$geometry, dist_count$V1)

### include district name in each element of list

add_distname <- function(list_sfobj, name_list){

  y <- as.data.table(list_sfobj)
  y$name <- name_list
  y <- st_as_sf(y, agr = "constant", crs = 4326)

  return(y)
}

simhh_point <- mapply(add_distname, simhh_point, dist_count$district, SIMPLIFY = FALSE)

simhh_point <- rbindlist(simhh_point)
simhh_point <- st_as_sf(simhh_point, crs = 4326, agr = "constant")

### merge back into the survey
colnames(simhh_point)[colnames(simhh_point) %in% "name"] <- "district"

maputo_dt <- maputo_dt[order(district),]
simhh_point <- simhh_point[order(simhh_point$district),]

maputo_dt <- cbind(maputo_dt, simhh_point[,"geometry"])
union_dt <- as.data.table(union_dt)

### clean up the dataset a little
union_dt <- st_as_sf(union_dt, crs = 4326, agr = "constant")

### drop katembe and kanyaka
union_dt <- union_dt[!(union_dt$Distrito %in% c("Katembe", "Kanyaka")),]
maputo_dt <- maputo_dt[!(district %in% c("Katembe", "Kanyaka")),]

### replace missing observations real quick
add_dt <- maputo_dt[,mean(whours, na.rm = TRUE), by = "district"]
setnames(add_dt, "V1", "whours")

maputo_dt[,whours := ifelse(is.na(whours) == TRUE,
                            median(whours, na.rm = TRUE),
                            whours),
          by = "district"]

maputo_dt[,rooms := ifelse(is.na(rooms) == TRUE,
                           median(rooms, na.rm = TRUE),
                           rooms),
          by = "district"]


### include fake weights such that we generate
rand_weight_assign <- function(size, total, sd = 3){

  y <- rnorm(size, total/size, sd)

  y <- y / sum(y) * total

  return(y)

}
pop_dt <- data.table(district = unique(maputo_dt$district),
                     population = c(5958, 326771, 195556, 76171, 319968, 127079))
maputo_dt <- pop_dt[maputo_dt, on = "district"]
maputo_dt[, popweight := rand_weight_assign(size = .N, total = unique(population), sd = 5),
          by = "district"]

###simulate welfare data
maputo_dt[, welfare := 50.2*age - 0.3*hsize + 1000*whours + 60*rooms + rnorm(1, sd = 0.01)]

#### create fake census data
census_dt <- data.table(district = do.call(c, mapply(rep, pop_dt$district,
                                                     pop_dt$population,
                                                     SIMPLIFY = FALSE)),
                        hhid = 1:sum(pop_dt$population),
                        age = rnorm(sum(pop_dt$population, na.rm = TRUE),
                                    mean = mean(maputo_dt$age, na.rm = TRUE),
                                    sd = 3),
                        hsize = sample(x = 1:16, size = sum(pop_dt$population), replace = TRUE),
                        whours = rnorm(sum(pop_dt$population, na.rm = TRUE),
                                       mean = mean(maputo_dt$whours, na.rm = TRUE),
                                       sd = 3),
                        rooms = sample(x = min(maputo_dt$rooms):max(maputo_dt$rooms),
                                       size = sum(pop_dt$population), replace = TRUE),
                        regioncode = as.integer(do.call(c, mapply(rep, unique(maputo_dt$regioncode),
                                                       pop_dt$population,
                                                       SIMPLIFY = FALSE))))




### save the data
maputo_dt <- st_as_sf(maputo_dt, crs = 4326, agr = "constant")
st_write(maputo_dt, dsn = "inst/extdata", layer = "maputosurvey_dt", driver = "ESRI Shapefile", append = FALSE)

saveRDS(union_dt, file = "inst/extdata/maputoshp.RDS")

saveRDS(census_dt, file = "inst/extdata/maputo_census.RDS")





