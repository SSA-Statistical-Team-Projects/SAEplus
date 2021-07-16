source("data-raw/prepare_gin.R")

library(emdi)

#### this script will be used to create the sub-area level model for Guinea
gin_master.dt <- st_as_sf(gin_master.dt, agr = "constant", crs = 4326)

gin_master.dt <- st_join(gin_master.dt, gin_masterpoly.dt[,c("id", "geometry")])

### compute poverty areas by polygon ID
gin_master.dt <- as.data.table(gin_master.dt)

gin_master.dt[, poor := ifelse(pcexp < 5006362, 1, 0)]
gin_master.dt[, povrate := weighted.mean(x = poor, w = popweight), by = "id"]
gin_master.dt[, sumpopweight := sum(popweight, na.rm = TRUE), by = "id"]

add.dt <- unique(gin_master.dt[,c("id", "povrate", "sumpopweight"),with=F])

## include poverty rates in the masterpolygon set and begin estimating models
gin_masterpoly.dt <- add.dt[gin_masterpoly.dt, on = "id"]
gin_mastercentroid.dt <- add.dt[gin_mastercentroid.dt, on = "id"]

## model selection on the sub-area model
selected.vars <- SAEplus::saeplus_selectmodel(dt = gin_mastercentroid.dt[is.na(povrate) == FALSE,],
                                              var_identifier = c("bld_", "_2018", "_2019",
                                                                 "rwi", "Conakry", "Kankan", "Nzerekore",
                                                                 "Faranah", "Labe", "Mamou", "Kindia", "Boke",
                                                                 "coverfraction"),
                                              outcomevar = "povrate")

selected.vars <- names(selected.vars$index[selected.vars$index == TRUE])

## model estimation
## prepare model for estimation
colnames(gin_mastercentroid.dt) <- gsub("-", "_", colnames(gin_mastercentroid.dt))
selected.vars <-  gsub("-", "_", selected.vars)
gin_model <- paste(selected.vars, collapse = " + ")
gin_model <- as.formula(paste("povrate", gin_model, sep = " ~ "))

selected.vars <- gsub("`", "", selected.vars)

gin_hhsurvey.dt <-
gin_mastercentroid.dt[is.na(povrate) == FALSE,
                      c(selected.vars, "povrate",
                        "ADM3_CODE", "sumpopweight"), with = F]

gin_hhcensus.dt <-
  gin_mastercentroid.dt[!(population == 0),
                        c(selected.vars,
                          "ADM3_CODE"), with = F]


# ginemdi_model2 <- emdi_ebp2(fixed = gin_model,
#                             pop_data = as.data.frame(na.omit(gin_hhcensus.dt)),
#                             pop_domains = "ADM3_CODE",
#                             smp_data = as.data.frame(na.omit(gin_hhsurvey.dt)),
#                             smp_domains = "ADM3_CODE",
#                             threshold = 0,
#                             L = 100,
#                             transformation = "no",
#                             na.rm = TRUE,
#                             smp_weight = "population",
#                             B = 100,
#                             pop_weight = "population",
#                             cpus = 30,
#                             MSE = TRUE)

ginemdi_samodel <- emdi::ebp(fixed = gin_model,
                             pop_data = as.data.frame(gin_hhcensus.dt),
                             pop_domains = "ADM3_CODE",
                             smp_data = as.data.frame(gin_hhsurvey.dt),
                             smp_domains = "ADM3_CODE",
                             threshold = 0,
                             L = 100,
                             transformation = "no",
                             na.rm = TRUE,
                             weights = "sumpopweight",
                             B = 100,
                             cpus = 30,
                             MSE = TRUE)

saveRDS(ginemdi_model2, "data/ginemdi_subarea.RDS")

### save objects
save(gin_model, gin_hhcensus.dt, gin_hhsurvey.dt, ginemdi_model2, file = "data/gin_subareaobjs.RData")














