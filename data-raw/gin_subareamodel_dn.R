rm(list=ls())
#source("data-raw/prepare_gin.R")
setwd("D:/Ify/GitProjects/SAEplus")
#load("./data/prepare_gin_Environment_dn.Rdata")
#saveRDS(gin_master.dt,"./data/gin_master_dn.dt")
#saveRDS(add.dt,"./data/add_dn.dt")
#saveRDS(gin_masterpoly.dt,"./data/gin_masterpoly_dn.dt")
#saveRDS(gin_mastercentroid.dt,"./data/gin_mastercentroid_dn.dt")

gin_master.dt <- readRDS("./data/gin_master_dn.dt")
add.dt <- readRDS("./data/add_dn.dt")
gin_masterpoly.dt <- readRDS("./data/gin_masterpoly_dn.dt")
gin_mastercentroid.dt <- readRDS("./data/gin_mastercentroid_dn.dt")
library(emdi)
library(nlme)
library(Hmisc)
library(sae)
library(DT)
library(dplyr)
library(tmap)
library(sf)
library(data.table)
library(openxlsx)
library(parallelMap)
source("./R/saeplus_calibratepovrate.R")
source("./R/emdi_ebp2.R")
source("./R/emdi_wtpointestimation.R")
source("./R/emdi_wtmse_estimation.R")
source("./R/emdi_wtoptimal_parameter.R")
source("./R/emdi_inners.R")
source("./R/emdi_wtframework_ebp.R")
source("./R/emdi_transformation_functions.R")

#### this script will be used to create the sub-area level model for Guinea
gin_master.dt <- st_as_sf(gin_master.dt, agr = "constant", crs = 4326)

gin_master.dt <- st_join(gin_master.dt, gin_masterpoly.dt[,c("id", "geometry")])

### compute poverty areas by polygon ID
gin_master.dt <- as.data.table(gin_master.dt)

gin_master.dt[, poor := ifelse(pcexp < 5006362, 1, 0)]
gin_master.dt[, povrate := weighted.mean(x = poor, w = popweight), by = "id"]

add.dt <- unique(gin_master.dt[,c("id", "povrate"),with=F])

## include poverty rates in the masterpolygon set and begin estimating models
gin_masterpoly.dt <- add.dt[gin_masterpoly.dt, on = "id"]
gin_mastercentroid.dt <- add.dt[gin_mastercentroid.dt, on = "id"]

source("R/saeplus_selectmodel.R")
## model selection on the sub-area model
selected.vars <- saeplus_selectmodel(dt = gin_mastercentroid.dt[is.na(povrate) == FALSE,],
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
                        "ADM3_CODE", "population"), with = F]

gin_hhcensus.dt <-
  gin_mastercentroid.dt[!(population == 0),
                        c(selected.vars,
                          "ADM3_CODE", "population"), with = F]


ginemdi_model2 <- emdi_ebp2(fixed = gin_model,
                             pop_data = as.data.frame(na.omit(gin_hhcensus.dt)),
                             pop_domains = "ADM3_CODE",
                             smp_data = as.data.frame(na.omit(gin_hhsurvey.dt)),
                             smp_domains = "ADM3_CODE",
                             threshold = 0,
                             L = 100,
                             transformation = "no",
                             na.rm = TRUE,
                             smp_weight = "population",
                             B = 100,
                             pop_weight = "population",
                             cpus = 1,
                             MSE = TRUE)

#ginemdi_model2 <- emdi_ebp2(fixed = gin_model,
 #                            pop_data = as.data.frame(na.omit(gin_hhcensus.dt)),
  #                           pop_domains = "ADM3_CODE",
   #                          smp_data = as.data.frame(na.omit(gin_hhsurvey.dt)),
    #                         smp_domains = "ADM3_CODE",
     #                        threshold = 0,
      #                       L = 100,
       #                      transformation = "dual",
        #                     na.rm = TRUE,
#
 #                            B = 100,
  #                           pop_weight = "population",
   #                          cpus = 1,
    #                         MSE = TRUE)




summary(ginemdi_model2$ind)
saveRDS(ginemdi_model2, "data/ginemdi_subarea.RDS")

### save objects
#save(gin_model, gin_hhcensus.dt, gin_hhsurvey.dt, ginemdi_model2, file = "data/gin_subareaobjs.RData")

#load("data/gin_subareaobjs.RData")

#load("data/gin_subareaobjs.RData")
ginemdi_model2 <- readRDS("data/ginemdi_subarea.RDS")
output <- as.data.frame(c(ginemdi_model2$ind,ginemdi_model2$MSE))
colnames(output)[colnames(output) == "Head_Count.1"] <- "Head_Count_MSE"
colnames(output)[colnames(output) == "Mean.1"] <- "Mean_MSE"
write.xlsx(x=output,file="./data/subarea_model_dual.xlsx",overwrite=TRUE,asTable=TRUE)

#emdi_writeexcel(ginemdi_model2,"./data/ginemdi_subarea.xlsx,MSE=TRUE")

ginemdi_model2$ind$Mean[ginemdi_model2$ind$Mean < 0] <- 0
ginemdi_model2$ind$Mean[ginemdi_model2$ind$Mean > 1] <- 1

### rename Mean to say Head_Count
colnames(ginemdi_model2$ind)[colnames(ginemdi_model2$ind) == "Head_Count"] <- "ZeroThres_Ind"
colnames(ginemdi_model2$ind)[colnames(ginemdi_model2$ind) == "Mean"] <- "Head_Count"

gridhh_count.dt[,ADM3_CODE := as.integer(substr(ADM3_CODE, 4, nchar(ADM3_CODE)))]
gin_master.dt[,ADM3_CODE := as.integer(substr(ADM3_CODE, 4, nchar(ADM3_CODE)))]

gridhh_count.dt[,ADM2_CODE := as.integer(substr(ADM2_CODE, 4, nchar(ADM2_CODE)))]
gin_master.dt[,ADM2_CODE := as.integer(substr(ADM2_CODE, 4, nchar(ADM2_CODE)))]

gridhh_count.dt[,ADM1_CODE := as.integer(substr(ADM1_CODE, 4, nchar(ADM1_CODE)))]
gin_master.dt[,ADM1_CODE := as.integer(substr(ADM1_CODE, 4, nchar(ADM1_CODE)))]

gin_subareabm <- saeplus_calibratepovrate(pop_dt = gridhh_count.dt[,c("ADM1_CODE", "ADM2_CODE",
                                                                        "ADM3_CODE", "ind_estimate"),
                                                                     with=F],
                                            hh_dt = gin_master.dt[,c("ADM1_CODE", "ADM2_CODE", "ADM3_CODE",
                                                                     "hhweight", "pcexp", "hhsize", "popweight"),
                                                                  with=F],
                                            weight = "popweight",
                                            povline = 5006362,
                                            pop_var = "ind_estimate",
                                            ebp_obj = ginemdi_model2)

gin_subareabm[BM_Head_Count > 1, BM_Head_Count := 1]

#### create actual poverty map
## include the benchmarked results
replace.dt <- gin_subareabm[,c("ADM3_CODE", "BM_Head_Count")]

setnames(replace.dt, colnames(replace.dt), c("Domain", "BM_Head_Count"))

replace.dt[, Domain := as.factor(Domain)]

ginemdi_model2$ind <- left_join(ginemdi_model2$ind, replace.dt, by = "Domain")

ginemdi_model2$ind$BM_Head_Count[is.na(ginemdi_model2$ind$BM_Head_Count)] <-
  ginemdi_model2$ind$Head_Count[is.na(ginemdi_model2$ind$BM_Head_Count)] ##replacing NAs in BM_Head_Count with Head_Count

ginemdi_model2$ind$Head_Count <- NULL
colnames(ginemdi_model2$ind)[colnames(ginemdi_model2$ind) %in% "BM_Head_Count"] <- "Head_Count"




### create the plot for the poverty grid
geo.dt <- as.data.table(ginshp)
geo.dt[,ADM3_CODE := as.integer(substr(ADM3_CODE, 4, nchar(ADM3_CODE)))]
geo.dt[,ADM2_CODE := as.integer(substr(ADM2_CODE, 4, nchar(ADM2_CODE)))]
geo.dt[,ADM1_CODE := as.integer(substr(ADM1_CODE, 4, nchar(ADM1_CODE)))]

povgrid.dt <- as.data.table(ginemdi_model2$ind)
setnames(povgrid.dt, "Domain", "ADM3_CODE")
povgrid.dt[,ADM3_CODE := as.integer(as.character(ADM3_CODE))]

povgrid.dt <- geo.dt[povgrid.dt, on = "ADM3_CODE"]
povgrid.dt <- povgrid.dt[is.na(Head_Count) == "FALSE",]


povgrid.dt <- st_as_sf(povgrid.dt, agr = "constant", crs = 4326)
figure1 <-
  tm_shape(povgrid.dt) +
  tm_polygons("Head_Count", title = "Headcount Rates") +
  tm_layout(title = "Guinea: Headcount Poverty Rates (Non Community Covariate Model)",
            title.size = 1.1,
            title.position = c("center", "top"))

# tmap_save(tm = figure1, filename = "data/gin_subareapovmap.pdf")
#
# save(ginemdi_model2, gin_subareabm, povgrid.dt, gin_hhcensus.dt, gin_hhsurvey.dt,
#     file = "data/ginspace_subarea.RData")




#############################################################################################################################
### building the area model

#prepare datasets to be adm level variables
sel.varnames <- colnames(gridhh_count.dt)[grepl("adm", colnames(gridhh_count.dt))]

gin_areacensus.dt <- unique(gridhh_count.dt[,c("ADM1_CODE", "ADM2_CODE", "ADM3_CODE", sel.varnames), with = F])

gin_master.dt[,povrate := weighted.mean(x = poor, w = popweight), by = "ADM3_CODE"]
gin_areasample.dt <- unique(gin_master.dt[,c("ADM1_CODE", "ADM2_CODE", "ADM3_CODE", sel.varnames, "povrate"), with = F])
gin_areasample.dt <- gin_areasample.dt[is.na(ADM3_CODE) == FALSE,]


## compute population size for each area
area_popn.dt <- gin_mastercentroid.dt[,sum(population), by = "ADM3_CODE"]
area_sample.dt <- gin_master.dt[,sum(popweight), by = "ADM3_CODE"]



# model selection
selected.vars <- saeplus_selectmodel(dt = gin_areasample.dt,
                                              var_identifier = c("bld_", "_2018", "_2019",
                                                                 "rwi", "Conakry", "Kankan", "Nzerekore",
                                                                 "Faranah", "Labe", "Mamou", "Kindia", "Boke",
                                                                 "coverfraction"),
                                              outcomevar = "povrate")

selected.vars <- names(selected.vars$index[selected.vars$index == TRUE])

colnames(gin_areacensus.dt) <- gsub("-", "_", colnames(gin_areacensus.dt))
colnames(gin_areasample.dt) <- gsub("-", "_", colnames(gin_areasample.dt))
selected.vars <- gsub("-", "_", selected.vars)
gin_model <- paste(selected.vars, collapse = " + ")
gin_model <- as.formula(paste("povrate", gin_model, sep = " ~ "))

## compute standard deviation of the direct estimators
# sd_dt <- gin_master.dt[, lapply(.SD, wtd.var, weights = popweight), by = "ADM3_CODE", .SDcols = sel.varnames]
# colnames(sd_dt) <- gsub("-", "_", colnames(sd_dt))
#sd_dt <- sd_dt[,sel.varnames,with=F][,lapply(.SD, sqrt), .SDcols = sel.varnames]

sd_dt <- gin_master.dt[,wtd.var(povrate, weights = popweight), by = "ADM3_CODE"]

colnames(sd_dt)[colnames(sd_dt) %in% "V1"] <- "povrate_sd"
gin_areasample.dt <- sd_dt[gin_areasample.dt, on = "ADM3_CODE"]

## compute domain size for population and sample dataset (needed for arcsin outcome transformation)
domain_size <- gin_master.dt[, length(hhid), by = "ADM3_CODE"]
setnames(domain_size, "V1", "domain_size")
gin_areasample.dt <- domain_size[gin_areasample.dt, on = "ADM3_CODE"]

domain_size <- gin_mastercentroid.dt[, sum(population), by = "ADM3_CODE"]
setnames(domain_size, "V1", "domain_size")
gin_areacensus.dt <- domain_size[gin_areacensus.dt, on = "ADM3_CODE"]
gin_areacensus.dt[, domain_rate := domain_size / sum(domain_size)]

# gin_direct <- emdi::direct(y = "pcexp", smp_data = gin_master.dt, smp_domains = "ADM3_CODE",
#                            weights = "popweight", B = 100, na.rm = TRUE,
#                            threshold = 5006362)

##### Start here to load the GIN_environmentALL.RData


gin_direct <- sae::direct(y = poor,
                          dom = ADM3_CODE,
                          sweight = popweight,
                          domsize = as.data.frame(area_sample.dt[is.na(ADM3_CODE) == FALSE,]),
                          data = as.data.frame(gin_master.dt))
gin_direct$variance <- gin_direct$SD^2
gin_direct$Direct[gin_direct$Direct==0] <- NA

gin_areacombine.dt <- emdi::combine_data(pop_data = as.data.frame(gin_areacensus.dt),
                                         pop_domains = "ADM3_CODE",
                                         smp_data = as.data.frame(gin_direct),
                                         smp_domains = "Domain")

# povrate.dt <- unique(gin_master.dt[,c("ADM3_CODE", "povrate"),with = F])
# gin_areacensus.dt <- povrate.dt[gin_areacensus.dt, on = "ADM3_CODE"]

gin_areacombine.dt$povrate <- gin_areacombine.dt$Direct

## model estimation time!
gin_areamodel <- fh(fixed = gin_model,
                    vardir = "variance",
                    combined_data = gin_areacombine.dt,
                    domains = "Domain", transformation = "no",
                    eff_smpsize = "domain_size",MSE=TRUE)

write.excel(gin_areamodel,file="./data/gin_areamodel.xlsx",MSE=TRUE)

### benchmark estimates
#### rearrange populations to match gin_areamodel object results
source("./R/fh_benchmark.R")
gin_areamodelbenchmark <- fh_calibratepovrate(hh_dt = gin_master.dt,
                                              pop_dt = gin_mastercentroid.dt,
                                              fh_obj = gin_areamodel,
                                              weight = "popweight")

### include indicator for in-sample out of sample
add.dt <- as.data.table(gin_areamodel$ind)
setnames(add.dt, "Domain", "ADM3_CODE")

gin_areamodel

##########################################################################################################################

#### Putting together the tables for the paper

















































