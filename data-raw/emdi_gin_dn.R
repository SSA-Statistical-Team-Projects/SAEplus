#devtools::load_all()
setwd("D:/Ify/Git")
library(emdi)
library(data.table)

gin_hhcensus.dt <- readRDS("data/gin_hhcensus.RDS")
gin_hhsurvey.dt <- readRDS("data/gin_hhsurvey.RDS")

selected.vars <- readRDS("data/gin_selectedvars.RDS")
# selected.vars <- selected.vars[!grepl("count_secondary_link", selected.vars)]
# selected.vars <- selected.vars[!grepl("length_secondary_link", selected.vars)]

colnames(gin_hhsurvey.dt) <- gsub("-", "_", colnames(gin_hhsurvey.dt))
colnames(gin_hhcensus.dt) <- gsub("-", "_", colnames(gin_hhcensus.dt))
selected.vars <-  gsub("-", "_", selected.vars)
gin_model <- paste(selected.vars, collapse = " + ")
gin_model <- as.formula(paste("pcexp", gin_model, sep = " ~ "))


# ###store the GINevnrionment and run ENDI in another script
# ginemdi_model <- emdi::ebp(fixed = gin_model, pop_data = gin_hhcensus.dt, pop_domains = "ADM3_CODE",
#                            smp_data = gin_hhsurvey.dt, smp_domains = "ADM3_CODE", threshold = 0,
#                            L = 100, transformation = "no", na.rm = TRUE)
#
# gin_hhcensus.dt[, pop_weight := 1]
#
# ginemdi_model2 <- emdi_ebp2(fixed = gin_model, pop_data = as.data.frame(gin_hhcensus.dt), pop_domains = "ADM3_CODE",
#                             smp_data = as.data.frame(gin_hhsurvey.dt), smp_domains = "ADM3_CODE", threshold = 0,
#                             L = 100, transformation = "no", na.rm = TRUE, smp_weight = "hhweight",
#                             pop_weight = "pop_weight")
#

#### compute the value
#
#
#
gin_hhsurvey.dt[,popweight := hhsize * hhweight]
gin_hhsurvey.dt[,spopweight := mean(popweight, na.rm = TRUE), by = "ADM3_CODE"]
gin_hhsurvey.dt[,spopweight := popweight / spopweight]

# #################################################################################################################
####### MODEL WITH COMMUNITY LEVEL VARIABLES (skip below for alternate model)
# ### Beyond this point will only be on a SERVER with multiple cores
## rescale popweights before imputation
#
### figure out what the NAs are and assign them names

#source("R/emdi_ebp2.R")
#source("R/emdi_wtframework_ebp.R")
#source("R/emdi_wtoptimal_parameter.R")
#source("R/emdi_wtpointestimation.R")
#source("R/emdi_wtmse_estimation.R")
#source("R/emdi_inners.R")
# with pp weights
#ginemdi_model2 <- emdi_ebp2(fixed = gin_model, pop_data = as.data.frame(gin_hhcensus.dt), pop_domains = "ADM3_CODE",
#                            smp_data = as.data.frame(gin_hhsurvey.dt), smp_domains = "ADM3_CODE", threshold = -0.448955,
#                            L = 100, transformation = "no", na.rm = TRUE, smp_weight = "spopweight", B = 100,
#                            pop_weight = "ind_estimate", cpus = 1, MSE = TRUE)


# with sample weights only

#gin_hhcensus.dt$ones <- 1
ginemdi_model2 <- ebp(fixed = gin_model, pop_data = as.data.frame(gin_hhcensus.dt), pop_domains = "ADM3_CODE",
                            smp_data = as.data.frame(gin_hhsurvey.dt), smp_domains = "ADM3_CODE", threshold = -0.448955,
                            L = 100, transformation = "no", na.rm = TRUE, weights = "popweight", B = 3,
                            cpus = 2, MSE = TRUE)




saveRDS(ginemdi_model2, "data/ginemdi_model2_dn.RDS")

#ginemdi_model2 <- readRDS("data/ginemdi_model2.RDS")

source("R/emdi_writeexcel.R")
source("R/emdi_summary.R")
source("R/emdi_maketables.R")
library(dplyr)
write.excel(ginemdi_model2, file = "data/emdi_results_dn.xlsx",
           indicator = "all", MSE = TRUE, CV = TRUE)
#emdi_writeexcel(ginemdi_model2, file = "data/emdi_results_dn.xlsx",
              #  indicator = "all", MSE = TRUE, CV = TRUE)


source("R/saeplus_calibratepovrate.R")
#### benchmark poverty estimates
gin_benchmark <- saeplus_calibratepovrate(pop_dt = gin_hhcensus.dt,
                                          hh_dt = gin_hhsurvey.dt,
                                          weight = "popweight",
                                          povline = -0.448955,
                                          pop_var = "ind_estimate")
#### replace benchmarked values from insample regions

#### create actual poverty map
## include the benchmarked results
replace.dt <- gin_benchmark[,c("ADM3_CODE", "BM_Head_Count")]

setnames(replace.dt, colnames(replace.dt), c("Domain", "BM_Head_Count"))

replace.dt[, Domain := as.factor(Domain)]

ginemdi_model2$ind <- left_join(ginemdi_model2$ind, replace.dt, by = "Domain")

ginemdi_model2$ind$BM_Head_Count[is.na(ginemdi_model2$ind$BM_Head_Count)] <-
  ginemdi_model2$ind$Head_Count[is.na(ginemdi_model2$ind$BM_Head_Count)] ##replacing NAs in BM_Head_Count with Head_Count

ginemdi_model2$ind$Head_Count <- NULL
colnames(ginemdi_model2$ind)[colnames(ginemdi_model2$ind) %in% "BM_Head_Count"] <- "Head_Count"

#### create the file with the EMDI estimates
write.excel(ginemdi_model2, file = "data/emdi_results_bm_dn.xlsx",
                               indicator = "all", MSE = TRUE, CV = TRUE)

#emdi_writeexcel(ginemdi_model2, file = "data/emdi_results_bm_dn.xlsx",
 #               indicator = "all", MSE = TRUE, CV = TRUE)



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
  tm_layout(title = "Guinea: Headcount Poverty Rates",
            title.size = 1.1,
            title.position = c("center", "top"))

tmap_save(tm = figure1, filename = "data/gin_povmap1_dn.pdf")

save(ginemdi_model2, gridhh_count.dt, gin_master.dt, gin_benchmark, povgrid.dt,
     file = "data/ginspace.RData")


##########################################################################################################################
#### MODEL WITHOUT COMMUNITY LEVEL VARIABLES
ginemdi_ncmodel2 <- emdi_ebp2(fixed = gin_model, pop_data = as.data.frame(gin_hhcensus.dt), pop_domains = "ADM3_CODE",
                              smp_data = as.data.frame(gin_hhsurvey.dt), smp_domains = "ADM3_CODE", threshold = -0.448955,
                              L = 100, transformation = "no", na.rm = TRUE, smp_weight = "spopweight", B = 100,
                              pop_weight = "ind_estimate", cpus = 30, MSE = TRUE)

saveRDS(ginemdi_ncmodel2, "data/ginemdi_ncmodel.RDS")

#ginemdi_model2 <- readRDS("data/ginemdi_model2.RDS")


emdi_writeexcel(ginemdi_ncmodel2, file = "data/emdi_ncresults.xlsx",
                indicator = "all", MSE = TRUE, CV = TRUE)


#### benchmark poverty estimates
gin_ncbenchmark <- saeplus_calibratepovrate(pop_dt = gin_hhcensus.dt,
                                            hh_dt = gin_hhsurvey.dt,
                                            weight = "popweight",
                                            povline = -0.448955,
                                            pop_var = "ind_estimate",
                                            ebp_obj = ginemdi_ncmodel2)
#### replace benchmarked values from insample regions

#### create actual poverty map
## include the benchmarked results
replace.dt <- gin_ncbenchmark[,c("ADM3_CODE", "BM_Head_Count")]

setnames(replace.dt, colnames(replace.dt), c("Domain", "BM_Head_Count"))

replace.dt[, Domain := as.factor(Domain)]

ginemdi_ncmodel2$ind <- left_join(ginemdi_ncmodel2$ind, replace.dt, by = "Domain")

ginemdi_ncmodel2$ind$BM_Head_Count[is.na(ginemdi_ncmodel2$ind$BM_Head_Count)] <-
  ginemdi_ncmodel2$ind$Head_Count[is.na(ginemdi_ncmodel2$ind$BM_Head_Count)] ##replacing NAs in BM_Head_Count with Head_Count

ginemdi_ncmodel2$ind$Head_Count <- NULL
colnames(ginemdi_ncmodel2$ind)[colnames(ginemdi_ncmodel2$ind) %in% "BM_Head_Count"] <- "Head_Count"

#### create the file with the EMDI estimates
emdi_writeexcel(ginemdi_ncmodel2, file = "data/emdi_ncresults_bm.xlsx",
                indicator = "all", MSE = TRUE, CV = TRUE)



### create the plot for the poverty grid
geo.dt <- as.data.table(ginshp)
geo.dt[,ADM3_CODE := as.integer(substr(ADM3_CODE, 4, nchar(ADM3_CODE)))]
geo.dt[,ADM2_CODE := as.integer(substr(ADM2_CODE, 4, nchar(ADM2_CODE)))]
geo.dt[,ADM1_CODE := as.integer(substr(ADM1_CODE, 4, nchar(ADM1_CODE)))]

povgrid.dt <- as.data.table(ginemdi_ncmodel2$ind)
setnames(povgrid.dt, "Domain", "ADM3_CODE")
povgrid.dt[,ADM3_CODE := as.integer(as.character(ADM3_CODE))]

povgrid.dt <- geo.dt[povgrid.dt, on = "ADM3_CODE"]
povgrid.dt <- povgrid.dt[is.na(Head_Count) == "FALSE",]


povgrid.dt <- st_as_sf(povgrid.dt, agr = "constant", crs = 4326)
ncpovgrid.dt <- povgrid.dt
figure1 <-
  tm_shape(povgrid.dt) +
  tm_polygons("Head_Count", title = "Headcount Rates") +
  tm_layout(title = "Guinea: Headcount Poverty Rates (Non Community Covariate Model)",
            title.size = 1.1,
            title.position = c("center", "top"))

tmap_save(tm = figure1, filename = "data/gin_ncpovmap1.pdf")

save(ginemdi_ncmodel2, gridhh_count.dt, gin_master.dt, gin_ncbenchmark, ncpovgrid.dt,
     file = "data/nc_ginspace.RData")





