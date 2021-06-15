devtools::load_all()

library(nlme)
library(emdi)

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
# #################################################################################################################
# ### Beyond this point will only be on a SERVER with multiple cores
## rescale popweights before imputation
gin_hhsurvey.dt[,popweight := hhsize * hhweight]
gin_hhsurvey.dt[,spopweight := mean(popweight, na.rm = TRUE), by = "ADM3_CODE"]
gin_hhsurvey.dt[,spopweight := popweight / spopweight]
#
### figure out what the NAs are and assign them names

ginemdi_model2 <- emdi_ebp2(fixed = gin_model, pop_data = as.data.frame(gin_hhcensus.dt), pop_domains = "ADM3_CODE",
                            smp_data = as.data.frame(gin_hhsurvey.dt), smp_domains = "ADM3_CODE", threshold = -0.448955,
                            L = 100, transformation = "no", na.rm = TRUE, smp_weight = "spopweight", B = 100,
                            pop_weight = "ind_estimate", cpus = 30, MSE = TRUE)

saveRDS(ginemdi_model2, "data/ginemdi_model2.RDS")

ginemdi_model2 <- readRDS("data/ginemdi_model2.RDS")


emdi_writeexcel(ginemdi_model2, file = "data/emdi_results.xlsx",
                indicator = "all", MSE = TRUE, CV = TRUE)


#### benchmark poverty estimates
gin_benchmark <- saeplus_calibratepovrate(pop_dt = gin_mastercentroid.dt,
                                          hh_dt = gin_hhsurvey.dt,
                                          weight = "popweight",
                                          povline = -0.448955)
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
emdi_writeexcel(ginemdi_model2, file = "data/emdi_results_bm.xlsx",
                indicator = "all", MSE = TRUE, CV = TRUE)







