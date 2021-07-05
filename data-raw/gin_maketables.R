library(Hmisc)
#### this script is used to prepare all the tables we need

load("data/prepare_gin_Environment_dn.RData")

#### compute the mean, min and max poverty rates

#first read xlsx files for benchmarked rates
hhm_results.dt <- as.data.table(read.xlsx("data/emdi_results_bm.xlsx", sheet = 2))
in_sample <- gin_master.dt[is.na(ADM3_CODE) == FALSE,unique(ADM3_CODE)]
in_sample <- as.integer(substr(in_sample, 4, nchar(in_sample)))

in_spl <- in_sample

hhm_results.dt[,in_sample := ifelse(Domain %in% in_spl, 1, 0)]

##include population
area_popn.dt <- gin_master.dt[,sum(popweight),by=ADM3_CODE]
setnames(area_popn.dt, c("ADM3_CODE", "V1"), c("Domain", "population"))
hhm_results.dt[,Domain := as.integer(Domain)]
hhm_results.dt <- area_popn.dt[hhm_results.dt, on = "Domain"]

## compute mean, min and max now
hhm_out <- hhm_results.dt[in_sample == 1 ,.(Mean = weighted.mean(x = Head_Count, w = population),
                                             Min = min(Head_Count),
                                             Max = max(Head_Count))]

## compute bm ratios
hhm_resultsnb.dt <- as.data.table(read.xlsx("data/emdi_results.xlsx", sheet = 2))
hhm_resultsnb.dt[,Domain := as.integer(Domain)]

setnames(hhm_results.dt, "Head_Count", "Head_Count_BM")

hhm_results.dt <- hhm_resultsnb.dt[,c("Domain", "Head_Count")][hhm_results.dt, on = "Domain"]

gin_master.dt[,poor := ifelse(pcexp < 5006362, 1, 0)]

gin_master.dt[ADM1_CODE %in% "GIN006\r\n", ADM1_CODE := "GIN006"]

gin_master.dt[,ADM3_CODE := as.integer(substr(ADM3_CODE, 4, nchar(ADM3_CODE)))]
gin_master.dt[,ADM2_CODE := as.integer(substr(ADM2_CODE, 4, nchar(ADM2_CODE)))]
gin_master.dt[,ADM1_CODE := as.integer(substr(ADM1_CODE, 4, nchar(ADM1_CODE)))]

state_pov <- unique(gin_master.dt[is.na(ADM1_CODE) == FALSE, .(weighted.mean(poor, popweight),
                                                        ADM2_CODE, ADM3_CODE, ADM1_NAME), by = "ADM1_CODE"])

setnames(state_pov, c("ADM3_CODE", "V1"), c("Domain", "state_povrates"))

hhm_results.dt <- state_pov[hhm_results.dt, on = "Domain"]

hhm_results.dt[,ADM2_CODE := substr(Domain, 1, 4)]
hhm_results.dt[,ADM1_CODE := substr(Domain, 1, 1)]

hhm_results.dt[,state_hhm_povrate := weighted.mean(Head_Count_BM, population, na.rm = TRUE), by = "ADM1_CODE"]

hhm_results.dt[,bm_ratio := state_povrates / state_hhm_povrate]

### compute the state level benchmarking numbers
state_bmratio <- hhm_results.dt[in_sample == 1,.(Mean = weighted.mean(bm_ratio, na.rm = TRUE),
                                                 Min = min(bm_ratio, na.rm = TRUE),
                                                 Max = max(bm_ratio, na.rm = TRUE),
                                                 sd =  wtd.var(bm_ratio, na.rm = TRUE))]

### compute CV across subprefectures
compute_cv <- function(MSE = ginemdi_model2$MSE$Mean,
                       povrates = , stat_level){

}






