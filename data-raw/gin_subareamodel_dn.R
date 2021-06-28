setwd("C:/Users/wb559885/Documents/github/SAEplus")
library(emdi)
load("./data/gin_areamodelobjs")

## model estimation time!

#gin_areamodel <- fh(fixed = gin_model,
#                    vardir = gin_direct,
#                    combined_data = gin_areacensus.dt,
#                    domains = "ADM3_CODE", transformation = "arcsin",
#                    backtransformation = "naive",
#                    eff_smpsize = "domain_size")

#gin_fhdata <- subset(gin_areacensus.dt,select=c(povrate,bld_count_adm,Kankan_adm,Mamou_adm,bare_coverfraction_adm, shrub_coverfraction_adm, ADM3_CODE, gin_direct, domain_size))

gin_areacensus.dt$dir_var <- gin_direct$SD^2



gin_areamodel <- fh(fixed = gin_model,
                    vardir = dir_var,
                    combined_data = gin_areacensus.dt,
                    domains = "ADM3_CODE", transformation = "no",
                    backtransformation = "NULL",
                    eff_smpsize = "domain_size")




























