##############################################################################################################################################################

## load("data/gin_hhmenvironment.RData)
###### Admin three level area model
sel.varnames <- colnames(gridhh_count.dt)[grepl("adm", colnames(gridhh_count.dt))]

gridhh_count.dt[,c(sel.varnames) := NULL]
gin_areacensus.dt <- unique(gridhh_count.dt[,c("ADM1_CODE", "ADM2_CODE", "ADM3_CODE", sel.varnames), with = F])

gin_master.dt[,povrate := weighted.mean(x = poor, w = popweight), by = "ADM3_CODE"]
gin_areasample.dt <- unique(gin_master.dt[,c("ADM1_CODE", "ADM2_CODE", "ADM3_CODE", sel.varnames, "povrate"), with = F])
gin_areasample.dt <- gin_areasample.dt[is.na(ADM3_CODE) == FALSE,]


## compute population size for each area
area_popn.dt <- gin_mastercentroid.dt[,sum(population), by = "ADM3_CODE"]
area_sample.dt <- gin_master.dt[,sum(popweight), by = "ADM3_CODE"]



# model selection
selected.vars <- SAEplus::saeplus_selectmodel(dt = gin_areasample.dt,
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

gin_areacombine.dt <- combine_data(pop_data = as.data.frame(gin_areacensus.dt),
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
                    eff_smpsize = "domain_size")



### benchmark estimates
#### rearrange populations to match gin_areamodel object results

gin_areamodelbenchmark <- fh_calibratepovrate(hh_dt = gin_master.dt,
                                              pop_dt = gin_mastercentroid.dt,
                                              fh_obj = gin_areamodel,
                                              weight = "popweight")

### include indicator for in-sample out of sample
add.dt <- as.data.table(gin_areamodel$ind)
setnames(add.dt, "Domain", "ADM3_CODE")

