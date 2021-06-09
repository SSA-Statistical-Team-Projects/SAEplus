devtools::load_all()
library(nlme)
library(emdi)

gin_hhcensus.dt <- readRDS("data/gin_hhcensus.RDS")
gin_hhsurvey.dt <- readRDS("data/gin_hhsurvey.RDS")

selected.vars <- readRDS("data/gin_selectedvars.RDS")
selected.vars <- selected.vars[!grepl("count_secondary_link", selected.vars)]
selected.vars <- selected.vars[!grepl("length_secondary_link", selected.vars)]

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
#
# ginemdi_model2 <- emdi_ebp2(fixed = gin_model, pop_data = as.data.frame(gin_hhcensus.dt), pop_domains = "ADM3_CODE",
#                             smp_data = as.data.frame(gin_hhsurvey.dt), smp_domains = "ADM3_CODE", threshold = -0.4486192,
#                             L = 100, transformation = "no", na.rm = TRUE, smp_weight = "hhweight", B = 100,
#                             pop_weight = "ind_estimate", cpus = 15, MSE = TRUE)
#
# saveRDS(ginemdi_model2, "data/ginemdi_model2.RDS")

ginemdi_model2 <- readRDS("data/ginemdi_model2.RDS")


# emdi_writeexcel(ginemdi_model2, file = "data/emdi_results.xlsx",
#                 indicator = "all", MSE = TRUE, CV = TRUE)


#### benchmark poverty estimates
gin_benchmark <- saeplus_calibratepovrate(pop_dt = gin_mastercentroid.dt)

#### replace benchmarked values from insample regions



#### create actual poverty map
## include the benchmarked results
replace.dt <- gin_benchmark[,c("ADM3_CODE", "BM_Head_Count")]

setnames(replace.dt, colnames(replace.dt), c("Domain", "BM_Head_Count"))

replace.dt[, Domain := as.factor(Domain)]

ginemdi_model2$ind <- left_join(ginemdi_model2$ind, replace.dt, by = "Domain")

ginemdi_model2$ind$BM_Head_Count[is.na(ginemdi_model2$ind$BM_Head_Count)] <-
  ginemdi_model2$ind$Head_Count[is.na(ginemdi_model2$ind$BM_Head_Count)] ##replacing NAs in BM_Head_Count with Head_Count

#### replace results in the EMDI file
gin_benchmark <- saeplus_addbenchmark()


## modify admin variables to match correctly
ginshp <- as.data.table(ginshp)
ginshp[,ADM3_CODE := as.integer(substr(ADM3_CODE, 4, nchar(ADM3_CODE)))]
ginshp[,ADM2_CODE := as.integer(substr(ADM2_CODE, 4, nchar(ADM2_CODE)))]
ginshp[,ADM1_CODE := as.integer(substr(ADM1_CODE, 4, nchar(ADM1_CODE)))]

ginshp <- sf::st_as_sf(ginshp, agr = "constant", crs = 4326)
ginshp <- as.data.table(ginshp)

setnames(gin_benchmark, "Domain", "ADM3_CODE")
gin_benchmark[, ADM3_CODE := as.integer(as.character(ADM3_CODE))]
gin_benchmark <- ginshp[,c("ADM3_CODE", "ADM3_NAME", "geometry")][gin_benchmark, on = "ADM3_CODE"]

## generate poverty maps
gin_benchmark <- st_as_sf(gin_benchmark, agr = "constant", crs = 4326)

gin_benchmark <- sfheaders::sf_remove_holes(gin_benchmark)
#gin_benchmark <- rmapshaper::ms_filter_islands(gin_benchmark, min_area = 1e+09)


#munis <- fortify(munis,region="ent_mun")
gin_benchmark <- rmapshaper::ms_simplify(gin_benchmark)



# tm_shape(gin_benchmark) +
#   tm_fill() +
#   tm_borders() +
#   tm_fill("BM_HeadCount", palette="-RdBu",title="Headcount Poverty\nHousehold Model",
#            colorNA="grey90",border.col="grey80",lwd=0.05)
#
# tmap_mode("view")
# tm_shape(ginshp) +
#   tm_borders() +
#   tm_shape(ginshp[ginshp$ADM3_NAME %in% c("Kaloum", "Koba", "Tanene", "Beindou", "Missira"),]) +
#   tm_borders(col = "red")
#
# tmap_mode("view")
# tm_shape(ginshp) +
#   tm_borders() +
#   tm_shape(ginshp[ginshp$ADM3_CODE == ginshp$ADM3_CODE[duplicated(ginshp$ADM3_CODE)],]) +
#   tm_borders(col = "red")
















