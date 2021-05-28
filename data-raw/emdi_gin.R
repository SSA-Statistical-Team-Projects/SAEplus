devtools::load_all()
library(nlme)
library(emdi)

gin_hhcensus.dt <- readRDS("data/gin_hhcensus.RDS")
gin_hhsurvey.dt <- readRDS("data/gin_hhsurvey.RDS")

load("data/gin_selectedvars.RDS")

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



#################################################################################################################
### Beyond this point will only be on a SERVER with multiple cores

ginemdi_model2 <- emdi_ebp2(fixed = gin_model, pop_data = as.data.frame(gin_hhcensus.dt), pop_domains = "ADM3_CODE",
                            smp_data = as.data.frame(gin_hhsurvey.dt), smp_domains = "ADM3_CODE", threshold = -0.4486192,
                            L = 3, transformation = "no", na.rm = TRUE, smp_weight = "hhweight",
                            pop_weight = "ind_estimate", cpus = 15, MSE = TRUE)

saveRDS(ginemdi_model2, "data/ginemdi_model2")


