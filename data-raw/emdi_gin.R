

gin_hhcensus.dt <- readRDS("data/gin_hhcensus.RDS")
gin_hhsurvey.dt <- readRDS("data/gin_hhsurvey.RDS")


###store the GINevnrionment and run ENDI in another script
ginemdi_model <- emdi::ebp(fixed = gin_model, pop_data = gin_hhcensus.dt, pop_domains = "ADM3_CODE",
                           smp_data = gin_hhsurvey.dt, smp_domains = "ADM3_CODE", threshold = 0,
                           L = 100, transformation = "no", na.rm = TRUE)
