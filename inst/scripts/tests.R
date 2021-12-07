#######################################################################################################
#This script generates all zonal statistics for small area poverty estimation in Zanzibar
#######################################################################################################
rm(list=ls())
gc()
#rgee::ee_clean_pyenv()  #restrat if need be

#load packages
conflictRules("tidyr", mask.ok = c("extract"))
conflictRules("dplyr", mask.ok = c("filter", "lag", "select","raster"))
pacman::p_load(readstata13, data.table,sf,dplyr,haven,stringr,tidyverse,leaflet, lubridate)

#set path
username <- Sys.info()[7]
if(username=="WB495141"){
  path <- "C:/Users/wb495141/OneDrive - WBG/poverty/zanzibar"
  path_github <- "C:/Users/WB495141/OneDrive - WBG/Documents/GitHub/zanzibar_sae"
}
if(username=="takaakimasaki" ){
  path <- "/Users/takaakimasaki/Documents/professional/world_bank/zanzibar"
  path_github <- "/Users/takaakimasaki/Documents/GitHub/zanzibar_sae"
}

#install.packages("devtools", dependencies = T)
#devtools::install_github("SSA-Statistical-Team-Projects/SAEplus")
##it seems like you need to have rgee installed before you can try to get SAEplus downloaded.
#install.packages("reticulate")
#install_miniconda(path = miniconda_path("C:/Users/WB495141"), update = TRUE, force = FALSE)

#library(rgee)
#library(remotes)
#install_github("r-spatial/rgee")
#ee_install(py_env = "rgee")#run once before running everything.
#rgee::ee_install_upgrade()#run once
#ee_check()
##reticulate::py_install('earthengine-api 0.1.288')#run once
library(rgee)
ee_install(py_env = "rgee")
#rgee::ee_install_upgrade()
ee_Initialize()
#ee_install_upgrade()
library(SAEplus)
library(sf)
library(stars)


#now extract geospatial features from GEE
##before running it, you need to run
##rgee::ee_Initialize(user = 'tmasaki.040685@gmail.com', drive = TRUE) ##you need to authenticate access to your own Google Drive


################################################################################
gps_sf <- st_read(dsn="data", layer="wpop_zanzibar_grid") %>% st_centroid()

################################################################################
# Nighttime light
#Sys.Date()
#y <- 2020
#start_date <- lubridate::floor_date(ymd(paste0(y,"-01-01")), "month")
#end_date <- lubridate::ceiling_date(ymd(paste0(y,"-03-01")), 'month') - 1
gee_map <-
  ee$ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG')$
  filterDate("2019-01-01", "2019-12-31")$
  map(function(x) x$select("avg_rad"))

##since computing zonal statats for all points faces memory issues, do this at an increment of 1,000.
for(i in 1:58) {
  if(i == 1) {
    init <- i
    end <- i + 999
    # convert ee to tibble
    df <- ee_extract(x = gee_map,
                     y = gps_sf[init:end,],
                     fun = ee$Reducer$mean())
    print(paste0("done with...",init," to ",end))
  }
  if(i > 1) {
    init <- i*1000 - 999
    end <- i*1000
    if(end > dim(gps_sf)[1]) {
      end <- dim(gps_sf)[1]
    }
    df2 <- ee_extract(x = gee_map,
                      y = gps_sf[init:end,],
                      fun = ee$Reducer$mean())

    df <- rbind(df, df2)
    print(paste0("done with...",init," to ",end))
  }
}

names(df) <- c(head(names(gps_sf), -1), rep(1:sum(str_count(names(df), 'avg_rad'))))
write.csv(df, paste0(path,"/input/","ntl_2019-2020.csv"), row.names = F)

################################################################################
# landcover
gee_map <-
  ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global")$
  map(function(x) x$select("tree-coverfraction","urban-coverfraction","grass-coverfraction",
                           "shrub-coverfraction","crops-coverfraction","bare-coverfraction",
                           "water-permanent-coverfraction","water-seasonal-coverfraction",
                           "moss-coverfraction"))  # Select only precipitation bands

##since computing zonal statats for all points faces memory issues, do this at an increment of 1,000.
for(i in 1:58) {
  if(i == 1) {
    init <- i
    end <- i + 999
    # convert ee to tibble
    df <- ee_extract(x = gee_map,
                     y = gps_sf[init:end,],
                     fun = ee$Reducer$mean())
    print(paste0("done with...",init," to ",end))
  }
  if(i > 1) {
    init <- i*1000 - 999
    end <- i*1000
    if(end > dim(gps_sf)[1]) {
      end <- dim(gps_sf)[1]
    }
    df2 <- ee_extract(x = gee_map,
                      y = gps_sf[init:end,],
                      fun = ee$Reducer$mean())

    df <- rbind(df, df2)
    print(paste0("done with...",init," to ",end))
  }
}

#names(df) <- c(head(names(gps_sf), -1), rep(1:12))
write.csv(df, paste0(path,"/input/","landcover.csv"), row.names = F)

################################################################################
#combine all variables
ntl <-  read.csv(paste0(path,"/input/","ntl_2019-2020.csv")) %>%
  rename(ntl_201901 = X1,
         ntl_201902 = X2,
         ntl_201903 = X3,
         ntl_201904 = X4,
         ntl_201905 = X5,
         ntl_201906 = X6,
         ntl_201907 = X7,
         ntl_201908 = X8,
         ntl_201909 = X9,
         ntl_201910 = X10,
         ntl_201911 = X11,
         ntl_201912 = X12)

landcover <- read.csv(paste0(path,"/input/","landcover.csv"))
colnames(landcover) = gsub("X2", "cp_2", colnames(landcover))
colnames(landcover) = gsub(".coverfraction", "", colnames(landcover))
colnames(landcover) = gsub(".", "_", colnames(landcover))

#get consumption data
lasso=cv.glmnet(x=as.matrix(smp1[,15:284]), y=as.matrix(smp1$lntc_imp_k), alpha=1,
                nfolds = 5, weights=as.matrix(smp1$popweight), type.measure="mse", family="gaussian")
lasso=cv.glmnet(x=as.matrix(smp1[,15:284]), y=as.matrix(smp1$tc_imp_nom), alpha=1,
                nfolds = 5, weights=as.matrix(smp1$popweight), type.measure="mse", family="gaussian")

#see https://cran.r-project.org/web/packages/StepReg/StepReg.pdf


################################################################################
# NO2
# convert ee to tibble
date_list <- format(seq(as.Date("2019-03-01"), as.Date("2020-02-29"), by="days"), format="%m-%d-%Y")

gee_map <-
  ee$ImageCollection("COPERNICUS/S5P/NRTI/L3_NO2")$
  filterDate("2019-03-01", "2019-03-01")$
  map(function(x) x$select("tropospheric_NO2_column_number_density"))  # Select only precipitation bands

# impervious surface
gee_map <-
  ee$ImageCollection("Tsinghua/FROM-GLC/GAIA/v10")$
  map(function(x) x$select("change_year_index"))  # Select only precipitation bands

##since computing zonal statats for all points faces memory issues, do this at an increment of 1,000.
for(i in 1:58) {
  if(i == 1) {
    init <- i
    end <- i + 999
    # convert ee to tibble
    df <- ee_extract(x = gee_map,
                     y = gps_sf[init:end,],
                     fun = ee$Reducer$mean())
    print(paste0("done with...",init," to ",end))
  }
  if(i > 1) {
    init <- i*1000 - 999
    end <- i*1000
    if(end > dim(gps_sf)[1]) {
      end <- dim(gps_sf)[1]
    }
    df2 <- ee_extract(x = gee_map,
                      y = gps_sf[init:end,],
                      fun = ee$Reducer$mean())

    df <- rbind(df, df2)
    print(paste0("done with...",init," to ",end))
  }
}

#names(df) <- c(head(names(gps_sf), -1), rep(1:12))
write.csv(df, paste0(path,"/data/","Tsinghua_change_year_index.csv"), row.names = F)

################################################################################
# cloud
gee_map <-
  ee$ImageCollection("COPERNICUS/S5P/NRTI/L3_CO")
filterDate("2019-03-01", "2019-03-01")$
  map(function(x) x$select("CO_column_number_density", "H2O_column_number_density",
                           "cloud_height"))  # Select only precipitation bands

# convert ee to tibble
df <- ee_extract(x = gee_map,
                 y = gps_sf[1:10,],
                 fun = ee$Reducer$mean()) %>%
  as_tibble()

#names(df) <- c(head(names(gps_sf), -1), rep(1:12))
write.csv(df, paste0(path,"/input/","cloud_conditions.csv"), row.names = F)


