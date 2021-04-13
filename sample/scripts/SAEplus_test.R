#install.packages("googledrive")
#install.packages("rgee")
#install.packages("SAEplus")
library("SAEplus")
library("rgee")
library("googledrive")
#first install rgee
##To install rgee on WB computer, one needs to first get Anaconda in C:/WBG/Anaconda3
##Then ee_install() should take care of the rest of the installation process
##ee_check() to see if everything is installed properly


gee_datapull(
  email = "tmasaki.040685@gmail.com",
  gee_boundary = "users/tmasaki040685/gadm36_CMR_0",
  gee_polygons = "users/tmasaki040685/gadm36_CMR_2",
  gee_dataname = "NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG",
  gee_datestart = "2018-01-01",
  gee_dateend = "2019-01-01",
  gee_band = "avg_rad",
  scale = 100,
  gee_desc = "nighttimelight_cmr",
  gee_stat = "mean",
  gdrive_folder = "SAEplus",
  ldrive_dsn = "SAEplus/cmr_nighttimelight"
)