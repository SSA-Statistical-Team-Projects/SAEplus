####################################################################################################################
#This R code combines all inputs for PTI in DRC.
#Author: Takaaki Masaki
#Last modified: 4/12/2021
####################################################################################################################

rm(list=ls())
#first get packages that we need to conduct various spatial analysis
package_list <- c("maptools","rgdal","GISTools","sp","osmdata","dplyr","MODISTools","MODIS","sf","exactextractr","SpatialPosition","data.table","tmap","haven")
new.packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
for(p in package_list) {
  library(p, character.only = T)
}

####################################################################################################################
username <- Sys.info()[7]
path <- "C:/Users/wb495141/OneDrive - WBG/Documents/GitHub/SAEplus/data"
####################################################################################################################
poly <- st_read(dsn=path, layer="GIN_gee_combined")
plot(st_geometry(poly),add=TRUE)

pts <- read_dta(paste0(path,"/","GIN-Grappe_GPS_2018.dta")) %>% sf::st_as_sf(coords = c("coordonnes_gps__Longitude", "coordonnes_gps__Latitude"),crs = 4326, agr = "constant") %>% dplyr::select(grappe)
plot(st_geometry(pts))
write_sf(pts,paste0(path,"/","GIN-Grappe.shp"), delete_layer = TRUE)
pts_poly <- st_join(pts,poly) 
