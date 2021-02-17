rm(list=ls())
gc()

packages = c( "sf","raster","tidyr","dplyr","spex","exactextractr","tmap")
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}
#set path
username <- Sys.info()[7]
if (username == "wb495141") {
  path <- "C:/Users/wb495141/OneDrive - WBG/Documents/GitHub/SAEplus/sample"
}

shp <- st_read(dsn=paste0(path,"/input"),layer="gadm36_CMR_0")
pop <- raster(paste0(path,"/input/","cmr_ppp_2020_UNadj_constrained.tif"))

#generate baseline raster
r <- raster(xmn= -180, ymn= -90, xmx = 180, ymx = 90, resolution = 0.008333,
            crs = '+proj=longlat +datum=WGS84 +no_defs')
e <- extent(shp)
r <- crop(r, e)
r[is.na(r)] <- 0
r <- raster::mask(r, shp)

#change raster to polygon
r_poly <- polygonize(r)
class(r_poly)
r_poly <- r_poly %>% rename(id = layer)
r_poly$id <- seq(1:dim(r_poly)[1])

#now compute zonal statistics of population
stats <- c('sum')
zonal_stats <- exact_extract(pop, r_poly, stats) %>% as.data.frame()
names(zonal_stats) <- c("pop_wp_2020_sum")
r_poly <- cbind(r_poly,zonal_stats) 
summary(r_poly$pop_wp_2020_sum)
sum(r_poly$pop_wp_2020_sum)
mymap <- tm_shape(r_poly) +
  tm_fill("pop_wp_2020_sum",title=paste0("Population in 2020 (WorldPop)"),style="quantile", legend.reverse = TRUE, palette="PuBu")  +
  tm_borders(col="black", lwd=0, alpha = 0) 