

remove(list = objects()) ## clear all objects in R workspace



##### Pulling in the building data
buildinglist <- wpopbuilding_vcheck()
tcdbuilding_pull <- wpopbuilding_pull(iso = "MLI",
                                      wpversion = "v1.1")

unzip("data-raw/MLI_buildings_v1_1.zip", exdir = "data")


##### Merging all the building data
dt <- gengrid(dsn = "data",
              layer = "afr_mli_l04",
              raster_tif = "MLI_buildings_v2_0_count.tif",
              grid_shp = T,
              featname = "bld_count",
              drop_Zero = F)
tcd.bld.dt <- as.data.table(dt$polygon_dt)
dt <- gengrid(dsn = "data",
              layer = "afr_mli_l04",
              raster_tif = "MLI_buildings_v2_0_cv_area.tif",
              stats = "mean",
              grid_shp = T,
              featname = "bld_cvarea",
              drop_Zero = F)
add.dt <- as.data.table(dt$polygon_dt)
tcd.bld.dt <- cbind(tcd.bld.dt, add.dt[,"bld_cvarea"])
dt <- gengrid(dsn = "data",
              layer = "afr_mli_l04",
              raster_tif = "MLI_buildings_v2_0_cv_length.tif",
              stats = "mean",
              grid_shp = T,
              featname = "bld_cvlength",
              drop_Zero = F)
add.dt <- as.data.table(dt$polygon_dt)
tcd.bld.dt <- cbind(tcd.bld.dt, add.dt[,"bld_cvlength"])
dt <- gengrid(dsn = "data",
              layer = "afr_mli_l04",
              raster_tif = "MLI_buildings_v2_0_density.tif",
              stats = "mean",
              grid_shp = T,
              featname = "bld_density",
              drop_Zero = F)
add.dt <- as.data.table(dt$polygon_dt)
tcd.bld.dt <- cbind(tcd.bld.dt, add.dt[,"bld_density"])
dt <- gengrid(dsn = "data",
              layer = "afr_mli_l04",
              raster_tif = "MLI_buildings_v2_0_mean_area.tif",
              stats = "mean",
              grid_shp = T,
              featname = "bld_meanarea",
              drop_Zero = F)
add.dt <- as.data.table(dt$polygon_dt)
tcd.bld.dt <- cbind(tcd.bld.dt, add.dt[,"bld_meanarea"])
dt <- gengrid(dsn = "data",
              layer = "afr_mli_l04",
              raster_tif = "MLI_buildings_v2_0_total_length.tif",
              grid_shp = T,
              stats = "mean",
              featname = "bld_totallength",
              drop_Zero = F)
add.dt <- as.data.table(dt$polygon_dt)
tcd.bld.dt <- cbind(tcd.bld.dt, add.dt[,"bld_totallength"])
saveRDS(tcd.bld.dt, "data/MLI_allbuilding.RDS")


##### Pulling in the Relative well index data
hdx_pull(iso = "MLI",
         location_folder = "data")
