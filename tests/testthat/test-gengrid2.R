## testing the gengrid2 function

### read in the data
gmb_shp <- sf::st_read(dsn = "georaw", layer = "District_Boundary")

gmb_grid <- gengrid2(shp_dt = gmb_shp,
                     grid_size = 200,
                     sqr = TRUE,
                     raster_path = "georaw/gmb_ppp_2020_UNadj.tif")

