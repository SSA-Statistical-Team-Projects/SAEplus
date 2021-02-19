
gengrid <- function(dsn = "data-raw",
                    layer = "gadm36_CMR_0",
                    stats = "sum",
                    raster_tif = "cmr_ppp_2020_UNadj_constrained.tif"){

  ## below are the packages needed for the function to run
  usepkgs <- c( "sf","raster","tidyr","dplyr","spex",
                "exactextractr","tmap", "rgdal", "data.table")

  missing <- usepkgs[!(usepkgs %in% installed.packages()[,"Package"])]

  if(is.null(missing) == FALSE){
    install.packages(missing,
                     dependencies = TRUE,
                     repos = "http://cran.us.r-project.org")
  }

  invisible(sapply(usepkgs, library, character.only = TRUE))

  shp <- st_read(dsn = dsn,
                 layer= layer)
  pop <- raster(paste("data-raw", raster_tif, sep = "/"))

  #generate baseline raster
  base_raster <- raster(xmn= -180, ymn= -90, xmx = 180, ymx = 90, resolution = 0.008333,
                        crs = '+proj=longlat +datum=WGS84 +no_defs')
  extent_shp <- extent(shp)
  base_raster <- crop(base_raster, extent_shp)
  base_raster[is.na(base_raster)] <- 0
  base_raster <- raster::mask(base_raster, shp)

  #change raster to polygon
  br_poly <- polygonize(base_raster)
  class(br_poly)
  br_poly <- br_poly %>% rename(id = layer)
  br_poly$id <- seq(1:dim(br_poly)[1])

  #now compute zonal statistics of population
  zonal_stats <- exact_extract(pop, br_poly, stats) %>% as.data.table()
  names(zonal_stats) <- stats
  br_poly <- cbind(br_poly,zonal_stats)
  mymap <- tm_shape(br_poly) +
    tm_fill(stats,
            title = raster_tif,
            style="quantile",
            legend.reverse = TRUE,
            palette="PuBu")  +
    tm_borders(col="black", lwd=0, alpha = 0)

  br_dt <- as.data.table(br_poly[,stats])


  return(list(total_popsize = sum(br_dt[, stats, with=F]),
              polygon_dt = br_poly,
              summary_stats = summary(br_dt)))

}
