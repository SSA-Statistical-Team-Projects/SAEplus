#' Polygonize a raster and compute zonal statistics at polygon and full shapefile level
#'
#' @param dsn stands for data source name (see sf::st_read documentation for more information).
#' @param layer layer name (see sf::st_read documentation for more information)
#' @param stats zonal statistics to be estimated
#' @param raster_tif raster file with tif extension
#' @param grid_size the diagonal length of the polygon in km
#' @param crs the coorindate reference system to be used
#' @return list object aggregated zonal statistic, polygon data and summary statistics on polygon data
#' @examples


gengrid <- function(dsn = "data-raw",
                    layer = "gadm36_CMR_0",
                    stats = "sum",
                    raster_tif = "cmr_ppp_2020_UNadj_constrained.tif",
                    grid_size = 1,
                    crs = '+proj=longlat +datum=WGS84 +no_defs'
                    ){

  ## below are the packages needed for the function to run
  usepkgs <- c( "sf","raster","tidyr","dplyr","spex", "stars",
                "exactextractr","tmap", "rgdal", "data.table")

  missing <- usepkgs[!(usepkgs %in% installed.packages()[,"Package"])]

  if(is.null(missing) == FALSE){
    install.packages(missing,
                     dependencies = TRUE,
                     repos = "http://cran.us.r-project.org")
  }

  invisible(sapply(usepkgs, library, character.only = TRUE))

  shp <- sf::st_read(dsn = dsn,
                 layer= layer)
  pop <- raster::raster(paste("data-raw", raster_tif, sep = "/"))

  #generate baseline raster
  resolution <- grid_size/111
  base_raster <- raster::raster(xmn= -180, ymn= -90, xmx = 180, ymx = 90,
                                resolution = resolution, crs = crs)
  extent_shp <- raster::extent(shp)
  base_raster <- raster::crop(base_raster, extent_shp)
  base_raster[is.na(base_raster)] <- 0
  base_raster <- raster::mask(base_raster, shp)

  #change raster to polygon
  br_poly <- spex::polygonize(base_raster)
  class(br_poly)
  br_poly <- br_poly %>% dplyr::rename(id = layer)
  br_poly$id <- seq(1:dim(br_poly)[1])

  #now compute zonal statistics of population
  zonal_stats <- exactextractr::exact_extract(pop, br_poly, stats) %>% as.data.table()
  names(zonal_stats) <- stats
  br_poly <- cbind(br_poly,zonal_stats)
  mymap <- tmap::tm_shape(br_poly) +
    tm_fill(stats,
            title = raster_tif,
            style="quantile",
            legend.reverse = TRUE,
            palette="PuBu")  +
    tm_borders(col="black", lwd=0, alpha = 0)

  br_dt <- data.table::as.data.table(br_poly[,stats])


  return(list(total_popsize = sum(br_dt[, stats, with=F]),
              polygon_dt = br_poly,
              summary_stats = summary(br_dt)))

}






































