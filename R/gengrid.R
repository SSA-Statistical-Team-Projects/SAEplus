#' Polygonize a raster and compute zonal statistics at polygon and full shape-file level
#'
#' @param dsn stands for data source name (see sf::st_read documentation for more information)
#' @param layer layer name (see sf::st_read documentation for more information), will also accept object
#' @param stats zonal statistics to be estimated
#' @param featname the feature that the zonal statistic is computed for
#' @param raster_tif raster file with tif extension
#' @param grid_shp if TRUE, a grid system will be created for the shapefile (shp), grid_size must be specified
#' @param grid_size the diagonal length of the polygon in km
#' @param crs the co-ordinate reference system to be used
#' @param drop_zero if TRUE, gengrid will keep only zonal statistics that are different from zero
#'
#' @return list object aggregated zonal statistic, polygon data and summary statistics on polygon data
#' @examples
#'
#' @export
#'
#' @import data.table tmap
#' @importFrom raster raster
#' @importFrom raster extent crop mask
#' @importFrom spex polygonize
#' @importFrom exactextractr exact_extract


gengrid <- function(dsn = "data-raw",
                    layer = "gadm36_CMR_0",
                    stats = "sum",
                    featname = "population",
                    raster_tif = "cmr_ppp_2020_UNadj_constrained.tif",
                    grid_shp = T,
                    grid_size = 1,
                    crs = '+proj=longlat +datum=WGS84 +no_defs',
                    drop_Zero = T
                    ){

  if (exists(layer) == FALSE){
    shp <- st_read(dsn = dsn,
                   layer = layer)
  } else {
    shp <- layer
  }

  if (exists(layer) == FALSE){
    pop <- raster(paste(dsn, raster_tif, sep = "/"))
  } else {
    pop <- raster_tif
  }


  if (grid_shp == T) {
    #generate baseline raster
    resolution <- grid_size/111
    base_raster <- raster(xmn= -180, ymn= -90, xmx = 180, ymx = 90,
                                  resolution = resolution, crs = crs)
    extent_shp <- extent(shp)
    base_raster <- crop(base_raster, extent_shp) ##we only care about base raster as long as it within shapefile
    base_raster[is.na(base_raster)] <- 0
    base_raster <- mask(base_raster, shp)

    #change raster to polygon
    br_poly <- polygonize(base_raster)
    class(br_poly)
    br_poly <- br_poly %>% rename(id = layer)
    br_poly$id <- seq(1:dim(br_poly)[1])

    #now compute zonal statistics of population
    zonal_stats <- exact_extract(pop, br_poly, stats) %>% as.data.table()
    names(zonal_stats) <- stats
    br_poly <- cbind(br_poly,zonal_stats)

    if(drop_Zero == T){
      br_poly[br_poly[[stats]] != 0,]
    }
  } else {

    extent_shp <- extent(shp)

    zonal_stats <- exact_extract(pop, shp, stats) %>% as.data.table()
    names(zonal_stats) <- stats
    br_poly <- cbind(shp, zonal_stats)

    if(drop_Zero == T) {
      br_poly[br_poly[[stats]] != 0,]
    }

  }

  mymap <- tm_shape(br_poly) +
    tm_fill(stats,
            title = raster_tif,
            style="quantile",
            legend.reverse = TRUE,
            palette="PuBu")  +
    tm_borders(col="black", lwd=0, alpha = 0)

  br_dt <- as.data.table(br_poly[,stats])

  ## if a feature name is provided, relabel the variable name in the data to show this
  if(is.null(featname) == FALSE){
    names(br_poly)[names(br_poly) == stats] <- featname
  }


  return(list(total_popsize = sum(br_dt[, stats, with=F]),
              polygon_dt = br_poly,
              summary_stats = summary(br_dt),
              mymap))

}






































