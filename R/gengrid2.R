#' Polygonize a raster and compute zonal statistics at polygon and full shape-file level
#'
#' @param dsn stands for data source name (see sf::st_read documentation for more information)
#' @param layer layer name (see sf::st_read documentation for more information), will also accept object of class "sf"
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
#' @importFrom raster raster crop crs
#' @importFrom raster mask
#' @importFrom spex polygonize
#' @importFrom exactextractr exact_extract
#' @importMethodsFrom raster extent


gengrid2 <- function(dsn = "data-raw",
                    layer = "gadm36_CMR_0",
                    stats = "sum",
                    featname = "population",
                    raster_tif = "cmr_ppp_2020_UNadj_constrained.tif",
                    grid_shp = T,
                    grid_size = 1,
                    drop_Zero = T
){

  if (is.character(layer) == TRUE){
    shp <- st_read(dsn = dsn,
                   layer = layer)
  } else {
    shp <- layer
  }

  if (is.character(raster_tif) == TRUE){
    pop <- raster(paste(dsn, raster_tif, sep = "/"))
  } else {
    pop <- raster_tif
  }

  crs_pop <- raster::crs(pop)

  if (grid_shp == T) {

    ## create the appropriate coordinate reference system
    shp <- shp %>%
            st_transform(4328)

    grid_system <- st_make_grid(x = shp, cellsize = c(grid_size, grid_size), square = TRUE) %>%
      st_sf()

    grid_system$id <- 1:nrow(grid_system)
    grid_system <- grid_system %>% st_transform(crs_pop)
    ### extract raster values into the grid system
    zonal_stats <- exact_extract(pop, grid_system, stats) %>% as.data.table()
    names(zonal_stats) <- stats
    grid_system <- as.data.table(grid_system)
    grid_system <- cbind(grid_system, zonal_stats)

    if(drop_Zero == T) {
      grid_system[grid_system[[stats]] != 0,]
    }

  }

  ## if a feature name is provided, relabel the variable name in the data to show this
  if(is.null(featname) == FALSE){
    names(grid_system)[names(grid_system) == stats] <- featname

    return(list(total_popsize = sum(grid_system[, featname, with=F]),
                polygon_dt = grid_system,
                summary_stats = summary(grid_system)))

  }

  return(list(total_popsize = sum(grid_system[, stats, with=F]),
              polygon_dt = grid_system,
              summary_stats = summary(grid_system)))


}






















