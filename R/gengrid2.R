#' Another function to create a gridified shapefile and extract a raster if specified
#'
#' This function takes in only a shapefile and creates a square or hexagon polygon grid based on a specified
#' grid size
#'
#' @param shp_dt an object of class 'sf' or 'sfc'
#' @param shp_dsn character; the local directory folder in which the shapefile is location. Must be specified
#' when shp_dt is not specified.
#' @param shp_layer character; the layer name for the shapefile. Must be specified with shp_dsn when shp_dt is not
#' specified
#' @param grid_size numeric of length 1; representing the desired size of the grid in meters
#' @param sqr logical; if TRUE, a square grid is created. If FALSE, a hexagonal polygon grid is created
#' @param pop_raster raster; an object of class 'raster'
#' @param raster_path character; if pop_raster is not specified but raster is to read in from file. raster_path is
#' the full name of the raster (including filepath)
#' @param extract_name character of length 1; the name of the indicator to be extracted from the raster
#' @param raster_function function to be applied in extracting raster into created grids
#'
#' @importFrom raster raster
#' @importFrom units set_units


gengrid2 <- function(shp_dt = NULL,
                     shp_dsn,
                     shp_layer,
                     grid_size,
                     sqr = TRUE,
                     pop_raster,
                     raster_path,
                     extract_name,
                     raster_function = "sum") {

  sf_use_s2(FALSE) ##just to ensure we don't begin to have issues with duplicate vertices

  if(is.null(shp_dt) == TRUE) {

    shp_dt <- st_read(dsn = shp_dsn,
                      layer = shp_layer)

  }


  ## now we are ready to grid our district shapefile
  if (sqr == TRUE) {

  grid_system <- st_make_grid(x = shp_dt,
                              cellsize = c(grid_size, grid_size),
                              square = sqr) %>%
    sf::st_sf()

  } else if (sqr == FALSE) {

  grid_system <- st_make_grid(x = shp_dt,
                              cellsize = grid_size,
                              square = sqr) %>%
    sf::st_sf()


  }


  ## the process creates squares with parts outside the GMB area so we should take the intersection
  ## of the shapefile with our newly created grid
  grid_system <- st_intersection(grid_system, shp_dt)

  grid_system$poly_area <- st_area(grid_system) ##compute area of each square
  grid_system$poly_area <- set_units(grid_system$poly_area, "km^2")

  summary(grid_system$poly_area) ## summary statistics show that are mostly 0.04sqkm with few exceptions

  ##ensuring the geometry is validly created
  grid_system <- st_make_valid(grid_system)



  ##extract population raster into the data
  ### read in the raster file
  if(is.null(raster_path) == FALSE) {
    pop_raster <- raster(raster_path)
    }

  ### convert crs of shapefile to the crs of the raster
  grid_system <- st_transform(x = grid_system, crs = pop_raster@crs)
  ### extract value into the grid system
  zonal_stats <- exact_extract(x = pop_raster,
                               y = grid_system,
                               fun = raster_function) %>% data.table()

  colnames(zonal_stats) <- extract_name

  grid_system <- cbind(grid_system, zonal_stats)

  ## return results

  return(grid_system)

}
