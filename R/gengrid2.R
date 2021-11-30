#' Another function to create a gridified shapefile
#'
#' This function takes in only a shapefile and creates a square or hexagon polygon grid based on a specified
#' grid size
#'
#' @param shp_dt an object of class 'sf' or 'sfc'
#' @param shp_dsn character; the local directory folder in which the shapefile is location. Must be specified
#' when shp_dt is not specified.
#' @param shp_layer character; the layer name for the shapefile. Must be specified with shp_dsn when shp_dt is not
#' specified
#' @param grid_size numeric of length 1; representing the desired size of the grid
#' @param sqr logical; if TRUE, a square grid is created. If FALSE, a hexagonal polygon grid is created


gengrid2 <- function(shp_dt,
                     grid_size,
                     sqr = TRUE) {

  if(is.null(shp_dt) == TRUE) {

    shp_dt <- sf::st_read(dsn = shp_dsn,
                          layer = shp_layer)

  }


  ## now we are ready to grid our district shapefile
  if (sqr == TRUE) {

  grid_system <- sf::st_make_grid(x = shp_dt,
                                  cellsize = c(grid_size, grid_size),
                                  square = sqr) %>%
    sf::st_sf()

  } else if (sqr == FALSE) {

  grid_system <- sf::st_make_grid(x = shp_dt,
                                  cellsize = grid_size,
                                  square = sqr) %>%
    sf::st_sf()


  }


  ## the process creates squares with parts outside the GMB area so we should take the intersection
  ## of the shapefile with our newly created grid
  grid_system <- sf::st_intersection(grid_system, shp_dt)

  grid_system$poly_area <- sf::st_area(grid_system) ##compute area of each square

  summary(grid_system$poly_area) ## summary statistics show that are mostly 0.04sqkm with few exceptions

  ### check that the total area of all squares in the grid_system and the total area of the original
  ### shapefile are roughly similar (its like comparing a riemann sum to the actual area under the curve)
  shp_dt$poly_area <- sf::st_area(shp_dt)
  shp_dt$poly_area <- units::set_units(shp_dt$poly_area, "km^2") ##convert to square km from sqm

  ##ensuring the geometry is validly created
  grid_system <- st_make_valid(grid_system)

  ## return results

  return(list(gridded_dt = grid_system,
              shapefile = shp_dt))

}
