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
#' @importFrom raster cellStats
#' @importFrom units set_units
#'
#' @export


gengrid2 <- function(shp_dt = NULL,
                     shp_dsn,
                     shp_layer,
                     grid_size,
                     sqr = TRUE,
                     pop_raster,
                     raster_path = NULL,
                     extract_name,
                     raster_function = "sum") {

  sf_use_s2(FALSE) ##just to ensure we don't begin to have issues with duplicate vertices

  if(is.null(shp_dt) == TRUE) {

    shp_dt <- st_read(dsn = shp_dsn,
                      layer = shp_layer)

  }


  ## now we are ready to grid our district shapefile
  print("Initiating shape object tesselation")
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
  print("Tesselation complete for shapefile extent, ensuring validity of shapefile ...")
  ## the process creates squares with parts outside the area so we should take the intersection
  ## of the shapefile with our newly created grid

  ## to avoid failures we need to make sure geometries are valid
  shp_checklist <- st_is_valid(shp_dt)

  while(sum(shp_checklist) != length(shp_checklist)){

    shp_dt <- st_make_valid(shp_dt)
    shp_checklist <- st_is_valid(shp_dt)

  }
  print("Limiting tesselated object to shapefile area ...")

  ## figure out which grids belong within the shapefile
  grid_system$poly_id <- 1:nrow(grid_system)
  grid_system <- st_join(grid_system, shp_dt, left = F, largest = TRUE)

  ## compute area of duplicated grids and assign shp_dt areas

  print("The shapefile is fully gridded!!")

  ## make sure all geometries are polygons
  clean_geometry <- function(geo_dt){

    geo_dt <- geo_dt[st_dimension(geo_dt) == 2,] ##ensure that all geometries are surfaces
    ##find any other enclosure geometries
    add_dt <- geo_dt[st_geometry_type(geo_dt) == "GEOMETRYCOLLECTION",]
    add_dt <- st_collection_extract(add_dt)
    add_dt <- add_dt[st_dimension(add_dt) == 2,]
    geo_dt <- geo_dt[!(st_geometry_type(geo_dt) == "GEOMETRYCOLLECTION"),]

    geo_dt <- rbind(geo_dt, add_dt)


    return(geo_dt)

  }

  grid_system <- clean_geometry(grid_system)

  print("Ensuring geometries are properly fixed")
  grid_system$poly_id <- 1:nrow(grid_system)

  grid_system$poly_area <- st_area(grid_system) ##compute area of each square
  grid_system$poly_area <- set_units(grid_system$poly_area, "km^2")

  grid_system <- grid_system[as.numeric(grid_system$poly_area) > 0,]

  print(paste0("The tesselated object represents a total area of ",
               round(sum(grid_system$poly_area, na.rm = TRUE),2),
               " km^2"))

  grid_check <- as.numeric(grid_system$poly_area)
  hist(x = grid_check,
       xlab = "Polygon Size (in km^2)",
       main = "Distribution of Polygon Size")

  print("The plot window should show you a distribution of the polygon sizes")

  ##extract population raster into the data
  ### read in the raster file
  if(is.null(raster_path) == FALSE) {
    pop_raster <- raster(raster_path)
    }



  ### convert crs of shapefile to the crs of the raster
  grid_system <- st_transform(x = grid_system, crs = pop_raster@crs)

  print("Initiating Raster Extraction into the shapefile")
  ### extract value into the grid system
  zonal_stats <- exact_extract(x = pop_raster,
                               y = grid_system,
                               fun = raster_function) %>% data.table()
  print("Raster Extraction Complete")
  colnames(zonal_stats) <- extract_name

  print("Combining zonal statistics with tesselated shapefile object")
  grid_system <- cbind(grid_system, zonal_stats)

  ### show proportion of extracted raster in the grid
  raster_stat <- raster::cellStats(pop_raster, raster_function)
  raster_rate <- abs(do.call("sum", list(grid_system[[extract_name]], na.rm = TRUE)) - raster_stat)
  raster_rate <- (raster_rate/raster_stat)*100
  print(paste0("There is a ", round(raster_rate, 3),
               "% difference between the aggregate estimates of the raster and gridded extract"))

  print("Done! Enjoy!")
  ## return results

  return(grid_system)

}
