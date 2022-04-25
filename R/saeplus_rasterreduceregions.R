#' Extract group of rasters into the same grid
#'
#' This function takes a list of rasters (rasters of the same theme, E.g daily rainfall, daily nighttimelights) and
#' computes a statistic which is extracted into the shapefile
#'
#' @param raster_list the list of rasters to be extracted
#' @param shp_dt the target shapefile to be extracted into
#' @param fun_list list of functions to be passed to each raster. must equal `length(raster_list)`
#' @param numCores number of cores to parallelize the process over
#'
#' @importFrom raster stack
#'
#' @export


parallel_extract <- function(shp_dt,
                             raster_list,
                             fun_list,
                             numCores){

  ##initiating the parallelization process
  numCores <- min(numCores, parallel::detectCores()) ##use the minimum of the specified processors or the max

  doParallel::registerDoParallel(cores = numCores) ##initiate the number of cores to be used
  parallelMap::parallelLibrary("foreach") ##loading the parallel looping library
  parallelMap::parallelLibrary("exactextractr") ##loading the parallel looping library

  ##the parallelization process
  grid_list <-
    foreach (i = 1:length(raster_list), .combine = "cbind") %dopar% {

      exactextractr::exact_extract(x = raster_list[[i]],
                                   y = shp_dt,
                                   fun = fun_list[i])

    }


  return(grid_list)

}
