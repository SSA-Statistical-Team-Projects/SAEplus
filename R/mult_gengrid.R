#' Polygonize multiple rasters (TIFs) and compute zonal statistics at polygon and full shape-file level
#'
#' This function builds on the gengrid() function to extract data from a list of
#' tagged image files.
#'
#' @param tif_namelist a list object of raw tif names to be extracted
#' @param location a folder containing the tif files
#' @param feature_name a list of names for each element of tif_namelist to name the indicator to be extracted
#' @param parallel if TRUE, parallelize the process of extract files to speed the gengrid() process
#' @param numCores the number of server cores to be used for this
#'
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores
#' @importFrom parallelMap parallelLibrary
#' @import foreach
#'
#'
mult_gengrid <- function(tif_namelist = gin_buildingtifs, ##names of raw building tifs
                         location = "tests/testdata", ##folder containing the tifs
                         feature_name = c("count", "cv_length",
                                          "imagery_year", "mean_length",
                                          "total_length", "cv_area",
                                          "density", "mean_area",
                                          "total_area", "urban"), ##name of the specific indicator
                                                                  ##in each file
                         parallel = T, ##true, if you want to extract features from each file in parallel
                         numCores = length(gin_buildingtifs)){ #specify the number of cores to be used
  result <- list()

  parver_wrapper <- function(){

    out <-
      SAEplus::gengrid(dsn = location,
                       layer = "gin_poppoly",
                       raster_tif = tif_namelist[i],
                       grid_shp = F,
                       featname = feature_name[i],
                       drop_Zero=F)

    return(out)

  }




  if (parallel == TRUE & numCores > 1) {

    numCores <- min(numCores, parallel::detectCores())
    parallelMap::parallelLibrary("foreach")

    registerDoParallel(cores = numCores) ##initiate the number of cores to be used

    result <-
      foreach (i = 1:length(tif_namelist)) %dopar% {

        parver_wrapper()

      }
  } else {

    for (i in seq_along(tif_namelist)){

      result[i] <- gengrid(dsn = location,
                           layer = "gin_poppoly",
                           raster_tif = tif_namelist[i],
                           grid_shp = F,
                           featname = feature_name[i],
                           drop_Zero=F)

    }
  }

  return(result)
}
