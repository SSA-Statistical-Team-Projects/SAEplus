#' Combine WORLDPOP building tifs data within a shapefile
#'
#' This function combines the data obtained from World Pop database on buildings with the filenames structured like
#' XXX_buildings_v2_0_indicator within a specified shapefile
#'
#' @param ldrive path to the local drive
#' @param identifier an identifier for the filnames obtained from WorldPop
#' @param shp shapefile for the country in question
#'
#' @return a datatable/dataframe object


combine_wpopbuilding <- function(ldrive = "./../S2S-REMDI/GIN_2021",
                                 identifier = "buildings_v2_0",
                                 shp = "./../S2S-REMDI/GIN_2021/sous_prefectures.shp"){

  ## read in all the building data
  dt <- list.files(path = ldrive,
                   pattern = identifier)

  ## create full path and read in file at once and compute zonal statistics

  cleancompute_zonal <- function(X){

    fp <- paste(ldrive, X, sep = "/")
    tif <- raster::raster(fp)


    if(exists(shp) == FALSE){
      shp <- sf::st_read(shp)
    }

    extent_shp <- raster::extent(shp)
    building_tif <- raster::crop(tif, extent_shp)
    building_tif[is.na(building_tif)] <- 0
    building_tif <- raster::mask(building_tif, shp)

    return(building_tif)

  }

  tif_set <- lapply(dt, cleancompute_zonal)



  return(tiff_set)


}
