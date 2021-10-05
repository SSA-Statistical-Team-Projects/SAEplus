#' Check for inconsistencies in the administrative boundaries shapefile
#'
#' This function checks for errors that are often common in admin boundary shapefiles and could lead to
#' potential problems when joining admin boundary shapefile data with other datasets such as geospatial
#' household surveys
#'
#' @param shp_dt a shapefile dataframe (class sf, data.frame/data.table) object
#' @param lowest_admin a variable name for the lowest admin level (typically admin 3)
#' @param crs the coordinate reference system associated with the shp_dt
#' @param file the filename (including extension) to which a geospatial plot to show duplicated regions
#'


saeplus_checkshpfiles <- function(shp_dt,
                                  lowest_admin,
                                  crs = 4326,
                                  file){

  shp_dt <- as.data.table(shp_dt)

  duplicated_admins <- which(duplicated(shp_dt[,get(lowest_admin)])) #find duplicate admin areas

  if (is.null(duplicated_admins) == TRUE){
    cat("Shapefile contains no duplicates at the ", lowest_admin, "level\n")
  } else {

    duplicated_names <- shp_dt[duplicated_admins, get(lowest_admin)]

    x <- st_as_sf(shp_dt[get(lowest_admin) %in% unique(duplicated_names),], crs = crs, agr = "constant")

    tmap_mode("view")
    chart <- tm_shape(x) + tm_polygons(col = lowest_admin)
    tmap_save(tm = chart,
              filename = file)

    cat("Don't forget the map that shows the potential regions worth looking into.\n\n")
    cat("You apparently have ", length(duplicated_names), " duplicated region IDs. \n\n")
    cat("I have saved you the set of duplicated regions as well. Happy Fixing!!! \n\n\n")

    return(x)
  }


}



