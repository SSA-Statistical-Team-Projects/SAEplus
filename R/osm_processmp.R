#' This function computes the number of points for the multipolygon data based on selected features
#'
#' @param shapefile_path filepath of country/region shapefile with polygons of interest
#' @param osm_path filepath to open street map lines data (road networks)
#' @param geoid_var the variable that points to the common identifier ID between shapefile_path object and osm_path object
#' @param feature_var specific feature of interest in the osm_path object
#'
#' @return A list containing two objects: a dataframe/datatable with original OSM multipolygon data containing feature length
#' in polygon shapefile (from shapefile_path argument) and a lazy datatable version of the full OSM multipolygon data
#' with point information
#'
#' @export
#'
#' @import data.table sf dtplyr tidyr


osm_processmp <- function(shapefile_path = "data/cmr_polypop_boundary.shp",
                          osm_path = "C:/Users/ifean/Documents/WorldBankWork/SAEPlus_Other/Cameroon_osmmp",
                          geoid_var = "id",
                          feature_var = c("amenity", "office", "shop"),
                          drop_missing = TRUE){


  requireNamespace(c("data.table", "sf", "dtplyr", "tidyr"), quietly = TRUE)
  agebs <- sf::st_read(shapefile_path)
  load(osm_path)

  osm_mp <- osm_mp[rowSums(is.na(osm_mp[,feature_var])) < 3L,]

  osm_mp <- sf::st_make_valid(osm_mp)
  osm_agebs <- dtplyr::lazy_dt(sf::st_intersection(osm_mp, st_make_valid(agebs)))

  out.dt <- osm_agebs$parent[,.(count = .N), by = c(geoid_var, feature_var)]

  agebs.dt <- data.table::as.data.table(agebs)

  joined.dt <- out.dt[agebs.dt, on = geoid_var]

  return(list(joined.dt, osm_agebs))

}






