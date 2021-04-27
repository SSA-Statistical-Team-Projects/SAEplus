#' This function cleans up OSM lines data in preparation for survey to survey imputation analysis
#'
#' @param shapefile_path filepath of country/region shapefile with polygons of interest
#' @param osm_path filepath to open street map lines data (road networks)
#' @param geoid_var the variable that points to the common identifier ID between shapefile_path object and osm_path object
#' @param feature_var specific feature of interest in the osm_path object
#'
#' @return A list containing two objects: a dataframe/datatable with original osm points data containing number of points
#' in polygon shapefile (from shapefile_path argument) and a lazy datatable version of the full osm points data
#' with point information
#'
#' @export
#'
#' @import data.table sf dtplyr


osm_processpoints <- function(shapefile_path = "data/cmr_polypop_boundary.shp",
                              osm_path = "C:/Users/ifean/Documents/WorldBankWork/SAEPlus_Other/Cameroon_osmpoints",
                              geoid_var = "id",
                              feature_var = "highway"){

  agebs <- sf::st_read(shapefile_path)
  load(osm_path)
  osm_agebs <- dtplyr::lazy_dt(st_intersection(osm_points,st_make_valid(agebs)))

  out.dt <- osm_agebs$parent[,count := .N,by = c(geoid_var, feature_var)]

  agebs.dt <- data.table::as.data.table(agebs)
  joined.dt <- out.dt[agebs.dt, on = geoid_var]

  ## flip the data from long to wide
  arg_form <- paste(paste(geoid_var, collapse = " + "), "~",
                    paste(feature_var, collapse = " + "))

  wide_join.dt <- dcast(joined.dt, formula = formula(arg_form),
                        value.var = "count",
                        fun.aggregate = mean)

  wide_join.dt <- agebs.dt[wide_join.dt, on = geoid_var]

  wide_join.dt <- osm_cleanprocess(dt = wide_join.dt)

  return(list(long_mp.dt = joined.dt,
              wide_mp.dt = wide_join.dt))

}
