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

  ## below are the packages needed for the function to run
  # usepkgs <- c("data.table", "sf", "lwgeom", "osmextract", "dtplyr", "tidygraph",
  #              "igraph", "tibble", "dplyr", "ggplot2", "units", "tmap", "rgrass7",
  #              "link2GI", "nabor")
  #
  # missing <- usepkgs[!(usepkgs %in% installed.packages()[,"Package"])]
  #
  # if(is.null(missing) == FALSE){
  #   install.packages(missing,
  #                    dependencies = TRUE,
  #                    repos = "http://cran.us.r-project.org")
  # }
  #
  # invisible(sapply(usepkgs, library, character.only = TRUE)) #load relevant libaries


  agebs <- sf::st_read(shapefile_path)
  load(osm_path)
  osm_agebs <- dtplyr::lazy_dt(st_intersection(osm_points,st_make_valid(agebs)))

  # out = group_by(osm_agebs,CVEGEO,highway) %>%
  #   summarize(count=n())


  out.dt <- osm_agebs$parent[,.(count = .N),by = c(geoid_var, feature_var)]

  agebs.dt <- data.table::as.data.table(agebs)
  joined.dt <- out.dt[agebs.dt, on = geoid_var]

  return(list(joined.dt, osm_agebs))

}
