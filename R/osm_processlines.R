#' This function creates indicators from OSM lines data in preparation for survey to survey imputation analysis
#'
#' @param shapefile_path filepath of country/region shapefile with polygons of interest
#' @param osm_path filepath to open street map lines data (road networks)
#' @param geoid_var the variable that points to the common identifier ID between shapefile_path object and osm_path object
#' @param feature_var specific feature of interest in the osm_path object
#'
#' @return A list containing two objects: OSM lines data.table in both wide and long formats
#'
#' @export
#'
#' @import data.table dtplyr sf



osm_processlines <- function(shapefile_path = "data/cmr_polypop_boundary.shp",
                             osm_path = "C:/Users/ifean/Documents/WorldBankWork/SAEPlus_Other/Cameroon_osmlines",
                             geoid_var = "id",
                             feature_var = "highway"){

  load(osm_path) #load the OSM lines

  agebs <- st_read(shapefile_path) #load a basic polygon shapefile to intersect with lines data
  agebs$poly_area <- st_area(agebs)

  osm_lines$fulllength <- st_length(osm_lines) #compute the length of the osm_line geometries

  osm_agebs <- st_intersection(osm_lines, st_make_valid(agebs))

  osm_agebs$length <-st_length(osm_agebs)

  osm_agebs <-lazy_dt(osm_agebs)

  out.dt <- osm_agebs$parent[,.(length = sum(length),
                                fulllength = sum(fulllength),
                                count = .N),
                             by = c(geoid_var, feature_var)]


  agebs.dt <- as.data.table(agebs)

  joined.dt <- out.dt[agebs.dt, on = geoid_var]

  joined.dt[,roaddensity := length/poly_area]

  ## flip the data from long to wide
  arg_form <- paste(paste(geoid_var, collapse = " + "), "~",
                    paste(feature_var, collapse = " + "))

  wide_join.dt <- dcast(joined.dt, formula = formula(arg_form),
                        value.var = c("roaddensity", "count", "length"),
                        fun.aggregate = mean)

  wide_join.dt <- agebs.dt[wide_join.dt, on = geoid_var]

  wide_join.dt <- osm_cleanprocess(dt = wide_join.dt)

  return(list(long_lines.dt = joined.dt,
              wide_lines.dt = wide_join.dt))


}
