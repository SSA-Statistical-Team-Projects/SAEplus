#' This function pulls specific layer data (lines, multipolygons and points) from the Open Street Maps for countries or regions
#'
#' @param country a string argument for the country name (or city) of interest i.e. place argument for oe_get function
#' @param ldrive a string argument for the local folder/drive location where the data should be stored
#' @param lines if TRUE, OSM features with lines data will be pulled
#' @param points if TRUE, OSM features with points data will be pulled
#' @param multipolygon if TRUE, OSM features will be pulled within polygons
#'
#' @return one shapefiles for each of lines, points and/or multipolygon pulls specified as TRUE
#'
#' @importFrom osmextract oe_get
#'
#'
#' @export


osm_datapull <- function(country = "Cameroon",
                         ldrive = "C:/Users/ifean/Documents/WorldBankWork/SAEPlus_Other",
                         lines = T,
                         points = T,
                         multipolygon = T){

  if (lines == TRUE){

    osm_lines <- oe_get(country, stringsAsFactors = TRUE)
    file <- paste(country, "osmlines", sep = "_")
    save (osm_lines, file = paste(ldrive, file, sep = "/"))

  }

  if(points == TRUE){

    osm_points <- oe_get(country, layer="points", stringsAsFactors = FALSE)
    file <- paste(country, "osmpoints", sep = "_")
    save (osm_points, file = paste(ldrive, file, sep = "/"))

  }

  if(multipolygon == TRUE) {

    osm_mp <- oe_get(country, layer="multipolygons", stringsAsFactors = TRUE)
    file <- paste(country, "osmmp", sep = "_")
    save (osm_mp, file = paste(ldrive, file, sep = "/"))

  } else {"You have not specified any of lines, points and multipolygon arguments as TRUE"}

}
