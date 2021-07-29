#' Create a synthetic census
#'
#' A function that generates a synthetic census from household level survey data with outcome and covariates and merges this
#' data with household survey data for
#'
#' @param poly_dt sf or dataframe object with polygons or multipolygons with average household size included
#' @param hhsize string for the name of the average household size variable
#' @param popsize string for the name of the population size for each polygon
#' @param poly_id string for the polygon id variable to be used for expanding poly_dt
#'
#' @import data.table
#'
#' @export


saeplus_gencensus <- function(poly_dt = polyest,
                              hhsize = "ind_estimate",
                              popsize = "population",
                              poly_id = "id"){

  ## compute the number of households in each polygon
  poly_dt <- setDT(poly_dt)

  poly_dt[,hhcount := round(get(popsize) / get(hhsize))]
  poly_dt[hhcount == Inf, hhcount := 0]

  ### expand the dataset to census size

  expanded_dt <- poly_dt[rep(seq(.N), hhcount),]


  return(expanded_dt)

}
