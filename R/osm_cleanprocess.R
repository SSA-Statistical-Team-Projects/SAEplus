#' This function does simple cleaning of the OSM data for lines, multipolygons and points
#'
#'
#' @param dropNA_cols if TRUE, unnamed classifications of features are dropped. Note: This is different from the
#' label "unclassified" as some features are tagged "NA" in OSM data (default is FALSE)
#' @param zero_NA if TRUE, NA observations are left as zero (default is FALSE)
#' @param missing_threshold numeric, if zero_NA is TRUE, missing_threshold is a value between 0 and 1. Columns with a rate
#' of missing observations less than missing_threshold will have missing values converted to 0.
#'
#' @export
#'
#' @return a cleaned data.table object




osm_cleanprocess <- function(dt,
                             dropNA_cols = TRUE,
                             zero_NA = TRUE,
                             missing_threshold = 0.3){

  ## if condition is met, we drop calls that contain NA in the column names
  if (dropNA_cols == TRUE){

    na_cols <- colnames(dt)[grepl("NA", colnames(dt))]
    dt[,(na_cols) := NULL]

  }

  ## if condition is met, we replace missing observations in variables below threshold rate of missing with zeros

  if (zero_NA == TRUE){

    ## figure out what variables are under the missingness threshold
    compute_missingrate <- function(X){

      value <- ifelse(is.na(X), 1, 0)
      value <- mean(value, na.rm = TRUE)
      return(value)

    }

    missings <- dt[,lapply(.SD, compute_missingrate)]
    ## restructure this information into a data.table with missing rate and corresponding variable name
    names_miss <- names(missings)
    missings <- as.data.table(unlist(missings))
    missings[,names_miss := names_miss]

    missings <- missings[(V1 <= missing_threshold),]


    replace_NA <- function(X){

      X[is.na(X)] <- 0

      return(X)
    }

    dt <- dt[, lapply(.SD, replace_NA), .SDcols = missings$names_miss]
  }

  return(dt)
}

