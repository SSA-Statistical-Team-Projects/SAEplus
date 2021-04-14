#' This function returns a datatable of all available versions of World Pop raster
#' building data in each country with available
#'
#' simply run : wpopbuilding_check() in your console.
#'
#' @note ALWAYS GIVE AT LEAST 2 MINUTES BETWEEN SUCCESSIVE RUNS OF THIS FUNCTION. THE PREVENTS SUBSEQUENT REQUESTS ON ITS
#' SERVER WITHIN THAT TIME INTERVAL, SO YOU WILL GET A <NOT SET> FUNCTION ERROR.
#'
#' @return a data.table object with list and versions of building data
#'
#' @import data.table
#' @export

wpopbuilding_vcheck <- function(){

  requireNamespace("data.table", quietly = TRUE)

  url <- "ftp://ftp.worldpop.org/repo/wopr/_MULT/buildings/"

  #construct the link to the world pop website with the building links
  #list the file names present on the specific version sub-website for world pop building
  find_sublinks <- function(url = "ftp://ftp.worldpop.org/repo/wopr/_MULT/buildings/"){

    versions <- RCurl::getURL(url = url,
                              ftp.use.epsv = FALSE,
                              dirlistonly = TRUE)
    versions <- strsplit(versions, "\r\n")
    versions <- unlist(versions)

    return(versions)

  }

  bld_vers <- find_sublinks() #function execution

  find_blddt <- function(versions){

    ##construct link
    suburl <- paste("ftp://ftp.worldpop.org/repo/wopr/_MULT/buildings/", versions, "", sep = "/")

    filenames <- find_sublinks(url = suburl)
    filenames <- data.table::as.data.table(filenames)
    filenames[,country := substr(filenames, 1, 3)]
    filenames[,versionnum := versions]

    return(filenames)
  }



  all_filenames <- data.table::rbindlist(lapply(bld_vers, find_blddt))

  return(all_filenames)

}

