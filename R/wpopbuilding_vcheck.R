#' This function returns a datatable of all available versions of World Pop raster
#' building data in each country with available
#'
#' simply run : wpopbuilding_check() in your console

wpopbuilding_vcheck <- function(){

  url <- "ftp://ftp.worldpop.org/repo/wopr/_MULT/buildings/"
  ## below are the packages needed for the function to run
  usepkgs <- c("RCurl", "data.table", "countrycode")

  missing <- usepkgs[!(usepkgs %in% installed.packages()[,"Package"])]

  if(is.null(missing) == FALSE){
    install.packages(missing,
                     dependencies = TRUE,
                     repos = "http://cran.us.r-project.org")
  }

  invisible(sapply(usepkgs, library, character.only = TRUE)) #load relevant libraries

  #construct the link to the world pop website with the building links
  #list the file names present on the specific version sub-website for world pop building
  find_sublinks <- function(url = "ftp://ftp.worldpop.org/repo/wopr/_MULT/buildings/"){

    versions <- getURL(url = url,
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
    filenames <- as.data.table(filenames)
    filenames[,country := substr(filenames, 1, 3)]
    filenames[,versionnum := versions]

    return(filenames)
  }



  all_filenames <- rbindlist(lapply(bld_vers, find_blddt))

  all_filenames <- all_filenames[(country %in% codelist$iso3c[is.na(codelist$iso3c) == FALSE]),]


  return(all_filenames)

}

