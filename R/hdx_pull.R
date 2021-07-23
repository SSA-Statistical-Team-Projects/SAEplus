#' Check and pull data from Humanitarian Data Exchange
#'
#' @param iso The standard ISO-3 country code for country of interest
#' @param identifier Type of data to check
#' @param location_folder Destination for the downloaded files
#'
#'
#' @export
#'
#' @import countrycode
#'
#'

hdx_pull <- function(iso = "BEN",
                     identifier = "Relative wealth indicator",
                     location_folder = NULL){


  devtools::install_github("dickoa/rhdx")
  rhdx::set_rhdx_config(hdx_site = "prod")

  ## Search all dataset with rwi
  ds <- rhdx::search_datasets(identifier, rows = 2)

  ## Check for the country

  ### List of all the sets on rwi
  list_sources <- rhdx::get_resources(ds[[1]])

  ### Names of sets
  list_name <- list()
  for (i in 1:length(list_sources)){

    list_name[[i]] <- list_sources[[i]]$data$name

  }

  list_name <- unlist(list_name)

  ### List of all the countries
  #list_country <- str_sub(list_name,end=-27)
  list_country <- stringr::str_split(list_name, "_", simplify = TRUE)[ ,1]


  ### check if the country is in the list
  country <- countrycode(iso, origin = "iso3c", destination = "country.name.en")

  if((country %in% list_country) == FALSE){

    return("Relative Wealth Index for this country is not available")

  } else {

    n <- which(list_country == country)

    if (is.null(location_folder)) {

      dt <- rhdx::get_resource(ds[[1]],n) %>% rhdx::read_resource()
      return(dt)
      #return(fwrite(dt, sep = ";", col.names = TRUE, row.names = TRUE))

    } else {

      dt <- rhdx::get_resource(ds[[1]],n) %>% rhdx::read_resource(download_folder = location_folder)
      return(dt)

    }

  }

}
