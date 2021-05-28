#' Compute National Poverty Line
#'
#' This function computes the poverty line along the order norm scale necessary for EMDI
#'
#' @param npl_value a numeric value for the national/international poverty line to be coverted to order norm
#' scale for imputation
#' @param pcexp a numeric vector of per capita expenditure values to be converted to undergo order norm normalization
#' @param method a string value indicating the method to be used to convert the npl from currency to normalization.
#' Three values could be selected "inclusion", "interpolation" and "limsup
#' @details The "inclusion" method add the poverty line value stipulated to the vector of welfare vector provided. The order
#' norm value obtained from the normalization process is the converted value
#' The interpolation method uses the 'pcexp' vector as given and then uses a linear interpolation method to estimate a conversion
#' The limsup method takes the converted value of the greatest welfare value below the poverty line

saeplus_ordernormpl <- function(npl_value = 5006362 ,
                                pcexp = hh.dt$pcexp,
                                method = c("inclusion", "interpolation", "limsup")){

  if(sum(grepl("inclusion", method)) == 1){
    pcexp <- c(pcexp, npl_value)
    ordered_pcexp <- bestNormalize::orderNorm(pcexp)

    ordered_pcexp <- as.data.table(cbind(ordered_pcexp$x, ordered_pcexp$x.t))

    inclusion_val <- ordered_pcexp[V1 == npl_value,V2]

  }

  if(sum(grepl("interpolation", method)) == 1){
    ordered_pcexp <- pcexp[order(pcexp)]
    ordered_pcexp <- bestNormalize::orderNorm(ordered_pcexp)

    ordered_pcexp <- as.data.table(cbind(ordered_pcexp$x, ordered_pcexp$x.t))
    ordered_pcexp <- na.omit(ordered_pcexp)

    interval <- findInterval(npl_value, ordered_pcexp$V1)

    #if index of interval is equal to poverty line return the normalization ordered norm value
    if(interval == 0){

      itp_value <- ordered_pcexp$V2[interval + 1]
    }

    if(ordered_pcexp$V1[interval] == npl_value && interval != 0){

      itp_value <- ordered_pcexp$V2[interval]

    } else if (interval == 1 | interval >= length(pcexp)){ #if the PL is the minimum or maximum

      itp_value <- ordered_pcexp[V1 == npl_value, V2]

    } else { #otherwise for the typical case of a PL within the limits of the distribution perform interpolation

      #compute slope
      index <- interval
      slope <- (ordered_pcexp$V2[index] - ordered_pcexp$V2[index + 1])
      slope <- slope/(ordered_pcexp$V1[index] - ordered_pcexp$V1[index + 1])

      intercept <- ordered_pcexp$V2[index] - slope * ordered_pcexp$V1[index]

      itp_value <- slope * npl_value + intercept

      }
    }

    if(sum(grepl("limsup", method)) == 1){

      ordered_pcexp <- pcexp[order(pcexp)]
      ordered_pcexp <- bestNormalize::orderNorm(ordered_pcexp)

      ordered_pcexp <- as.data.table(cbind(ordered_pcexp$x, ordered_pcexp$x.t))
      ordered_pcexp <- na.omit(ordered_pcexp)

      interval <- findInterval(npl_value, ordered_pcexp$V1)

      if(interval == 0){

        limsup_val <- ordered_pcexp$V2[interval + 1]

      }


      if(ordered_pcexp$V1[interval] == npl_value && interval == 0){

        limsup_val <- ordered_pcexp$V2[interval]

      } else if (interval == 1 | interval >= length(pcexp)){ #if the PL is the minimum or maximum

        limsup_val <- ordered_pcexp[V1 == npl_value,V2]

      } else {

        limsup_val <- ordered_pcexp$V2[interval + 1]

      }

    }

    #### if the if-statement doesnt evaluate, return NA
    return_NULL <- function(obj){
      if (exists(deparse(substitute(obj))) == TRUE){
        return(obj)
      } else {
        return("NA")
      }
    }

    inclusion_val <- return_NULL(inclusion_val)
    itp_value <- return_NULL(itp_value)
    limsup_val <- return_NULL(limsup_val)

  return(list(inclusion_line = inclusion_val,
              interpolation_line = itp_value,
              limsup_line = limsup_val))

}


