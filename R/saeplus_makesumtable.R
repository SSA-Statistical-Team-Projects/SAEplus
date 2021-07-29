#' Compute Outcome Variable Summary Statistics
#'
#' A function to compute summary statistics for an outcome variable at the group level
#'
#' @param dt a data.table or data.frame object (object will be coerced to data.table for analysis)
#' @param out_var a character variable for outcome variable of interest (for instance household consumption estimate)
#' @param weight a weighting variable (replace with vector of 1s if not including weights)
#' @param group_var a grouping variable
#' @param threshold threshold to be applied to out_var (for example a poverty line)
#' @param hhid household ID variable
#'
#' @importFrom stats weighted.mean
#'
#' @return a table
#'
#' @export



saeplus_conssummary <- function(dt = gin_master.dt,
                                out_var = "pcexp",
                                weight = "popweight",
                                group_var = "ADM1_NAME",
                                threshold = 5006362,
                                hhid = "hhid"){

  dt <- as.data.table(dt)
  dt[,poor := ifelse(get(out_var) < threshold, 1, 0)]



  sumstats <-
    dt[,.(mean_outvar = weighted.mean(x = get(out_var), w = get(weight), na.rm = TRUE),
          thres_rate = weighted.mean(x = poor, w = get(weight), na.rm = TRUE),
          median_outvar = median(x = get(out_var), na.rm = TRUE),
          sample_count = .N),
       by = group_var]

  sumstats <- sumstats[is.na(ADM1_NAME) == FALSE,]

  sumstats <- sumstats[order(ADM1_NAME),]

  Totals <-
    dt[,.(mean_outvar = weighted.mean(x = get(out_var), w = get(weight), na.rm = TRUE),
          thres_rate = weighted.mean(x = poor, w = get(weight), na.rm = TRUE),
          median_outvar = median(x = get(out_var), na.rm = TRUE),
          sample_count = .N)]

  Totals[,substitute(group_var) := "OVerall"]

  Totals <- Totals[,colnames(sumstats), with = F]

  sumstats <- rbind(sumstats, Totals)

  return(sumstats)

}
