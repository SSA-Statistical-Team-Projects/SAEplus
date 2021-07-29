#' A simple function to include benchmarked results to preferred file
#'
#' This function is built to take the results of the benchmarked poverty rates and use them to replace
#' the original ebp estimates that were written to an xlsx file using the SAEplus::emdi_writeexcel()
#' or emdi::write.excel() functions
#'
#' @param ebp_obj an object of class "ebp"
#' @param csv_file full file name (including path) where results will be outputted
#' @param bm_obj an object of class data.table / data.frame which is the result of benchmarking the poverty rates
#' (usually the output of the function saeplus_calibratepovrate())
#' @param bm_unitid the id variable within the bm_obj object argument
#'
#' @import data.table
#'
#' @export
#'


saeplus_addbenchmark <- function(ebp_obj = ginemdi_model2,
                                 csv_file = "data/emdi_results2.csv"){

  ## combine the MSE and IND results from ebp object
  dt <- as.data.table(left_join(ginemdi_model2$MSE, ginemdi_model2$ind, by = "Domain"))

  setnames(dt, c("Mean.x", "Head_Count.x", "Mean.y", "Head_Count.y"),
           c("Mean_MSE", "Head_Count_MSE", "Mean", "Head_Count"))

  dt <- dt[,Head_Count := NULL]

  dt[,Mean_CV := sqrt(Mean_MSE) / Mean]
  dt[,Head_Count_CV := sqrt(Head_Count_MSE) / BM_Head_Count]


  ### write results to new sheet of xlsx file

  write.csv(dt, file = csv_file)

  return(dt)

}
