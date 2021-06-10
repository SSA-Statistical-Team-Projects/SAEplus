#' Create report suitable tables from EMDI-EBP function outputs
#'
#' @param obj object of classes 'ebp', 'emdi' i.e. the resulting object of the ebp survey to survey function (emdi::ebp())
#' @param docx if TRUE, the tables will be outputted straight to Microsoft Word (Ensure Java installation on your system to use
#' this option)
#'
#' @import openxlsx



emdi_maketables <- function(obj = ginemdi_model2,
                            outfolder = "data"){

  # construct regression result outputs and write results to CSV

  value <- obj$model$model$coefficients$fixed
  se <- sqrt(diag(obj$model$model$varFix))
  tvalue <- value/se

  reg.table <- data.table(Variable = names(obj$model$model$coefficients$fixed),
                          Value = round(value, 4),
                          Std.Error = round(se, 4),
                          DF = round(obj$model$model$fixDF$X, 4),
                          tvalue = round(tvalue, 4),
                          pvalue = round(2*pt(abs(tvalue), df = obj$model$model$fixDF$X, lower.tail = F), 4))


  ## write to an excel file
  wb <- createWorkbook()

  ws <- createSheet(wb, sheetName = "RegTable")

  # create a new row
  writeData(wb, ws, "RegressionTable", startCol = 1, startRow = 1)

  writeData(wb, ws, x = reg.table, startCol = 1, startRow = 3)


  saveWorkbook(wb, file = paste(outfolder, "report.xlsx", sep = "/"), overwrite = TRUE)



  return(reg.table)
}
