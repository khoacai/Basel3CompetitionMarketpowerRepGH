
getQuantiles <- function (refdata, basedVariables, 
                          dataSource="FED", exportFile=F, 
                          fileOut="FedBHCF_Quantiles", fileType=".csv", 
                          headBlank="") 
{

  message(sprintf(paste0(headBlank, "Quantiles: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Quantiles: done!"))))
  
  quantileDataCols = c(2,3,4,5)
  
  quantilesFnc = ifelse(dataSource == "FED", 
                        fedQuantiles, 
                        bsQuantiles) #BANKSCOPE database

  quantilesDS = quantilesFnc(refdata, basedVariables[1])
  
  basedVariables = basedVariables[-(1)]
  
  for (basedVariable in basedVariables) {
    
    quantileX = quantilesFnc(refdata, basedVariable)
    
    quantilesDS = cbind(quantilesDS, quantileX[ , quantileDataCols])
    
  }

  if (exportFile) {
    
    saveAs(quantilesDS, fileOut, fileType)
    
  } 
  
  return (as.data.frame(quantilesDS))
  
}
