
getMarketShares <- function (refdata, basedVariables, dataSource="FED", headBlank="") 
{

  message(sprintf(paste0(headBlank, "Market shares: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Market shares: done!"))))
  
  suppressPackageStartupMessages(require(data.table))
  
  refdata = winsorizeX(inf2NA(refdata), dataSource, basedVariables, headBlank=" ")
  
  #detach("package:data.table")
  
  for (basedVariable in basedVariables) {
    
	if (dataSource == "FED") {
	
      refdata = fedMarketShares(refdata, paste0("ws", basedVariable))
    
	}
    
    if (dataSource == "BANKSCOPE") {
	
      refdata = bsMarketShares(refdata, paste0("ws", basedVariable))
    
	}
    
  }
  
  return (as.data.frame(refdata))
  
}
