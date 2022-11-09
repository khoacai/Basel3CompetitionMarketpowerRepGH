
bsNPL <- function (refdata, headBlank="") 
{
  
  message(sprintf(paste0(headBlank, "NPL: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "NPL: done!"))))
  
  refdata$bootUp = TRUE
  
  return (processExec(function() {
    
    suppressPackageStartupMessages(require(data.table))
    
    setnames(refdata, "DATA18200", "NPL")
    
    detach("package:data.table")
    
    suppressPackageStartupMessages(require(dplyr))
    
    #Reoder the columns.
    refdata = refdata %>% select(NPL, everything())
    refdata = refdata %>% select(-NPL, everything())
    
    detach("package:dplyr")
    
    return (as.data.frame(refdata))
  
  }, paste0(headBlank, " ")))
  
}
