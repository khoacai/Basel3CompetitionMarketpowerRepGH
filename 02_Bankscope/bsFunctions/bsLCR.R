
bsLCR <- function (refdata, headBlank="") 
{
  message(sprintf(paste0(headBlank, "LCR: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "LCR: done!"))))
  
  refdata$bootUp = TRUE
  
  return (processExec(function() {
  
    suppressPackageStartupMessages(require(data.table))
    suppressPackageStartupMessages(require(dplyr))
    
    setnames(refdata, "DATA4035", "LCR") #Liquid Assets / Dep & ST Funding
    
    #Reoder the columns.
    refdata = refdata %>% select(-LCR, everything())
  
    detach("package:dplyr")
    detach("package:data.table")
    
    return (as.data.frame(refdata))
  
  }, paste0(headBlank, " ")))
}
