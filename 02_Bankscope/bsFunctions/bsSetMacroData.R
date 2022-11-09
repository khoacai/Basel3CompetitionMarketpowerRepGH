

bsSetMacroData <- function (refdata, macroData, macroVariables, headBlank="") 
{
  
  message(sprintf(paste0(headBlank, "Set macro data: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Set macro data: done!"))))
  
  refdata$bootUp = TRUE
  
  return (processExec(function() {
    
    for (macroVariablecd in macroVariables) {
  
      #Because the macro variables are stored in "macroData" in row format, 
      #Thus, subsetting and joinning each of them to the main dataset    
      subMacroData = subset(macroData, MacroVariableCode == macroVariablecd)
      
      subMacroData = subMacroData[ , c("CTRYCODE", "CLOSDATE_YEAR", "MacroVariableVal")]
    
      suppressPackageStartupMessages(require(data.table))  
      setnames(subMacroData, "MacroVariableVal", names(which(macroVariables == macroVariablecd)))
      
      detach("package:data.table")
    
      suppressPackageStartupMessages(require(dplyr))  
      refdata = right_join(subMacroData, refdata)
      detach("package:dplyr")
      
    }
    
    return (as.data.frame(refdata))
  
  }, paste0(headBlank, " ")))
  
}
