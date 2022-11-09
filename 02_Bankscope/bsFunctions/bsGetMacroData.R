
bsGetMacroData <- function (countryCodes=c("US"), 
                            macroDataFile="MacroData.xlsx", 
                            macroVariables=setNames(c("EXCHUD", "CPI", "GDPV_ANNPCT"), 
                                                    c("EXCHRATE", "CPI", "GDPGrowthRate")), 
                            countryCodeMapFile="CountryCodeMap.xls", 
                            headBlank="") 
{
  
  message(sprintf(paste0(headBlank, "Get macro data: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Get macro data: done!"))))
  
  return (processExec(function() {
    
    suppressPackageStartupMessages(require(readxl))
  
    # load the original macro data
    macroData = as.data.frame(read_excel(macroDataFile))[ , c("LOCATION", "VARIABLE", "TIME", "Value")]
    
    macroData = subset(macroData, VARIABLE %in% macroVariables)
    
    # load the country codes mapping data
    countryCodeMap = as.data.frame(read_excel(countryCodeMapFile))
    
    detach("package:readxl")
    
    suppressPackageStartupMessages(require(dplyr))
    
    # match the time dimension
    macroData = left_join(macroData, countryCodeMap)
    
    macroData$VARIABLENAME = NA
    
    for (macroVariable in macroVariables) {
    
      #set names for the marco variables  
      macroData = macroData %>% 
        mutate(VARIABLENAME = ifelse(.$VARIABLE == macroVariable, 
                                     names(which(macroVariables == macroVariable)), 
                                     .$VARIABLENAME))  
    
    }

    detach("package:dplyr")
  
    macroData = subset(macroData, CTRYCODE %in% countryCodes)
    
    suppressPackageStartupMessages(require(data.table))
    
    setnames(macroData, "TIME", "CLOSDATE_YEAR")
    setnames(macroData, "VARIABLE", "MacroVariableCode")
    setnames(macroData, "VARIABLENAME", "MacroVariable")
    setnames(macroData, "Value", "MacroVariableVal")
    
    detach("package:data.table")
    
	  return (as.data.frame(macroData))
  
  }, paste0(headBlank, " ")))
  
}
