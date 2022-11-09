
bsMerge2Origin <- function (refdata, finalProcessedVariables, 
                            CountryGroup="US", 
                            countryCodes=c("US"), 
                            macroData, macroVariables, 
                            originalRDataset="financialsOrg.RData", 
                            exportFile=F, 
                            fileOut="bsFinancialsMerged", 
                            fileType=".RData",
                            headBlank="")
{
  
  message(sprintf(paste0(headBlank, "Merge measures to the original dataset: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Merge measures to the original dataset: done!"))))
  
  suppressPackageStartupMessages(require(data.table))
  
  commonInfoVariables = c("CLOSDATE", "SPECIAL", "ENTITYTYPE", "NAME", 
                          "SWIFT", "LISTED", "STATE", "CITY")
  
  selectedDatacodes = c(commonInfoVariables, finalProcessedVariables)
  
  undefinedDatacodes = selectedDatacodes[!selectedDatacodes %in% names(refdata)]
  
  assertthat::assert_that(length(undefinedDatacodes)==0, msg = paste0("the undefined data codes in the refdata: \n", toString(undefinedDatacodes)))
  
  refdata = inf2NA(data.table(refdata[ , c(commonInfoVariables, finalProcessedVariables)]))
  gc()
  
  setnames(refdata, "CLOSDATE_YEAR", "CLOSDATE_YEAR2")
  
  #Select name and value of the macro variables, of the two critical information
  macroData = macroData[ , c("CTRYCODE", "CLOSDATE_YEAR", 
                             "MacroVariableCode", "MacroVariableVal")]
  
  #rename the year column for joinning to main dataset
  setnames(macroData, "CLOSDATE_YEAR", "CLOSDATE_YEAR2")
  
  #detach("package:data.table")
  
  bsFinancialsMerged = bsInitialLoadNFilter(loadFromSAS=F, 
                                            file=originalRDataset, 
                                            countryCodes=countryCodes, 
                                            headBlank=paste0(" ", headBlank))

  bsFinancialsMerged = bsCleanup(refdata=bsFinancialsMerged, 
                                 CountryGroup=CountryGroup, 
                                 changeDatacodesName=F, 
                                 headBlank=paste0(" ", headBlank))
  
  #Common variables are no longer needed
  bsFinancialsMerged[ , c(commonInfoVariables, "EXCHRATE")] = NULL
  
  message(sprintf(paste0(headBlank, " ", "Merge: started")))
  
  bsFinancialsMerged = processExec(function() {
  
    #Clean up the existing CPI information
    refdata$CPI = refdata$CPIUS = NULL
    
    suppressPackageStartupMessages(require(data.table))
    suppressPackageStartupMessages(require(dplyr))
    
    for (macroVariableCd in macroVariables) {
    
      #Because the macro variables are stored in "macroData" in row format, 
      #Thus, subsetting and joinning each of them to the main dataset
      subMacroData = subset(macroData, MacroVariableCode == macroVariableCd)
      
      #MacroVariableVal column hold the values of the macro variable
      subMacroData = subMacroData[ , c("CTRYCODE", "CLOSDATE_YEAR2", "MacroVariableVal")]
      
      #Change name of "MacroVariableVal" column to make it comprehensive
      setnames(subMacroData, "MacroVariableVal", names(which(macroVariables == macroVariableCd)))
      
      #Clean up the unused information
      subMacroData[ , c("LOCATION", "MacroVariableCode", "MacroVariable")] = NULL
      
      refdata = left_join(refdata, subMacroData, by = c("CTRYCODE", "CLOSDATE_YEAR2"))
      
    }
    
    #Get US's CPI, i.e., in case of a group of countries, the final merged dataset 
    #contains two CPI information: one for US and the other is for country-specific level
    if (CountryGroup != "US") {
      
      #Get US's macro data
      subMacroData = subset(macroData, CTRYCODE == "US")
      
      # load US's CPI data
      US_CPIs = subset(subMacroData, MacroVariableCode == "CPI")
      
      setnames(US_CPIs, "MacroVariableVal", "CPIUS")
      
      US_CPIs[ , c("LOCATION", "CTRYCODE", "MacroVariableCode", "MacroVariable")] = NULL
      
      refdata = left_join(refdata, US_CPIs, by=c("CLOSDATE_YEAR2"))
      
    }
    
    setnames(refdata, "CLOSDATE_YEAR2", "CLOSDATE_YEAR")

    refdata[ , c("INDEXX", "CLOSDATE_YEAR", "CLOSDATE")] = as.data.frame(lapply(refdata[,c("INDEXX", "CLOSDATE_YEAR", "CLOSDATE")], as.numeric))
    
    bsFinancialsMerged[ , c("INDEXX", "CLOSDATE_YEAR")] = as.data.frame(lapply(bsFinancialsMerged[,c("INDEXX", "CLOSDATE_YEAR")], as.numeric))
    
    bsFinancialsMerged = left_join(refdata, 
                                   bsFinancialsMerged, 
                                   by = c("COUNTRY" = "COUNTRY", 
                                          "CTRYCODE" = "CTRYCODE", 
                                          "BVDIDNUM" = "BVDIDNUM", 
                                          #at this point, CLOSDATE_YEAR and INDEXX is a 1-1 relationship, 
                                          #and CLOSDATE_YEAR is unique to a specific BVDIDNUM, i.e., a bank
                                          "CLOSDATE_YEAR" = "CLOSDATE_YEAR", 
                                          "INDEXX" = "INDEXX"))
    
    #Reoder the columns.
    bsFinancialsMerged = bsFinancialsMerged %>% 
      select(COUNTRY, CTRYCODE, 
             BVDIDNUM, SPECIAL, ENTITYTYPE, NAME, NICKNAME, CITY, STATE, 
             CLOSDATE_YEAR, INDEXX, everything())
    
    detach("package:dplyr")
    
    refdata = NULL
    gc()
  
    setnames(bsFinancialsMerged, "INDEXX", "INDEX")
    
    #detach("package:data.table")
    
    bsFinancialsMerged = bsFinancialsMerged[order(bsFinancialsMerged$COUNTRY, 
                                                  bsFinancialsMerged$CTRYCODE, 
                                                  bsFinancialsMerged$BVDIDNUM, 
                                                  bsFinancialsMerged$CLOSDATE_YEAR, 
                                                  bsFinancialsMerged$INDEX),]

    return (bsFinancialsMerged)
  
  }, paste0(headBlank, "  "))
  
  message(sprintf(paste0(headBlank, " ", "Merge: done!")))
  
  if (exportFile) {
  
    saveAs(bsFinancialsMerged, fileOut, fileType)
    
  } 
  
  return (as.data.frame(bsFinancialsMerged))
  
}
