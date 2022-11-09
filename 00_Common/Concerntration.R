
concerntration <- function (refdata, databaseSource="FED", topN=0, 
                            CountryGroup="US", CountryGroup_method=1, group="COUNTRY", 
                            by="wsdfTotalDeposits", 
                            exportFile=F, fileOut="FedBHCF_CCI", fileType=".RData", headBlank="") 
{
  
  message(sprintf(paste0(headBlank, paste0("Concerntration by ", by, " : started"))))
  
  on.exit(message(sprintf(paste0(headBlank, paste0("Concerntration by ", by, " : done!")))))
  
  refdata$bootUp = TRUE
  
  CCIs = (processExec(function() {
    
    suppressPackageStartupMessages(require(dplyr))
    
    refdata$CCIDataField = refdata[, by]
    
    if (databaseSource == "FED") {
      
      refdata = refdata %>% group_by(RSSD9999)
      
    }
    
    if (databaseSource == "BANKSCOPE") {
      
      if (group == "COUNTRY") {
        
        refdata = refdata %>% group_by(COUNTRY, CTRYCODE, CLOSDATE_YEAR)  
        
      }
      
      if (group == "STATE") {
        
        refdata = refdata %>% filter(!(is.na(STATE) | STATE=="")) %>% group_by(STATE, CLOSDATE_YEAR)
        
      }
      
    }
    
    suppressPackageStartupMessages(require(DescTools))
    
    refdata = refdata %>% summarise(marketSize = sum(topBigN(CCIDataField*UNIT, 0), na.rm=TRUE), 
                                    N = topN, 
                                    topBigNCnt = length(topBigN(CCIDataField*UNIT, topN)), #Count the available top big BHCs
                                    CCI = sum(topBigN(CCIDataField*UNIT, topN), na.rm=TRUE)/sum(topBigN(CCIDataField*UNIT, 0), na.rm=TRUE))
    
    detach("package:DescTools")
    detach("package:dplyr")
    
    refdata$CCIDataField = NULL
    gc()
    
    if (databaseSource == "FED") {
      
      refdata = refdata[order(refdata$RSSD9999), ] 
      
    }
    
    if (databaseSource == "BANKSCOPE") {
      
      if (by == "COUNTRY") {
        
        refdata = refdata[order(refdata$CTRYCODE, refdata$CLOSDATE_YEAR), ] 
        
      }
      
      if (by == "STATE") {
        
        refdata = refdata[order(refdata$STATE, refdata$CLOSDATE_YEAR), ] 
        
      }
      
    }
    
    return (refdata)
    
  }, paste0(headBlank, " ")))
  
  
  if (exportFile) {
    
    saveAs(CCIs, fileOut, fileType)
    
  } 
  
  return (as.data.frame(CCIs))
  
}
