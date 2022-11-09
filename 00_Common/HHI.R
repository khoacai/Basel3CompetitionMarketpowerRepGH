
HHI <- function (refdata, databaseSource="FED", topN=0, 
                 CountryGroup="US", CountryGroup_method=1, group="COUNTRY", 
                 by="wsdfTotalAssets", 
                 exportFile=F, fileOut="FedBHCF_HHI", fileType=".RData", headBlank="") 
{

  message(sprintf(paste0(headBlank, "Herfindahl-Hirschman Index (HHI) at %s-level by %s : started"), group, by))
  
  on.exit(message(sprintf(paste0(headBlank, "Herfindahl-Hirschman Index (HHI) at %s-level by %s : done!"), group, by)))
  
  refdata$bootUp = TRUE
  
  HHIs = (processExec(function() {

    refdata2 = refdata
    
    suppressPackageStartupMessages(require(dplyr))
    
    refdata2$HHIDataField = refdata2[, by]
    
    if (databaseSource == "FED") {
      
      refdata2$UNIT = 1
      
      if (group == "COUNTRY") {
        
        refdata2 = refdata2 %>% group_by(YEAR)  
        
      }
      
      if (group == "STATE") {
        
        refdata2 = refdata2 %>% 
          filter(!(is.na(RSSD9200) | RSSD9200=="")) %>% group_by(RSSD9200, YEAR)
        
      }
      
    }
    
    if (databaseSource == "BANKSCOPE") {
      
      if (group == "COUNTRY") {
        
        refdata2 = refdata2 %>% group_by(COUNTRY, CTRYCODE, CLOSDATE_YEAR)  
        
      }
      
      if (group == "STATE") {
        
        refdata2 = refdata2 %>% filter(!(is.na(STATE) | STATE=="")) %>% group_by(STATE, CLOSDATE_YEAR)
        
      }
      
    }
    
    suppressPackageStartupMessages(require(DescTools))

    #call SUMMARISE function, with the argument .groups = "keep" to use the same group structure as .data (i.e. refdata2),
    #to fix the warning message: 
    #    `summarise()` has grouped output by 'COUNTRY', 'CTRYCODE'. 
    #     You can override using the `.groups` argument.
    #OR
    #Please see the mannual @https://cran.r-project.org/web/packages/dplyr/dplyr.pdf
    refdata2 = refdata2 %>% 
      summarise(marketSize = sum(topBigN(HHIDataField*UNIT, topN), na.rm=TRUE), 
                N = topN, 
                topBigNCnt = length(topBigN(HHIDataField*UNIT, topN)), #Count the available top big BHCs
                HHI = 10000*Herfindahl(topBigN(HHIDataField*UNIT, topN)), #Herfindahl-Hirschman Index) # the estimated market size
                .groups = "keep") 
    
    detach("package:DescTools")
    detach("package:dplyr")
    
    refdata2$HHIDataField = NULL
    gc()
  
    if (databaseSource == "FED") {
      
      if (by == "COUNTRY") {
        
        refdata2 = refdata2[order(refdata2$YEAR), ] 
        
      }
      
      if (by == "STATE") {
        
        refdata2 = refdata2[order(refdata2$RSSD9200, refdata2$YEAR), ] 
        
      }
      
    }
      
    if (databaseSource == "BANKSCOPE") {
      
      if (by == "COUNTRY") {
        
        refdata2 = refdata2[order(refdata2$CTRYCODE, refdata2$CLOSDATE_YEAR), ] 
        
      }
      
      if (by == "STATE") {
        
        refdata2 = refdata2[order(refdata2$STATE, refdata2$CLOSDATE_YEAR), ] 
        
      }
      
    }
    
    return (refdata2)
  
  }, paste0(headBlank, " ")))
  
  
  if (exportFile) {
    
    saveAs(HHIs, fileOut, fileType)
    
  } 
  
  return (as.data.frame(HHIs))
  
}
