
stdevRMsPerBank <- function (refdata, databaseSource="FED", 
                             variables, exportFile=F, 
                             fileOut="FedBHCF_RMsPerBank.RData", fileType=".RData") 
{

  message(sprintf("Stdev for risk measures per bank: started"))
  
  on.exit(message(sprintf("Stdev for risk measures per bank: done!")))
  
  refdata$bootUp = TRUE
  
  stdevRMsPerBank = (processExec(function() {

    refdata = cbind(refdata, 
                 setNames(refdata[variables], paste0(variables, "_Stdev")), 
                 setNames(refdata[variables], paste0(variables, "_TotalCnt")), 
                 setNames(refdata[variables], paste0(variables, "_AvlbCnt")), 
                 setNames(refdata[variables], paste0(variables, "_NaInfCnt")))
    
    suppressPackageStartupMessages(require(dplyr))

    if (databaseSource == "FED") {
     
      refdata = refdata %>% group_by(RSSD9001)
    
    }
    
    if (databaseSource == "BANKSCOPE") {
      
      refdata = refdata %>% group_by(CTRYCODE, BVDIDNUM)

    }
    
    refdata = refdata %>% mutate_at(vars(ends_with("_Stdev")), stdev) %>% 
      mutate_at(vars(ends_with("_TotalCnt")), totalCount) %>% 
      mutate_at(vars(ends_with("_AvlbCnt")), availableCount) %>% 
      mutate_at(vars(ends_with("_NaInfCnt")), defectCount)

    if (databaseSource == "FED") {

      refdata = refdata %>% 
        summarise_at(.vars=vars(starts_with("RSSD"), 
                                ends_with("_Stdev"), 
                                ends_with("_TotalCnt"), 
                                ends_with("_AvlbCnt"), 
                                ends_with("_NaInfCnt")), 
                     .funs=summarizze)
    
      refdata = refdata[order(refdata$RSSD9001),] 
      
    }
    
    if (databaseSource == "BANKSCOPE") {
      
      refdata = refdata %>% 
        summarise_at(.vars=vars(NAME, SWIFT, COUNTRY, LISTED, 
                                ends_with("_Stdev"), 
                                ends_with("_TotalCnt"), 
                                ends_with("_AvlbCnt"), 
                                ends_with("_NaInfCnt")), 
                     .funs=summarizze)
      
      refdata = refdata[order(refdata$COUNTRY, refdata$CTRYCODE, refdata$BVDIDNUM), ] 
      
      #Reoder the columns.
      refdata = refdata %>% select(COUNTRY, CTRYCODE, BVDIDNUM, NAME, SWIFT, LISTED, everything())
      
    }
    
    detach("package:dplyr")
    
    return (refdata)
    
  }, " "))
  
  refdata = NULL
  gc()

  if (exportFile) {
    
    saveAs(stdevRMsPerBank, fileOut, fileType)
    
  }
  
  return (as.data.frame(stdevRMsPerBank))
  
}
