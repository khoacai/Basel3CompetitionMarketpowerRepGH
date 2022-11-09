
getListedEntities <- function (refdata, databaseSource="FED", 
                               startDate="19991231", 
                               exportFile=T, 
                               fileOut="FedBHCF_listed", 
                               fileType=".RData", headBlank="") 
{

  message(sprintf(paste0(headBlank, "Get listed entities (BHCs, Commercial Banks): started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Get listed entities (BHCs, Commercial Banks): done!"))))
  
  refdata$bootUp = TRUE
  
  refdata = processExec(function() {
  
    sqlListed = paste0("select * from refdata where LISTED='Listed'", 
                       " and ", 
                       ifelse(databaseSource == "FED", "RSSD9999 >= ", "CLOSDATE_YEAR >= ") , 
                       startDate)
    
    suppressPackageStartupMessages(require(sqldf))
    
    refdata = sqldf(sqlListed)
    
    detach("package:sqldf")
    
    if (databaseSource == "FED") {
      
      refdata = refdata[order(refdata$RSSD9001, refdata$RSSD9999), ]
    
    }
    
    if (databaseSource == "BANKSCOPE") {
      
      refdata = refdata[order(refdata$COUNTRY, refdata$CTRYCODE, refdata$BVDIDNUM, 
                        refdata$CLOSDATE_YEAR, refdata$INDEX, refdata$CLOSDATE), ]
      
    }
  
    return (refdata)
    
  }, paste0(headBlank, " "))
  
  if (exportFile) {
    
    saveAs(refdata, fileOut, fileType)
    
  } 
  
  return (as.data.frame(refdata))
  
}
