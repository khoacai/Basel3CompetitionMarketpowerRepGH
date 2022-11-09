#http://www.sciencedirect.com/science/article/pii/S0304405X09000816
#http://www.sciencedirect.com/science/article/pii/S0304405X10000401
#http://www.sciencedirect.com/science/article/pii/S0378426614001071


bsRollingZScore <- function (refdata, rollingYears, headBlank=" ") 
{
  
  message(sprintf(paste0(headBlank, "Rolling Zscores: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Rolling Zscores: done!"))))
  
  refdata$bootUp = TRUE
  
  return (processExec(function() {
  
    refdata = refdata[order(refdata$COUNTRY, refdata$CTRYCODE, refdata$BVDIDNUM, 
                            refdata$CLOSDATE_YEAR, refdata$INDEXX, refdata$CLOSDATE), ]
    
    suppressPackageStartupMessages(require(sqldf))
    
    rollingZscoreSql = ""
    for (rollingYear in rollingYears) {
      rollingZscoreSql = paste0(rollingZscoreSql, 
                                "ZscoreNumerator/RollingStdevROA", rollingYear, "Years", 
                                " as RollingZscore", rollingYear, "years, ")
    }
    
    refdata = suppressWarnings(sqldf(paste0("select *, ", rollingZscoreSql, "1 from refdata"), row.names=TRUE))  
    refdata$'1' = NULL
  
    detach("package:sqldf")
  
    return (as.data.frame(refdata))
  
  }, paste0(headBlank, " ")))
  
}
