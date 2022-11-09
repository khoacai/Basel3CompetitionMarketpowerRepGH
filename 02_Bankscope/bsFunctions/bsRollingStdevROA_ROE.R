
bsRollingStdevROA_ROE <- function (refdata, rollingYears, headBlank=" ") 
{
  
  message(sprintf(paste0(headBlank, "Rolling stdevs (ROA), (ROE): started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Rolling stdevs (ROA), (ROE): done!"))))
  
  refdata$bootUp = TRUE
  
  return (processExec(function() {
    
    refdata[ , c("ROA")][is.infinite(refdata[, c("ROA")])] = NA
    refdata[ , c("ROE")][is.infinite(refdata[, c("ROE")])] = NA
    
    refdata = refdata[order(refdata$COUNTRY, refdata$CTRYCODE, refdata$BVDIDNUM, 
                            refdata$CLOSDATE_YEAR, refdata$INDEXX, refdata$CLOSDATE), ]
    
    suppressPackageStartupMessages(require(sqldf))
    
    subCaseStdev = " stdev(rollTbl.Return) " #Involving built-in SQL's STDEV function
    
    subClauseTmpl = paste0(" refdata rollTbl", 
                           " where rollTbl.CTRYCODE = A.CTRYCODE ", 
                           " and rollTbl.BVDIDNUM = A.BVDIDNUM ", 
                           " and rollTbl.CLOSDATE_YEAR between ", 
                           "  (A.CLOSDATE_YEAR - rollingYearMinus1) and A.CLOSDATE_YEAR")
    
    subCondNA = paste0(" rollTbl.Return is not NULL ")
    
    rollingStdevSql = ""
    for (rollingYear in rollingYears) {
      rollingStdevSql = paste0(rollingStdevSql, 
                             "(select ", 
                             " case when count(*) > 1", 
                             " then ", gsub("rollTbl", paste0("B1", rollingYear), gsub("Return", "ROA", subCaseStdev)),  
                             " else NULL end ", 
                             " from ", gsub("rollTbl", paste0("B1", rollingYear), gsub("rollingYearMinus1", rollingYear-1, subClauseTmpl)), 
                             " and ", gsub("rollTbl", paste0("B1", rollingYear), gsub("Return", "ROA", subCondNA)), 
                             ") as RollingStdevROA", rollingYear, "years, ", 
                             "(select count(*) ", 
                             " from ", gsub("rollTbl", paste0("B2", rollingYear), gsub("rollingYearMinus1", rollingYear-1, subClauseTmpl)), 
                             " and ", gsub("rollTbl", paste0("B2", rollingYear), gsub("Return", "ROA", subCondNA)), 
                             ") as RollingROA", rollingYear, "AvailCt, ", 
                             "(select ", 
                             " case when count(*) > 1", 
                             " then ", gsub("rollTbl", paste0("C1", rollingYear), gsub("Return", "ROE", subCaseStdev)), 
                             " else NULL end ", 
                             " from ", gsub("rollTbl", paste0("C1", rollingYear), gsub("rollingYearMinus1", rollingYear-1, subClauseTmpl)), 
                             " and ", gsub("rollTbl", paste0("C1", rollingYear), gsub("Return", "ROE", subCondNA)), 
                             ") as RollingStdevROE", rollingYear, "years, ", 
                             "(select count(*) ", 
                             " from ", gsub("rollTbl", paste0("C2", rollingYear), gsub("rollingYearMinus1", rollingYear-1, subClauseTmpl)), 
                             " and ", gsub("rollTbl", paste0("C2", rollingYear), gsub("Return", "ROE", subCondNA)), 
                             ") as RollingROE", rollingYear, "AvailCt, ") 
    }
      
    refdata = suppressWarnings(sqldf(c("create index refdataX on refdata(CTRYCODE, BVDIDNUM)", 
                               paste0("select *, ", 
                                      rollingStdevSql, 
                                      "1 from refdata A")), 
                             row.names=TRUE))
    refdata$'1' = NULL
    gc()
    
    detach("package:sqldf")
  
    return (as.data.frame(refdata))
  
  }, paste0(headBlank, " ")))
  
}
