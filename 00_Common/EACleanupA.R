
EACleanupA <- function (refdata, databaseSource="FED", 
                         exportFile=F, fileOut="U.S._EA", 
                         fileType=".RData", headBlank="") 
{

  message(sprintf(paste0(headBlank, "Cleanup Enforcement actions (EA): started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Cleanup Enforcement actions (EA): done!"))))
  
  EA = refdata
  
  EA = (processExec(function() {

    EA$DSANCTION = 1
    EA = EA[,c("INSTITUTION",	"EACITY",	"EASTATE", "EASTATECODE",	
               "EASTARTYEAR",	"EATERMINATIONYEAR",	"EATYPE", "DSANCTION")]
    
    EA[is.na(EA)] = 3000
    
    EA$INSTITUTION = sub("N.A.", "National Association", EA$INSTITUTION)
    EA$INSTITUTION2 = EA$INSTITUTION
    EA$INSTITUTION = toupper(EA$INSTITUTION)
    
    suppressPackageStartupMessages(require(sqldf))
    
    EAa = EAb = EA
    
    #package 'RSQLite' is required by 'sqldf' so it will be loaded automatically
    m = dbDriver("SQLite")
    connection = dbConnect(m, dbname = ":memory:")
    initExtension(connection)
    
    dbWriteTable(connection, 'EA', EA, row.names = FALSE)
    dbExecute(connection, paste0("update EA ", 
                                 "set EASTARTYEAR = ", 
                                 " (select min(EAa.EASTARTYEAR) ", 
                                 "  from EAa ", 
                                 "  where EAa.EATYPE <> 'Bank Civil Money Penalties' ", 
                                 "  and EA.INSTITUTION like EAa.INSTITUTION ", 
                                 "  and EA.EASTATECODE = EAa.EASTATECODE ", 
                                 "  and EA.EACITY = EAa.EACITY ", 
                                 "  and EA.EASTARTYEAR >= EAa.EASTARTYEAR ", 
                                 "  and EA.EASTARTYEAR <= EAa.EATERMINATIONYEAR), ", 
                                 " EATERMINATIONYEAR = ", 
                                 " (select max(EAb.EATERMINATIONYEAR) ", 
                                 "  from EAb ", 
                                 "  where EAb.EATYPE <> 'Bank Civil Money Penalties' ", 
                                 "  and EA.INSTITUTION like EAb.INSTITUTION ", 
                                 "  and EA.EASTATECODE = EAb.EASTATECODE ", 
                                 "  and EA.EACITY = EAb.EACITY ", 
                                 "  and EA.EATERMINATIONYEAR >= EAb.EASTARTYEAR ", 
                                 "  and EA.EATERMINATIONYEAR <= EAb.EATERMINATIONYEAR) ", 
                                 "where EA.EATYPE <> 'Bank Civil Money Penalties'"),
                        row.names=FALSE)
    EA = dbGetQuery(connection, "select * from EA", 
                       row.names=FALSE)
    
    EAa = EAb = EA
    
    EA = dbExecute(connection, paste0("delete from EA ", 
                                      "where EA.EATYPE = 'Bank Civil Money Penalties' ", 
                                      "  and EA.EASTARTYEAR >=  ", 
                                      " (select min(EAa.EASTARTYEAR) ", 
                                      "  from EAa ", 
                                      "  where EAa.EATYPE <> 'Bank Civil Money Penalties' ", 
                                      "  and EA.INSTITUTION like EAa.INSTITUTION ", 
                                      "  and EA.EASTATECODE = EAa.EASTATECODE ", 
                                      "  and EA.EACITY = EAa.EACITY) ", 
                                      "  and EA.EASTARTYEAR <= ", 
                                      " (select max(EAb.EATERMINATIONYEAR) ", 
                                      "  from EAb ", 
                                      "  where EAb.EATYPE <> 'Bank Civil Money Penalties' ", 
                                      "  and EA.INSTITUTION like EAb.INSTITUTION ", 
                                      "  and EA.EASTATECODE = EAb.EASTATECODE ", 
                                      "  and EA.EACITY = EAb.EACITY) "),
                   row.names=FALSE)
    
    EA = dbGetQuery(connection, "select * from EA", row.names=FALSE)

    EAa = EAb = EA
    
    EA = dbExecute(connection, paste0("delete from EA ", 
                                      "where ", 
                                      " EA.EATYPE <> 'Bank Civil Money Penalties' ", 
                                      " and 0 < (select count(*) ", 
                                      "         from EAa ", 
                                      "         where EAa.EATYPE <> 'Bank Civil Money Penalties' ", 
                                      "         and EA.INSTITUTION like EAa.INSTITUTION ", 
                                      "         and EA.EASTATECODE = EAa.EASTATECODE ", 
                                      "         and EA.EACITY = EAa.EACITY ", 
                                      "         and EA.EASTARTYEAR > EAa.EASTARTYEAR ", 
                                      "         and EA.EATERMINATIONYEAR <= EAa.EATERMINATIONYEAR) "),
                   row.names=FALSE)
    
    EA = dbGetQuery(connection, "select * from EA", row.names=FALSE)
    
    EAa = EAb = EA
    
    EA = dbExecute(connection, paste0("delete from EA ", 
                                      " where 0 < (select count(*) ", 
                                      "         from EAa ", 
                                      "         where EAa.EATYPE <> 'Bank Civil Money Penalties' ", 
                                      "         and EA.INSTITUTION like EAa.INSTITUTION ", 
                                      "         and EA.EASTATECODE = EAa.EASTATECODE ", 
                                      "         and EA.EACITY = EAa.EACITY ", 
                                      "         and EA.EASTARTYEAR = EAa.EASTARTYEAR ", 
                                      "         and EA.EATERMINATIONYEAR = EAa.EATERMINATIONYEAR", 
                                      "         and EA.EATYPE <> EAa.EATYPE) "),
                   row.names=FALSE)
    
    EA = dbGetQuery(connection, "select * from EA", row.names=FALSE)
    
    EA = EA[order(EA$INSTITUTION, EA$EASTATECODE, EA$EACITY, EA$EASTARTYEAR, EA$EATERMINATIONYEAR),]
    
    eaNonPenaltyUnique = sqldf(paste0( "select distinct * ", 
                                       "from EA ", 
                                       "group by INSTITUTION, EASTATECODE, EACITY, EASTARTYEAR ", 
                                       "having count(DSANCTION) = 1 ", 
                                       "and EATYPE <> 'Bank Civil Money Penalties'"), 
                               row.names=TRUE)
    
    eaNonPenaltyUnDuplicated = sqldf(paste0( "select distinct * ", 
                                             "from EA ", 
                                             "group by INSTITUTION, EASTATECODE, EACITY, EASTARTYEAR ", 
                                             "having count(DSANCTION) > 1 ", 
                                             "and EATYPE <> 'Bank Civil Money Penalties'"), 
                                     row.names=TRUE)
    
    eaPenaltyUnique = sqldf(paste0( "select distinct * ", 
                                    "from EA ", 
                                    "group by INSTITUTION, EASTATECODE, EACITY, EASTARTYEAR ", 
                                    "having count(DSANCTION) = 1 ", 
                                    "and EATYPE = 'Bank Civil Money Penalties'"), 
                            row.names=TRUE)
    
    eaPenaltyUnDuplicated = sqldf(paste0( "select distinct * ", 
                                          "from EA ", 
                                          "group by INSTITUTION, EASTATECODE, EACITY, EASTARTYEAR ", 
                                          "having count(DSANCTION) > 1 ", 
                                          "and EATYPE = 'Bank Civil Money Penalties'"), 
                                  row.names=TRUE)
    
    dbDisconnect(connection)
    
    detach("package:sqldf")
    
    EA = rbind(eaNonPenaltyUnique, eaNonPenaltyUnDuplicated, 
               eaPenaltyUnique, eaPenaltyUnDuplicated)
    
    EA[,c("EATERMINATIONYEAR")][EA[ , c("EATERMINATIONYEAR")] == 3000] = NA
    EA$INSTITUTION = EA$INSTITUTION2
    EA$INSTITUTION2=NULL
    
    return (EA)
  
  }, paste0(headBlank, " ")))
  
  
  if (exportFile) {
    
    saveAs(EA, fileOut, fileType)
    
  } 
  
  return (as.data.frame(EA))
  
}
