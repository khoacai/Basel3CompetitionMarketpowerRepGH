
EACleanupQ <- function (refdata, 
                        exportFile=F, fileOut="U.S._EA", 
                        fileType=".RData", headBlank="") 
{

  message(sprintf(paste0(headBlank, "Cleanup Enforcement actions (EA) Quarterly: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Cleanup Enforcement actions (EA) Quarterly: done!"))))
  
  EA = refdata
  
  EA = (processExec(function() {

    EA$DSANCTION = 1
    EA = EA[,c("INSTITUTION",	"EACITY",	"EASTATE", "EASTATECODE",	
               "EASTARTYEAR",	"EASTARTQUARTER", "EASTARTYQ", 
               "EATERMINATIONYEAR",	"EATERMINATIONQUARTER", "EATERMINATIONYQ", 
               "EATYPE", "DSANCTION")]
    
    EA[is.na(EA)] = 99999
    
    EA$INSTITUTION = sub("N.A.", "National Association", EA$INSTITUTION)
    EA$INSTITUTION2 = EA$INSTITUTION
    EA$INSTITUTION = toupper(EA$INSTITUTION)
    
    suppressPackageStartupMessages(require(sqldf))
    EAa = EAb = EA
    EA = sqldf(c(paste0("update EA ", 
                        "set EASTARTYQ = ", 
                        " (select min(EAa.EASTARTYQ) ", 
                        "  from EAa ", 
                        "  where EAa.EATYPE <> 'Bank Civil Money Penalties' ", 
                        "  and EA.INSTITUTION like EAa.INSTITUTION ", 
                        "  and EA.EASTATECODE = EAa.EASTATECODE ", 
                        "  and EA.EACITY = EAa.EACITY ", 
                        "  and EA.EASTARTYQ >= EAa.EASTARTYQ", 
                        "  and EA.EASTARTYQ <= EAa.EATERMINATIONYQ), ", 
                        " EATERMINATIONYQ = ", 
                        " (select max(EAb.EATERMINATIONYQ) ", 
                        "  from EAb ", 
                        "  where EAb.EATYPE <> 'Bank Civil Money Penalties' ", 
                        "  and EA.INSTITUTION like EAb.INSTITUTION ", 
                        "  and EA.EASTATECODE = EAb.EASTATECODE ", 
                        "  and EA.EACITY = EAb.EACITY ", 
                        "  and EA.EATERMINATIONYQ >= EAb.EASTARTYQ ", 
                        "  and EA.EATERMINATIONYQ <= EAb.EATERMINATIONYQ) ", 
                        "where EA.EATYPE <> 'Bank Civil Money Penalties'"), 
                 "select * from EA"))

    EAa = EAb = EA
    EA = sqldf(c(paste0("delete from EA ", 
                        "where EA.EATYPE = 'Bank Civil Money Penalties' ", 
                        "  and EA.EASTARTYQ >=  ", 
                        " (select min(EAa.EASTARTYQ) ", 
                        "  from EAa ", 
                        "  where EAa.EATYPE <> 'Bank Civil Money Penalties' ", 
                        "  and EA.INSTITUTION like EAa.INSTITUTION ", 
                        "  and EA.EASTATECODE = EAa.EASTATECODE ", 
                        "  and EA.EACITY = EAa.EACITY) ", 
                        "  and EA.EASTARTYQ <= ", 
                        " (select max(EAb.EATERMINATIONYQ) ", 
                        "  from EAb ", 
                        "  where EAb.EATYPE <> 'Bank Civil Money Penalties' ", 
                        "  and EA.INSTITUTION like EAb.INSTITUTION ", 
                        "  and EA.EASTATECODE = EAb.EASTATECODE ", 
                        "  and EA.EACITY = EAb.EACITY) "), 
                 "select * from EA"))
    
    EAa = EAb = EA
    EA = sqldf(c(paste0("delete from EA ", 
                        "where ", 
                        " EA.EATYPE <> 'Bank Civil Money Penalties' ", 
                        " and 0 < (select count(*) ", 
                        "         from EAa ", 
                        "         where EAa.EATYPE <> 'Bank Civil Money Penalties' ", 
                        "         and EA.INSTITUTION like EAa.INSTITUTION ", 
                        "         and EA.EASTATECODE = EAa.EASTATECODE ", 
                        "         and EA.EACITY = EAa.EACITY ", 
                        "         and EA.EASTARTYQ > EAa.EASTARTYQ ", 
                        "         and EA.EATERMINATIONYQ <= EAa.EATERMINATIONYQ) "), 
                 "select * from EA"))
    
    EAa = EAb = EA
    EA = sqldf(c(paste0("delete from EA ", 
                        " where 0 < (select count(*) ", 
                        "         from EAa ", 
                        "         where EAa.EATYPE <> 'Bank Civil Money Penalties' ", 
                        "         and EA.INSTITUTION like EAa.INSTITUTION ", 
                        "         and EA.EASTATECODE = EAa.EASTATECODE ", 
                        "         and EA.EACITY = EAa.EACITY ", 
                        "         and EA.EASTARTYQ = EAa.EASTARTYQ ", 
                        "         and EA.EATERMINATIONYQ = EAa.EATERMINATIONYQ", 
                        "         and EA.EATYPE <> EAa.EATYPE) "), 
                 "select * from EA"))
    
    EA = EA[order(EA$INSTITUTION, EA$EASTATECODE, EA$EACITY, EA$EASTARTYQ, EA$EATERMINATIONYQ),]
    
    eaNonPenaltyUnique = sqldf(paste0( "select distinct * ", 
                                       "from EA ", 
                                       "group by INSTITUTION, EASTATECODE, EACITY, EASTARTYQ ", 
                                       "having count(DSANCTION) = 1 ", 
                                       "and EATYPE <> 'Bank Civil Money Penalties'"), 
                               row.names=TRUE)
    
    eaNonPenaltyUnDuplicated = sqldf(paste0( "select distinct * ", 
                                             "from EA ", 
                                             "group by INSTITUTION, EASTATECODE, EACITY, EASTARTYQ ", 
                                             "having count(DSANCTION) > 1 ", 
                                             "and EATYPE <> 'Bank Civil Money Penalties'"), 
                                     row.names=TRUE)
    
    eaPenaltyUnique = sqldf(paste0( "select distinct * ", 
                                    "from EA ", 
                                    "group by INSTITUTION, EASTATECODE, EACITY, EASTARTYQ ", 
                                    "having count(DSANCTION) = 1 ", 
                                    "and EATYPE = 'Bank Civil Money Penalties'"), 
                            row.names=TRUE)
    
    eaPenaltyUnDuplicated = sqldf(paste0( "select distinct * ", 
                                          "from EA ", 
                                          "group by INSTITUTION, EASTATECODE, EACITY, EASTARTYQ ", 
                                          "having count(DSANCTION) > 1 ", 
                                          "and EATYPE = 'Bank Civil Money Penalties'"), 
                                  row.names=TRUE)
    
    EA = rbind(eaNonPenaltyUnique, eaNonPenaltyUnDuplicated, 
               eaPenaltyUnique, eaPenaltyUnDuplicated)
    
    EA[,][EA[ , ] == 99999] = NA
    
    EA$EASTARTYEAR = EA$EASTARTYQ%/%10
    EA$EASTARTQUARTER = EA$EASTARTYQ%%10
    EA$EATERMINATIONYEAR = EA$EATERMINATIONYQ%/%10
    EA$EATERMINATIONQUARTER = EA$EATERMINATIONYQ%%10
    EA$INSTITUTION = EA$INSTITUTION2
    EA$INSTITUTION2=NULL
    
    return (EA)
  
  }, paste0(headBlank, " ")))
  
  
  if (exportFile) {
    
    saveAs(EA, fileOut, fileType)
    
  } 
  
  return (as.data.frame(EA))
  
}
