
bsWatchFluctuations <- function (refdata, 
                                 exportFile=F, fileOut="WatchFluctuations", fileType=".csv", 
                                 headBlank="") 
{

  message(sprintf(paste0(headBlank, "Watch the fluctuations over years: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Watch the fluctuations over years: done!"))))
  
  refdata$bootUp = TRUE
  
  bsWatch = (processExec(function() {
    
    suppressPackageStartupMessages(require(dplyr))
    
    refdata = refdata %>% mutate(DNSFR = ifelse(.$wsNSFR >= 1, 1, 0))
    
    detach("package:dplyr")
    
    suppressPackageStartupMessages(require(sqldf))
    
    #Consider the entities of commercial banks
    data2 = sqldf("select * from refdata where SPECIAL='Commercial banks'")
    #Consider the entities of bank holding company
    data3 = sqldf("select * from refdata where SPECIAL='Bank holdings & Holding companies'")
    
    #Watch the shifting up and down across the threshold of NSFR, value of 1.
    #package 'RSQLite' is required by 'sqldf' so it will be loaded automatically
    m = dbDriver("SQLite")
    connection = dbConnect(m, dbname = ":memory:")
    initExtension(connection)
    
    #(CBs)
    dbWriteTable(connection, 'data2', data2, row.names = TRUE)
    dbExecute(connection, 'create index dataX2 on data2(CTRYCODE, BVDIDNUM)', row.names=TRUE)
    data2 = dbGetQuery(connection, paste0("select A.*, ", 
                                          "(select ", 
                                          " case when count(*) = 1 ", 
                                          " then 1 ", 
                                          " else NULL end ", 
                                          " from data2 B ", 
                                          " where B.CTRYCODE = A.CTRYCODE ", 
                                          " and B.BVDIDNUM = A.BVDIDNUM ", 
                                          " and B.CLOSDATE_YEAR = (A.CLOSDATE_YEAR - 1)", 
                                          " and (B.DNSFR is not NULL and B.DNSFR = 0)", 
                                          " and (A.DNSFR is not NULL and A.DNSFR = 1)", 
                                          ") as DNSFRUP, ", 
                                          
                                          "(select ", 
                                          " case when count(*) = 1 ", 
                                          " then 1 ", 
                                          " else NULL end ", 
                                          " from data2 C ", 
                                          " where C.CTRYCODE = A.CTRYCODE ", 
                                          " and C.BVDIDNUM = A.BVDIDNUM ", 
                                          " and C.CLOSDATE_YEAR = (A.CLOSDATE_YEAR - 1)", 
                                          " and (C.DNSFR is not NULL and C.DNSFR = 1)", 
                                          " and (A.DNSFR is not NULL and A.DNSFR = 0)", 
                                          ") as DNSFRDOWN ",
                                          "from data2 A"), 
                       row.names=TRUE)
    
    #(BHCs)
    dbWriteTable(connection, 'data3', data3, row.names = TRUE)
    dbExecute(connection, 'create index dataX3 on data3(CTRYCODE, BVDIDNUM)', row.names=TRUE)
    data3 = dbGetQuery(connection, paste0("select A.*, ", 
                                          "(select ", 
                                          " case when count(*) = 1 ", 
                                          " then 1 ", 
                                          " else NULL end ", 
                                          " from data3 B ", 
                                          " where B.CTRYCODE = A.CTRYCODE ", 
                                          " and B.BVDIDNUM = A.BVDIDNUM ", 
                                          " and B.CLOSDATE_YEAR = (A.CLOSDATE_YEAR - 1)", 
                                          " and (B.DNSFR is not NULL and B.DNSFR = 0)", 
                                          " and (A.DNSFR is not NULL and A.DNSFR = 1)", 
                                          ") as DNSFRUP, ", 
                                          
                                          "(select ", 
                                          " case when count(*) = 1 ", 
                                          " then 1 ", 
                                          " else NULL end ", 
                                          " from data3 C ", 
                                          " where C.CTRYCODE = A.CTRYCODE ", 
                                          " and C.BVDIDNUM = A.BVDIDNUM ", 
                                          " and C.CLOSDATE_YEAR = (A.CLOSDATE_YEAR - 1)", 
                                          " and (C.DNSFR is not NULL and C.DNSFR = 1)", 
                                          " and (A.DNSFR is not NULL and A.DNSFR = 0)", 
                                          ") as DNSFRDOWN ",
                                          "from data3 A"),
                       row.names=TRUE)  
    
    dbDisconnect(connection)
    
    detach("package:sqldf")
    
    suppressPackageStartupMessages(require(dplyr))
    
    #The fluctuation of NSFR in CBs entities
    bsWatchDNSFRCBs = data2 %>% 
      group_by(CLOSDATE_YEAR) %>% 
      summarise(dNSFRTUPCBs = sum(DNSFRUP, na.rm=T), 
                dNSFRTDOWNCBs = sum(DNSFRDOWN, na.rm=T), 
                dNSFRTDIFFUPDOWNCBs = sum(DNSFRUP, na.rm=T) - sum(DNSFRDOWN, na.rm=T))
    
    #The fluctuation of NSFR in BHCs entities
    bsWatchDNSFRBHCs = data3 %>% 
      group_by(CLOSDATE_YEAR) %>% 
      summarise(dNSFRTUPBHCs = sum(DNSFRUP, na.rm=T), 
                dNSFRTDOWNBHCs = sum(DNSFRDOWN, na.rm=T), 
                dNSFRTDIFFUPDOWNBHCs = sum(DNSFRUP, na.rm=T)- sum(DNSFRDOWN, na.rm=T))
    
    #The fluctuation in quantity of entities
    bsWatchNumOfEntitiesAll = refdata %>% 
      group_by(CLOSDATE_YEAR) %>% 
      summarise(cntEntitiesBHCs =  sum(SPECIAL=="Bank holdings & Holding companies", na.rm=T), 
                cntEntitiesCBs = sum(SPECIAL=="Commercial banks", na.rm=T), 
                cntEntitiesTotal = n(), 
                cntEntitiesListed = sum(LISTED == "Listed", na.rm=T), 
                cntEntitiesNonListed = n() - sum(LISTED == "Listed", na.rm=T))
    
    #The fluctuation in number of banks with NSFR < 1 and their corresponding portion of Marker's Assets
    bsWatchNSFRLeq1CntNAssets = refdata %>% filter(!is.na(wsNSFR)) %>%
      group_by(CLOSDATE_YEAR) %>% 
      summarise(numOfNSFRLt1 = sum(wsNSFR < 1, na.rm=T), 
                numOfNSFRLt1Pct = sum(wsNSFR < 1, na.rm=T)/n(), 
                totalOfNSFRLt1Asset = sum(if_else(wsNSFR < 1, wsdfTotalAssets*UNIT, 0), na.rm=T), 
                totalOfNSFRLt1AssetPct = sum(if_else(wsNSFR < 1, wsdfTotalAssets*UNIT, 0), na.rm=T)/sum(wsdfTotalAssets*UNIT, na.rm=T))
    
    bsWatch = inner_join(bsWatchDNSFRCBs, 
                         bsWatchDNSFRBHCs, 
                         by=c("CLOSDATE_YEAR"))
    
    bsWatch = inner_join(bsWatch, 
                         bsWatchNumOfEntitiesAll, 
                         by=c("CLOSDATE_YEAR"))
    
    bsWatch = inner_join(bsWatch, 
                         bsWatchNSFRLeq1CntNAssets, 
                         by=c("CLOSDATE_YEAR"))
    
    detach("package:dplyr")
  
    return (bsWatch)
  
  }, paste0(headBlank, " ")))
  
  if (exportFile) {
    
    saveAs(bsWatch, fileOut, fileType)
    
  } 
  
  return (as.data.frame(bsWatch))
  
}
