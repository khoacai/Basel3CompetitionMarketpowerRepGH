
bsYearlyFillingGaps <- function (refdata, headBlank=" ") 
{

  message(sprintf(paste0(headBlank, "Filling gaps at the non-consecutive yearly data: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Filling gaps at the non-consecutive yearly data: done!"))))
  
  refdata = refdata[order(refdata$COUNTRY, refdata$CTRYCODE, refdata$BVDIDNUM, refdata$CLOSDATE_YEAR, refdata$INDEXX, refdata$CLOSDATE), ]
  
  #Add a flag indicating that whether a bank's observations is consecutive yearly data
  bsFinancialsCandidate = bsYearlyFlag(refdata)
  
  return (processExec(function() {
    
    #Filling the gaps in the report dates to ensure they are fully yearly data for all banks.
    #Except for the key information, the remaning datacodes are set to NA, i.e., missing.
    suppressPackageStartupMessages(require(plyr))
    suppressPackageStartupMessages(require(dplyr))
    
    #Construct the fullfilled dataset by combining yearly and filled datasets
    bsFinancialsCandidate = rbind(
      #consecutive yearly banks with minimum unique keys information
      as.data.frame(match_df(refdata, 
                             bsFinancialsCandidate %>% 
                               group_by(COUNTRY, CTRYCODE, BVDIDNUM) %>% 
                               filter(izYearly) %>% 
                               select(COUNTRY, CTRYCODE, BVDIDNUM), 
                             on = c("COUNTRY", "CTRYCODE", "BVDIDNUM"))[,c("COUNTRY", "CTRYCODE", 
                                                                           "BVDIDNUM", "SPECIAL", 
                                                                           "ENTITYTYPE", "INDEXX", 
                                                                           "NAME", "SWIFT", 
                                                                           "STATE", "CLOSDATE_YEAR")]), 
      #Fill gaps in CLOSDATE_YEAR to generate consecutive yearly data from defected banks
      as.data.frame(bsFinancialsCandidate %>% filter(!izYearly) %>% 
                      group_by(COUNTRY, CTRYCODE, BVDIDNUM) %>% 
                      do(data.frame(SPECIAL = .$SPECIAL, ENTITYTYPE = .$ENTITYTYPE, 
                                    INDEXX = .$INDEXX, NAME = .$NAME, 
                                    SWIFT = .$SWIFT, STATE = .$STATE, 
                                    CLOSDATE_YEAR = seq(from = .$minCLOSDATE_YEAR, 
                                                        to = .$maxCLOSDATE_YEAR))))) 
    
    #Reoder the columns.
    bsFinancialsCandidate = bsFinancialsCandidate %>% 
      select(COUNTRY, CTRYCODE, BVDIDNUM, STATE, 
             SPECIAL, ENTITYTYPE, INDEXX, everything())
    
    #The complement information from "data", on the right hand side of the join, 
    #are appended to the fullfilled constructed dataset
    refdata = left_join(bsFinancialsCandidate, refdata)
    
    refdata = refdata[order(refdata$COUNTRY, refdata$CTRYCODE, refdata$BVDIDNUM, 
                      refdata$CLOSDATE_YEAR, refdata$INDEXX, refdata$CLOSDATE), ]
    
    detach("package:dplyr")
    detach("package:plyr")
  
    return (as.data.frame(refdata))
  
  }, paste0(headBlank, " ")))
  
}
