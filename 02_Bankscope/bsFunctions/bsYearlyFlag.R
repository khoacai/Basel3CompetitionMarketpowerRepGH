
bsYearlyFlag <- function (refdata, headBlank="  ") 
{

  message(sprintf(paste0(headBlank, "Add consecutive yearly flag to dataset: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Add consecutive yearly flag to dataset: done!"))))

  refdata$bootUp = TRUE
  
  return (processExec(function() {
    
    refdata = refdata[order(refdata$COUNTRY, refdata$CTRYCODE, refdata$BVDIDNUM, refdata$CLOSDATE_YEAR, refdata$INDEXX, refdata$CLOSDATE), ]
      
    suppressPackageStartupMessages(require(dplyr))
    
    #Add a flag indicating that whether a bank's observations is consecutive yearly data
    refdata = refdata %>% 
      group_by(COUNTRY, CTRYCODE, BVDIDNUM) %>% 
      summarise(SPECIAL = summarizze(SPECIAL), ENTITYTYPE = summarizze(ENTITYTYPE), 
                INDEXX = summarizze(INDEXX), NAME = summarizze(NAME), 
                SWIFT = summarizze(SWIFT), STATE = summarizze(STATE), 
                minCLOSDATE_YEAR = min(CLOSDATE_YEAR), maxCLOSDATE_YEAR = max(CLOSDATE_YEAR), 
                izYearly = (with(rle(diff(CLOSDATE_YEAR)), 
                                 ifelse(length(lengths) == 1 & 
                                          lengths[1] == (length(CLOSDATE_YEAR)-1) & 
                                          length(values) == 1 & 
                                          values[1] == 1, TRUE, FALSE))) 
                | length(diff(CLOSDATE_YEAR)) == 0,
                .groups = "keep")
  
    detach("package:dplyr")
  
    return (as.data.frame(refdata))
  
  }, paste0(headBlank, " ")))
  
}
