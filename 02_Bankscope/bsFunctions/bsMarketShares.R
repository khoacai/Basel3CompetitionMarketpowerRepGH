
bsMarketShares <- function (refdata, basedVariable="wsTotalAssets", headBlank=" ") 
{

  message(sprintf(paste0(headBlank, "Market shares basing on %s: started"), basedVariable))
  
  on.exit(message(sprintf(paste0(headBlank, "Market shares basing on %s: done!"), basedVariable)))
  
  refdata$bootUp = TRUE
  
  #refdata = (processExec(function() {
    
    refdata$basedVariable = refdata[ , c(basedVariable)]
      
    suppressPackageStartupMessages(require(dplyr))
    
    refdata = refdata %>% group_by(CLOSDATE_YEAR)
    
    wholeMarketDS = refdata %>% summarise(wholeMarket = sum(basedVariable*UNIT, na.rm=TRUE))
    
    refdata = as.data.frame(left_join(refdata, wholeMarketDS)) %>% 
      mutate(MarketShare = .$basedVariable*UNIT/.$wholeMarket)
    
    detach("package:dplyr")
    
    basedVariable2 = paste0("MarketShare", substr(basedVariable, 3, nchar(basedVariable)))
    refdata[ , c(basedVariable2)] = refdata$MarketShare
    
    refdata[ , c("basedVariable","MarketShare")] = NULL
    gc()
    
    refdata = refdata[order(refdata$COUNTRY, refdata$CTRYCODE, refdata$BVDIDNUM, 
                      refdata$CLOSDATE_YEAR, refdata$INDEXX, refdata$CLOSDATE),]
      
    return (as.data.frame(refdata))
  
  #}, paste0(headBlank, " ")))
  
}
