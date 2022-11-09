#http://www.sciencedirect.com/science/article/pii/S0304405X09000816
#http://www.sciencedirect.com/science/article/pii/S0304405X10000401
#http://www.sciencedirect.com/science/article/pii/S0378426614001071


bsZScoreNumerator <- function (refdata, macroData, CountryGroup="US", CountryGroup_method=1, headBlank=" ") 
{
  message(sprintf(paste0(headBlank, "Zscore's Numerator: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Zscore's Numerator: done!"))))
 
  refdata$bootUp = TRUE
  
  return (processExec(function() {
  
    refdata = refdata[order(refdata$COUNTRY, refdata$CTRYCODE, refdata$BVDIDNUM, 
                            refdata$CLOSDATE_YEAR, refdata$INDEXX, refdata$CLOSDATE),]
    
    suppressPackageStartupMessages(require(dplyr))
    
    #Net Income
    #Preferring the data from DATA2115, DATA10285 sequentially.
    refdata = refdata %>% mutate(NetIncome = ifelse(!is.na(.$DATA2115), .$DATA2115, .$DATA10285))
    
    detach("package:dplyr")
    
    suppressPackageStartupMessages(require(dplyr))
    
    #Return On Equity (ROE)
    refdata = refdata %>% mutate(ROE = 100*.$NetIncome/.$TotalEquity)
    
    #Return On Total Assets (ROA)
    refdata = refdata %>% mutate(ROA = 100*.$NetIncome/.$TotalAssets)
    
    #Calculating the Zscores' numerators
    refdata = refdata %>% mutate(ZscoreNumerator = .$ROA + 
                             100*.$TotalEquity/.$TotalAssets)
    
    #Reoder the columns.
    refdata = refdata %>% select(NetIncome, FixedAssets, 
                           TotalAssets, TotalEquity, 
                           ROA, ROE, ZscoreNumerator, everything())
    refdata = refdata %>% select(-NetIncome, -FixedAssets, 
                           -TotalAssets, -TotalEquity, 
                           -ROA, -ROE, -ZscoreNumerator, everything())
    
    detach("package:dplyr")
    
    return (refdata)
  
  }, paste0(headBlank, " ")))
  
}
