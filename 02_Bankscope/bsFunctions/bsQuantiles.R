
bsQuantiles <- function (refdata, basedVariable="wsMarketShareNetLoans", headBlank=" ") 
{

  message(sprintf(paste0(headBlank, "Quantiles basing on %s: started"), basedVariable))
  
  on.exit(message(sprintf(paste0(headBlank, "Quantiles basing on %s: done!"), basedVariable)))
  
  refdata$bootUp = TRUE
  
  refdata = (processExec(function() {
    
    refdata$basedVariable = refdata[ , c(basedVariable)]
    
    suppressPackageStartupMessages(require(dplyr))
    
    refdata = as.data.frame(refdata %>% 
                              group_by(CLOSDATE_YEAR) %>% 
                              do({ 
                                quantiles = quantile(.$basedVariable, 
                                                    c(0.25, 0.5, 0.75, 0.9), #percentiles
                                                    na.rm=TRUE)
                                data.frame(quantile25 = quantiles[1], #quantile of 0.25
                                          quantile50 = quantiles[2], #quantile of 0.5
                                          quantile75 = quantiles[3], #quantile of 0.75
                                          quantile90 = quantiles[4]) #quantile of 0.9
                                }))
    
    suppressPackageStartupMessages(require(data.table))
    
    setnames(refdata, "quantile25", paste0("qlt25", basedVariable))
    setnames(refdata, "quantile50", paste0("qlt50", basedVariable))
    setnames(refdata, "quantile75", paste0("qlt75", basedVariable))
    setnames(refdata, "quantile90", paste0("qlt90", basedVariable))
    
    detach("package:data.table")
    
    refdata = refdata[order(refdata$CLOSDATE_YEAR), ]
      
    return (as.data.frame(refdata))
  
  }, paste0(headBlank, " ")))
  
}
