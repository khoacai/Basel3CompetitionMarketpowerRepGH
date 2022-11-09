


bsDeflateX <-
  function (refdata,
            deflatedVariables,
            macroData,
            CountryGroup = c("US"),
            CountryGroup_method = 1,
            headBlank = "")
  {
    message(sprintf(paste0(headBlank, "Deflate: started")))
    
    on.exit(message(sprintf(paste0(
      headBlank, "Deflate: done!"
    ))))
    
    refdata$CPI = NULL
    
    if (CountryGroup == "US" | CountryGroup_method == 2) {
      #Dealing with USD only
      macroData = subset(macroData, CTRYCODE == "US")
      
    }#Otherwise working with multiple currencies in OECD or BRICS countries
    
    # load CPI data
    CPIData = subset(macroData, MacroVariable == "CPI")
    
    suppressPackageStartupMessages(require(data.table))
    
    setnames(CPIData, "MacroVariableVal", "CPI")
    
    detach("package:data.table")
    
    #Clean up the existing CPI information
    refdata$CPI = refdata$CPIUS = NULL
    
    #Clean up the unused information
    CPIData[, c("LOCATION", "MacroVariableCode", "MacroVariable")] = NULL
    
    suppressPackageStartupMessages(require(dplyr))
    
    # Merge the CPI data to the main dataset by joinning the keys
    if (CountryGroup == "US" | CountryGroup_method == 2) {
      #Drop the CTRYCODE since dealing with USD only
      CPIData$CTRYCODE = NULL
      refdata = left_join(refdata, CPIData, by = c("CLOSDATE_YEAR"))
      
    } else {
      #Otherwise working with multiple currencies in OECD or BRICS countries
      
      # Keep CTRYCODE as a key of joinning since dealing with many countries
      refdata = left_join(refdata, CPIData, by = c("CTRYCODE", "CLOSDATE_YEAR"))
      
    }
    
    gc()
    
    detach("package:dplyr")
    
    # initialize the deflated data
    refdata = cbind(refdata, setNames(refdata[deflatedVariables], paste0("dftmp", deflatedVariables)))
    
    suppressPackageStartupMessages(require(dplyr))
    
    # deflate
    #refdata = refdata %>% mutate_at(vars(starts_with("dftmp")), funs(deflate, .args = list(.$CPI)))
    #The use of funs() is deprecated from newer versions of dplyr
    #funs() is soft deprecated as of dplyr 0.8.0
    #* Please use a list of either functions or lambdas:*
    #   Simple named list:*
    #   list(mean = mean, median = median) *
    #   Auto named with tibble::lst():*
    #   tibble::lst(mean, median) *
    #   Using lambdas *
    #   list( ~ mean(., trim = .2), ~ median(., na.rm = TRUE)) *
    refdata = refdata %>% mutate_at(vars(starts_with("dftmp")), list(~deflate(., CPI)))
    
    detach("package:dplyr")
    
    suppressPackageStartupMessages(require(data.table))
    
    setnames(refdata,
             paste0("dftmp", deflatedVariables),
             paste0("df", deflatedVariables))
    
    setnames(refdata, "CPI", "CPIUS")
    
    detach("package:data.table")
    
    return (as.data.frame(refdata))
    
  }
