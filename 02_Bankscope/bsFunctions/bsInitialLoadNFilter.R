
bsInitialLoadNFilter <- function (loadFromSAS=FALSE, file="financialsOrg.RData", 
                                  countryCodes=c("US"), headBlank="")
{
  
  message(sprintf(paste0(headBlank, "Load and initially filter original dataset: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Load and initially filter original dataset: done!"))))
  
  return (processExec(function() {
    
    if (loadFromSAS) {
      
	  require(haven)
      financials = read_sas(file)
      detach("package:haven")
	  
    } else {
      
	  load(file)  
    
	}
    
    ###  https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2191449
    #The variable FORMAT displays the type of data available for each bank:
    #.RD: statement available in the raw data format; 
    #.RF: statement under processing by Fitch ratings;
    #.BS: branch of another bank with financial statement available;
    #.BR: branch with no statement;
    #.DC: no longer existing bank, with statements for previous years;
    #.DD: no longer existing bank, without statements;
    #.NA: banks with no statement; only the name and address are available.
    #RF, BR, DD and NA should be dropped as it does not provide valuable balance
    #sheet observations. Nevertheless, depending on the research question
    #at hand, it may come handy to flag defunct banks for which past information
    #is still available, which are signaled by DC.
    
    #We suggest to work with {C1/C2/U1} in order to get country aggregates
    #or to capture the actual size of the banking market for instance or design
    #concentration measures. Conversely, if you are interested in bank balance
    #sheet sensitivity you may want to keep {U1/U2/C1} in order to keep most
    #banks at the disaggregated (group/affiliates/subsidiaries) level to maximize
    #sample size and avoid the variations you are looking at to be offset or reduced
    #at the group level.
    
    #if you want to maximise the lenght of your time series, you
    #have to keep these consol_code C* and U*.
    #However, for a given bvdidnum and for a given year, you can have duplicates,
    #i.e. you can have several observations with various consolidation
    #codes : Cstar/C2/U2, U2/Ustar/C2.
    attach(financials)
    
	financials = subset(financials, 
                        CTRYCODE %in% countryCodes & 
                          CONSOL %in% c("C1", "C2", "C*", "U1", "U2", "U*") & 
                          !(FORMAT %in% c("RF", "BR", "DD", "NA")) & 
                          SPECIAL %in% c("Bank holdings & Holding companies", "Commercial banks") & 
                          ENTITYTYPE %in% c("GUO", "Single location", "Independent co"))
    
	detach(financials)
    
	return (financials)
  
  }, paste0(headBlank, " ")))
  
}
