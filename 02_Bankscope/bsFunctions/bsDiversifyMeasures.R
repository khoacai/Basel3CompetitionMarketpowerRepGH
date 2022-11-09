
bsDiversifyMeasures <- function (refdata, headBlank="") 
{
  message(sprintf(paste0(headBlank, "Diversification measures: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Diversification measures: done!"))))
  
  refdata$bootUp = TRUE
  
  return (processExec(function() {
  
    suppressPackageStartupMessages(require(data.table))
    suppressPackageStartupMessages(require(dplyr))
    
    #######
    ##For asset diversification (ADIV)
    ##
    #1. Bank loans (IBLOAN)
    setnames(refdata, "DATA2180", "dvIBLOAN")
    
    #2. Customer loans (CLOAN)
    tmpDatacodes = c("DATA11040", "DATA11045", "DATA11050", "DATA11060", "DATA11070")
    refdata[ , tmpDatacodes][is.na(refdata[ , tmpDatacodes])] = 0
    refdata = refdata %>% mutate(dvCLOAN = .$DATA11040 + .$DATA11045 + .$DATA11050 + .$DATA11060 + .$DATA11070)
    refdata[ , tmpDatacodes] = NULL
    gc()
    
    #3. Total Securities (TSEC)
    setnames(refdata, "DATA11210", "dvTSEC")
    
    #4. Investment (INVM)
    tmpDatacodes = c("DATA11220", "DATA11230", "DATA11240")
    refdata[ , tmpDatacodes][is.na(refdata[ , tmpDatacodes])] = 0
    refdata = refdata %>% mutate(dvINVM = ifelse(!is.na(.$DATA2007), 
                                            .$DATA2007, 
                                            ifelse(!is.na(.$DATA2009), 
                                                   .$DATA2009, 
                                                   .$DATA11220 + .$DATA11230 + .$DATA11240)))

    #5. Total Earning Assets (TEA): the sum of the five numerators: 1+2+3+4
    #DATA11250, already loaded
    setnames(refdata, "DATA11250", "dvTEA")
    refdata[ , tmpDatacodes] = NULL
    gc()
    
    #Asset diversification (ADIV)
    refdata = refdata %>% mutate(dvADIV = 1 - ((.$dvIBLOAN/.$dvTEA)^2 + 
                                           (.$dvCLOAN/.$dvTEA)^2 + 
                                           (.$dvTSEC/.$dvTEA)^2 + 
                                           (.$dvINVM/.$dvTEA)^2))
    
    #######
    ##For funding diversification (FDIV)
    ##
    #6. Equity (Equity): is already from NSFR
    refdata$dvEQUI = refdata$TotalEquity
    
    #7. Customer deposits (CDEP)
    refdata = refdata %>% mutate(dvCDEP = ifelse(!is.na(.$DATA2031), .$DATA2031, .$DATA11550))
	  refdata$DemandDeposits = refdata$dvCDEP
    refdata$DATA2031 = refdata$DATA11550 = NULL
    gc()
    
    #8. Deposits from Banks (BDEP):
    tmpDatacodes = c("DATA11560", "DATA11565")
    refdata[ , tmpDatacodes][is.na(refdata[ , tmpDatacodes])] = 0
    refdata = refdata %>% mutate(dvBDEP = .$DATA11560 + .$DATA11565)
    refdata[ , tmpDatacodes] = NULL
    gc()
    
    #9. Other interest bearing liabilities (OIBL)
    tmpDatacodes = c("DATA11620", "DATA11630", "DATA11640", "DATA11770")
    refdata[ , tmpDatacodes][is.na(refdata[ , tmpDatacodes])] = 0
    refdata = refdata %>% mutate(dvOIBL = ifelse(!is.na(.$DATA2035), 
                           .$DATA2035, 
                           .$DATA11250 + .$DATA2180 + .$DATA11040 + .$DATA11210))
    refdata[ , tmpDatacodes] = refdata$DATA2035 = NULL
    gc()

    #FUND: the sum of the four numerators
    numerators = c("dvEQUI", "dvCDEP", "dvBDEP", "dvOIBL")
    refdata[ , numerators][is.na(refdata[ , numerators])] = 0
    refdata = refdata %>% mutate(dvFUND = .$dvEQUI + .$dvCDEP + .$dvBDEP + .$dvOIBL)
    
    #Funding diversification (FDIV)
    refdata = refdata %>% mutate(dvFDIV = 1 - ((.$dvEQUI/.$dvFUND)^2 + 
                                           (.$dvCDEP/.$dvFUND)^2 + 
                                           (.$dvBDEP/.$dvFUND)^2 + 
                                           (.$dvOIBL/.$dvFUND)^2))
    
    #######
    ##For income diversification (IDIV)
    #10. Net Interest Income (NII)
    refdata = refdata %>% mutate(dvNII = ifelse(!is.na(.$DATA2080), .$DATA2080, .$DATA10080))
    refdata$DATA2080 = refdata$DATA10080 = NULL
    gc()
    
    #11. Net fee and commisions (NFCI)
    refdata = refdata %>% mutate(dvNFCI = ifelse(!is.na(.$DATA2088), .$DATA2088, .$DATA10120))
    refdata$DATA2088 = refdata$DATA10120 = NULL
    gc()
    
    #14. Total operating income (TOPI)
    setnames(refdata, "DATA2190", "dvTOPI") #the sum of the three numerators=10+11+12
    refdata$DATA2190 = NULL
    gc()
    
    #12. Other income (OI): = 14-10-11
    numerators = c("dvNII", "dvNFCI")
    refdata[ , numerators][is.na(refdata[ , numerators])] = 0
    refdata = refdata %>% mutate(dvOI = .$dvTOPI - .$dvNII - .$dvNFCI)
    
    #Income diversification (IDIV)
    refdata = refdata %>% mutate(dvIDIV = 1 - ((.$dvNII/.$dvTOPI)^2 + 
                                           (.$dvNFCI/.$dvTOPI)^2 + 
                                           (.$dvOI/.$dvTOPI)^2))
    
    detach("package:dplyr")
    detach("package:data.table")
    
    return (as.data.frame(refdata))
  
  }, paste0(headBlank, " ")))
}
