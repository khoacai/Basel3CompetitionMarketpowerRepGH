#https://link.springer.com/book/10.1057%2F9781137533845
#http://web.mit.edu/cbouwman/www/data.html
#http://web.mit.edu/cbouwman/www/downloads/BergerBouwmanBankLiquidityCreationRFSforthc.pdf


bsFatLC <- function (refdata, headBlank="") 
{
  
  message(sprintf(paste0(headBlank, "Fat Liquidity Creation: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Fat Liquidity Creation: done!"))))
  
  refdata$bootUp = TRUE
  
  refdata = (processExec(function() {
  
    #The datacodes of "Illiquid assets", "Liquid liabilities", and "Illiquid OBS" which will be weighted with 0.5
    wgt_plus0.5indexes = c(### ASSETS
	  ##"Iilliquid assets" (weight = 1/2)
      "DATA11060", #Corporate & Commercial Loans
      "DATA11070", #Other loans
      "DATA11220", #Investments in Property
      "DATA11230", #Insurance Assets
      "DATA11280", #Foreclosed Real Estate
      "DATA11290", #Fixed Assets
      ifelse("Goodwill" %in% names(refdata), "Goodwill", "DATA11300"), #Goodwill ~ DATA11300
      ifelse("Other_Intangibles" %in% names(refdata), "Other_Intangibles", "DATA11310"), #Other Intangibles ~ DATA11310
      "DATA11315", #Current Tax Assets
      "DATA11320", #Deferred Tax Assets
      "Other_Assets", #DATA11340
      ### LIABILITIES PLUS EQUITY
      ##Liquid liabilities (weight = 1/2)		
      "Customer_Deposits_Current", #DATA11520
      "Customer_Deposits_Savings", #DATA11530
      "DATA11630", #Derivatives
      "DATA11640", #Trading Liabilities
      ###OFF - BALANCE - SHEET ACTIVITIES
      ##Illiquid OBS (weight = 1/2)		
      "DATA18320", #Acceptances and documentary credits reported off-balance-sheet
      "DATA18325", #Committed Credit Lines
      "DATA18330" #Other Contingent Liabilities
    )
    
    #The datacodes of "Liquid assets", "Illiquid liabilities + equity", and "Liquid OBS" which will be weighted with -0.5
    wgt_minus0.5indexes = c(### ASSETS
      ##Liquid assets (weight = -1/2)
      ifelse("Cash_and_Due_From_Banks" %in% names(refdata), "Cash_and_Due_From_Banks", "DATA11270"), #Cash and due from banks ~ DATA11270
      "DATA11150", #Trading Securities and at FV through income
      "DATA11160", #Derivatives
      "DATA11170", #Available for Sale Securities
      "DATA11190", #At-equity Investments in Associates
      "DATA11200", #Other Securities
      ### LIABILITIES PLUS EQUITY
      ##Illiquid liabilities plus equity (weight = -1/2)		
      "DATA11590", #Senior Debt Maturing after 1 Year
      "DATA11600", #Subordinated Borrowing
      "DATA11610", #Other Funding
      "DATA11680", #Credit impairment reserves
      "DATA11690", #Reserves for Pensions and Other
      "DATA11695", #Current Tax Liabilities
      "DATA11700", #Deferred Tax Liabilities
      "DATA11740", #Other Liabilities
      "DATA11840" #Total Equity
    )
    
    #Set NA values to zero
    refdata[ , c(wgt_plus0.5indexes, wgt_minus0.5indexes)][is.na(refdata[ , c(wgt_plus0.5indexes, wgt_minus0.5indexes)])] = 0
    
    #Apply the weight of 0.5 to "Illiquid assets", "Liquid liabilities", and "Illiquid OBS"
    refdata[ , wgt_plus0.5indexes] = refdata[ , wgt_plus0.5indexes]*rep(0.5, nrow(refdata))
    
    #Apply the weight of -0.5 to "Liquid assets", "Illiquid liabilities + equity", and "Liquid OBS"
    refdata[ , wgt_minus0.5indexes] = refdata[ , wgt_minus0.5indexes]*rep(-0.5, nrow(refdata))
    
    ###FAT LIQUIDITY CREATION
    refdata$FatLC = as.numeric(as.matrix(refdata[, c(wgt_plus0.5indexes, wgt_minus0.5indexes)])%*%as.matrix(rep(1, length(c(wgt_plus0.5indexes, wgt_minus0.5indexes))))) / refdata$TotalAssets
    
    wgt_plus0.5indexes = wgt_minus0.5indexes = NULL
    gc()
  
    return (as.data.frame(refdata))
  
  }, paste0(headBlank, " ")))
  
}
