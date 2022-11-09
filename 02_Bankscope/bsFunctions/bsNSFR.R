#http://www.sciencedirect.com/science/article/pii/S0378426615002344


bsNSFR <- function (refdata, version=1, headBlank="") 
{
  message(sprintf(paste0(headBlank, "NSFR: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "NSFR: done!"))))
  
  refdata$bootUp = TRUE
  
  return (processExec(function() {
  
    suppressPackageStartupMessages(require(dplyr))
    suppressPackageStartupMessages(require(data.table))
  
    if (version == 1) {
      # 1.B Other Earning Assets
      # Preferring the data from DATA2005, DATA11240 sequentially.
      refdata = refdata %>% 
        mutate(OtherEarningAssets = ifelse(!is.na(.$DATA2005), .$DATA2005, .$DATA11240))
      
      # 2. Fixed Assets
      # Preferring the data from DATA2015, DATA11290 sequentially.
      refdata = refdata %>% 
        mutate(FixedAssets = ifelse(!is.na(.$DATA2015), .$DATA2015, .$DATA11290))
      
      # 3.A	Cash and Due From Banks
      # Preferring the data from DATA11270, DATA36310, sequentially.
      refdata = refdata %>% 
        mutate(Cash_and_Due_From_Banks = ifelse(!is.na(.$DATA11270), .$DATA11270, .$DATA36310))
      
      # 3.B Goodwill
      # Preferring the data from DATA11300, DATA18625 sequentially.
      refdata = refdata %>% 
        mutate(Goodwill = ifelse(!is.na(.$DATA11300), .$DATA11300, .$DATA18625))
      
      # 3.C Other Intangibles
      # Preferring the data from DATA11310, DATA18630 sequentially.
      refdata = refdata %>% 
        mutate(Other_Intangibles = ifelse(!is.na(.$DATA11310), .$DATA11310, .$DATA18630))
      
      #	1.B Deposits from Banks
      # Preferring the data from DATA2185, DATA11560 sequentially.
      refdata = refdata %>% 
        mutate(Deposits_from_Banks = ifelse(!is.na(.$DATA2185), .$DATA2185, .$DATA11560))
      
      #	1.C Other Deposits and Short-term Borrowings
      # Preferring the data from DATA2033, DATA11570 sequentially.
      refdata = refdata %>% 
        mutate(Other_Deposits_n_Shortterm_Borrowings = ifelse(!is.na(.$DATA2033), .$DATA2033, .$DATA11570))
      
      #	2.A Derivatives
      # Preferring the data from DATA2036, DATA11630 sequentially.
      refdata = refdata %>% 
        mutate(Derivatives = ifelse(!is.na(.$DATA2036), .$DATA2036, .$DATA11630))
      
      #	2.B Trading Liabilities
      # Preferring the data from DATA2037, DATA11640 sequentially.
      refdata = refdata %>% 
        mutate(Trading_Liabilities = ifelse(!is.na(.$DATA2037), .$DATA2037, .$DATA11640))
      
      #Removing the no longer used data fields (reserve DATA11240, DATA11560 for diversifiation measures)
      refdata[c("DATA2005", "DATA2015", 
             "DATA36310", "DATA18625", "DATA18630", "DATA2185",  
             "DATA2033", "DATA11570", "DATA2036", "DATA2037")] = NULL
  
      ##Changing the name of data fields for easily accessing.
      setnames(refdata, "DATA2000", "Loan") #Loans
      setnames(refdata, "DATA11340", "Other_Assets") #Other Assets
      setnames(refdata, "DATA11520", "Customer_Deposits_Current")
      setnames(refdata, "DATA11530", "Customer_Deposits_Savings")
      setnames(refdata, "DATA11540", "Customer_Deposits_Term")
      setnames(refdata, "DATA2038", "Longterm_Funding") #Long Term Funding
      setnames(refdata, "DATA2040", "Non_Interest_bearing") #Other (Non-Interest bearing)
      setnames(refdata, "DATA2070", "Loan_Loss_Reserves") #Loan Loss Reserves
      setnames(refdata, "DATA2050", "Other_Reserves") #Other Reserves
    }
    
    if (version == 2) {
      ##Changing the name of data fields for easily accessing.
      setnames(refdata, "DATA2000", "Loan") #Loans
      setnames(refdata, "DATA2005", "OtherEarningAssets") #Other Earning Assets
      setnames(refdata, "DATA2015", "FixedAssets") #Fixed Assets
      setnames(refdata, "DATA11270", "Cash_and_Due_From_Banks") #Cash and Due From Banks
      setnames(refdata, "DATA11300", "Goodwill") #Goodwill
      setnames(refdata, "DATA11310", "Other_Intangibles") #Other Intangibles
      setnames(refdata, "DATA11340", "Other_Assets") #Other Assets --> NSFR
      setnames(refdata, "DATA11520", "Customer_Deposits_Current") #Customer Deposits - Current
      setnames(refdata, "DATA11530", "Customer_Deposits_Savings") #Customer Deposits - Savings
      setnames(refdata, "DATA11540", "Customer_Deposits_Term") #Customer Deposits - Term
      setnames(refdata, "DATA2185", "Deposits_from_Banks") #Deposits from Banks
      setnames(refdata, "DATA2033", "Other_Deposits_n_Shortterm_Borrowings") #Other Deposits and Short-term Borrowings
      setnames(refdata, "DATA2036", "Derivatives") #Derivatives
      setnames(refdata, "DATA2037", "Trading_Liabilities") #Trading Liabilities
      setnames(refdata, "DATA2038", "Longterm_Funding") #Long Term Funding
      setnames(refdata, "DATA2040", "Non_Interest_bearing") #Other (Non-Interest bearing)
      setnames(refdata, "DATA2070", "Loan_Loss_Reserves") #Loan Loss Reserves
      setnames(refdata, "DATA2050", "Other_Reserves") #Other Reserves
    }
  
    #Setting up the weights for the balance sheet's items.
    weightsAssets = matrix(c(100, 35, 100, 0, 100, 100, 100))
    weightsLiabilities = matrix(c(85, 70, 70, 0, 0, 0, 0, 100, 100, 100, 100, 100))
  
    #Keep tracking the "Not Avaialble" assets and liabilities items.
    refdata = refdata %>% 
      mutate(assetsNA = paste("", ifelse(is.na(.$Loan),"Loan",""), 
                              ifelse(is.na(.$OtherEarningAssets),"OtherEarningAssets",""), 
                              ifelse(is.na(.$FixedAssets),"FixedAssets",""), 
                              ifelse(is.na(.$Cash_and_Due_From_Banks),"Cash_and_Due_From_Banks",""), 
                              ifelse(is.na(.$Goodwill),"Goodwill",""), 
                              ifelse(is.na(.$Other_Intangibles),"Other_Intangibles",""), 
                              ifelse(is.na(.$Other_Assets),"Other_Assets",""), sep=";"), 
             liabilitiesNA = paste("", ifelse(is.na(.$Customer_Deposits_Current),"Customer_Deposits_Current",""),
                                   ifelse(is.na(.$Customer_Deposits_Savings),"Customer_Deposits_Savings",""),
                                   ifelse(is.na(.$Customer_Deposits_Term),"Customer_Deposits_Term",""), 
                                   ifelse(is.na(.$Deposits_from_Banks),"Deposits_from_Banks",""), 
                                   ifelse(is.na(.$Other_Deposits_n_Shortterm_Borrowings),"Other_Deposits_n_Shortterm_Borrowings",""), 
                                   ifelse(is.na(.$Derivatives),"Derivatives",""), 
                                   ifelse(is.na(.$Trading_Liabilities),"Trading_Liabilities",""), 
                                   ifelse(is.na(.$Longterm_Funding),"Longterm_Funding",""), 
                                   ifelse(is.na(.$Non_Interest_bearing),"Non_Interest_bearing",""), 
                                   ifelse(is.na(.$Loan_Loss_Reserves),"Loan_Loss_Reserves",""), 
                                   ifelse(is.na(.$Other_Reserves),"Other_Reserves",""), 
                                   ifelse(is.na(.$TotalEquity),"Total_Equity",""), sep=";"))
    
    asset_dataIndexes = c("Loan", "OtherEarningAssets", "FixedAssets", "Cash_and_Due_From_Banks", 
                          "Goodwill", "Other_Intangibles", "Other_Assets")
    
    liability_dataIndexes = c("Customer_Deposits_Current", "Customer_Deposits_Savings", 
                              "Customer_Deposits_Term", "Deposits_from_Banks", 
                              "Other_Deposits_n_Shortterm_Borrowings", "Derivatives", 
                              "Trading_Liabilities", "Longterm_Funding", 
                              "Non_Interest_bearing", "Loan_Loss_Reserves", 
                              "Other_Reserves", "TotalEquity")  
    
    #Set NA values to zero
    refdata[ , c(asset_dataIndexes, liability_dataIndexes)][is.na(refdata[ , c(asset_dataIndexes, liability_dataIndexes)])] = 0
    #NSRF computation
    refdata$WL = as.numeric(as.matrix(refdata[ , liability_dataIndexes])%*%weightsLiabilities)
    refdata$WA = as.numeric(as.matrix(refdata[ , asset_dataIndexes])%*%weightsAssets)
    refdata$WLminusWA = refdata$WL - refdata$WA
    refdata$NSFR = refdata$WL / refdata$WA
    asset_dataIndexes = liability_dataIndexes = weightsAssets = weightsLiabilities = NULL
    gc()
  
    detach("package:data.table")
    detach("package:dplyr")
    
    return (as.data.frame(refdata))
  
  }, paste0(headBlank, " ")))
  
}
