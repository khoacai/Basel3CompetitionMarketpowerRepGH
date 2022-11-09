
bsMainModelVariables <- function (refdata, macroData, CountryGroup=c("US"), CountryGroup_method=1, headBlank="") 
{

  message(sprintf(paste0(headBlank, "Main model variables: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Main model variables: done!"))))
  
  attach(refdata)
  
  ##Outputs
  #(Y1) Output 1 = Net loans
  refdata$Y1 = NetLoans
  
  #(Y2) Output 2 = OEA (OtherEarningAssets)   OPT
  refdata$Y2 = OtherEarningAssets
  
  #(Y3) Output 3 = Demand deposits (~Customer deposits) OPT
  refdata$Y3 = DemandDeposits
  ##
  
  ##Inputs
  #(W1) Input 1 = Price of Deposits = Interest expenses/Total Deposits 
  refdata$W1 = TotalInterestExpense/TotalDeposits
  
  #(W2) Input 2 = Price of Labour = Personnel expenses/Total assets 
  refdata$W2 = PersonnelExpenses/TotalAssets

  #(W3) Input 3 = Price of Physical capital = (Overheads - Personnel expenses)/Fixed assets 
  refdata$W3 = (Overheads - PersonnelExpenses)/FixedAssets
  ##
  
  ##Performance ratios
  #Total Cost
  refdata$TotalCost = TotalInterestExpense + TotalNonInterestExpenses
  
  #Total Revenue
  refdata$TotalRevenue = GrossInterestNDivIncome + TotalNonInterestOpIncome
  
  #Asset Earning Power (~Before taxed ROA) = Profit Before Tax/Total Assets
  refdata$BTROA = ProfitBeforeTax/TotalAssets
  
  #Total Expense Ratio = Total Cost / Total Assets
  refdata$TotalExpenseRatio = refdata$TotalCost/TotalAssets
  
  #Assets turnover = Total Revenue / Total Assets
  refdata$PRatio = refdata$TotalRevenue/TotalAssets
  
  #Core deposit = Total Customer Deposits / Total Assets
  refdata$CoreDeposit = refdata$Y3/TotalAssets
  
  #Operating Profit (EBIT)
  refdata$OperatingProfit = DATA10220
  
  #AAC = Total Cost / Operating Profit (EBIT)
  refdata$AAC = refdata$TotalCost/refdata$OperatingProfit

  #For adjusted Lerner calculation
  refdata$PBTplusTOC = refdata$ProfitBeforeTax + refdata$TotalCost
  
  ##Temp additional ratios OPT
  refdata$InterestExpenseRatio = TotalInterestExpense/TotalAssets
  refdata$DepositRatio = TotalDeposits/TotalAssets
  refdata$GrowthofTotalAssets = refdata$DATA18190
  refdata$GrowthofGrossLoans = refdata$DATA18195
  refdata$CostToIncomeRatio = refdata$DATA4029
  refdata$RecurringEarningPower = refdata$DATA4030
  
  refdata$GrossLoansTA = refdata$GrossLoans/refdata$TotalAssets
  refdata$TermDepositsTA = refdata$Customer_Deposits_Term/refdata$TotalAssets
  refdata$TotalDepositsTA = refdata$TotalDeposits/refdata$TotalAssets
  
  refdata[,c("DATA18190","DATA18195","DATA4029","DATA4030")] = NULL

  detach(refdata)
  
  commonVariables = c("FixedAssets", "TotalAssets", "TotalEquity", 
                      "Y1", "Y2", "Y3", "Customer_Deposits_Term", "GrossLoans", 
                      "TotalCost", "TotalRevenue", "PersonnelExpenses", 
                      "NetIncome", "OperatingProfit", "ProfitBeforeTax", 
                      #Extra variables
                      "Overheads", 
                      "TotalInterestExpense", 
                      "TotalNonInterestExpenses", 
                      "TotalDeposits", 
                      "GrossInterestNDivIncome", 
                      "TotalNonInterestOpIncome", 
                      "PBTplusTOC")
  
  if (CountryGroup == "US") {
    
    ########################################################
    ######## One steps process for US: DEFLATE ########BEGIN
    ########################################################
    #DEFLATE: generate deflated data with 2010 based-year 
    #*Notes: - Total_Assets, TotalEquity, AverageAssets, and AverageEquity 
    #          are already DEFLATED at Zscore's computation
    #        - Add more variables to deflatedVariables
    #        - The outputed deflated-variables will be prefixed by "df"

    #Deflate data basing on US CPI annually indices
    refdata = bsDeflateX(refdata, 
                         deflatedVariables=commonVariables, 
                         macroData=macroData, 
                         CountryGroup=CountryGroup, 
                         CountryGroup_method=CountryGroup_method, 
                         headBlank=" ")
    
    ########################################################
    ######## One steps process for US: DEFLATE ########BEGIN
    ########################################################
    
  } else {#OECD or BRICS countries
    
    if (CountryGroup_method == 2) {#(1st)EXCHANGE, and then (2nd)DEFLATE
      
      #####################################################################################
      ######## Two steps process (A & B)                                         #####BEGIN
      ######## CountryGroup.2.:                                                  ##########
      ########  (A)EXCHANGE selected datacodes from local currencies to USD, and ##########
      ########  (B)DEFLATE                                                       ##########
      #####################################################################################
      
      #***NOTE:
      #FixedAssets, TotalAssets, TotalEquity, AverageFixedAssets, AverageAssets, 
      #and AverageEquity are already EXCHANGED and DEFLATED at Zscore's computation
      
      #2.(A)::
      #EXCHANGE: Convert from local currencies to USD
      #Notes: - Add more variables to exchangedVariables
      #       - The outputed exchanged-data will be prefixed by "ex"
      refdata = bsExchange(refdata, commonVariables, headBlank=" ")
      
      #2.B::
      #Deflate: generate deflated data with 2014 based-year using US's CPI annually Index
      #Notes: - Add more variables to deflatedVariables
      #       - The outputed deflated-data will be prefixed by "df"
      #
      #Deflate data basing on US's CPI annually index
      refdata = bsDeflateX(refdata, 
                           deflatedVariables=paste0("ex", commonVariables), 
                           macroData=macroData, 
                           CountryGroup=CountryGroup, 
                           CountryGroup_method=CountryGroup_method, 
                           headBlank=" ")
      
      #################################################################################
      ######## CountryGroup.1.:                                              #######END
      ########  CONVERT selected datacodes from local currencies to USD, and ##########
      ########  DEFLATE                                                      ##########
      #################################################################################
      
    } else {#OECD or BRICS countries with method 1: #(1st)DEFLATE, and then (2nd)EXCHANGE
      
      ###################################################################################
      ######## Two steps process (A & B)                                    ########BEGIN
      ######## CountryGroup.1.:                                             #############
      ########  (A)DEFLATE, and                                             ############# 
      ########  (B)EXCHANGE selected variables from local currencies to USD #############
      ###################################################################################
      
      #***NOTE:
      #Total_Assets, TotalEquity, AverageAssets, and AverageEquity are already DEFLATED and 
      #EXCHANGED at Zscore's computation
      
      #1.(A):: 
      #DEFLATE: generate deflated data with 2010 based-year 
      #Notes: - Add more variables to deflatedVariables
      #       - The outputed deflated-variables will be prefixed by "df"
      #
      #Deflate data basing on grouped countries' CPI annually indices
      refdata = bsDeflateX(refdata, 
                           deflatedVariables=commonVariables, 
                           macroData=macroData, 
                           CountryGroup=CountryGroup, 
                           CountryGroup_method=CountryGroup_method, 
                           headBlank=" ")
      
      #1.(B)::
      #EXCHANGE: Convert from local currencies to USD
      #Notes: - Add more variables to exchangedVariables
      #       - The outputed exchanged-variables will be prefixed by "ex"
      refdata = bsExchange(refdata, paste0("df", commonVariables), headBlank=" ")
      
      ###############################################################################
      ######## CountryGroup.1. (A)DEFLATE and                               #########
      ########  (B)EXCHANGE selected variables from local currencies to USD ######END
      ###############################################################################
      
    }
    
  }

  suppressPackageStartupMessages(require(dplyr))
  
  # Asset growth
  refdata = refdata %>% mutate(AssetGrowth = (dfTotalAssets/lag(dfTotalAssets,1)) - 1)
  
  detach("package:dplyr")
  
  return (as.data.frame(refdata))
  
}
