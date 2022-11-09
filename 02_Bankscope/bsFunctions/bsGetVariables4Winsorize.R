
bsGetVariables4Winsorize <- function (CountryGroup="US", CountryGroup_method=1) 
{
  
  variablesRatio = c("NPL","CAR","NSFR","LCR", #Liquidity
                     #Zscores
                     "RollingStdevROE3years","RollingStdevROE5years", 
                     "RollingStdevROA3years","RollingStdevROA5years", 
                     "RollingZscore3years","RollingZscore5years","FatLC", 
                     #Diversifications
                     "dvADIV", "dvFDIV", "dvIDIV", 
                     #Main models
                     "W1", "W2", "W3", 
                     #Performance
                     "ROA", "ROE", 
                     "PRatio", "BTROA", "TotalExpenseRatio", 
                     "AAC", "Tier1Ratio", "CoreDeposit", "AssetGrowth", 
                     #Marker shares
                     "MarketShareNetLoans", "MarketShareTotalAssets", 
                     "MarketShareTotalDeposits", "MarketShareProfitBeforeTax", 
                     "InterestExpenseRatio", 
                     "DepositRatio", 
                     "GrowthofTotalAssets", 
                     "GrowthofGrossLoans", 
                     "CostToIncomeRatio", 
                     "RecurringEarningPower", 
                     "GrossLoansTA", 
                     "TermDepositsTA", 
                     "TotalDepositsTA")
  
  variablesValue = c("FixedAssets", "TotalAssets", "TotalEquity", 
                     "Y1", "Y2", "Y3", 
                     "TotalCost", "TotalRevenue", "PersonnelExpenses", 
                     "NetIncome", "OperatingProfit", "ProfitBeforeTax", 
                     "Overheads", 
                     "TotalInterestExpense", 
                     "TotalNonInterestExpenses", 
                     "TotalDeposits", 
                     "GrossInterestNDivIncome", 
                     "TotalNonInterestOpIncome", 
                     "PBTplusTOC", 
                     "Customer_Deposits_Term", 
                     "GrossLoans")
  
  if (CountryGroup == "US") { # US case
    
    variablesValue = paste0("df", variablesValue)

  }
  
  if (CountryGroup != "US") { #OECD or BRICS countries
    
    if (CountryGroup_method == 2) {#(1st)EXCHANGE, and then (2nd)DEFLATE
      
      variablesValue = c(paste0("ex", variablesValue), 
                         paste0("dfex", variablesValue))
      
    }else{ #CountryGroup_method == 1: #(1st)DEFLATE, and then (2nd)EXCHANGE
      
      variablesValue = c(paste0("df", variablesValue), 
                         paste0("exdf", variablesValue))
      
    }
    
  }
  
  return (c(variablesRatio, variablesValue))
  
}
