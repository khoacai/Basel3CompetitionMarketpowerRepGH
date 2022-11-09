
bsSubDataset <- function(refdata, CountryGroup="US", version=1)
{
  
  message(sprintf("Drawn main sample data: started"))
  
  on.exit(message(sprintf("Drawn main sample data: done!"))) 
  
  attach(refdata)
  
  selectedDatacodes = c()
    
  selectedDatacodes = c("INDEX", "BVDIDNUM", 
                        "NAME", "SWIFT", 
                        "STATE", 
                        "CITY", 
                        "CLOSDATE", "CLOSDATE_YEAR", "CONSOL", "COUNTRY", "CTRYCODE", 
                        "SPECIAL", #Specialisation
                        "ENTITYTYPE", #Entity type
                        "DATA9427", #BvD Independence Indicator
                        "LISTED", #Listed/ unlisted /delisted
                        "SD_TICKER",	#Ticker Symbol
                        "DATA9354", #Number of recorded subsidiaries
                        "DATA9055", #Number of recorded shareholders
                        "DATA38000", #Number of Employees
                        "UNIT", 
                        
                        #Market share basing on Net Loans
                        "DATA11090",#Net Loans (~Loans)
                        
                        "DATA2001",	#Gross Loans

                        "DATA18190", #Growth of Total Assets
                        "DATA18195", #Growth of Gross Loans
                        "DATA4029", #Cost To Income Ratio
                        "DATA4030",	#Recurring Earning Power
                        #DATA10010	Numeric	8	BEST32.		Interest Income on Loans
                        #DATA10020	Numeric	8	BEST32.		Other Interest Income
                        #DATA10120	Numeric	8	BEST32.		Net Fees and Commissions
                        #DATA10130	Numeric	8	BEST32.		Other Operating Income

                        ###MAIN MODEL
                        ##Outputs
                        #(Y1) Output 1 = Net loans 
                        #(Y2) Output 2 = OEA (Other_Earning_Assets) 
                        #(Y3) Output 3 = Demand deposits (~Customer deposits) (already loaded at Diversification)
                        ##
                        
                        ##Inputs
                        #(W1) Input 1 = Price of Deposits = Interest expenses/Total Deposits 
                        "DATA10070", #Total Interest Expense
                        "DATA2030",	#Deposits & Short term funding (Total Deposits)
                        #(W1) Input 1 = Price of Deposits = Interest expenses/Total deposits and money market funding
                        "DATA11580",	#TlDepositsNMoneyMarketNShorttermFunding

                        #(W2) Input 2 = Price of Labour = Personnel expenses/Total assets 
                        "DATA10150", #Personnel Expenses
                        #Total_Assets (already loaded)

                        #(W3) Input 3 = Price of Physical capital = (Overheads - Personnel expenses)/Fixed assets 
                        "DATA2090", #Overheads
                        #"Fixed Assets" (already loaded at NSFR)
                        #(W3) Input 3 = Price of Physical capital = (other operating expenses + Personnel expenses)/Total assets 
                        "DATA10160", #Other Operating Expenses
                        
                        "DATA2105", #Profit before Tax
                        ##
                        
                        ##Total Cost, and Total Revenue
                        #Total Cost = total interest expense + total noninterest expense
                        #Total interest expense: (already loaded)
                        "DATA10170", #Total Non-Interest Expenses
                        
                        #Total Revenue = Gross Interest and Dividend Income 
                        #                 + Total Non-interest operating income
                        "DATA10040", #Gross Interest and Dividend Income
                        "DATA10140", #Total Non-Interest Operating Income
                        ##
                        
                        "DATA10220",	#Operating Profit
                        ###
                        
                        ### Diversification
                        ##For asset diversification (ADIV)
                        #1. Bank loans (IBLOAN)
                        "DATA2180", #Loans and Advances to Banks
                        #2. Customer loans (CLOAN): DATA11040+DATA11045+DATA11050+DATA11060+DATA11070
                        "DATA11040", #Residential Mortgage Loans	
                        "DATA11045", #Other Mortgage Loans	
                        "DATA11050", #Other Consumer/ Retail Loans	
                        "DATA11060", #Corporate & Commercial Loans	
                        "DATA11070", #Other Loans
                        #3. Total Securities (TSEC)
                        "DATA11210", #Total Securities	
                        #4. Investment (INVM): DATA2007+DATA2009
                        "DATA2007", #Derivatives OR 
                        "DATA2009", #Remaining earning assets OR sum of 3:
                        #"DATA11220", #Investments in Property (already loaded at FLC)
                        #"DATA11230", #Insurance Assets (already loaded at FLC)
                        "DATA11240", #Other Earning Assets
                        #5. Total Earning Assets (TEA)
                        "DATA11250", 
                        ##
                        
                        ##For funding diversification (FDIV)
                        #6. Equity (EQUI)
                        #"DATA2055", (already loaded at NSFR)
                        #7. Customer deposits (CDEP)
                        "DATA2031", #Total Customer Deposits OR
                        "DATA11550", #Total Customer Deposits
                        #8. Deposits from Banks (BDEP): DATA11560+DATA11565
                        "DATA11560", #Deposits from Banks
                        "DATA11565", #Repos and Cash Collateral
                        #9. Other interest bearing liabilities (OIBL)
                        "DATA2035", #Other interest bearing liabilities
                        "DATA11620", #Total Long Term Funding	
                        "DATA11630", #Derivatives	
                        "DATA11640", #Trading Liabilities	
                        "DATA11770", #Pref. Shares and Hybrid Capital accounted for as Debt	
                        ##
                        
                        ##For income diversification (IDIV)
                        #10. Net interest income (NII)
                        "DATA2080", #Net Interest Revenue OR
                        "DATA10080", #Net Interest Income
                        #11. Net fee and commisions (NFCI)
                        "DATA2088", #Net Fees and Commissions OR
                        "DATA10120", #Net Fees and Commissions
                        #12. Other income (OI): = 14-10-11
                        #14. Total operating income (TOPI)
                        "DATA2190" #Operating Income (Memo)
                        ##
                        )

  if (version == 1) {
    
    selectedDatacodes = c(selectedDatacodes, 
                          c(### NSFR: The data columns for "STYLIZED BANK BALANCE SHEET" items
                          ##LEFT HAND: ASSETS categories
                          # 1. Total Earning Assets
                          # 1.A Loans
                          "DATA2000", 
                          # 1.B Other Earning Assets
                          "DATA2005", 
                          #"DATA11240", (already loaded at Diversification)
                          # 2. Fixed Assets
                          "DATA2015", 
                          "DATA11290", 
                          # 3. Non Earning Assets
                          # 3.A	Cash and Due From Banks
                          "DATA11270", 
                          "DATA36310", 
                          # 3.B Goodwill
                          "DATA11300", 
                          "DATA18625",	
                          # 3.C Other Intangibles
                          "DATA11310", 
                          "DATA18630",
                          # 3.D Other Assets
                          "DATA11340", 
                          ##
                          
                          ##RIGHT HAND: LIABILITIES categories
                          # 1. Deposits & Short term funding
                          # 1.A.1	Customer Deposits - Current
                          "DATA11520", 
                          #	1.A.2 Customer Deposits - Savings
                          "DATA11530", 
                          #	1.A.3 Customer Deposits - Term
                          "DATA11540", 
                          #	1.B Deposits from Banks
                          "DATA2185", 
                          #"DATA11560", (already loaded at Diversifiation measures)
                          #	1.C Other Deposits and Short-term Borrowings
                          "DATA2033", 
                          "DATA11570", 
                          # 2. Other interest bearing liabilities
                          #	2.A Derivatives
                          "DATA2036", 
                          #"DATA11630", (already loaded at Diversification measures)
                          #	2.B Trading Liabilities
                          "DATA2037", 
                          #"DATA11640", (already loaded at Diversification measures)
                          # 2.C Long term funding
                          "DATA2038", 
                          # 3. Other (Non-Interest bearing)
                          "DATA2040",	
                          # 4. Loan Loss Reserves (Memo) ~ add more NOTE
                          "DATA2070", 
                          # 5. Other Reserves
                          "DATA2050",	
                          # 6. Equity
                          "DATA2055", 
                          ##
                          ###
                          
                          ### Z-Score:
                          "DATA2025", #Total Assets
                          "DATA2115", #Net Income
                          "DATA10285", #Net Income
                          ###
                          
                          ### LCRs:
                          "DATA4035", #Liquid Assets / Dep & ST Funding
                          ###
                          
                          ### NPL ratio:
                          "DATA18200", #Impaired Loans(NPLs)/ Gross Loans
                          ###
                          
                          ### CAR: Capital Adequacy Ratio
                          "DATA2125",	#Total Capital Ratio (CAR)
                          "DATA2130",	#Tier 1 Ratio
                          "DATA2135",	#Total Capital
                          "DATA2140",	#Tier 1 Capital
                          "DATA4007",	#Tier 1 Ratio
                          "DATA4008",	#Total Capital Ratio (CAR)
                          "DATA18150", #Tier 1 Regulatory Capital Ratio	
                          "DATA18155", #Total Regulatory Capital Ratio (CAR)
                          "DATA30660", #Regulatory Tier 1 Capital
                          "DATA30670", #Total Regulatory Capital
                          "DATA30680", #Tier 1 Regulatory Capital Ratio
                          "DATA30690", #Total Regulatory Capital Ratio (CAR)
                          "DATA30700", #Risk Weighted Assets including floor/cap per Basel II	
                          ###
                          
                          #### FLC: FAT LIQUIDITY CREATION
                          #### http://www.palgrave.com/us/book/9781137533821
                          ### ASSETS
                          ##Illiquid assets (weight = 1/2)		
                          #"DATA11060", #Corporate & Commercial Loans (already loaded at Diversification measures)
                          #"DATA11070", #Other loans (already loaded at Diversification measures)
                          "DATA11220", #Investments in Property
                          "DATA11230", #Insurance Assets
                          "DATA11280", #Foreclosed Real Estate
                          #DATA11290, #Fixed Assets (already loaded at NSFR)
                          #DATA11300, #Goodwill (already loaded at NSFR)
                          #DATA11310, #Other Intangibles (already loaded at NSFR)
                          "DATA11315", #Current Tax Assets
                          "DATA11320", #Deferred Tax Assets
                          #DATA11340, #Other Assets (already loaded at NSFR)
                          ##
                          ##Liquid assets (weight = -1/2)		
                          #DATA11270, #Cash and due from banks (already loaded at NSFR)
                          "DATA11150", #Trading Securities and at FV through income
                          "DATA11160", #Derivatives
                          "DATA11170", #Available for Sale Securities
                          "DATA11190", #At-equity Investments in Associates
                          "DATA11200", #Other Securities
                          ##
                          ###
                          
                          ### LIABILITIES PLUS EQUITY
                          ##Liquid liabilities (weight = 1/2)		
                          #DATA11520, #Customer Deposits - Current (already loaded at NSFR)
                          #DATA11530, #Customer Deposits - Savings (already loaded at NSFR)
                          #DATA11630, #Derivatives (already loaded at NSFR)
                          #DATA11640, #Trading Liabilities (already loaded at NSFR)
                          ##
                          ##Illiquid liabilities plus equity (weight = -1/2)		
                          "DATA11590", #Senior Debt Maturing after 1 Year
                          "DATA11600", #Subordinated Borrowing
                          "DATA11610", #Other Funding
                          "DATA11680", #Credit impairment reserves
                          "DATA11690", #Reserves for Pensions and Other
                          "DATA11695", #Current Tax Liabilities
                          "DATA11700", #Deferred Tax Liabilities
                          "DATA11740", #Other Liabilities
                          "DATA11840", #Total Equity
                          ##
                          ###
                          
                          ###OFF - BALANCE - SHEET ACTIVITIES
                          ##Illiquid OBS (weight = 1/2)		
                          "DATA18320", #Acceptances and documentary credits reported off-balance-sheet
                          "DATA18325", #Committed Credit Lines
                          "DATA18330" #Other Contingent Liabilities
                          ##
                          ###
                          ))
    
  }
  
  if (version == 2) {
    
    selectedDatacodes = c(selectedDatacodes, 
                          c(### NSFR: The data columns for "STYLIZED BANK BALANCE SHEET" items
                          ##LEFT HAND: ASSETS categories
                          # 1. Total Earning Assets
                          # 1.A Loans
                          "DATA2000", 
                          # 1.B Other Earning Assets
                          "DATA2005", 
                          # 2. Fixed Assets
                          "DATA2015", 
                          # 3. Non Earning Assets
                          # 3.A	Cash and Due From Banks
                          "DATA11270", 
                          # 3.B Goodwill
                          "DATA11300", 
                          # 3.C Other Intangibles
                          "DATA11310", 
                          # 3.D Other Assets
                          "DATA11340", 
                          ##
                          
                          ##RIGHT HAND: LIABILITIES categories
                          # 1. Deposits & Short term funding
                          # 1.A.1	Customer Deposits - Current
                          "DATA11520", 
                          #	1.A.2 Customer Deposits - Savings
                          "DATA11530", 
                          #	1.A.3 Customer Deposits - Term
                          "DATA11540", 
                          #	1.B Deposits from Banks
                          "DATA2185", 
                          #	1.C Other Deposits and Short-term Borrowings
                          "DATA2033", 
                          # 2. Other interest bearing liabilities
                          #	2.A Derivatives
                          "DATA2036", 
                          #	2.B Trading Liabilities
                          "DATA2037", 
                          # 2.C Long term funding
                          "DATA2038", 
                          # 3. Other (Non-Interest bearing)
                          "DATA2040",	
                          # 4. Loan Loss Reserves (Memo) ~ add more NOTE
                          "DATA2070", 
                          # 5. Other Reserves
                          "DATA2050",	
                          # 6. Equity
                          "DATA2055", 
                          ##
                          ###
                          
                          ### Z-Score:
                          "DATA2025", #Total Assets
                          "DATA2115", #Net Income
                          "DATA10285", #Net Income
                          ###
                          
                          ### LCRs:
                          "DATA4035", #Liquid Assets / Dep & ST Funding
                          ###
                          
                          ### NPL ratio:
                          "DATA18200", #Impaired Loans(NPLs)/ Gross Loans
                          ###
                          
                          ### CAR: Capital Adequacy Ratio
                          "DATA2125",	#Total Capital Ratio (CAR)
                          "DATA2130",	#Tier 1 Ratio
                          "DATA2135",	#Total Capital
                          "DATA2140",	#Tier 1 Capital
                          "DATA4007",	#Tier 1 Ratio
                          "DATA4008",	#Total Capital Ratio (CAR)
                          "DATA18150", #Tier 1 Regulatory Capital Ratio	
                          "DATA18155", #Total Regulatory Capital Ratio (CAR)
                          "DATA30660", #Regulatory Tier 1 Capital
                          "DATA30670", #Total Regulatory Capital
                          "DATA30680", #Tier 1 Regulatory Capital Ratio
                          "DATA30690", #Total Regulatory Capital Ratio (CAR)
                          "DATA30700", #Risk Weighted Assets including floor/cap per Basel II	
                          ###
                          
                          #### FLC: FAT LIQUIDITY CREATION
                          #### http://www.palgrave.com/us/book/9781137533821
                          ### ASSETS
                          ##Illiquid assets (weight = 1/2)		
                          #"DATA11060", #Corporate & Commercial Loans (already loaded at Diversification measures)
                          #"DATA11070", #Other loans (already loaded at Diversification measures)
                          "DATA11220", #Investments in Property
                          "DATA11230", #Insurance Assets
                          "DATA11280", #Foreclosed Real Estate
                          "DATA11290", #Fixed Assets
                          #DATA11300, #Goodwill (already loaded at NSFR)
                          #DATA11310, #Other Intangibles (already loaded at NSFR)
                          "DATA11315", #Current Tax Assets
                          "DATA11320", #Deferred Tax Assets
                          #DATA11340, #Other Assets (already loaded at NSFR)
                          ##
                          ##Liquid assets (weight = -1/2)		
                          #DATA11270, #Cash and due from banks (already loaded at NSFR)
                          "DATA11150", #Trading Securities and at FV through income
                          "DATA11160", #Derivatives
                          "DATA11170", #Available for Sale Securities
                          "DATA11190", #At-equity Investments in Associates
                          "DATA11200", #Other Securities
                          ##
                          ###
                          
                          ### LIABILITIES PLUS EQUITY
                          ##Liquid liabilities (weight = 1/2)		
                          #DATA11520, #Customer Deposits - Current (already loaded at NSFR)
                          #DATA11530, #Customer Deposits - Savings (already loaded at NSFR)
                          #"DATA11630", #Derivatives (already loaded at Diversification measures)
                          #"DATA11640", #Trading Liabilities (already loaded at Diversification measures)
                          ##
                          ##Illiquid liabilities plus equity (weight = -1/2)		
                          "DATA11590", #Senior Debt Maturing after 1 Year
                          "DATA11600", #Subordinated Borrowing
                          "DATA11610", #Other Funding
                          "DATA11680", #Credit impairment reserves
                          "DATA11690", #Reserves for Pensions and Other
                          "DATA11695", #Current Tax Liabilities
                          "DATA11700", #Deferred Tax Liabilities
                          "DATA11740", #Other Liabilities
                          "DATA11840", #Total Equity
                          ##
                          ###
                          
                          ###OFF - BALANCE - SHEET ACTIVITIES
                          ##Illiquid OBS (weight = 1/2)		
                          "DATA18320", #Acceptances and documentary credits reported off-balance-sheet
                          "DATA18325", #Committed Credit Lines
                          "DATA18330" #Other Contingent Liabilities
                          ##
                          ###
                          ))
    
  }
  
  refdata = subset(refdata, TRUE, select=selectedDatacodes)

  detach(refdata)
  
  return (as.data.frame(refdata))
  
}