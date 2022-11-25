###############
#1.
#This is the main R file for compiling and process data retrieved from Bankscope database.
#This file measures and calculates the variables for regression models and 
#further statistical analyses in Stata files 
#
#Copyright @2019 by Khoa Cai. All rights reserved.
###############

#Compatible with R 4.1.2 and RStudio RStudio 2021.09.0+351 "Ghost Orchid" Release 
#(077589bcad3467ae79f318afe8641a1899a51606, 2021-09-20) for Windows
#Sometimes, updating or upgrading R and/or R Studio would cause problems with dependent packages,
#Some messages like 'this package' was installed prior to R 4.0.0, re-install it,
#library(parentpackage) command should be issued from the console mode to check
#what dependent packages are causing the problems and must be re-installed

##Install the prerequisite packages
# Package names to be checked
p_needed <- c(
  "dplyr",
  "sqldf",
  "ggplot2",
  "gplots",
  "zoo",
  "readr",
  "readxl",
  "data.table",
  "DescTools",
  "timeSeries",
  "lubridate",
  "maps",
  "scales",
  "future"
)

# Install packages not yet installed
installed_packages <- p_needed %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(p_needed[!installed_packages], dependencies = TRUE)
}

# Packages loading
#invisible(lapply(p_needed, library, character.only = TRUE))
invisible(lapply(p_needed, require, character.only = TRUE))

#Path to R code file
rootDirPath = "D:\\02_Bankscope\\"

#Other offset paths (almost no need to change)
setwd(rootDirPath)
path =  "./bsFunctions/"
invisible(lapply(paste0(
  rootDirPath,
  path,
  list.files(
    path = path,
    pattern = ".R",
    recursive = TRUE
  )
), source))
path =  "./../00_Common/"
invisible(lapply(paste0(
  rootDirPath,
  path,
  list.files(
    path = path,
    pattern = ".R",
    recursive = TRUE
  )
), source))


#To fix the Warning message:
#   In result_fetch(res@ptr, n = n) :
#    SQL statements must be issued with dbExecute() or dbSendStatement() instead of dbGetQuery() or dbSendQuery().
#during executing an delete, update, and insert statements using sqldf library
#please see the code for handling this issue @ bsWatchFluctuations.R

# Start the timer
ptm = proc.time()

#The original dataset will be loaded. Default is R data file, but it could be SAS data file.
#The absolute path can be prefixed to it.
BsFinancials_OrgAll_ds = paste0(rootDirPath, "./BS_FinancialsOrgAllDataset.RData")

#The two ways, arcording to two versions, that variables for measures' computations are constructed
#Version 1 or 2
version = 1
#Version1:-Get multiple datacodes from database with the same description for
#         handling missing values of variables.
#Version2:-Get one datacode, the most official data with the lowest code, and
#         let the variables are missing values even their description are the
#         same as other datacodes with available values.

#Set flag for appropriate markets: "US", "BRICS", or "OECD", "ASIAN"
CountryGroup = "US"

#The two ways that the grouped countries' value data are deflated
#Method1: -A.Deflate data basing on grouped countries' CPI annually indices, and then
#         -B.Convert values in local currencies to USD
#Method2: -A.Convert values in local currencies to USD, and then
#         -B.Deflate data basing on US's CPI annually index
CountryGroup_method = 1

#Outputted filenames of the calculation functions
BsFinancials_RiskMeasure_file = "BsFinancials_RiskMeasures_V%d"
BsFinancials_HHI_file = "BsFinancials_HHI_V%d"
BsFinancials_Quantiles_file = "BsFinancials_Quantiles_V%d"
BsFinancials_WatchFluctuation_file = "BsFinancials_WatchFluctuations_Group%d_V%d"
BsFinancials_StdevRMsPerBank_file = "BsFinancials_StdevRMsPerBank_V%d"
BsFinancials_FinalMerged_file = "BsFinancials_FinalMerged_V%d"
BsFinancials_Plots_Measures_pdf = "BsFinancials_PlotsMeasures_V%d.pdf"

#Change the settings appropriately for nonUS cases,
#i.e., CPI index files and outputted file names,
if (CountryGroup == "US") {
  #US's country code (DEFAULT)
  countryCodes = c("US")
  
  CountryGroupNamePrefix = "US_"
  
} else {
  if (CountryGroup == "OECD") {
    #OECD's countries code
    countryCodes = c(
      "AU",
      "AT",
      "BE",
      "CA",
      "CL",
      "CZ",
      "DK",
      "EE",
      "FI",
      "FR",
      "DE",
      "GR",
      "HU",
      "IS",
      "IE",
      "IL",
      "IT",
      "JP",
      "KR",
      "LV",
      "LU",
      "MX",
      "NL",
      "NZ",
      "NO",
      "PL",
      "PT",
      "SK",
      "SI",
      "ES",
      "SE",
      "CH",
      "TR",
      "GB",
      "US"
    )
    
    CountryGroupNamePrefix = "OECD_method%d_"
    
  }
  
  if (CountryGroup == "BRICS") {
    #BRICS's countries code
    countryCodes = c("BR", "RU", "IN", "CN", "ZA")
    
    CountryGroupNamePrefix = "BRICS_method%d_"
    
  }
  
  if (CountryGroup == "ASIAN") {
    #ASIAN's countries code
    
    #Eastern asia:
    #countryCodes = c("CN", "HK", "ID", "JP", "KR", "MY", "PH", "SG", "TW", "TH", "VN")
    
    #Asia-Pacific:
    #countryCodes = c("JP", "IN", "ID", "SG", "LK", "TH", "PK", "PH", "MY", "KR", "HK", "TW", "CN", "VN", "AU", "KZ", "MO", "TO", "NZ", "FJ", "WS", "RU", "US", "KE", "BD")
    
    #Southeast Asian:
    #countryCodes = c("KH", "BD", "CN", "MM", "ID", "LA", "MY", "PH", "TH", "VN")
    
    #emerging and developing asian:
    #EMAs
    #countryCodes = c("CN", "IN", "ID", "MY", "PH", "TH", "VN")
    #DAs
    #countryCodes = c("BD", "KH", "FJ", "LA", "NP", "LK")
    countryCodes = c("CN",
                     "IN",
                     "ID",
                     "MY",
                     "PH",
                     "TH",
                     "VN",
                     "BD",
                     "KH",
                     "FJ",
                     "LA",
                     "NP",
                     "LK")
    
    CountryGroupNamePrefix = "ASIAN_method%d_"
    
  }
  
}

prefix = ifelse(CountryGroup == "US",
                # US case
                "wsdf",
                #deflated only
                ifelse(CountryGroup_method == 2, #OECD or BRICS countries
                       "wsdfex", #(1st)EXCHANGE, and then (2nd)DEFLATE
                       "wsexdf")) #(1st)DEFLATE, and then (2nd)EXCHANGE

BsFinancials_RiskMeasure_file = paste0(rootDirPath,
                                       CountryGroupNamePrefix,
                                       BsFinancials_RiskMeasure_file)

BsFinancials_HHI_file = paste0(rootDirPath,
                               CountryGroupNamePrefix,
                               BsFinancials_HHI_file)

BsFinancials_Quantiles_file = paste0(rootDirPath,
                                     CountryGroupNamePrefix,
                                     BsFinancials_Quantiles_file)

BsFinancials_StdevRMsPerBank_file = paste0(rootDirPath,
                                           CountryGroupNamePrefix,
                                           BsFinancials_StdevRMsPerBank_file)

BsFinancials_FinalMerged_file = paste0(rootDirPath,
                                       CountryGroupNamePrefix,
                                       BsFinancials_FinalMerged_file)

BsFinancials_Plots_Measures_pdf = paste0(rootDirPath,
                                         CountryGroupNamePrefix,
                                         BsFinancials_Plots_Measures_pdf)


####################################################################
######## Drawn the main sample from Bankscope database ########BEGIN
####################################################################

#Load the original dataset and initially filter: Country code,
#statement report's and BHCs' characteristics
financials = bsInitialLoadNFilter(loadFromSAS = F,
                                  file = BsFinancials_OrgAll_ds,
                                  countryCodes = countryCodes)

#Select the mandatory datacodes used for the measures' calculations
financials = bsSubDataset(financials, CountryGroup, version)

######## Drawn the main sample from Bankscope database ########END
##################################################################


####################################################
######## 0. Clean up Data: first stage ########BEGIN
####################################################

bsFinancialsDataset = bsCleanup(financials, CountryGroup, changeDatacodesName =
                                  T)

financials = NULL
#free up memory by doing the garbage collection.
gc()


##The dataset containing the following macroVariables:
# EXCHRATE: Exchange rate, National currency per USD
# CPI: Consumer Price Index (based 2014);
# CurrAcntBalPerGDP: Current account balance, as a percentage of GDP;
# GDPGrowthRate: GDP growth;
# ShortTermInterestRate: short-term interest rate;
# LongTermInterestRate: Long-term interest rate on government bonds;
##UnemploymentRate: Unemployment rate.

#Macro data from OECD.Stat database (http://www.oecd.org/)
macroDataFile = paste0(rootDirPath,
                       "../../01_RCode/02_Bankscope/CountryLevelMacroData.xlsx")

#Map file contains CTRYCODE-LOCATION links between Bankscope database
#and Macro data from OECD.Stat database
countryCodeMapFile = paste0(rootDirPath,
                            "../../01_RCode/02_Bankscope/CountryCodeMap.xls")

#For BRICS case, including US's country code for deflatting data using US's CPI
countryCodesUSEx = countryCodes

if (CountryGroup == "BRICS") {
  countryCodesUSEx = c(countryCodes, c("US"))
}

#EXCHUD: Exchange rate, National currency per USD
#CPI: Consumer price index
#CBGDPR: Current account balance, as a percentage of GDP;
#GDPV_ANNPCT: GDP growth;
#IRS: short-term interest rate;
#IRL: Long-term interest rate on government bonds;
#UNR: Unemployment rate.
macroVariables = setNames(c("EXCHUD", "CPI"), #codes
                          #Comprehensive name of the macro variale
                          c("EXCHRATE", "CPI")) #names

#get EXCHANGE rate info from the main macro data file
macroData = bsGetMacroData(
  countryCodes = countryCodesUSEx,
  macroDataFile = macroDataFile,
  macroVariables = macroVariables,
  countryCodeMapFile = countryCodeMapFile
)

#include the obtained EXCHANGE rate info the dataset
bsFinancialsDataset = bsSetMacroData(bsFinancialsDataset,
                                     macroData,
                                     setNames(c("EXCHUD"), #code
                                              c("EXCHRATE"))) #name

######## 0. Clean up Data: first stage ########END
##################################################


##########################################
######## 1. NSFR Computation ########BEGIN
##########################################

#This calculation also generate a dummy variable titled
#"gt1at2010NSFR": =1: if NSFR is greater than 1 in 2010, otherwise 0.
bsFinancialsDataset = bsNSFR(bsFinancialsDataset, version)

######## 1. NSFR Computation ########END
########################################


#########################################
######## 2. LCR Computation ########BEGIN
#########################################

bsFinancialsDataset = bsLCR(bsFinancialsDataset)

######## 2. LCR Computation ########END
#######################################


#############################################
######## 3. Z-score Computation ########BEGIN
###???:
#http://www.sciencedirect.com/science/article/pii/S0304405X09000816
#http://www.sciencedirect.com/science/article/pii/S0929119916300980
#Using log(Z-score) in the regression analysis to minimize the
#possible outlier effect and reduce the skewness.???
#############################################
message(sprintf("Zscore Computation: started"))

rollingYears = c(3, 5)

########
####START: 3.1. Z-score's numerator
########

bsFinancialsDataset = bsZScoreNumerator(
  refdata = bsFinancialsDataset,
  macroData = macroData,
  CountryGroup = CountryGroup,
  CountryGroup_method = CountryGroup_method
)

####END: 3.1. Z-score's numerator
#########


#########
####START: 3.3. Rolling standard variations: stdev(ROA), stdev(ROE)
#########

bsFinancialsDataset = bsRollingStdevROA_ROE(refdata = bsFinancialsDataset,
                                            rollingYears = rollingYears)

####END: 3.3. Rolling standard variations: stdev(ROA), stdev(ROE)
#########


########
####START: 3.4. Rolling Zscores
########

bsFinancialsDataset = bsRollingZScore(refdata = bsFinancialsDataset,
                                      rollingYears = rollingYears)

####END: 3.4. Rolling Zscores
#########

message(sprintf("Zscore Computation: done!"))
######## 3.Z-score Computation ########END
##########################################


#############################
######## 4. NPL ########BEGIN
#############################

bsFinancialsDataset = bsNPL(bsFinancialsDataset)

######## 4. NPL ########END
###########################


#############################
######## 5. CAR ########BEGIN
#############################

bsFinancialsDataset = bsCAR(bsFinancialsDataset)

######## 5. CAR ########END
###########################


################################################
######## 6. Fat Liquidity Creation ########BEGIN
################################################

bsFinancialsDataset = bsFatLC(bsFinancialsDataset)

##############################################
######## 6. Fat Liquidity Creation ########END
##############################################


###################################################
######## 7. Diversifiacation measures ########BEGIN
###################################################

#Generate the Diversification measures for Assets, Funding, and Income
#Notes: - The diversification measures will be prefixed by "dv

bsFinancialsDataset = bsDiversifyMeasures(bsFinancialsDataset)

################################################
######## 7. Diversification measures ########END
################################################


##################################################
######## 8. Variables for main model ########BEGIN
##################################################

bsFinancialsDataset = bsMainModelVariables(
  refdata = bsFinancialsDataset,
  macroData = macroData,
  CountryGroup = CountryGroup,
  CountryGroup_method = CountryGroup_method
)

################################################
######## 8. Variables for main model ########END
################################################


#######################################################################
######## 9. Generate dummies for ownership concerntration ########BEGIN
########################################################################

#Notes: - the names of ouputed dummies are: ownrshpConcnAPlus,
#         ownrshpConcnA, ownrshpConcnAMinus, ownrshpConcnBPlus,
#         ownrshpConcnB, ownrshpConcnBMinus, ownrshpConcnCPlus,
#         ownrshpConcnC, ownrshpConcnD
bsFinancialsDataset = bsOwnershipConcnDummies(bsFinancialsDataset)

#####################################################################
######## 9. Generate dummies for ownership concerntration ########END
#####################################################################


#######################################
######## 10. Market share ########BEGIN
#######################################

#Basing on (Net Loans, Total Deposits, Profit Before Tax)
variables = c("NetLoans", "TotalAssets",
              "TotalDeposits", "ProfitBeforeTax")

bsFinancialsDataset = getMarketShares(refdata = bsFinancialsDataset,
                                      basedVariables = variables,
                                      dataSource = "BANKSCOPE")

#####################################
######## 10. Market share ########END
#####################################


######################################
######## 11. Winsorizing ########BEGIN
######################################

#Winsorization: Peplace outliers using 99% and 1% percentiles
#Notes: - The outputed winsorized-variables will be prefixed by "ws"
variables4Winsorize = bsGetVariables4Winsorize(CountryGroup,
                                               CountryGroup_method)

bsFinancialsDataset = winsorizeX(
  refdata = bsFinancialsDataset,
  dataSource = "BANKSCOPE",
  winsorizedVariables = variables4Winsorize
)

####################################
######## 11. Winsorizing ########END
####################################


#########################################################################
######## 12. Matching OCC Enforcement Actions (IV for U.S.) ########BEGIN
#########################################################################

#suppressPackageStartupMessages(require(readr))
#EA = read_csv(paste0(rootDirPath, 
#                     "../../01_RCode/02_Bankscope/IVs/IVsOCCEnforcementActionsInstitutionData.csv"))
#detach("package:readr")

#EA = EACleanupA(EA,
#               databaseSource="BANKSCOPE",
#               exportFile=T, fileOut="U.S._EACleanedup",
#               fileType=".csv")
suppressPackageStartupMessages(require(readr))
EA = read_csv("../../01_RCode/02_Bankscope/U.S._EACleanedup.csv") #use the resultant outcome, because it is run once
detach("package:readr")

suppressPackageStartupMessages(require(sqldf))

bsFinancialsDataset$NAME2 = sub("N.A.", "National Association", bsFinancialsDataset$NAME)

bsFinancialsDataset = sqldf(
  paste0(
    "select *",
    "from bsFinancialsDataset a left join EA b  ",
    "on ((b.EATYPE <> 'Bank Civil Money Penalties' ",
    "     and (a.CLOSDATE_YEAR >= b.EASTARTYEAR ",
    "          and (b.EATERMINATIONYEAR is NULL ",
    "               or (b.EATERMINATIONYEAR is not NULL ",
    "                   and a.CLOSDATE_YEAR < b.EATERMINATIONYEAR)))) ",
    "    or (b.EATYPE = 'Bank Civil Money Penalties' ",
    "        and a.CLOSDATE_YEAR = b.EASTARTYEAR)",
    ") ",
    "and a.STATE like b.EASTATE ",
    "and a.CITY like '%'||b.EACITY||'%' ",
    "and a.NAME2 like '%'||b.INSTITUTION||'%'"
  )
)

bsFinancialsDataset$NAME2 = EA = NULL

detach("package:sqldf")

suppressPackageStartupMessages(require(dplyr))

bsFinancialsDataset$ivDSANCTION = bsFinancialsDataset$DSANCTION
bsFinancialsDataset$DSANCTION = NULL

bsFinancialsDataset = bsFinancialsDataset %>%
  mutate(ivDSANCTION = replace(ivDSANCTION, is.na(ivDSANCTION), 0))

detach("package:dplyr")

bsFinancialsDataset = bsFinancialsDataset[!duplicated(bsFinancialsDataset[, c("BVDIDNUM", "CLOSDATE_YEAR", "ivDSANCTION")]), ]

#######################################################################
######## 12. Matching OCC Enforcement Actions (IV for U.S.) ########END
#######################################################################


####################################################################
######## 13. Matching State-level Macro varibles (IVs) ########BEGIN
####################################################################

##State-level GSP, SA4, PCPCE, and PCE of the U.S.
suppressPackageStartupMessages(require(readr))
GSPSA4PCPCE = read_csv("../../01_RCode/02_Bankscope/GSPSA4PCPCE.csv")
detach("package:readr")

GSPSA4PCPCE = GSPSA4PCPCE[, c(
  "STATE",
  "YEAR",
  "gspRealGrowthRate",
  "GSPComponent200Industry1",
  "GSPComponent900Industry1",
  "GSPComponent1000Industry1",
  "gspDeflator",
  "SA4Line20",
  "SA4Line10",
  "SA4Line30",
  "SA4Line7010",
  "SA4Line50",
  "PCPCEComponent2Line1",
  "PCEComponent1Line1"
)]

suppressPackageStartupMessages(require(data.table))

setnames(GSPSA4PCPCE, "gspRealGrowthRate", "ivgsprealgrowth")
setnames(GSPSA4PCPCE, "GSPComponent200Industry1", "ivgsp")
setnames(GSPSA4PCPCE, "GSPComponent900Industry1", "ivgspreal")
setnames(GSPSA4PCPCE, "GSPComponent1000Industry1", "ivgsppercap")
setnames(GSPSA4PCPCE, "SA4Line20", "ivstatepopulation")
setnames(GSPSA4PCPCE, "SA4Line10", "ivpersonalinc")
setnames(GSPSA4PCPCE, "SA4Line30", "ivpcpersonalinc")
setnames(GSPSA4PCPCE, "SA4Line7010", "ivtotalemp")
setnames(GSPSA4PCPCE, "SA4Line50", "ivwagesal")
setnames(GSPSA4PCPCE, "PCPCEComponent2Line1", "ivpcpersonalconsexp")
setnames(GSPSA4PCPCE, "PCEComponent1Line1", "ivpersonalconsexp")

attach(GSPSA4PCPCE)

GSPSA4PCPCE$ivrealdpi = 100 * (ivpersonalinc - ivpersonalconsexp) / gspDeflator
GSPSA4PCPCE$ivrealpi = 100 * ivpersonalinc / gspDeflator
GSPSA4PCPCE$ivrealpcpinc = 100 * ivpcpersonalinc / gspDeflator
GSPSA4PCPCE$ivemprate = ivtotalemp / ivstatepopulation
GSPSA4PCPCE$ivrealwagesal = 100 * ivwagesal / gspDeflator
GSPSA4PCPCE$ivrealpcnsexp = 100 * ivpersonalconsexp / gspDeflator
GSPSA4PCPCE$ivrealpcpcnsexp = 100 * ivpcpersonalconsexp / gspDeflator
GSPSA4PCPCE$ivrealgsppc = 100 * ivgsppercap / gspDeflator

detach(GSPSA4PCPCE)

detach("package:data.table")

suppressPackageStartupMessages(require(dplyr))

GSPSA4PCPCE = GSPSA4PCPCE %>% group_by(STATE) %>%
  mutate(
    ivdpirealgrowth = ivrealdpi / lag(ivrealdpi, 1) - 1,
    ivstpirealgrowth = ivrealpi / lag(ivrealpi, 1) - 1,
    ivstpcpincrealgrowth = ivrealpcpinc / lag(ivrealpcpinc, 1) - 1,
    ivstempraterealgrowth = ivemprate / lag(ivemprate, 1) - 1,
    ivstwagesalrealgrowth = ivrealwagesal / lag(ivrealwagesal, 1) - 1,
    ivstpcnsexprealgrowth = ivrealpcnsexp / lag(ivrealpcnsexp, 1) - 1,
    ivstpcpcnsexprealgrowth = ivrealpcpcnsexp / lag(ivrealpcpcnsexp, 1) - 1,
    ivgsppcrealgrowth = ivrealgsppc / lag(ivrealgsppc, 1) - 1,
    ivstpoprealgrowth = ivstatepopulation / lag(ivstatepopulation, 1) - 1
  )

bsFinancialsDataset = left_join(bsFinancialsDataset,
                                GSPSA4PCPCE,
                                by = c("STATE" = "STATE", "CLOSDATE_YEAR" = "YEAR"))
detach("package:dplyr")

##################################################################
######## 13. Matching State-level Macro varibles (IVs) ########END
##################################################################


#############################################################
######## 12. Merge measures to original dataset ########BEGIN
#############################################################

finalProcessedVariables = bsGetFinalProcessedVariables(variables4Winsorize)

if (CountryGroup == "US") {
  finalProcessedVariables = c(
    finalProcessedVariables,
    "ivDSANCTION",
    "ivgsprealgrowth",
    "ivdpirealgrowth",
    "ivstpirealgrowth",
    "ivstpcpincrealgrowth",
    "ivstempraterealgrowth",
    "ivstwagesalrealgrowth",
    "ivstpcnsexprealgrowth",
    "ivstpcpcnsexprealgrowth",
    "ivgsppcrealgrowth",
    "ivstpoprealgrowth"
  )
  
}

macroVariables = macroVariables[-c(1)]

fileOut = ifelse(
  CountryGroup != "US",
  sprintf(BsFinancials_FinalMerged_file, CountryGroup_method, version),
  sprintf(BsFinancials_FinalMerged_file, version)
)

#EXCHUD: Exchange rate, National currency per USD
#CPI: Consumer price index
#CBGDPR: Current account balance, as a percentage of GDP;
#GDPV_ANNPCT: GDP growth;
#IRS: short-term interest rate;
#IRL: Long-term interest rate on government bonds;
#UNR: Unemployment rate.
macroVariables = setNames(
  c(
    "EXCHUD",
    "CPI",
    "CBGDPR",
    #codes
    "GDP",
    "GDPV_ANNPCT",
    "IRS",
    #codes
    "IRL",
    "UNR",
    "ET_ANNPCT",
    "POP",
    "TEVD",
    "WRT"
  ),
  #codes
  #Comprehensive names of the macro variales
  c(
    "EXCHRATE",
    "CPI",
    "ivCurrAcntBalPerGDP",
    "ivGDP",
    "ivGDPGrowthRate",
    "ivShortTermInterestRate",
    "ivLongTermInterestRate",
    "ivUnemploymentRate",
    "ivTotalEmploymentGr",
    "ivPopulation",
    "ivTotalExpenditure",
    "ivWageRate"
  )
)

#get the selected macro variables from the main macro data file
macroData = bsGetMacroData(
  countryCodes = countryCodesUSEx,
  macroDataFile = macroDataFile,
  macroVariables = macroVariables,
  countryCodeMapFile = countryCodeMapFile
)

bsFinalMerged = bsMerge2Origin(
  refdata = bsFinancialsDataset,
  finalProcessedVariables = finalProcessedVariables,
  CountryGroup = CountryGroup,
  countryCodes = countryCodes,
  macroData = macroData,
  macroVariables = macroVariables,
  originalRDataset = BsFinancials_OrgAll_ds,
  exportFile = F,
  fileOut = fileOut,
  fileType = ".csv"
)

attach(bsFinalMerged)
bsFinalMerged$realgdp = ivGDP / CPI
bsFinalMerged$realexpenditure = ivTotalExpenditure / CPI
detach(bsFinalMerged)

suppressPackageStartupMessages(require(dplyr))

bsFinalMerged = bsFinalMerged %>%
  mutate(
    ivrealgdpgr = realgdp / lag(realgdp, 1) - 1,
    ivrealexpendituregr = realexpenditure / lag(realexpenditure, 1) - 1,
    ivpopulationgr = ivPopulation / lag(ivPopulation, 1) - 1,
    ivwagertgr = ivWageRate / lag(ivWageRate, 1) - 1,
    ivexpendituregr = ivTotalExpenditure / lag(ivTotalExpenditure, 1) - 1
  )

detach("package:dplyr")

###########################################################
######## 12. Merge measures to original dataset ########END
###########################################################



###############
### OPTIONs ###

#############################################################
######## OPT.1. Watch and plot the fluctuations ########BEGIN
#############################################################

fileOut = ifelse(
  CountryGroup != "US",
  sprintf(
    BsFinancials_WatchFluctuation_file,
    CountryGroup_method,
    version
  ),
  sprintf(BsFinancials_WatchFluctuation_file, 1, version)
)

bsWatch = bsWatchFluctuations(
  refdata = bsFinancialsDataset,
  exportFile = F,
  fileOut = fileOut,
  fileType = ".csv"
)

##Plot the fluctuation in number of Banks (NSFR < 1)
par(mar = c(4, 5, 4, 4) + 0.1)

#Counting the number of Banks
plot(
  bsWatch$CLOSDATE_YEAR,
  bsWatch$numOfNSFRLt1,
  pch = 16,
  axes = FALSE,
  ylim = c(0, 10000),
  xlab = "",
  ylab = "",
  type = "b",
  col = "blue",
  main = "Number of banks with NSFR < 1"
)
axis(
  2,
  ylim = c(0, 10000),
  las = 1,
  cex.axis = 0.8,
  tck = -.01
)
mtext(
  "Number of banks",
  side = 2,
  col = "blue",
  line = 3.5,
  cex = 0.9
)
box()
# Allow a second plot on the same graph
par(new = TRUE)

# Percentage of Banks (axis scale on right)
plot(
  bsWatch$CLOSDATE_YEAR,
  bsWatch$numOfNSFRLt1Pct * 100,
  pch = 14,
  axes = FALSE,
  ylim = c(0, 100),
  xlab = "",
  ylab = "",
  type = "b",
  col = "red"
)
mtext(
  "(%)",
  side = 4,
  col = "red",
  line = 2,
  cex = 0.9
)
axis(
  4,
  ylim = c(0, 100),
  las = 1,
  cex.axis = 0.8,
  tck = -.01
)
## The time axis
axis(1,
     bsWatch$CLOSDATE_YEAR,
     cex.axis = 0.8,
     tck = -.01)
mtext(
  "Year",
  side = 1,
  col = "black",
  line = 2.5,
  cex = 0.9
)
# Add Legend
legend(
  "topright",
  legend = c("Counting number", "Percentage of market"),
  text.col = c("blue", "red"),
  pch = c(16, 14),
  col = c("blue", "red"),
  cex = 0.9
)


##Plot the fluctuation of Banks' Total Assets (NSFR < 1)
par(mar = c(4, 5, 4, 4) + 0.1)
# The aggregated total assets
plot(
  bsWatch$CLOSDATE_YEAR,
  bsWatch$totalOfNSFRLt1Asset,
  pch = 16,
  axes = FALSE,
  ylim = c(0, 8000000),
  xlab = "",
  ylab = "",
  type = "b",
  col = "blue",
  main = "Total assets of banks with NSFR < 1"
)
axis(
  2,
  ylim = c(0, 8000000),
  las = 1,
  cex.axis = 0.8,
  tck = -.01
)
mtext(
  "Aggregated total assets",
  side = 2,
  col = "blue",
  line = 3.5,
  cex = 0.9
)
box()
par(new = TRUE)
# Percentage (axis scale on right)
plot(
  bsWatch$CLOSDATE_YEAR,
  bsWatch$totalOfNSFRLt1AssetPct * 100,
  pch = 14,
  axes = FALSE,
  ylim = c(0, 100),
  xlab = "",
  ylab = "",
  type = "b",
  col = "red"
)
mtext(
  "(%)",
  side = 4,
  col = "red",
  line = 2,
  cex = 0.9
)
axis(
  4,
  ylim = c(0, 100),
  las = 1,
  cex.axis = 0.8,
  tck = -.01
)
axis(1,
     bsWatch$CLOSDATE_YEAR,
     cex.axis = 0.8,
     tck = -.01)
mtext(
  "Year",
  side = 1,
  col = "black",
  line = 2.5,
  cex = 0.9
)
legend(
  "topright",
  legend = c("Aggregation", "Percentage of market"),
  text.col = c("blue", "red"),
  pch = c(16, 14),
  col = c("blue", "red"),
  cex = 0.9
)

###########################################################
######## OPT.1. Watch and plot the fluctuations ########END
###########################################################


#########################################################################
######## OPT.2. Quantiles of the Marketshares, ROA, and ROE ########BEGIN
#########################################################################

variables = c(paste0("wsMarketShare", variables),
              c("ROA", "ROE"))

fileOut = ifelse(
  CountryGroup != "US",
  sprintf(BsFinancials_Quantiles_file, CountryGroup_method, version),
  sprintf(BsFinancials_Quantiles_file, version)
)

quantilesDS = getQuantiles(
  refdata = bsFinancialsDataset,
  basedVariables = variables,
  dataSource = "BANKSCOPE",
  exportFile = T,
  fileOut = fileOut,
  fileType = ".csv"
)

#########################################################################
######## OPT.2. Quantiles of the Marketshares, ROA, and ROE ########BEGIN
#########################################################################


##############################################################
######## OPT.3. Herfindahl-Hirschman Index (HHI) ########BEGIN
##############################################################

fileOut = ifelse(
  CountryGroup != "US",
  sprintf(BsFinancials_HHI_file, CountryGroup_method, version),
  sprintf(BsFinancials_HHI_file, version)
)

bsFinancialsHHITA = HHI(
  refdata = bsFinalMerged,
  databaseSource = "BANKSCOPE",
  topN = 0,
  CountryGroup = CountryGroup,
  CountryGroup_method = CountryGroup_method,
  group = "COUNTRY",
  exportFile = F,
  fileOut = fileOut,
  fileType = ".csv"
)


bsFinancialsHHITAState = HHI(
  refdata = bsFinalMerged,
  databaseSource = "BANKSCOPE",
  topN = 0,
  CountryGroup = CountryGroup,
  CountryGroup_method = CountryGroup_method,
  group = "STATE",
  exportFile = F,
  fileOut = fileOut,
  fileType = ".csv"
)

############################################################
######## OPT.3. Herfindahl-Hirschman Index (HHI) ########END
############################################################


#########################################################
######## OPT.4. Bank power at COUNTRY-level ########BEGIN
#########################################################

fileOut = paste0(sprintf(BsFinancials_HHI_file, version), "_U.S.States")

suppressPackageStartupMessages(require(data.table))

bsFinancialsHHIUSTA = HHI(
  refdata = bsFinalMerged,
  databaseSource = "BANKSCOPE",
  by = paste0(prefix, "TotalAssets")
)
bsFinancialsHHIUSTA = bsFinancialsHHIUSTA[, -c(1, 2, 5, 6)]
setnames(bsFinancialsHHIUSTA, "marketSize", "marketSizeTA")
setnames(bsFinancialsHHIUSTA, "HHI", "marketHHITA")

bsFinancialsHHIUSDeposit = HHI(
  refdata = bsFinalMerged,
  databaseSource = "BANKSCOPE",
  by = paste0(prefix, "TotalDeposits")
)
bsFinancialsHHIUSDeposit = bsFinancialsHHIUSDeposit[, -c(1, 2, 5, 6)]
setnames(bsFinancialsHHIUSDeposit, "marketSize", "marketSizeDeposits")
setnames(bsFinancialsHHIUSDeposit, "HHI", "marketHHIDeposits")

suppressPackageStartupMessages(require(dplyr))

bsFinalMerged = left_join(bsFinalMerged, bsFinancialsHHIUSTA)

bsFinalMerged = left_join(bsFinalMerged, bsFinancialsHHIUSDeposit)

temp = bsFinalMerged %>%
  group_by(CLOSDATE_YEAR) %>%
  summarise(marketDepositsTA = sum(wsTotalDepositsTA, na.rm = TRUE))
bsFinalMerged = left_join(bsFinalMerged, temp)

attach(bsFinalMerged)

bsFinalMerged$compTotalDepositsTA = wsTotalDepositsTA / bsFinalMerged$marketDepositsTA
#bsFinalMerged$compTotalDepositsTAHHI = wsTotalDepositsTA*marketHHIDeposits/10000

detach(bsFinalMerged)

detach("package:dplyr")
detach("package:data.table")

#######################################################
######## OPT.4. Bank power at COUNTRY-level ########END
#######################################################


##############################################################
######## OPT.5. State-level Macro varibles (IVs) ########BEGIN
##############################################################

##State-level GSP, SA4, PCPCE, and PCE of the U.S.
#GSPSA4PCPCE = getGSPSA4PCPCE(fileTemplate = paste0(rootDirPath, "ivStateLevelTemplate.csv"),
#                             fileGSP = paste0(rootDirPath, "GSPsData.xlsx"),
#                             fileSA4 = paste0(rootDirPath, "SA4sData.xlsx"),
#                             filePCPCE = paste0(rootDirPath, "PCPCEsData.xlsx"),
#                             filePCE = paste0(rootDirPath, "PCEsData.xlsx"),
#                             exportFile=T, fileOut="GSPSA4PCPCE",
#                             fileType=".csv", headBlank="")

suppressPackageStartupMessages(require(readr))

#GSPSA4PCPCE = read_csv("GSPSA4PCPCE.csv")
#GSPSA4PCPCE$STATE = tolower(GSPSA4PCPCE$STATE)

statesLandArea = read_csv("../../01_RCode/02_Bankscope/usstateslandarea.csv")
statesLandArea$STATE = tolower(statesLandArea$STATE)

detach("package:readr")

suppressPackageStartupMessages(require(dplyr))

bsFinalMerged$STATE2 = tolower(bsFinalMerged$STATE)

bsFinalMerged = left_join(bsFinalMerged, statesLandArea,
                          by = c("STATE2" = "STATE"))

bsFinalMerged$STATE2 = GSPSA4PCPCE = statesPopulation = statesLandArea = NULL

detach("package:dplyr")

############################################################
######## OPT.5. State-level Macro varibles (IVs) ########END
############################################################


#######################################################################
######## OPT.6. Country-level IV variables (Freedom, HHI) ########BEGIN
#######################################################################

#Freedoms data
suppressPackageStartupMessages(require(readr))
FreedomIVs = read_csv("../../01_RCode/02_Bankscope/IVsCountryLevelFreedoms.csv")
detach("package:readr")

suppressPackageStartupMessages(require(dplyr))
#Country-level Freedoms of the U.S.
bsFinalMerged = left_join(bsFinalMerged, FreedomIVs)
detach("package:dplyr")

#Extra macro variables (IVs)
macroVariables = setNames(
  c(
    "CGV_ANNPCT",
    "CPV_ANNPCT",
    "ET_ANNPCT",
    "FDDV_ANNPCT",
    "GDPVTR_ANNPCT",
    "ITV_ANNPCT",
    "KTPV_ANNPCT",
    "TDDV_ANNPCT",
    "TGSVD_ANNPCT",
    "CPIDR",
    "TKPG",
    "ULCDR",
    "NLGQ",
    "ITISK",
    "TEVD",
    "RSCRP",
    "YDRH",
    "WSSS",
    "WRT",
    "WSST",
    "SAVH",
    "PPP",
    "ULC"
  ),
  #Comprehensive names of the macro variales
  c(
    "ivCGV_ANNPCT",
    "ivCPV_ANNPCT",
    "ivET_ANNPCT",
    "ivFDDV_ANNPCT",
    "ivGDPVTR_ANNPCT",
    "ivITV_ANNPCT",
    "ivKTPV_ANNPCT",
    "ivTDDV_ANNPCT",
    "ivTGSVD_ANNPCT",
    "ivCPIDR",
    "ivTKPG",
    "ivULCDR",
    "ivNLGQ",
    "ivITISK",
    "ivTEVD",
    "ivRSCRP",
    "ivYDRH",
    "ivWSSS",
    "ivWRT",
    "ivWSST",
    "ivSAVH",
    "ivPPP",
    "ivULC"
  )
)

#get the selected macro variables from the main macro data file
macroData = bsGetMacroData(
  countryCodes = countryCodesUSEx,
  macroDataFile = macroDataFile,
  macroVariables = macroVariables,
  countryCodeMapFile = countryCodeMapFile
)

bsFinalMerged = bsSetMacroData(bsFinalMerged,
                               macroData,
                               macroVariables)

#####################################################################
######## OPT.6. Country-level IV variables (Freedom, HHI) ########END
#####################################################################

exportCSV(bsFinalMerged, file = "US_BsFinancials_FinalMerged_V1.csv")



###########################################################################
######## OPT.7. Standard deviation for risk measures per bank ########BEGIN
###########################################################################

#The standard deviation of each risk measure for each bank,
#i.e., sd(riskmeasure(i,t)) for every bank(i)
#fileOut = ifelse(CountryGroup != "US",
#                 sprintf(BsFinancials_StdevRMsPerBank_file, CountryGroup_method, version),
#                 sprintf(BsFinancials_StdevRMsPerBank_file, version))
#
#bsFinancialsStdevRMsPerBank = stdevRMsPerBank(refdata=bsFinancialsDataset,
#                                              databaseSource="BANKSCOPE",
#                                              variables=finalProcessedVariables,
#                                              exportFile=F,
#                                              fileOut=fileOut,
#                                              fileType=".csv")

#########################################################################
######## OPT.7. Standard deviation for risk measures per bank ########END
#########################################################################


###############################################
######## OPT.8. Plot the measures ########BEGIN
###############################################

fileOut = ifelse(
  CountryGroup != "US",
  sprintf(
    BsFinancials_Plots_Measures_pdf,
    CountryGroup_method,
    version
  ),
  sprintf(BsFinancials_Plots_Measures_pdf, version)
)

plotMeasures(refdata = bsFinancialsDataset,
             dataSource = "BANKSCOPE",
             pdfOutName = fileOut)

#############################################
######## OPT.8. Plot the measures ########END
#############################################


##############################################
######## OPT.9. Plot the GEO map ########BEGIN
##############################################

suppressPackageStartupMessages(require(readr))
bsFinalGEOLOC = read_csv("../../01_RCode/02_Bankscope/GEOLocations2015.csv")
detach("package:readr")

#Simple map
suppressPackageStartupMessages(require(maps))
suppressPackageStartupMessages(require(dplyr))
map("usa")
map("state")
text(
  x = state.center$x,
  y = state.center$y,
  state.abb,
  cex = 1,
  font = 2
)

######################################
##POINTS heat map
## Use n equally spaced breaks to assign each value to n-1 equal sized bins
ii = cut(
  bsFinalGEOLOC$valNSFR,
  breaks = seq(
    min(bsFinalGEOLOC$valNSFR),
    max(bsFinalGEOLOC$valNSFR),
    len = length(bsFinalGEOLOC$valNSFR)
  ),
  include.lowest = TRUE
)

title(main = "Distribution of NSFR over U.S. states in 2015")
## Use bin indices, ii, to select color from vector of n-1 equally spaced colors
colors = colorRampPalette(c("blue", "darkblue"))(length(bsFinalGEOLOC$valNSFR) -
                                                   1)[ii]
rgbCol = col2rgb(colors)
points(
  bsFinalGEOLOC$lon,
  bsFinalGEOLOC$lat,
  col = rgb(
    red = rgbCol[1,],
    green = rgbCol[2,],
    blue = rgbCol[3,],
    40 * bsFinalGEOLOC$valNSFR,
    max = 255
  ),
  cex = 1,
  pch = 19
)
legend(
  title = "NSFR",
  x = "bottomright",
  legend = c("high", "low"),
  col = c("blue", "#B8D5EC"),
  lwd = 1,
  bty = "n",
  lty = c(0, 0),
  pch = c(19, 19)
)


#########################
#Points TRUE v.s.FALSE
map("usa")
map("state")
text(
  x = state.center$x,
  y = state.center$y,
  state.abb,
  cex = 1,
  font = 2
)
title(main = "Distribution of NSFR over the U.S. states in 2015")
bsFinalGEOLOCNSFRgteq1 = bsFinalGEOLOC %>% filter(valNSFR >= 1)
points(
  bsFinalGEOLOCNSFRgteq1$lon,
  bsFinalGEOLOCNSFRgteq1$lat,
  col = "green",
  cex = 0.6,
  pch = 20
)
bsFinalGEOLOCNSFRlt1 = bsFinalGEOLOC %>% filter(valNSFR < 1)
points(
  bsFinalGEOLOCNSFRlt1$lon,
  bsFinalGEOLOCNSFRlt1$lat,
  col = "red",
  cex = 0.5,
  pch = 18
)
legend(
  title = "NSFR",
  x = "bottomright",
  legend = c(">= 1", "< 1"),
  col = c("green", "red"),
  lwd = 1,
  lty = c(0, 0),
  bty = "n",
  pch = c(20, 18)
)

#Mean NSFR (equal-wweight and weighted by total assets)
suppressPackageStartupMessages(require(dplyr))

bsFinalNSFRStates = bsFinancialsDataset %>%
  group_by(CLOSDATE_YEAR, STATE) %>%
  summarise(
    equalWghtNSFR = mean(wsNSFR, na.rm = T),
    totalAssetsWghtNSFR = weighted.mean(wsNSFR, wsdfTotalAssets *
                                          UNIT, na.rm = T)
  )

bsFinalNSFRStates = bsFinalNSFRStates[order(bsFinalNSFRStates$CLOSDATE_YEAR,
                                            bsFinalNSFRStates$STATE),]

bsFinalNSFRStatesYear = bsFinalNSFRStates %>% filter(CLOSDATE_YEAR == 2015 &
                                                       STATE != "")

suppressPackageStartupMessages(require(maps))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(scales))

############################
#Bar chart
ggplot(aes(x = reorder(STATE, equalWghtNSFR),
           y = equalWghtNSFR),
       data = bsFinalNSFRStatesYear %>%
         mutate(fill = ifelse(equalWghtNSFR < 1,
                              "#6EE543",
                              "#F43E3E"))) +
  geom_col(aes(fill = fill),
           color = "black",
           width = 1,
           alpha = 0.8) +
  coord_flip() +
  labs(x = "States",
       y = "NSFR (equal weighted)",
       title = "NSFR over U.S. States") +
  scale_fill_discrete("", labels = c("<1", ">=1"))


ggplot(aes(x = reorder(STATE, totalAssetsWghtNSFR),
           y = totalAssetsWghtNSFR),
       data = bsFinalNSFRStatesYear %>%
         mutate(fill = ifelse(
           totalAssetsWghtNSFR < 1,
           "#6EE543",
           "#F43E3E"
         ))) +
  geom_col(aes(fill = fill),
           color = "black",
           width = 1,
           alpha = 0.8) +
  coord_flip() +
  labs(x = "States",
       y = "NSFR (weighted by total assets)",
       title = "NSFR over U.S. States") +
  scale_fill_discrete("", labels = c("<1", ">=1"))

############################
#Heat map
bsFinalNSFRStatesYear$CLOSDATE_YEAR = NULL
bsFinalNSFRStatesYear$STATE = tolower(bsFinalNSFRStatesYear$STATE)

states = map_data("state")

states = left_join(states, bsFinalNSFRStatesYear, by = c("region" = "STATE"))

statenames = states %>%
  group_by(region) %>%
  summarise(
    long = mean(range(long)),
    lat = mean(range(lat)),
    group = mean(group),
    equalWghtNSFR = mean(equalWghtNSFR),
    totalAssetsWghtNSFR = mean(totalAssetsWghtNSFR)
  )

ggplot(states, aes(
  x = long,
  y = lat,
  group = group,
  fill = equalWghtNSFR
)) +
  geom_polygon(color = "white", show.legend = T) +
  scale_fill_gradient(
    name = "NSFR",
    low = "#B8D5EC",
    high = "#0A4B7D",
    #low#FAB8D2 high#F91C74
    guide = "colorbar",
    na.value = "black",
    breaks = pretty_breaks(n = 2)
  ) +
  labs(title = "NSFR over the U.S. states in 2015 (equal weighted)",
       x = "Longitude", y = "Latitude") +
  coord_map() +
  #adding States names to the states on the map
  geom_text(data = statenames,
            aes(x = long, y = lat, label = region),
            size = 3)

ggplot(states,
       aes(
         x = long,
         y = lat,
         group = group,
         fill = totalAssetsWghtNSFR
       )) +
  geom_polygon(color = "white", show.legend = T) +
  scale_fill_gradient(
    name = "NSFR",
    low = "#B8D5EC",
    high = "#0A4B7D",
    #low high
    guide = "colorbar",
    na.value = "black",
    breaks = pretty_breaks(n = 5)
  ) +
  labs(title = "NSFR over the U.S. states in 2015 (weighted by total assets)",
       x = "Longitude", y = "Latitude") +
  coord_map() +
  #adding States names to the states on the map
  geom_text(data = statenames,
            aes(x = long, y = lat, label = region),
            size = 3)

########################
##STATES: NSFR: >=1 v.s. <1
statesVec = data.frame(region = map(database = "state", namesonly = TRUE),
                       stringsAsFactors = F)
#Equal weight
statenamesLt1 = statenames %>% filter(equalWghtNSFR < 1)
iiLt1 = cut(
  statenamesLt1$equalWghtNSFR,
  breaks = seq(
    min(statenamesLt1$equalWghtNSFR),
    max(statenamesLt1$equalWghtNSFR),
    len = length(statenamesLt1$equalWghtNSFR)
  ),
  include.lowest = TRUE
)
colorsLt1 = colorRampPalette(c("#F91C74", "#FAB8D2"))(length(statenamesLt1$equalWghtNSFR) -
                                                        1)[iiLt1]
statenamesLt1$colors = colorsLt1

statenamesGteq1 = statenames %>% filter(equalWghtNSFR >= 1)
iiGteq1 = cut(
  statenamesGteq1$equalWghtNSFR,
  breaks = seq(
    min(statenamesGteq1$equalWghtNSFR),
    max(statenamesGteq1$equalWghtNSFR),
    len = length(statenamesGteq1$equalWghtNSFR)
  ),
  include.lowest = TRUE
)
colorsGteq1 = colorRampPalette(c("#B8D5EC", "#0A4B7D"))(length(statenamesGteq1$equalWghtNSFR) -
                                                          1)[iiGteq1]
statenamesGteq1$colors = colorsGteq1

statenamesColors = rbind(statenamesLt1, statenamesGteq1)

suppressPackageStartupMessages(require(sqldf))
statesColors = sqldf(
  paste0(
    "select x.region, y.colors",
    " from statesVec x left join statenamesColors y",
    " on x.region like y.region || '%'"
  )
)
map(database = "state",
    col = statesColors$colors,
    fill = T)
title(main = "Distribution of NSFR over the U.S. states in 2015")
text(
  x = state.center$x,
  y = state.center$y,
  state.abb,
  cex = 1,
  font = 2
)
legend(
  title = "NSFR\n(equal-weight)",
  x = "bottomright",
  legend = c(">= 1", "< 1"),
  col = c("#0A4B7D", "#F91C74"),
  lwd = 1,
  bty = "n",
  lty = c(0, 0),
  pch = c(15, 15)
)

#Weighted by total assets
statenamesLt1 = statenames %>% filter(totalAssetsWghtNSFR < 1)
iiLt1 = cut(
  statenamesLt1$totalAssetsWghtNSFR,
  breaks = seq(
    min(statenamesLt1$totalAssetsWghtNSFR),
    max(statenamesLt1$totalAssetsWghtNSFR),
    len = length(statenamesLt1$totalAssetsWghtNSFR)
  ),
  include.lowest = TRUE
)
colorsLt1 = colorRampPalette(c("#F91C74", "#FAB8D2"))(length(statenamesLt1$totalAssetsWghtNSFR) -
                                                        1)[iiLt1]
statenamesLt1$colors = colorsLt1

statenamesGteq1 = statenames %>% filter(totalAssetsWghtNSFR >= 1)
iiGteq1 = cut(
  statenamesGteq1$totalAssetsWghtNSFR,
  breaks = seq(
    min(statenamesGteq1$totalAssetsWghtNSFR),
    max(statenamesGteq1$totalAssetsWghtNSFR),
    len = length(statenamesGteq1$totalAssetsWghtNSFR)
  ),
  include.lowest = TRUE
)
colorsGteq1 = colorRampPalette(c("#B8D5EC", "#0A4B7D"))(length(statenamesGteq1$totalAssetsWghtNSFR) -
                                                          1)[iiGteq1]
statenamesGteq1$colors = colorsGteq1

statenamesColors = rbind(statenamesLt1, statenamesGteq1)

suppressPackageStartupMessages(require(sqldf))
statesColors = sqldf(
  paste0(
    "select x.region, y.colors",
    " from statesVec x left join statenamesColors y",
    " on x.region like y.region || '%'"
  )
)
map(database = "state",
    col = statesColors$colors,
    fill = T)
title(main = "Distribution of NSFR over the U.S. states in 2015")
text(
  x = state.center$x,
  y = state.center$y,
  state.abb,
  cex = 1,
  font = 2
)
legend(
  title = "NSFR\n(by total assets)",
  x = "bottomright",
  legend = c(">= 1", "< 1"),
  col = c("#0A4B7D", "#F91C74"),
  lwd = 1,
  bty = "n",
  lty = c(0, 0),
  pch = c(15, 15)
)

detach("package:scales")
detach("package:ggplot2")
detach("package:maps")
detach("package:dplyr")
detach("package:sqldf")

#############################################
######## OPT.9. Plot the GEO map ########END
#############################################

bsFinancialsDataset = NULL
gc()


# Stop the timer
proc.time() - ptm
