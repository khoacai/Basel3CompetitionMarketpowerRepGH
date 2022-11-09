
getGSPSA4PCPCE <- function (fileTemplate = "ivStateLevelTemplate.csv", 
                            fileGSP = "GSPsData.xlsx", 
                            fileSA4 = "SA4sData.xlsx", 
                            filePCPCE = "PCPCEsData.xlsx", 
                            filePCE = "PCEsData.xlsx", 
                            exportFile=F, fileOut="GSPSA4PCPCE", 
                            fileType=".RData", headBlank="") 
{

  message(sprintf(paste0(headBlank, "Get state-level variables: GSP, SA4, PCPCE, PCE: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Get state-level variables: GSP, SA4, PCPCE, PCE: done!"))))
  
  GSPSA4PCPCE = (processExec(function() {

    suppressPackageStartupMessages(require(readr))
    ivGSPs = read_csv(fileTemplate)
    detach("package:readr")
    ivGSPs$YEAR = as.numeric(ivGSPs$YEAR)
    
    states = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
               "Colorado", "Connecticut", "Delaware", "District of Columbia", 
               "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
               "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
               "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
               "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
               "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
               "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
               "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
               "Washington", "West Virginia", "Wisconsin", "Wyoming", "New England",
               "Mideast", "Great Lakes", "Plains", "Southeast", "Southwest", 
               "Rocky Mountain", "Far West")
    
    suppressPackageStartupMessages(require(readxl))
    gspData = read_excel(fileGSP)
    detach("package:readxl")
    componentIds = c(2:10)*100
    industryIds = c(1:90)
    
    suppressPackageStartupMessages(require(sqldf))
    suppressPackageStartupMessages(require(dplyr))
    
    for (componentId in componentIds) {
      
      for (industryId in industryIds) {
        
        message(sprintf(paste0("[GSP : ComponentId:IndustryId]: [%d : %d]"), 
                        componentId, industryId))
        
        if (!(componentId == 1000 & (industryId %in% c(2:90)))) {
          
          tmpDF = data.frame(STATE=NA, YEAR=NA, MACROVAL=NA, stringsAsFactors=F)
          names(tmpDF)[3] = paste0("GSPComponent", componentId, "Industry", industryId)
          tmpDF = tmpDF[c(-1),]
          
          subGSPData = sqldf(paste0("select * from gspData ", 
                                    " where ComponentId = ", componentId, 
                                    " and IndustryId = ", industryId))
          
          subGSPData[,c("GeoFIPS", "Region", 
                        "ComponentId", "ComponentName", 
                        "IndustryId", "IndustryClassification", "Description")] = NULL
          
          subGSPData = setNames(data.frame(names(subGSPData)[-1], t(subGSPData[ , -1]), stringsAsFactors=F), 
                                c("YEAR", subGSPData[ , 1]))
          
          subGSPData$YEAR = as.numeric(subGSPData$YEAR)
          
          for(state in states) {
            
            tmpDF2 = data.frame(STATE=state, subGSPData[, c("YEAR", state)], stringsAsFactors=F)
            names(tmpDF2)[3] = paste0("GSPComponent", componentId, "Industry", industryId)
            tmpDF2[,3] = as.numeric(tmpDF2[,3])
            
            tmpDF = rbind(tmpDF, tmpDF2)
            
            tmpDF2 = NULL
            
          }
          
          ivGSPs = left_join(ivGSPs, tmpDF)
          
        }
        
      }
      
    }
    detach("package:sqldf")
    
    #http://thismatter.com/economics/nominal-and-real-gdp.htm
    #GDP Deflator = 100 กั Nominal GDP / Real GDP
    #Real GDP is simply the nominal GDP deflated by the price index:
    #Real GDP = Nominal GDP / (GDP Deflator/100)
    #The GDP deflator is based on a GDP price index and is calculated much like 
    #the Consumer Price Index (CPI), based on data collected by the government. 
    #The GDP index covers many more goods and services than the CPI, including 
    #goods and services bought by businesses. The CPI only covers consumer goods 
    #and services, while the GDP index also covers capital goods, government purchases, 
    #and goods and services traded worldwide.
    
    ivGSPs = ivGSPs %>% group_by(STATE) %>% 
      mutate(gspRealGrowthRate = GSPComponent900Industry1/lag(GSPComponent900Industry1, 1) - 1, 
             gspRealPerCapitalGrowthRate = GSPComponent1000Industry1/lag(GSPComponent1000Industry1, 1) - 1, 
             gspDeflator = 100*GSPComponent200Industry1/GSPComponent900Industry1)
    
    detach("package:dplyr")
    
    exportCSV(ivGSPs, file = "ivGSPs.csv")
    
    
    ##
    suppressPackageStartupMessages(require(readr))
    ivSA4s = read_csv(fileTemplate)
    detach("package:readr")
    ivSA4s$YEAR = as.numeric(ivSA4s$YEAR)
    
    suppressPackageStartupMessages(require(readxl))
    sa4Data = read_excel(fileSA4)
    detach("package:readxl")
    lineIds = c(10:12, 20, 30, 35:38, 42, 45:47, 50, 60:62, 70:72, 7010, 7020, 7040)
    
    suppressPackageStartupMessages(require(sqldf))
    suppressPackageStartupMessages(require(dplyr))
    
    for (lineId in lineIds) {
      
      message(sprintf(paste0("[SA4: LineId]: [%d]"), lineId))
      
      tmpDF = data.frame(STATE=NA, YEAR=NA, MACROVAL=NA, stringsAsFactors=F)
      names(tmpDF)[3] = paste0("SA4Line", lineId)
      tmpDF = tmpDF[c(-1), ]
      
      subSA4Data = sqldf(paste0("select * from sa4Data ", 
                                "where LineCode = ", lineId))
      
      subSA4Data[,c("GeoFIPS", "Region", "Table", 
                    "LineCode", "IndustryClassification", "Description")] = NULL
      
      subSA4Data = setNames(data.frame(names(subSA4Data)[-1], t(subSA4Data[ , -1]), stringsAsFactors=F), 
                            c("YEAR",subSA4Data[ , 1]))
      
      subSA4Data$YEAR = as.numeric(subSA4Data$YEAR)
      
      for(state in states) {
        
        tmpDF2 = data.frame(STATE=state, subSA4Data[, c("YEAR", state)], stringsAsFactors=F)
        names(tmpDF2)[3] = paste0("SA4Line", lineId)
        tmpDF2[,3] = as.numeric(tmpDF2[,3])
        
        tmpDF = rbind(tmpDF, tmpDF2)
        
        tmpDF2 = NULL
        
      }
      
      ivSA4s = left_join(ivSA4s, tmpDF)
      
    }
    detach("package:sqldf")
    
    ivSA4s$PersonalIncomeAvg = ivSA4s$SA4Line10/ivSA4s$SA4Line7010
    
    ivSA4s = ivSA4s %>% group_by(STATE) %>% 
      mutate(PersonalIncomeGrowthRate = SA4Line10/lag(SA4Line10, 1) - 1, 
             PersonalIncomeAvgGrowthRate = PersonalIncomeAvg/lag(PersonalIncomeAvg, 1) - 1)
    
    detach("package:dplyr")  
    
    exportCSV(ivSA4s, file = "ivSA4s.csv")
    
    
    ##
    suppressPackageStartupMessages(require(readr))
    ivPCPCEs = read_csv(fileTemplate)
    detach("package:readr")
    ivPCPCEs$YEAR = as.numeric(ivPCPCEs$YEAR)
    
    suppressPackageStartupMessages(require(readxl))
    pcpceData = read_excel(filePCPCE)
    detach("package:readxl")
    lineIds = c(1:24)
    
    suppressPackageStartupMessages(require(sqldf))
    suppressPackageStartupMessages(require(dplyr))
    
    for (lineId in lineIds) {
      
      message(sprintf(paste0("[PCPCE: ComponentId:LineId]: [2:%d]"), lineId))
      
      tmpDF = data.frame(STATE=NA, YEAR=NA, MACROVAL=NA, stringsAsFactors=F)
      names(tmpDF)[3] = paste0("PCPCEComponent2Line", lineId)
      tmpDF = tmpDF[c(-1), ]
      
      subPCPCEData = sqldf(paste0("select * from pcpceData ", 
                                  "where Line = ", lineId))
      
      subPCPCEData[,c("GeoFIPS", "Region", 
                      "ComponentId", "ComponentName", "Line", 
                      "IndustryClassification", "Description")] = NULL
      
      subPCPCEData = setNames(data.frame(names(subPCPCEData)[-1], t(subPCPCEData[ , -1]), stringsAsFactors=F), 
                              c("YEAR",subPCPCEData[ , 1]))
      
      subPCPCEData$YEAR = as.numeric(subPCPCEData$YEAR)
      
      for(state in states) {
        
        tmpDF2 = data.frame(STATE = state, subPCPCEData[, c("YEAR", state)], stringsAsFactors=F)
        names(tmpDF2)[3] = paste0("PCPCEComponent2Line", lineId)
        tmpDF2[,3] = as.numeric(tmpDF2[,3])
        
        tmpDF = rbind(tmpDF, tmpDF2)
        
        tmpDF2 = NULL
        
      }
      
      ivPCPCEs = left_join(ivPCPCEs, tmpDF)
      
    }
    detach("package:dplyr")  
    detach("package:sqldf")
    
    exportCSV(ivPCPCEs, file = "ivPCPCEs.csv")
    
    ##
    suppressPackageStartupMessages(require(readr))
    ivPCEs = read_csv(fileTemplate)
    detach("package:readr")
    ivPCEs$YEAR = as.numeric(ivPCEs$YEAR)
    
    suppressPackageStartupMessages(require(readxl))
    pceData = read_excel(filePCE)
    detach("package:readxl")
    lineIds = c(1:24)
    
    suppressPackageStartupMessages(require(sqldf))
    suppressPackageStartupMessages(require(dplyr))
    
    for (lineId in lineIds) {
      
      message(sprintf(paste0("[PCE: ComponentId:LineId]: [1:%d]"), lineId))
      
      tmpDF = data.frame(STATE=NA, YEAR=NA, MACROVAL=NA, stringsAsFactors=F)
      names(tmpDF)[3] = paste0("PCEComponent1Line", lineId)
      tmpDF = tmpDF[c(-1), ]
      
      subPCEData = sqldf(paste0("select * from pceData ", 
                                "where Line = ", lineId))
      
      subPCEData[,c("GeoFIPS", "Region", 
                    "ComponentId", "ComponentName", "Line", 
                    "IndustryClassification", "Description")] = NULL
      
      subPCEData = setNames(data.frame(names(subPCEData)[-1], t(subPCEData[ , -1]), stringsAsFactors=F), 
                            c("YEAR",subPCEData[ , 1]))
      
      subPCEData$YEAR = as.numeric(subPCEData$YEAR)
      
      for(state in states) {
        
        tmpDF2 = data.frame(STATE = state, subPCEData[, c("YEAR", state)], stringsAsFactors=F)
        names(tmpDF2)[3] = paste0("PCEComponent1Line", lineId)
        tmpDF2[,3] = as.numeric(tmpDF2[,3])
        
        tmpDF = rbind(tmpDF, tmpDF2)
        
        tmpDF2 = NULL
        
      }
      
      ivPCEs = left_join(ivPCEs, tmpDF)
      
    }
    
    detach("package:sqldf")
    
    exportCSV(ivPCEs, file = "ivPCEs.csv")
    
    GSPSA4PCPCE = inner_join(ivGSPs, ivSA4s, by=c("STATE", "YEAR"))
    GSPSA4PCPCE = inner_join(GSPSA4PCPCE, ivPCPCEs, by=c("STATE", "YEAR"))
    GSPSA4PCPCE = inner_join(GSPSA4PCPCE, ivPCEs, by=c("STATE", "YEAR"))
    
    GSPSA4PCPCE$PersonalIncomeReal = 100*GSPSA4PCPCE$SA4Line10/GSPSA4PCPCE$gspDeflator
    GSPSA4PCPCE$PersonalIncomeAvgReal =  GSPSA4PCPCE$PersonalIncomeReal/GSPSA4PCPCE$SA4Line7010
    
    GSPSA4PCPCE = GSPSA4PCPCE %>% group_by(STATE) %>% 
      mutate(PersonalIncomeRealGrowthRate = PersonalIncomeReal/lag(PersonalIncomeReal, 1) - 1, 
             PersonalIncomeAvgRealGrowthRate = PersonalIncomeAvgReal/lag(PersonalIncomeAvgReal, 1) - 1)

    detach("package:dplyr")  
    
    return (GSPSA4PCPCE)
  
  }, paste0(headBlank, " ")))
  
  
  if (exportFile) {
    
    saveAs(GSPSA4PCPCE, fileOut, fileType)
    
  } 
  
  return (as.data.frame(GSPSA4PCPCE))
  
}
