
getSQ4 <- function (fileTemplate = "ivStateLevelTemplateQ.csv", 
                    fileQGSP = "QGSPData.xlsx", 
                    exportFile=F, fileOut="QGSP", 
                    fileType=".RData", headBlank="") 
{

  message(sprintf(paste0(headBlank, "Get state-level variables: quarterly GSP: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Get state-level variables: quarterly GSP: done!"))))
  
  SQ4 = (processExec(function() {

    suppressPackageStartupMessages(require(readr))
    ivQGSPs = read_csv(fileTemplate)
    detach("package:readr")
    ivQGSPs$YQ = as.numeric(ivQGSPs$YQ)
    
    statenames = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
               "Colorado", "Connecticut", "Delaware", "District of Columbia", 
               "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
               "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
               "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
               "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
               "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
               "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
               "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
               "Washington", "West Virginia", "Wisconsin", "Wyoming", 
               "New England", "Mideast", 
               "Great Lakes", "Plains", "Southeast", "Southwest", 
               "Rocky Mountain", "Far West")
    
    suppressPackageStartupMessages(require(readxl))
    qgspData = read_excel(fileQGSP)
    detach("package:readxl")
    components = c(200, 800, 900, 1100)
    
    suppressPackageStartupMessages(require(sqldf))
    suppressPackageStartupMessages(require(dplyr))
    
    for (componentId in components) {
      
      message(sprintf(paste0("[QGSP: IndustryId:ComponentId]: [1:%d]"), componentId))
      
      tmpDF = data.frame(STATE=NA, YQ=NA, MACROVAL=NA, stringsAsFactors=F)
      names(tmpDF)[3] = paste0("QGSPIndustry1Component", componentId)
      tmpDF = tmpDF[c(-1), ]
      
      subQGSPData = sqldf(paste0(" select * from qgspData ", 
                                  " where ComponentId = ", componentId, 
                                  " and IndustryId = 1"))
      
      subQGSPData[,c("GeoFIPS", "Region", 
                      "ComponentId", "ComponentName", "IndustryId", 
                      "IndustryClassification", "Description")] = NULL
      
      subQGSPData = setNames(data.frame(names(subQGSPData)[-1], t(subQGSPData[ , -1]), stringsAsFactors=F), 
                              c("YQ",subQGSPData[ , 1]))
      
      subQGSPData$YQ = as.numeric(subQGSPData$YQ)
      
      for(statename in statenames) {
        
        tmpDF2 = data.frame(STATENAME = statename, subQGSPData[, c("YQ", statename)], stringsAsFactors=F)
        names(tmpDF2)[3] = paste0("QGSPIndustry1Component", componentId)
        tmpDF2[,3] = as.numeric(tmpDF2[,3])
        
        tmpDF = rbind(tmpDF, tmpDF2)
        
        tmpDF2 = NULL
        
      }
      
      ivQGSPs = left_join(ivQGSPs, tmpDF)
      tmpDF = NULL
      gc()
      
    }
    
    detach("package:sqldf")
    
    ivQGSPs = ivQGSPs %>% group_by(STATE) %>% 
      mutate(gspRealGrowthRate = QGSPIndustry1Component900/lag(QGSPIndustry1Component900, 1) - 1, 
             gspDeflator = 100*QGSPIndustry1Component200/QGSPIndustry1Component900)
    
    detach("package:dplyr")  
    
    return (ivQGSPs)
  
  }, paste0(headBlank, " ")))
  
  
  if (exportFile) {
    
    saveAs(SQ4, fileOut, fileType)
    
  } 
  
  return (as.data.frame(SQ4))
  
}
