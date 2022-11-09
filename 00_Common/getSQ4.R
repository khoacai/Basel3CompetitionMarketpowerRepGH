
getSQ4 <- function (fileTemplate = "ivStateLevelTemplateQ.csv", 
                    fileSQ4 = "SQ4Data.xlsx", 
                    exportFile=F, fileOut="SQ4", 
                    fileType=".RData", headBlank="") 
{

  message(sprintf(paste0(headBlank, "Get state-level variables: SQ4: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Get state-level variables: SQ4: done!"))))
  
  SQ4 = (processExec(function() {

    suppressPackageStartupMessages(require(readr))
    ivSQ4s = read_csv(fileTemplate)
    detach("package:readr")
    ivSQ4s$YQ = as.numeric(ivSQ4s$YQ)
    
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
    sq4Data = read_excel(fileSQ4)
    detach("package:readxl")
    #lineCds = c(10, 11, 12, 20, 30, 35, 36, 37, 38, 42, 45, 46, 47, 50, 60, 61, 62, 70, 71, 72)
    lineCds = c(10, 50)
    
    suppressPackageStartupMessages(require(sqldf))
    suppressPackageStartupMessages(require(dplyr))
    
    for (lineCd in lineCds) {
      
      message(sprintf(paste0("[SQ4: LineCode: [%d]"), lineCd))
      
      tmpDF = data.frame(STATE=NA, YQ=NA, MACROVAL=NA, stringsAsFactors=F)
      names(tmpDF)[3] = paste0("SQ4LineCd", lineCd)
      tmpDF = tmpDF[c(-1), ]
      
      subSQ4Data = sq4Data %>% filter(LineCode == lineCd)
      
      subSQ4Data[,c("GeoFIPS", "Region", "Table", "LineCode",  
                    "IndustryClassification", "Description")] = NULL
      
      subSQ4Data = setNames(data.frame(names(subSQ4Data)[-1], t(subSQ4Data[ , -1]), stringsAsFactors=F), 
                            c("YQ",subSQ4Data[ , 1][[1]]))
      
      subSQ4Data$YQ = as.numeric(subSQ4Data$YQ)
      
      for(statename in statenames) {
        
        tmpDF2 = data.frame(STATENAME = statename, subSQ4Data[, c("YQ", statename)], stringsAsFactors=F)
        names(tmpDF2)[3] = paste0("SQ4LineCd", lineCd)
        tmpDF2[,3] = as.numeric(tmpDF2[,3])
        
        tmpDF = rbind(tmpDF, tmpDF2)
        
        tmpDF2 = NULL
        gc()
        
      }
      
      ivSQ4s = left_join(ivSQ4s, tmpDF)
      tmpDF = NULL
      gc()
      
    }
    
    detach("package:sqldf")
    
    return (ivSQ4s)
  
  }, paste0(headBlank, " ")))
  
  
  if (exportFile) {
    
    saveAs(SQ4, fileOut, fileType)
    
  } 
  
  return (as.data.frame(SQ4))
  
}
