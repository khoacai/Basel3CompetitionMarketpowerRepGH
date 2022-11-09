## The main gateway of plotting the risk measures comming 
## from the FED and BankScope databases
# --------------------------------------------------------
# Author: Khoa Cai
# --------------------------------------------------------

plotMs <- function (refdata, dataSource="FED", balance=FALSE, 
                    measures=setNames(c("wsNSFR"), #add more measures
                                      c("Net stable funding ratio (NSFR)")), #add more labels
                    graphTypes=setNames(c(T, F), c("heterogeneity","trends")), 
                    xAxisDimensions=setNames(c(F, F), c("time","bank")), 
                    plotTypes=setNames(c(F, T), c("line","box")), 
                    pdfOutName=NULL, xWindow=TRUE, headBlank="")
{
  
  message(sprintf(paste0(headBlank, "Plot the measures (%s panel): start"), 
                  ifelse(balance, "balance", "unbalance")))
  
  on.exit(message(sprintf(paste0(headBlank, "Plot the measures (%s panel): done!"), 
                          ifelse(balance, "balance", "unbalance"))))
  
  refdata = as.data.frame(refdata)
  
  if (!is.null(pdfOutName)) {
    
    xWindow = FALSE
    
    plotsRec <- vector(0, mode='list')
    
  } else {
    
    message(sprintf(paste0(headBlank, " ", "Plot on the windows: started")))
  
  }
  
  #graph's settings in according to FED's database spec.
  if (dataSource == "FED") { 
    
    timeVariable="RSSD9999"
    bankIDVariable="RSSD9001"  
    
    factorsTime = paste0(timeVariable, "yqfct")
    stringsTime = paste0(timeVariable, "yqstr")
    
    suppressPackageStartupMessages(require(zoo))
    
    refdata[, stringsTime] = gsub(" ", 
                               "\n", 
                               as.character(as.yearqtr(as.Date(as.character(refdata[, timeVariable]), "%Y%m%d"), 
                                                       format = "%Y-%m-%d")))
    
    refdata[, factorsTime] = as.factor(refdata[, stringsTime])
    
    detach("package:zoo")
    
  }
  
  #graph's settings in according to BANKSCOPE's database spec.
  if (dataSource == "BANKSCOPE") { 
    
    timeVariable="CLOSDATE_YEAR"
    bankIDVariable="BVDIDNUM"
    
    factorsTime = paste0(timeVariable, "fct")
    stringsTime = paste0(timeVariable, "str")
    
    refdata[, factorsTime] = as.factor(refdata[, timeVariable])
    refdata[, stringsTime] = as.character(refdata[, timeVariable])
    
  }
  
  
  if (graphTypes[c("heterogeneity")]) { #.e., plot means
  
    #####Draw the heterogeneity at a 95% confidence interval around the means
    message(sprintf(paste0(headBlank, " ", "Plot MEANS: started")))
    
    if (xAxisDimensions[c("time")]) { #over the time
    
      plotsRec = plotMsX(refdata, dataSource=dataSource, balance=balance, 
                         plotsRecM=plotsRec, 
                         bankIDVariable=bankIDVariable, timeVariable=stringsTime, 
                         measures=measures, 
                         xWindow=xWindow, headBlank="  ")
      
    }
    
    if (xAxisDimensions[c("bank")]) { #across banks
      
      plotsRec = plotMsX(refdata, dataSource=dataSource, balance=balance, 
                         plotsRecM=plotsRec, 
                         bankIDVariable=bankIDVariable, 
                         measures=measures, 
                         xAxisDimension="bank", 
                         xWindow=xWindow, headBlank="  ")
      
    }
    
    message(sprintf(paste0(headBlank, " ", "Plot MEANS: done!")))
    #####Draw the heterogeneity:END
    
  }
  
  
  if (graphTypes[c("trends")]) { #plot trends
  
    #####Draw the trends
    message(sprintf(paste0(headBlank, " ", "Plot TRENDS over the time: started")))
    
    if (plotTypes[c("line")]) {
    
      message(sprintf(paste0(headBlank, "  ", "Plot LINE trends: started")))
      
      plotsRec = plotMsX(refdata, dataSource=dataSource, balance=balance, 
                         plotsRecM=plotsRec, 
                         bankIDVariable=bankIDVariable, timeVariable=factorsTime, 
                         measures=measures, 
                         graphType="trends", 
                         plotType="line", xWindow=xWindow)
      
      message(sprintf(paste0(headBlank, "  ", "Plot LINE trends: done!")))
      
    }
    
    
    if (plotTypes[c("box")]) {
    
      message(sprintf(paste0(headBlank, "  ", "Plot BOX trend: started")))
      
      plotsRec = plotMsX(refdata, dataSource=dataSource, balance=balance, 
                         plotsRecM=plotsRec, 
                         bankIDVariable=bankIDVariable, timeVariable=factorsTime, 
                         measures=measures, 
                         graphType="trends", 
                         plotType = "box", xWindow=xWindow)
      
      message(sprintf(paste0(headBlank, "  ", "Plot BOX trend: done!")))
      
    }
    
    message(sprintf(paste0(headBlank, " ", "Plot TRENDS over the time: done!")))
    #####Draw the trends:END
  
  }
  
  if (balance & plotTypes[c("box")]) {
    
    message(sprintf(paste0(headBlank, " ", "Plot GROUPED BOXS: started")))
    
    plotsRec = plotMsX(refdata, dataSource=dataSource, balance=balance, 
                       plotsRecM=plotsRec, 
                       bankIDVariable=bankIDVariable, 
                       measures=measures, 
                       graphType="trends", 
                       xAxisDimension="bank", 
                       plotType="box", 
                       xWindow=xWindow, headBlank="  ")
    
    message(sprintf(paste0(headBlank, " ", "Plot GROUPED BOXS: done!")))
    
  }
  
  if (!is.null(pdfOutName)) {
    
    message(sprintf(paste0(headBlank, " ", "Save plots to %s : started"), pdfOutName))
    
    pdf(file=pdfOutName, width=14, height=10, onefile=T)
    
    plotRecIdx = 0
    pb = txtProgressBar(min=0, max=length(plotsRec), style=3)
    
    for (plotRec in plotsRec) {
      
      replayPlot(plotRec)
      
      plotRecIdx = plotRecIdx + 1
      
      setTxtProgressBar(pb, plotRecIdx)
      
    }
    
    dev.off()
    close(pb)
    
    message(sprintf(paste0(headBlank, " ", "Save plots to %s : done!"), pdfOutName))
    
  } else {
    
    message(sprintf(paste0(headBlank, " ", "Plot on the windows: done!")))
    
  }

}
