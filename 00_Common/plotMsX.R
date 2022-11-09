## The delegated gateway for plotting ALL risk measures comming 
## from the FED and BankScope databases
# Author: Khoa Cai
# --------------------------------------------------------

plotMsX <- function (refdata, dataSource="FED", balance=FALSE, 
                     measures, plotsRecM, 
                     bankIDVariable, timeVariable=NULL, 
                     graphType="heterogeneity", plotType=NULL, 
                     xAxisDimension="time", xWindow=TRUE, headBlank="   ") 
{

  plotTypeFn = NULL
  groupVariable = NULL
  kolor = NULL

  subtitle = ifelse(balance, "(Balance panel data)", "(Unbalance panel data)") 
  caption = sprintf("based on data from the %s", dataSource)
  
  if (graphType == "heterogeneity") {

    suppressPackageStartupMessages(require(gplots))
    plotFn = plotmeans
    detach("package:gplots")
    
    if (xAxisDimension == "bank") { #across the banks
      
      #draw across the banks
      xlab = "Bank"
      x = groupVariable = bankIDVariable
      
    } else { #over the time
      
      xlab = ifelse(dataSource=="FED", "Quarter", "Year")  
      x = timeVariable
      
    }
    
    ptitle = paste(sprintf("Heterogeineity across %ss", xlab), caption, subtitle, sep = "\n")  
    
  }
  
  
  if (graphType == "trends") {
    
    suppressPackageStartupMessages(require(ggplot2))
    
    #Default: draw the measures over the time, i.e., the x axis is time dimension    
    #in case of line graph, draw the measure over the time using the default x axis
    #and each line is represented for one bank with its own color

    ptitle = "Changing of %s for each of banks"
    
    if (plotType == "line") {
      
      plotFn = ggplot
      
      plotTypeFn = geom_line
      
      xlab = ifelse(dataSource == "FED", "Quarter", "Year")  
      
      x = timeVariable
      
      groupVariable = bankIDVariable
      
      kolor = sprintf("factor(%s)", bankIDVariable)
      
    }
    
    if (plotType == "box") {
      
      plotFn = ggplot
      
      plotTypeFn = geom_boxplot
      
      kolor = NULL
      
      if (xAxisDimension == "bank") { #across banks
        
        #draw across the banks
        xlab = "Bank"
        
        x = groupVariable = bankIDVariable
        
      } else { #over time
        
        ptitle = "Changing of %s aggregated all banks"
        
        xlab = ifelse(dataSource=="FED", "Quarter", "Year")  
        
        x = timeVariable
        
      }
      
    }  
    
    detach("package:ggplot2")
    
  }

  riskMeasures = list()
  
  for (measure in measures) {
    
    riskMeasures[[measure]] = c(x=x, y=measure, 
                                title=sprintf(ptitle, measure), 
                                xlab=xlab, ylab=names(which(measures == measure)))

  }
  

  for (riskMeasure in riskMeasures) {
    plotsRecM = plotX(refdata, x=riskMeasure["x"], y=riskMeasure["y"], 
                      plotsRecX=plotsRecM, graphType=graphType, 
                      plotFn=plotFn, plotTypeFn=plotTypeFn, groupVariable=groupVariable, 
                      title=riskMeasure["title"], subtitle=subtitle, 
                      xlab=riskMeasure["xlab"], ylab=riskMeasure["ylab"], caption=caption, 
                      kolor=kolor, xWindow=xWindow, headBlank=headBlank) 
  }
  
  return (plotsRecM)
  
}
