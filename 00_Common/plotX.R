## The worker being responsible for plotting a risk measure
# --------------------------------------------------------
# Author: Khoa Cai
# --------------------------------------------------------

plotX <- function (refdata, x, y, plotsRecX, graphType, plotFn, 
                   plotTypeFn=NULL, groupVariable=NULL, 
                   title=NULL, subtitle=NULL, xlab=NULL, ylab=NULL, caption=NULL, 
                   kolor=NULL, legend=FALSE, xWindow=FALSE, headBlank="   ") 
{
  
  suppressPackageStartupMessages(require(ggplot2))
  
  plotType = ifelse(graphType == "heterogeneity", 
                    "MEANS", 
                    ifelse(identical(plotTypeFn, geom_line), 
                           "LINE TREND", 
                           ifelse(!is.null(groupVariable), "GROUPED BOX", "BOX TREND")))
  
  detach("package:ggplot2")
  
  message(sprintf(paste0(headBlank, " ", "Plot (%s) %s: started"), plotType, ylab))
  
  on.exit(message(sprintf(paste0(headBlank, " ", "Plot (%s) %s: done!"), plotType, ylab))) 
  
  plotFunc = match.fun((match.call())$plotFn)
  
  if (graphType == "heterogeneity") {
    
    plotsRecX[[length(plotsRecX)+1]] = processExec(function() {

      windows()
      plot.new()
      par(mar=c(4.5, 4.5, 4.5, 1.5))
      
      plotFunc(refdata[,y] ~ refdata[,x], data=refdata, 
               main=title, xlab=xlab, ylab=ylab, 
               n.label=F, barwidth=3, cex.axis=0.7, cex.lab=0.9, 
               barcol="blue", col="red", ccol="black")
      
      plotRecX = NULL
      if (!xWindow) {
        
        plotRecX = recordPlot()
        
        graphics.off()
        
      }
      
      return (plotRecX)
      
    }, paste0(headBlank, "  "))
  
  } 
  
  
  if (graphType == "trends") {

      plotTypeFunc = match.fun((match.call())$plotTypeFn)
      
      plotsRecX[[length(plotsRecX)+1]] = processExec(function() {
      
        suppressPackageStartupMessages(require(ggplot2))
        
        p = plotFunc(data=refdata, aes_string(x, y, group=groupVariable, color=kolor))
        p = p + plotTypeFunc(show.legend=legend)
        p = p + theme_bw()
        p = p + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
        p = p + labs(title=title, subtitle=subtitle, x=xlab, y=ylab, caption=caption)
        p = p + theme(axis.text.x=element_text(colour="grey20", size=rel(0.8), angle=0, hjust=0.5, vjust=0, face="plain"))
        p = p + theme(axis.text.y=element_text(colour="grey20", size=rel(0.8), angle=0, hjust=0, vjust=0, face="plain"))
        
        windows()
        plot.new()
        par(mar=c(4.5, 4.5, 4.5, 1.5))
        
        print(p)
      
        plotRecX = NULL
        if (!xWindow) {
          
          plotRecX = recordPlot()
          
          graphics.off()
          
        }
      
        detach("package:ggplot2")
        
        return (plotRecX)
        
      }, paste0(headBlank, "  "))
  }
  
  return (plotsRecX)
  
}
