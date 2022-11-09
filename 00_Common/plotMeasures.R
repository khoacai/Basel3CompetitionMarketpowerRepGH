
plotMeasures <- function (refdata, dataSource="FED", pdfOutName="FedBHCF_PlotsMeasures.pdf") 
{

  #Variables' (y axis) string patterns
  y_strVrRollStdev = "wsRollingStdev%s%dyears"
  y_strVrRollZscore = "wsRollingZscore%dyears"
  
  #Variables' (y axis) string labels
  ylab_RollingStdev = "Rolling %d-year stdev of %s"
  ylab_RollingZscore = "Rolling %d-year Zscore"
  
  #The measures will be plotted
  plottedMeasures = setNames(c("wsNSFR", 
                               #"wsLCR", 
                               #"wsNPL", 
                               #"wsCAR", 
                               #"wsFatLC", 
                               #sprintf(y_strVrRollStdev, "ROE", 3), 
                               #sprintf(y_strVrRollStdev, "ROE", 5), 
                               #sprintf(y_strVrRollStdev, "ROA", 3), 
                               #sprintf(y_strVrRollStdev, "ROA", 5), 
                               #sprintf(y_strVrRollZscore, 3), 
                               sprintf(y_strVrRollZscore, 5)), 
                             #y axis's labels
                             c("Net stable funding ratio (NSFR)", 
                               #"Liquidity Coverage ratio (LCR) (%)", 
                               #"Impaired Loans (NPLs) (%)", 
                               #"Capital Assets Ratio (CAR) (%)", 
                               #"Fat Liquidity Creation (FLC)", 
                               #sprintf(ylab_RollingStdev, 3, "ROE (%)"), 
                               #sprintf(ylab_RollingStdev, 5, "ROE (%)"), 
                               #sprintf(ylab_RollingStdev, 3, "ROA (%)"), 
                               #sprintf(ylab_RollingStdev, 5, "ROA (%)"), 
                               #sprintf(ylab_RollingZscore, 3), 
                               sprintf(ylab_RollingZscore, 5)))
  
  #heterogeneity: the means of measures across banks or over time
  #trends: the changes of measures over time
  graphTypes = setNames(c(T, T), 
                        c("heterogeneity","trends"))
  
  #Setting for x axis, being:
  #   affective: if plotting heterogeneity
  #   ignored: if plotting trends
  xDimensions =setNames(c(T, F), 
                        c("time","bank")) 
  
  #Setting for pattern of shapes, being:
  #   affective: if plotting trends
  #   ignored: if plotting heterogeneity
  plotTypes=setNames(c(F, T), 
                     c("line","box"))
  
  plotMs(refdata=refdata, 
         dataSource=dataSource, 
         measures=plottedMeasures, 
         graphTypes=graphTypes, 
         xAxisDimensions=xDimensions, 
         plotTypes=plotTypes, 
         pdfOutName=pdfOutName)

}
