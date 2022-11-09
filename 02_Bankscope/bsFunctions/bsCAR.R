
bsCAR <- function (refdata, version=1, headBlank="") 
{
  
  message(sprintf(paste0(headBlank, "CAR: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "CAR: done!"))))
  
  refdata$bootUp = TRUE
  
  return (processExec(function() {
  
    suppressPackageStartupMessages(require(dplyr))
  
    refdata$CAR = NA

	  #CAR
    refdata = refdata %>% 
      mutate(CAR = ifelse(!is.na(.$DATA2125), 
                          .$DATA2125, 
                          ifelse(!is.na(.$DATA4008), 
                                 .$DATA4008, 
                                 ifelse(!is.na(.$DATA18155), 
                                        .$DATA18155, 
                                        .$DATA30690))))
    
    #Tier 1 ratio
    refdata = refdata %>% 
      mutate(Tier1Ratio = ifelse(!is.na(.$DATA2130), 
                                 .$DATA2130, 
                                 ifelse(!is.na(.$DATA4007), 
                                        .$DATA4007, 
                                        ifelse(!is.na(.$DATA18150), 
                                               .$DATA18150, 
                                               .$DATA30680))))

	  #Look up other equivalent datacodes
    if (version == 1) {
      
      #CAR
      refdata = refdata %>% 
        mutate(CAR = ifelse(!is.na(.$CAR), 
                            .$CAR, 
                            ifelse(!is.na(.$DATA2130) & !is.na(.$DATA2135) & !is.na(.$DATA2140), 
                                   .$DATA2135/(.$DATA2140/.$DATA2130), 
                                   ifelse(!is.na(.$DATA4007) & !is.na(.$DATA2135) & !is.na(.$DATA2140), 
                                          .$DATA2135/(.$DATA2140/.$DATA4007), 
                                          ifelse(!is.na(.$DATA18150) & !is.na(.$DATA2135) & !is.na(.$DATA2140), 
                                                 .$DATA2135/(.$DATA2140/.$DATA18150), 
                                                 ifelse(!is.na(.$DATA30660) & !is.na(.$DATA30670) & !is.na(.$DATA30680), 
                                                 .$DATA30670/(.$DATA30660/.$DATA30680), 
                                                 ifelse(!is.na(.$DATA2135) & !is.na(.$DATA30700), 
                                                        100*.$DATA2135/.$DATA30700, 
                                                        100*.$DATA30670/.$DATA30700)))))))
      
      #Tier 1 Ratio
      refdata = refdata %>% 
        mutate(Tier1Ratio = ifelse(!is.na(.$Tier1Ratio), 
                                   .$Tier1Ratio, 
                            ifelse(!is.na(.$DATA2125) & !is.na(.$DATA2135) & !is.na(.$DATA2140), 
                                   .$DATA2140/(.$DATA2135/.$DATA2125), 
                                   ifelse(!is.na(.$DATA4008) & !is.na(.$DATA2135) & !is.na(.$DATA2140), 
                                          .$DATA2140/(.$DATA2135/.$DATA4008), 
                                          ifelse(!is.na(.$DATA18155) & !is.na(.$DATA2135) & !is.na(.$DATA2140), 
                                                 .$DATA2140/(.$DATA2135/.$DATA18155), 
                                                 ifelse(!is.na(.$DATA30690) & !is.na(.$DATA30670) & !is.na(.$DATA30660), 
                                                        .$DATA30660/(.$DATA30670/.$DATA30690), 
                                                        ifelse(!is.na(.$DATA2135) & !is.na(.$DATA30700), 
                                                               100*.$DATA2140/.$DATA30700, 
                                                               100*.$DATA30660/.$DATA30700)))))))
      
    
    }
    
    detach("package:dplyr")
    
    return (as.data.frame(refdata))
  
  }, paste0(headBlank, " ")))
  
}
