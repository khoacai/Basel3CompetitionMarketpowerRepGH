
bsOwnershipConcnDummies <- function (refdata, headBlank="") 
{
  
  message(sprintf(paste0(headBlank, "Generate ownership concerntration dummies: started")))
  
  on.exit(message(sprintf(paste0(headBlank, "Generate ownership concerntration dummies: done!"))))
  
  refdata$bootUp = TRUE
  
  return (processExec(function() {
    
    suppressPackageStartupMessages(require(dplyr))
    
    refdata = refdata %>% mutate(ownrshpConcnAPlus=ifelse(.$OwnershipConcn=='A+', 1, 0), 
                                 ownrshpConcnA=ifelse(.$OwnershipConcn=='A', 1, 0), 
                                 ownrshpConcnAMinus=ifelse(.$OwnershipConcn=='A-', 1, 0), 
                                 ownrshpConcnBPlus=ifelse(.$OwnershipConcn=='B+', 1, 0), 
                                 ownrshpConcnB=ifelse(.$OwnershipConcn=='B', 1, 0), 
                                 ownrshpConcnBMinus=ifelse(.$OwnershipConcn=='B-', 1, 0), 
                                 ownrshpConcnCPlus=ifelse(.$OwnershipConcn=='C+', 1, 0), 
                                 ownrshpConcnC=ifelse(.$OwnershipConcn=='C', 1, 0), 
                                 ownrshpConcnD=ifelse(.$OwnershipConcn=='D', 1, 0)) #Otherwise: U or others
    
    detach("package:dplyr")
    
    return (as.data.frame(refdata))
  
  }, paste0(headBlank, " ")))
  
}
