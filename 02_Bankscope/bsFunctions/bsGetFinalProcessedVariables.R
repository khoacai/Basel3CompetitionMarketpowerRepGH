
bsGetFinalProcessedVariables <- function (variables4Winsorize) 
{
  finalProcessedVariables = c("COUNTRY", "CTRYCODE", 
                              "BVDIDNUM", "CLOSDATE_YEAR", "INDEXX", 
                              "ownrshpConcnAPlus", "ownrshpConcnA", "ownrshpConcnAMinus", 
                              "ownrshpConcnBPlus", "ownrshpConcnB", "ownrshpConcnBMinus", 
                              "ownrshpConcnCPlus", "ownrshpConcnC", "ownrshpConcnD") 
                              #"EATYPE")
  
  finalProcessedVariables = c(finalProcessedVariables,
                              variables4Winsorize, 
                              paste0("ws", variables4Winsorize))
                
  return (finalProcessedVariables)
  
}
