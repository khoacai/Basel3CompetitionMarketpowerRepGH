


bsExchange <- function (refdata, exchangedVariables, headBlank = "")
{
  message(sprintf(paste0(headBlank, "Exchange: started")))
  
  on.exit(message(sprintf(paste0(
    headBlank, "Exchange: done!"
  ))))
  
  suppressPackageStartupMessages(require(dplyr))
  
  # initialize the exchaged data
  refdata = cbind(refdata, setNames(refdata[exchangedVariables], paste0("extmp", exchangedVariables)))
  
  # exchange
  exchangeX = function(x, exchRate) {
    return (x / exchRate)
  }
  
  #refdata = refdata %>% mutate_at(vars(starts_with("extmp")), funs(exchangeX, .args=list(.$EXCHRATE)))
  #The use of funs is depricated from newer versions of dplyr
  #funs() is soft deprecated as of dplyr 0.8.0
  #* Please use a list of either functions or lambdas:*
  #   Simple named list:*
  #   list(mean = mean, median = median) *
  #   Auto named with tibble::lst():*
  #   tibble::lst(mean, median) *
  #   Using lambdas *
  #   list( ~ mean(., trim = .2), ~ median(., na.rm = TRUE)) *
  refdata = refdata %>% mutate_at(vars(starts_with("extmp")), list(~exchangeX(., EXCHRATE)))
  
  detach("package:dplyr")
  
  suppressPackageStartupMessages(require(data.table))
  
  setnames(refdata,
           paste0("extmp", exchangedVariables),
           paste0("ex", exchangedVariables))
  
  detach("package:data.table")
  
  return (as.data.frame(refdata))
  
}
