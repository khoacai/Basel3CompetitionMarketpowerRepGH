#http://www.sciencedirect.com/science/article/pii/S0304405X12001894


winsorizeX <-
   function (refdata,
             dataSource = "FED",
             winsorizedVariables,
             headBlank = "")
   {
      message(sprintf(paste0(headBlank, "Winsorize: started")))
      
      on.exit(message(sprintf(paste0(
         headBlank, "Winsorize: done!"
      ))))
      
      refdata$bootUp = TRUE
      
      refdata = (processExec(function() {
         suppressPackageStartupMessages(require(data.table))
         
         refdata = inf2NA(data.table(refdata))
         
         detach("package:data.table")
         
         refdata = cbind(refdata, setNames(refdata[winsorizedVariables], paste0("wstmp", winsorizedVariables)))
         
         suppressPackageStartupMessages(require(dplyr))
         
         if (dataSource == "FED") {
            refdata = as.data.frame(refdata %>% group_by(YEAR) %>%
                                       mutate_at(vars(starts_with("wstmp")), funs(winsoriz)))
            
            refdata = refdata[order(refdata$RSSD9001, refdata$RSSD9999),]
            
         }
         
         if (dataSource == "BANKSCOPE") {
            
               #refdata = as.data.frame(refdata %>%
               #                         group_by(CTRYCODE) %>%
               #                         mutate_at(vars(starts_with("wstmp")), funs(winsoriz)))
               #The use of funs() is deprecated from newer versions of dplyr:
               #funs() is soft deprecated as of dplyr 0.8.0
               #* Please use a list of either functions or lambdas:*
               #   Simple named list:*
               #   list(mean = mean, median = median) *
               #   Auto named with tibble::lst():*
               #   tibble::lst(mean, median) *
               #   Using lambdas *
               #   list( ~ mean(., trim = .2), ~ median(., na.rm = TRUE)) *
               
               refdata = as.data.frame(refdata %>%
                                          group_by(CTRYCODE) %>%
                                          mutate_at(vars(starts_with("wstmp")), list(~winsoriz(.))))
               
               refdata = refdata[order(refdata$BVDIDNUM,
                                       refdata$CLOSDATE_YEAR,
                                       refdata$INDEXX),]
               
         }
         
         detach("package:dplyr")
         
         suppressPackageStartupMessages(require(data.table))
         
         setnames(
            refdata,
            paste0("wstmp", winsorizedVariables),
            paste0("ws", winsorizedVariables)
         )
         
         detach("package:data.table")
         
         return (refdata)
         
      }, paste0(headBlank, " ")))
      
   }
