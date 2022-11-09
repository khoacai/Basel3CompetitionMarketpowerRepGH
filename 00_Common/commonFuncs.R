## readr, readxl, data.table, DescTools, zoo, future, sqldf, gplots, ggplot2 
## dplyr, plyr, ellipsis, assertthat, glue, magrittr, methods, pkgconfig, R6, Rcpp, rlang, tibble, tidyselect, utils, BH, plogr, Rcpp, lifecycle, bit64, callr, covr, crayon DBI, vctrs, blob, dbplyr, dtplyr, ggplot2, hms, knitr, Lahman, lubridate, MASS, mgcv, microbenchmark, nycflights13, rmarkdown, RMySQL, RPostgreSQL, RSQLite, testthat, withr, broom, purrr, readr, proto, gsubfn
## update.packages(ask=FALSE, checkBuilt=TRUE)

stdev <- function (dataColumn) 
{
  return (sqrt(var(dataColumn[c(!is.na(dataColumn - dataColumn*0))], na.rm=TRUE)))
}


totalCount <- function (dataColumn) 
{
  return (length(dataColumn))
}


availableCount <- function (dataColumn) 
{
  return (length(dataColumn) - defectCount(dataColumn))
}


defectCount <- function (dataColumn) 
{
  return (length(dataColumn[is.na(dataColumn) | is.infinite(dataColumn)]))
}


consecDefectCount = function(x) 
{
  with(rle(is.na(x) | is.infinite(x) | x==0), lengths)
}


summarizze <- function (dataColumn) 
{
  return (dataColumn[1])
}


topBigN <- function(Xs, N=0) 
{
  
  if (N == 0) {
    
    return (sort(Xs, decreasing=T))  
    
  } else {
    
    return (head(sort(Xs, decreasing=T), N))
    
  }
  
}


deflate <- function(x, cpiIdx) 
{
  return (x/cpiIdx)
}


inf2NA <- function (dataTable) 
{

  for (j in 1:ncol(dataTable)) 
  { 
    
    set(dataTable, which(is.infinite(dataTable[[j]])), j, NA)
    
  }
  
  return (as.data.frame(dataTable))
  
}


exportCSV <- function (refdata, file = "FedBHCF_RiskMeasures.csv") 
{
  
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(readr))

  write_excel_csv(inf2NA(refdata), 
                  file=file, 
                  delim=",", 
                  na="", 
                  append=FALSE, 
                  escape="double", 
                  eol="\n")
  #write.table(inf2NA(refdata), file=file, sep=",", na="", row.names=FALSE)
  
  detach("package:readr")
  detach("package:data.table")
  
}


saveAs <- function (refdata, fileOut="exported_data", fileType = ".RData", headBlank=" ") 
{
  
  message(sprintf(paste0(headBlank, "Save as %s file: started"), fileType))
  
  on.exit(message(sprintf(paste0(headBlank, "Save as %s file: done!"), fileType)))
  
  (processExec(function() {
  
    if (fileType == ".RData") {
      
      save(refdata, file=paste0(fileOut, fileType))
      
    }
    
    if (fileType == ".csv") {
      
      exportCSV(refdata, paste0(fileOut, fileType))
      
    }
    
  }, paste0(headBlank, " ")))
  
}


## This function is initially copied from code by Mai Feng at: 
## https://gist.github.com/maifeng/3e5a1f0dd65bf0fd9c2b8f4ac8f5fab3
# --------------------------------------------------------
# Author: Khoa Cai
# based on code by Mai Feng 
# --------------------------------------------------------
# Data cleaning by winsorization
# Winsorization: Replace the extreme observations using 99% and 1% percentiles

winsoriz <- function(x, cut=0.01)
{
  cut_point_top <- quantile(x, 1 - cut, na.rm=T)
  cut_point_bottom <- quantile(x, cut, na.rm=T)
  
  i = which(x >= cut_point_top) 
  x[i] = cut_point_top
  
  j = which(x <= cut_point_bottom) 
  x[j] = cut_point_bottom
  
  return (x)
}


## Display the messages indicating the states of a process
# --------------------------------------------------------
# Author: Khoa Cai
# --------------------------------------------------------

processExec <- function(func=NULL, heading="") 
{

  options(future.globals.maxSize=40050*1024^2)
  
  on.exit(cat("\r", 
              paste0(heading, "process completed."), 
              paste(rep(" ", 9), collapse = ''), 
              "\n\r"))
  
  suppressPackageStartupMessages(require(future))
  
  
  #For fixing the Warning message:
  #  UNRELIABLE VALUE: Future ('<none>') unexpectedly generated random numbers without specifying argument 'seed'. 
  #  There is a risk that those random numbers are not statistically sound and the overall results might be invalid. 
  #  To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe random numbers are produced via the L'Ecuyer-CMRG method. 
  #  To disable this check, use 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".
  # OR
  #Please see the usage of the future() function @https://cran.r-project.org/web/packages/future/future.pdf
  
  f <- future({
    func()
  }, seed = TRUE) %plan% multisession #specify 'seed=TRUE'. This ensures that proper, parallel-safe random numbers are produced via the L'Ecuyer-CMRG method. 
  
  progressChars = c("-", "\\", "|", "/")
  
  while (!resolved(f)) {
    
	for (i in 1:8) {
      
	  idx = ifelse(i %% 4 == 0, 4, i %% 4)
      
	  cat("\r", heading, progressChars[idx], "processing")
      cat(paste(rep(".", i), collapse = ''))
      
	  Sys.sleep(1)
    
	}
    
	cat("\r", heading, progressChars[idx], "processing", paste(rep(" ", i), collapse = ''))
  
  }
  
  returnVal = value(f)
  
  f = NULL
  gc()
  detach("package:future")
  gc()
  
  return (returnVal)
  
}


colsWithNAs <- function(x) 
{
  
  return (names(which(colSums(is.na(tTestTrain)) > 0)))

}

colsWithNoVariance <- function(x) 
{
  
  return (names(x[, sapply(x, function(v) var(v, na.rm=TRUE)==0)]))
  
}





varsNotInDataSet <- function(testvars, testdataset) 
{
  
  return (testvars[which(!testvars %in% colnames(testdataset))])

}


imputate <- function(imputationData, 
                     pm = 5,
                     pmaxit = 50,
                     pmeth = 'cart',
                     pseed = 500) 
{
  
  #MICE can't use vars with some special characters:
  #such as spaces, (, ), -, &, /, or ,'s
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = " ",
      replacement = "."
    )
  
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = "\\(",
      replacement = "10"
    )
  
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = "\\)",
      replacement = "20"
    )
  
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = "-",
      replacement = "30"
    )
  
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = "[&]",
      replacement = "40"
    )
  
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = "[/]",
      replacement = "50"
    )
  
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = "[,]",
      replacement = "60"
    )
  
  #do the imputations
  tempData <- mice(
    imputationData,
    m = pm,
    maxit = pmaxit,
    meth = pmeth,
    seed = pseed
  )
  
  imputationData <- complete(tempData, 1)
  
  #undo the replacements of special characters
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = "60",
      replacement = ","
    )
  
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = "50",
      replacement = "/"
    )
  
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = "40",
      replacement = "&"
    )
  
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = "30",
      replacement = "-"
    )
  
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = "20",
      replacement = ")"
    )
  
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = "10",
      replacement = "("
    )
  
  names(imputationData) <-
    gsub(
      x = names(imputationData),
      pattern = "\\.",
      replacement = " "
    )
  
  return (imputationData)
  
}

loadSymbolz <- function(symbolz,
                        env = new.env(),
                        src = 'yahoo',
                        periodicity = "monthly",
                        from = "2011-04-01",
                        to = Sys.Date(),
                        curl.options = list()) 
{

  loadSymbols(
    ifelse(length(symbolz) > 1, symbolz, paste0("^",symbolz)),
    env = env,
    src = src,
    periodicity = periodicity,
    from = from,
    to = to,
    curl.options = curl.options
  )
  
  return(env)
  
}