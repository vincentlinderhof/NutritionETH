#Writing regressions to a neat table 

reg <- function(regobject1, title, filename){
  #Creating a list of regression objects to cycle through and initiating the output table

  regression_summary <- as.data.frame(summary(regobject1)[6])
  regtable <- list()
  
  for(var in rownames(regression_summary)){
  varsum <- as.data.frame(regression_summary[var,])
  
  coef <- as.numeric(varsum$estimate.Estimate)
  std <- as.numeric(varsum$estimate.Std..error)
  pval <- as.numeric(varsum$estimate.Pr...t.)
  
  coef <- round(coef, 3)
  std  <- round(std, 3)
  
  coef[pval<=0.01] <- paste(coef,"***",sep="")
  coef[pval<=0.05 & pval>0.01] <- paste(coef,"**",sep="")
  coef[pval<=0.10 & pval>0.05] <- paste(coef,"*",sep="")
  
  line = paste(format(var, justify="left", width=15), format(coef, justify="right",width=14),sep="")
  line2 = format((line2 = paste("(", (round(std,3)), ")",sep="")), justify="right",width=30)
  
  regtable <- c(regtable, line)
  regtable <- c(regtable, line2)
  #regtable <- c(regtable, "")
  }
  rownames(regtable) <- NULL
  
  cat(format(title, justify='centre',width='30'))
  cat("\n")
  cat(rep("=", 30), sep="")
  cat("\n")
  
  for (line in regtable){
    cat(line)
    cat("\n")
  }
}


