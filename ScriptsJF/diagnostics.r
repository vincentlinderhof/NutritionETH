#http://stats.stackexchange.com/questions/118051/how-to-perform-residual-analysis-for-binary-dichotomous-independent-predictors-i


#Regressions diagnostics
data <- read.csv('R-Output/data_corrected.csv')[,3:37]
data$household_id <- as.character(ifelse((data$region %in% c("Harari", "Gambella", "Dire Dawa")), data$household_id, paste(0,data$household_id,sep="")))
data2011 <- dplyr::filter(data, year=="2011")
data2013 <- dplyr::filter(data, year=="2013")

#Test and graph the original count data for observed frequencies and fitted frequencies
gfit11 <- goodfit(data2011$tech_count)
gfit13 <- goodfit(data2013$tech_count)
gfit   <- goodfit(data$tech_count)

rootogram(gfit11)
rootogram(gfit13)
rootogram(gfit)

Ord_plot(data2011$tech_count, type="poisson")
Ord_plot(data2013$tech_count, type="poisson")
Ord_plot(data$tech_count, type="poisson")

#Goodness of fit measures (likelihood ratio statistics vs null model)
summary(fit11)
summary(fit13)
anova(fit11, test="Chisq")
anova(fit13, test="Chisq")

#Checking for overdispersion
dispersiontest(fit11)
dispersiontest(fit13)

#Multicollinearity
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  for(val in names(in_frame)){
    form_in<-formula(paste(val,' ~ .'))
    vif_init<-rbind(vif_init,c(val,VIF(lm(form_in,data=in_frame,...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(names(in_frame))
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      
      for(val in names(in_dat)){
        form_in<-formula(paste(val,' ~ .'))
        vif_add<-VIF(lm(form_in,data=in_dat,...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}
