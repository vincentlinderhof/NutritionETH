library(MASS)
library(AER)
library(plm)
library(pglm)
library(polycor)
library(stargazer)
library(car)
library(dplyr)
library(foreign)


logit0_1234 <- pglm(techsplit0_1234 ~ 
                   age 
                  + gender  
                  + educationlevel 
                  + hhsize 
                  #+ roofmaterial 
                  #+ television 
                  + area 
                  + LS
                  + dist_household 
                  + croprotate 
                  + offpart 
                  + ownership_index 
                  #+ plots
                  + extension  
                  + credit  
                  #+ advise 
                  + oxen 
                  #+ dist_road 
                  + dist_market 
                  + anntot_avg 
                  #+ average_slope 
                  + nutrient 
                  #+ drought
                  #+ flood
                  #+ heavy_rain
                  #+ pinput_rise
                 ,data = regdata
                 ,effect = ("individual")         
                 , family = binomial('logit'),         
                 , method = "bfgs"          
                 , model="random"
                 )

#--------------------------------------------------
poisson1.0 <- pglm(tech_count ~ 
                     # age 
                    + gender  
                    + educationlevel 
                    + hhsize 
                    #+ roofmaterial 
                    #+ television 
                    + area 
                    + LS
                    + dist_household 
                    + croprotate 
                    + offpart 
                    + ownership_index 
                    #+ plots
                    + extension  
                    + credit  
                    #+ advise 
                    + oxen 
                    #+ dist_road 
                    + dist_market 
                    + anntot_avg 
                    #+ average_slope 
                    + nutrient 
                    #+ drought
                    #+ flood
                    #+ heavy_rain
                    #+ pinput_rise
             ,data = regdata
             #,effect = ("twoways")
             ,index = c('household_id','year')
             ,family = poisson,         
             ,method = "bfgs"          
             ,model="within"
)
summary(poisson1.0)

#-----------------------------------------

ordlogit2.1 <- pglm(as.numeric(tech_count2) ~ 
                     age 
                    + gender 
                    + education
                    #+ educationlevel 
                    + hhsize 
                    #+ roofmaterial 
                    #+ television 
                    + area 
                    + LS
                    #+ dist_household 
                    #+ croprotate 
                    #+ offpart 
                    + ownership_index 
                    #+ plots
                    + extension  
                    + credit  
                    #+ advise 
                    #+ oxen 
                    #+ dist_road 
                    + dist_market 
                    + anntot_avg 
                    #+ average_slope 
                    + nutrient 
                    #+ drought
                    #+ flood
                    #+ heavy_rain
                    #+ pinput_rise
                    ,data = regdata_v2
                    #,effect = ("individual") 
                    , index = "household_id"
                    , family = ordinal('logit')
                   #Using an other method than bfgs
                    , method = "bfgs"          
                    , model="random"
)
summary(ordlogit2.1)

#Prepare dataset for Ordered Logit (by removing all NAs and only using households that appear twice in the dataset, once for the first wave and once for the second wave)
regdata2 <- regdata
regdata2$house_age <- NULL
regdata2$in_education <- NULL
regdata2$dist_household <- NULL

b <- na.omit(regdata2)
b = as.vector(b$household_id)
a = 1
households <- list()
for(household in b){
  ifelse((household == a), households <- cbind(households, household), 0)  
  a = household 
}

regdata_v2 <- dplyr::filter(regdata2, household_id %in% unlist(households))
regdata_v2$tech_count2 <- regdata_v2$tech_count + 1


linear1.1 <- plm(techindex ~ 
                     age 
                   + gender  
                   + educationlevel 
                   + hhsize 
                   #+ roofmaterial 
                   #+ television 
                   + area 
                   + LS
                   + dist_household 
                   + croprotate 
                   #+ offpart 
                   + ownership_index 
                   #+ plots
                   + extension  
                   + credit  
                   #+ advise 
                   + oxen 
                   #+ dist_road 
                   + dist_market 
                   + anntot_avg 
                   #+ average_slope 
                   + nutrient 
                   #+ drought
                   #+ flood
                   #+ heavy_rain
                   #+ pinput_rise
                   ,data = regdata
                   #,effect = ("individual")         
                   #,family = gaussian,         
                   #,method = "bfgs"          
                   ,model="random"
)
summary(linear1.1)

