clogit0_1234 <- glm(as.numeric(techsplit0_1234) ~ 
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
             + extension  
             + credit  
             + oxen 
             #+ dist_road 
             + dist_market 
             + anntot_avg 
             #+ average_slope 
             + nutrient 
             ,data = regdata2013 
            # , effect = "individual"         
            # , family = "poisson"     
             ,family = binomial(link = "logit")
            # , method = "bfgs"          
             #, model="within"
)
summary()


ordlogit1.0 <- polr(tech_count ~ 
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
                   + extension  
                   + credit  
                   + oxen 
                   #+ dist_road 
                   + dist_market 
                   + anntot_avg 
                   #+ average_slope 
                   + nutrient 
                   ,data = regdata2013 
                   #, effect = "twoways"         
                   #, family = poisson     
                   #,family = ordinal(link = "logit")
                   , method = "logistic"          
                   #, model="within"
)
