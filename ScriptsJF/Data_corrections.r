#'Setting the working directory
setwd('C:/Users/Jasper/Documents/LEI - internship') #working from home
setwd('M:/My Documents/LEI - internship')           #working from LEI

#Comparing variables across years
data <- read.csv('R-Output/data_original.csv')
data$household_id <- as.character(ifelse((data$region %in% c("Harari", "Gambella", "Dire Dawa")), data$household_id, paste(0,data$household_id,sep="")))

varcompare <- function(data, variable){
  #Step 1: create data frame with households and variable
  data2011 <- dplyr::filter(data, year=="2011")
  data2013 <- dplyr::filter(data, year=="2013")
  
  data2011 <- data.frame(household_id = data2011$household_id,
                         individual_id = as.character(data2011$individual_id),
                         lat1 = data2011$lat_dd_mod,
                         lon1 = data2011$lon_dd_mod,
                         var = data2011[,variable])
  data2013 <- data.frame(household_id = data2013$household_id,
                         individual_id2 = as.character(data2013$individual_id),
                         lat2 = data2013$lat_dd_mod,
                         lon2 = data2013$lon_dd_mod,
                         var2 = data2013[,variable])
  
  #Step 2: add 2013 value to the data frame
  combined <- left_join(data2011, data2013)
  combined$var_diff <- combined$var2 - combined$var
  
  combined$individual_id <- as.character(combined$individual_id)
  combined$individual_id2 <- as.character(combined$individual_id2)
  
  combined$lat_diff <- combined$lat1 == combined$lat2
  combined$lon_diff <- combined$lon1 == combined$lon2
  
  combined$individual_id[is.na(combined$individual_id)] <- 0
  combined$individual_id2[is.na(combined$individual_id2)] <- 0
  
  combined$id_diff <- combined$individual_id == combined$individual_id2
  
  return(combined)
}


#----------------------------------------------------------------------------------------------------------
#Age
age <- varcompare(data, 'age') 
#solution
age$age_corrected <- ifelse((age$id_diff==TRUE & age$var_diff!=2), age$var+2, age$var2)
agehh <- data.frame(household_id = data$household_id[data$year==2013])
age2013 <- dplyr::select(age, household_id, age_corrected)
agehh <- left_join(agehh, age2013)
data$age[data$year==2013] <- agehh$age_corrected

#----------------------------------------------------------------------------------------------------------
#gender 
gender <- varcompare(data, 'gender')
#solution: remove the observations where gender does not match
a <- dplyr::filter(gender, var_diff!=0 & id_diff==TRUE)
b <- a$household_id
data <- data[!data$household_id %in% b,]


#----------------------------------------------------------------------------------------------------------
#Moved households
moved <- dplyr::filter(gender, lat_diff==FALSE | lon_diff==FALSE)
c <- moved$household_id
data <- data[!data$household_id %in% c,]

#----------------------------------------------------------------------------------------------------------
#Changing individual_ids
changeid <- varcompare(data, 'individual_id')
changeid <- dplyr::filter(changeid, id_diff==FALSE)
d <- changeid$household_id
data <- data[!data$household_id %in% d,]

#----------------------------------------------------------------------------------------------------------
#education
edu <- varcompare(data, 'education')
#solution
edu$var[edu$var2==0] <- 0
edu$edu_corrected <- ifelse((edu$id_diff==TRUE & edu$var_diff!=0), edu$var, edu$var2)
eduhh <- data.frame(household_id = data$household_id[data$year==2013])
edu2013 <- dplyr::select(edu, household_id, edu_corrected)
eduhh <- left_join(eduhh, edu2013)
data$education[data$year==2013] <- eduhh$edu_corrected

#----------------------------------------------------------------------------------------------------------
#education level
edulvl <- varcompare(data, 'educationlevel')
#solution
edulvl$edulvl_corrected <- ifelse((edulvl$id_diff==TRUE & edulvl$var_diff!=0 & edulvl$var_diff<0), edulvl$var, edulvl$var2)

edulvlhh <- data.frame(household_id = data$household_id[data$year==2013])
edulvl2013 <- dplyr::select(edulvl, household_id, edulvl_corrected)
edulvlhh <- left_join(edulvlhh, edulvl2013)
data$educationlevel[data$year==2013] <- edulvlhh$edulvl_corrected
#----------------------------------------------------------------------------------------------------------
#Livestock
lim_LS <- quantile(data[,'LS'], probs = 0.99, na.rm=TRUE)
data[,'LS'][data[,'LS'] > lim_LS] <- lim_LS 
#----------------------------------------------------------------------------------------------------------
#Plot distance to the household
lim_plot <- quantile(data$dist_household, probs=0.975, na.rm=TRUE)
data$dist_household[data$dist_household > lim_plot] <- lim_plot
#----------------------------------------------------------------------------------------------------------


#THIS REMOVES THEM FROM THE PANEL REGRESSION SINCE THERE IS NO DIFFERENCE BETWEEN SURVEY WAVES
#Distance to the nearest main road
dist_r <- varcompare(data, 'dist_road')
dist_r$roaddistance_corrected <- ifelse((dist_r$lat_diff==TRUE & dist_r$lon_diff==TRUE & dist_r$var_diff!=0), dist_r$var, dist_r$var2)

dist_r_hh <- data.frame(household_id = data$household_id[data$year==2013])
dist_r2013 <- dplyr::select(dist_r, household_id, roaddistance_corrected)
dist_r_hh <- left_join(dist_r_hh, dist_r2013)

data$dist_road[data$year==2013] <- dist_r_hh$roaddistance_corrected
#----------------------------------------------------------------------------------------------------------
#Distance to the nearest market
dist_m <- varcompare(data, 'dist_market')
dist_m$marketdistance_corrected <- ifelse((dist_m$lat_diff==TRUE & dist_m$lon_diff==TRUE & dist_m$var_diff!=0), dist_m$var, dist_m$var2)

dist_m_hh <- data.frame(household_id = data$household_id[data$year==2013])
dist_m2013 <- dplyr::select(dist_m, household_id, marketdistance_corrected)
dist_m_hh <- left_join(dist_m_hh, dist_m2013)

data$dist_market[data$year==2013] <- dist_m_hh$marketdistance_corrected
#solution: remove households that have moved?

write.csv(data, 'R-Output/data2.csv')












