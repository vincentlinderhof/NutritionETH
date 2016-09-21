#'This file loads the data for the panel or cross sectional regressions

#'Setting the working directory
setwd('C:/Users/Jasper/Documents/LEI - internship') #working from home
setwd('M:/My Documents/LEI - internship')           #working from LEI

data <- read.csv('R-Output/data_original.csv')[2:39]
data <- read.csv('R-Output/data2.csv')[,3:40]

data$household_id <- as.character(ifelse((data$region %in% c("Harari", "Gambella", "Dire Dawa")), data$household_id, paste(0,data$household_id,sep="")))
data$techsplit0_1234[data$tech_count>0] <- 1
data$techsplit0_1234[data$tech_count<1] <- 0

data$techsplit01_234[data$tech_count>1] <- 1
data$techsplit01_234[data$tech_count<2] <- 0

data$techsplit012_34[data$tech_count>2] <- 1
data$techsplit012_34[data$tech_count<3] <- 0

data$techsplit0123_4[data$tech_count>3] <- 1
data$techsplit0123_4[data$tech_count<4] <- 0


data2011 <- dplyr::filter(data, year=="2011")
data2013 <- dplyr::filter(data, year=="2013")

regdata <- plm.data(data, c("household_id","year"))
regdata2011 <- plm.data(data2011, c("household_id", "year"))
regdata2013 <- plm.data(data2013, c("household_id", "year"))

