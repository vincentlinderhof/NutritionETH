# Tom
# dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data"

# LEI Path
# dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Data"

# ACT: Tom has to change his dataPath

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2013/Data"
} else {
  dataPath <- "D:/Analyses/CIMMYT/NutritionETH/SurveyData/2013/Data"
}
setwd("D:/Analyses/CIMMYT/NutritionETH")

# load packages
library(haven)
library("stringr")
library("reshape2")
library(dplyr)
library("markdown")
library(tidyr) # Necessary for spread function in mutate command
library(Deducer) # necessary for descriptives.tables

options(scipen=999)

# ***************************************************************************************************
#Creation of FVS
# ***************************************************************************************************

#FOOD2013 <- read_dta(file.path(dataPath, "sect7_hh_w1.dta"))  # CSI saq08 hh_s7q02_a to hh_s7q02_h
#FOOD2013 <- read_dta(file.path(dataPath, "sect5b_hh_w1.dta"))  # DDS and FCS hh_s5aq00 hh_s5aq0a hh_s5aq01 hh_s5aq02_a hh_s5aq02_b
#FOOD2013 <- read_dta(file.path(dataPath, "sect5a_hh_w1.dta"))  # FVS and DDS hh_s5aq00 hh_s5aq0a hh_s5aq01 hh_s5aq02_a hh_s5aq02_b

FOOD2013 <- read_dta(file.path(dataPath, "household/sect5b_hh_w2.dta"))  # FVS and DDS hh_s5aq00 hh_s5aq0a hh_s5aq01 hh_s5aq02_a hh_s5aq02_b
FOOD2013 <- subset(FOOD2013, select=c(household_id, hh_s5bq00, hh_s5bq0a, hh_s5bq01, hh_s5bq02))

# How food items are connected to food groups, See FAO (2013) 
# 2-13       100       cereals = mean(cereals, na.rm = TRUE),
# 14-20      200       rootsandtubers = mean(rootsandtubers, na.rm = TRUE),
# 21-23      300       vegetables = mean(vegetables, na.rm=TRUE),
# 24         400       pulsesandnuts = mean(pulsesandnuts, na.rm=TRUE),
# 25-28      500       fruits = mean(fruits, na.rm=TRUE),
# 29-31      600       meat = mean(meat, na.rm=TRUE),
# 32-35      700       eggs = mean(eggs, na.rm=TRUE),
# 36-38      800       fishandseafood= mean(fishandseafood, na.rm=TRUE),
# 39-48      900       milkandmilkproducts= mean(milkandmilkproducts, na.rm=TRUE),
# 48-50     1000       oilsandfats=mean(oilsandfats, na.rm=TRUE),
# 50-53     1100       sugar=mean(sugar, na.rm=TRUE),
# 53-60     1200       condiments=mean(condiments, na.rm=TRUE))

#aggregate(FOOD2013, by=(FOOD2013$hh_s5bq00), FUN=count, na.rm=TRUE)


# Construct dummy variables for food items: do not use as it produces wrong results
#NUTR2013 <-
#  mutate(FOOD2013, count = ifelse(hh_s5bq01 == 1, hh_s5bq02, ifelse(NA))) %>%
#  group_by(household_id) %>%
#  spread(hh_s5bq00, count) %>%
#  filter (! duplicated(household_id)) %>%
#  replace(is.na(.), 0)
#NUTR2013CH <- NUTR2013[ c(1,2,3,4) ]
#NUTR2013 <- NUTR2013[ -c(2,3,4) ]
#summary(NUTR2013CH)

FOOD2013$FI01_Enjera       <- 1*(FOOD2013$hh_s5bq00==1 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI02_OtherCereals <- 1*(FOOD2013$hh_s5bq00==2 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI03_Potatoes     <- 1*(FOOD2013$hh_s5bq00==3 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI04_Pasta        <- 1*(FOOD2013$hh_s5bq00==4 & FOOD2013$hh_s5bq01==1)

FOOD2013$FI05_Sugar         <- 1*(FOOD2013$hh_s5bq00==5 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI06_PulsesandNuts <- 1*(FOOD2013$hh_s5bq00==6 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI07_Vegetables    <- 1*(FOOD2013$hh_s5bq00==7 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI08_Fruits        <- 1*(FOOD2013$hh_s5bq00==8 & FOOD2013$hh_s5bq01==1)

FOOD2013$FI09_RedMeat <- 1*(FOOD2013$hh_s5bq00==9 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI10_Poultry <- 1*(FOOD2013$hh_s5bq00==10 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI11_Eggs    <- 1*(FOOD2013$hh_s5bq00==11 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI12_Fish    <- 1*(FOOD2013$hh_s5bq00==12 & FOOD2013$hh_s5bq01==1)

FOOD2013$FI13_FatsandOils   <- 1*(FOOD2013$hh_s5bq00==13 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI14_DairyProducts <- 1*(FOOD2013$hh_s5bq00==14 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI15_Condiments    <- 1*(FOOD2013$hh_s5bq00==15 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI16_KochoandBula  <- 1*(FOOD2013$hh_s5bq00==16 & FOOD2013$hh_s5bq01==1)

NUTR2013a <- aggregate(FI01_Enjera       ~ household_id, FOOD2013, sum)
NUTR2013a <- NUTR2013a[2:3777,]
NUTR2013 <- NUTR2013a; rm(NUTR2013a)
NUTR2013a <- aggregate(FI02_OtherCereals ~ household_id, FOOD2013, sum)
NUTR2013a <- NUTR2013a[2:3777,]
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)
NUTR2013a <- aggregate(FI03_Potatoes     ~ household_id, FOOD2013, sum)
NUTR2013a <- NUTR2013a[2:3777,]
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)
NUTR2013a <- aggregate(FI04_Pasta        ~ household_id, FOOD2013, sum)
NUTR2013a <- NUTR2013a[2:3777,]
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)

NUTR2013a <- aggregate(FI05_Sugar         ~ household_id, FOOD2013, sum)
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)
NUTR2013a <- aggregate(FI06_PulsesandNuts ~ household_id, FOOD2013, sum)
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)
NUTR2013a <- aggregate(FI07_Vegetables    ~ household_id, FOOD2013, sum)
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)
NUTR2013a <- aggregate(FI08_Fruits        ~ household_id, FOOD2013, sum)
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)

NUTR2013a <- aggregate(FI09_RedMeat ~ household_id, FOOD2013, sum)
NUTR2013a <- NUTR2013a[2:3777,]
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)
NUTR2013a <- aggregate(FI10_Poultry ~ household_id, FOOD2013, sum)
NUTR2013a <- NUTR2013a[2:3777,]
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)
NUTR2013a <- aggregate(FI11_Eggs    ~ household_id, FOOD2013, sum)
NUTR2013a <- NUTR2013a[2:3777,]
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)
NUTR2013a <- aggregate(FI12_Fish    ~ household_id, FOOD2013, sum)
NUTR2013a <- NUTR2013a[2:3777,]
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)

NUTR2013a <- aggregate(FI13_FatsandOils   ~ household_id, FOOD2013, sum)
NUTR2013a <- NUTR2013a[2:3777,]
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)
NUTR2013a <- aggregate(FI14_DairyProducts ~ household_id, FOOD2013, sum)
NUTR2013a <- NUTR2013a[2:3777,]
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)
NUTR2013a <- aggregate(FI15_Condiments    ~ household_id, FOOD2013, sum)
NUTR2013a <- NUTR2013a[2:3777,]
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)
NUTR2013a <- aggregate(FI16_KochoandBula  ~ household_id, FOOD2013, sum)
NUTR2013a <- NUTR2013a[2:3777,]
NUTR2013 <- left_join(NUTR2013, NUTR2013a); rm(NUTR2013a)

NUTR2013$FVS16 <- rowSums(NUTR2013[2:17])

# descriptives of food group dummy variables and FVS and DDS
descriptive.table(vars = d(FI01_Enjera, FI02_OtherCereals, FI03_Potatoes, FI04_Pasta, FI05_Sugar, FI06_PulsesandNuts,
                           FI07_Vegetables, FI08_Fruits, FI09_RedMeat, FI10_Poultry, FI11_Eggs, FI12_Fish, 
                           FI13_FatsandOils, FI14_DairyProducts, FI15_Condiments, FI16_KochoandBula, FVS16),data= NUTR2013, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))

FNS2013 <- NUTR2013[ c("household_id", "FVS16") ]

# ***************************************************************************************************
#Construction of DDS, uses the data of the FVC construction!
# ***************************************************************************************************
# Columns correspond to list of food items!
# sum fooditems into 12 foodgroups for FVS: columns correspond to list of food items!
NUTR2013$cereals         <- 1*((NUTR2013$FI01_Enjera+NUTR2013$FI02_OtherCereals++NUTR2013$FI04_Pasta )  > 0 ) 
#NUTR2013$cereals         <- 1*((NUTR2013[ c("FI01_Enjera", "FI02_OtherCereals", "FI04_Pasta") ] )  > 0 ) 
NUTR2013$rootsandtubers  <- 1*((NUTR2013$FI03_Potatoes+NUTR2013$FI16_KochoandBula )  > 0 ) 
#NUTR2013$rootsandtubers  <- 1*((NUTR2013[ c("FI03_Potatoes", "FI16_KochoandBula") ] )  > 0 ) 
NUTR2013$vegetables      <- NUTR2013$FI07_Vegetables
NUTR2013$fruits          <- NUTR2013$FI08_Fruits
NUTR2013$meat            <- 1*((NUTR2013$FI09_RedMeat+NUTR2013$FI10_Poultry )  > 0 ) 
#NUTR2013$meat            <- 1*((NUTR2013[ c("FI09_RedMeat", "FI10_Poultry")] )  > 0 ) 
NUTR2013$eggs            <- NUTR2013$FI11_Eggs
NUTR2013$fish            <- NUTR2013$FI12_Fish
NUTR2013$pulsesandnuts   <- NUTR2013$FI06_PulsesandNuts
NUTR2013$dairyproducts   <- NUTR2013$FI14_DairyProducts
NUTR2013$oilsandfats     <- NUTR2013$FI13_FatsandOils
NUTR2013$condiments      <- NUTR2013$FI15_Condiments
NUTR2013$sugar           <- NUTR2013$FI05_Sugar

#install.packages("Hmisc")
#library(Hmisc)
#label(NUTR2013$cereals) <- "FG Cereals" 

NUTR2013$DDS12 <- rowSums(NUTR2013[ c("cereals", "rootsandtubers", "vegetables",
                                         "fruits", "meat", "eggs", "fish",
                                         "pulsesandnuts", "dairyproducts", "oilsandfats",
                                         "sugar","condiments")] )

DDS2013 <- NUTR2013[ c("household_id", "DDS12") ]

FNS2013 <-left_join(FNS2013, DDS2013)
rm(DDS2013)

# descriptives of food group dummy variables and FVS and DDS
descriptive.table(vars = d(cereals, rootsandtubers, vegetables, fruits, meat, eggs, fish,
                           pulsesandnuts, dairyproducts, oilsandfats, sugar, condiments, DDS12, FVS16),data= NUTR2013, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))



# ***************************************************************************************************
#Construction of FCS
# ***************************************************************************************************
FOOD2013$hh_s5bq02t <- ifelse(is.na(FOOD2013$hh_s5bq02),0,FOOD2013$hh_s5bq02)

FOOD2013$FI01_Enjera       <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==1 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI02_OtherCereals <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==2 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI03_Potatoes     <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==3 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI04_Pasta        <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==4 & FOOD2013$hh_s5bq01==1)

FOOD2013$FI05_Sugar         <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==5 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI06_PulsesandNuts <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==6 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI07_Vegetables    <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==7 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI08_Fruits        <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==8 & FOOD2013$hh_s5bq01==1)

FOOD2013$FI09_RedMeat <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==9 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI10_Poultry <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==10 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI11_Eggs    <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==11 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI12_Fish    <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==12 & FOOD2013$hh_s5bq01==1)

FOOD2013$FI13_FatsandOils   <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==13 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI14_DairyProducts <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==14 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI15_Condiments    <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==15 & FOOD2013$hh_s5bq01==1)
FOOD2013$FI16_KochoandBula  <- FOOD2013$hh_s5bq02t*(FOOD2013$hh_s5bq00==16 & FOOD2013$hh_s5bq01==1)

NUTR2013 <- aggregate(FOOD2013, by=list(FOOD2013$household_id), FUN=max )


NUTR2013$FCS16w <- 
  NUTR2013$FI01_Enjera*2 + 
  NUTR2013$FI02_OtherCereals*2 + 
  NUTR2013$FI03_Potatoes*2 + 
  NUTR2013$FI04_Pasta*2 + 
  NUTR2013$FI05_Sugar*0.5 + 
  NUTR2013$FI06_PulsesandNuts*3 + 
  NUTR2013$FI07_Vegetables*1 + 
  NUTR2013$FI08_Fruits*1 + 
  NUTR2013$FI09_RedMeat*4 +
  NUTR2013$FI10_Poultry*4 +
  NUTR2013$FI11_Eggs*4 +
  NUTR2013$FI12_Fish*4 +
  NUTR2013$FI13_FatsandOils*0.5 +
  NUTR2013$FI14_DairyProducts*4 +
  NUTR2013$FI15_Condiments*0 +
  NUTR2013$FI16_KochoandBula*2

NUTR2013$FCS16u <- 
  NUTR2013$FI01_Enjera + 
  NUTR2013$FI02_OtherCereals + 
  NUTR2013$FI03_Potatoes + 
  NUTR2013$FI04_Pasta + 
  NUTR2013$FI05_Sugar + 
  NUTR2013$FI06_PulsesandNuts + 
  NUTR2013$FI07_Vegetables + 
  NUTR2013$FI08_Fruits + 
  NUTR2013$FI09_RedMeat +
  NUTR2013$FI10_Poultry +
  NUTR2013$FI11_Eggs +
  NUTR2013$FI12_Fish +
  NUTR2013$FI13_FatsandOils +
  NUTR2013$FI14_DairyProducts +
  NUTR2013$FI15_Condiments +
  NUTR2013$FI16_KochoandBula 

#rm(NUTR2010, NUTR2013ALL, NUTR2013b, NUTR2013c, NUTR2013CH, NUTR2013d)

FCS2013 <- NUTR2013[ c("household_id", "FCS16w", "FCS16u")]
FNS2013 <- left_join(FNS2013, FCS2013)
rm(FCS2013)

# descriptives of food group dummy variables and FVS and DDS
library("Deducer")
descriptive.table(vars = d(FI01_Enjera ,  FI02_OtherCereals ,  FI03_Potatoes , 
                           FI04_Pasta ,  FI05_Sugar ,  FI06_PulsesandNuts , 
                           FI07_Vegetables ,  FI08_Fruits ,  FI09_RedMeat ,
                           FI10_Poultry ,  FI11_Eggs , FI12_Fish ,
                           FI13_FatsandOils ,  FI14_DairyProducts ,  FI15_Condiments ,  FI16_KochoandBula,
                           FCS16w, FCS16u),data= NUTR2013, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))

# Histograms of nutrition indicators: FCS
hist(NUTR2013$FCS16w, freq = FALSE, ylim = c(0, 0.05), xlab="FCS weighted", ylab="%", main="Freguency of FCS (weighted) in 2010")

# Histograms of nutrition indicators: FCS
hist(NUTR2013$FCS16u, freq = FALSE, ylim = c(0, 0.05), xlab="FCS unweighted", ylab="%", main="Freguency of FCS (unweighted) in 2010")

# calculation of correlation coefficent of DDS and FVS
myvars <- c("FCS16u", "FCS16w")
NUTR2013sub <- NUTR2013[myvars]
cor(NUTR2013sub, use="complete.obs", method="pearson")
rm(NUTR2013sub, myvars)

# ***************************************************************************************************
#Construction of CSI
# ***************************************************************************************************
CSI2013 <- read_dta(file.path(dataPath, "Household/sect7_hh_w2.dta"))  # CSI hh_s7q01 hh_s7q02_a hh_s7q02_b hh_s7q02_c hh_s7q02_d hh_s7q02_e hh_s7q02_f hh_s7q02_g hh_s7q02_h
CSI2013 <-CSI2013[ c("household_id", "hh_s7q01", "hh_s7q02_a", "hh_s7q02_b", "hh_s7q02_c", "hh_s7q02_d", 
                     "hh_s7q02_e", "hh_s7q02_f", "hh_s7q02_g", "hh_s7q02_h" )]

#D:\Analyses\CIMMYT\NutritionTZA\SurveyData\2010\Data\TZNPS2HH1DTA
#D:\Analyses\CIMMYT\NutritionTZA\SurveyData\2010\Data\TZNPS2HH1DTA

descriptive.table(vars = d(hh_s7q02_a, hh_s7q02_b, hh_s7q02_c, hh_s7q02_d, hh_s7q02_e, hh_s7q02_f, 
                           hh_s7q02_g, hh_s7q02_h),data= CSI2013, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))


CSI2013$CSI <- 
  CSI2013$hh_s7q02_a*1 + 
  CSI2013$hh_s7q02_b*1 + 
  CSI2013$hh_s7q02_c*1 + 
  CSI2013$hh_s7q02_d*1 + 
  CSI2013$hh_s7q02_e*3 + 
  CSI2013$hh_s7q02_f*2 + 
  CSI2013$hh_s7q02_g*0 + 
  CSI2013$hh_s7q02_h*4

CSI2013$rCSI <- 
  CSI2013$hh_s7q02_a*1 + 
  CSI2013$hh_s7q02_c*1 + 
  CSI2013$hh_s7q02_d*1 + 
  CSI2013$hh_s7q02_e*3 + 
  CSI2013$hh_s7q02_f*2  

descriptive.table(vars = d(hh_s7q02_a, hh_s7q02_b, hh_s7q02_c, hh_s7q02_d, hh_s7q02_e, hh_s7q02_f, 
                           hh_s7q02_g, hh_s7q02_h, CSI, rCSI),data= CSI2013, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))

CSI2013 <- CSI2013[ c("household_id", "CSI", "rCSI")]

FNS2013 <- left_join(FNS2013, CSI2013)

# ***************************************************************************************************
# Descriptive statistics of FNS indicators in 2013
# ***************************************************************************************************
# Histograms of nutrition indicators: DDS
hist(FNS2013$DDS12, freq = FALSE, ylim = c(0, 0.2), xlab="DDS", ylab="%", main="Frequency in 2013")

# Histograms of nutrition indicators: FVS
hist(FNS2013$FVS16, freq = FALSE, ylim = c(0, 0.2), xlab="FVS", ylab="%", main="Frequency in 2013")

# Histograms of nutrition indicators: FCS weighted
hist(FNS2013$FCS16w, freq = FALSE, ylim = c(0, 0.2), xlab="FCS weighted", ylab="%", main="Frequency in 2013")

# Histograms of nutrition indicators: FCS unweighted
hist(FNS2013$FCS16u, freq = FALSE, ylim = c(0, 0.2), xlab="FCS unweighted", ylab="%", main="Frequency in 2013")

# Histograms of nutrition indicators: CSI
hist(FNS2013$CSI, freq = FALSE, ylim = c(0, 0.2), xlab="CSI", ylab="%", main="Frequency in 2013")

# Histograms of nutrition indicators: rCSI
hist(FNS2013$rCSI, freq = FALSE, ylim = c(0, 0.2), xlab="rCSI", ylab="%", main="Frequency in 2013")


# calculation of correlation coefficent of DDS and FVS
 myvars <- c("DDS12", "FVS16","FCS16w","FCS16u","CSI","rCSI")
FNS2013sub <- FNS2013[myvars]
FNS2013matrix <- cor(FNS2013sub, use="complete.obs", method="pearson")
rm(FNS2013sub, myvars)

# Simple Scatterplot of DDS and FVS
plot(FNS2013$DDS12, FNS2013$FVS16, main="Coherence between DDS and FVS in 2013", 
     xlab="DDS ", ylab="FVS ", pch=19) 

# Simple Scatterplot of DDS and FVS
plot(FNS2013$FCS16w, FNS2013$FCS16u, main="Coherence between FCS (weighted) and FCS (unweighted) in 2013", 
     xlab="FCS16w ", ylab="FCS16u ", pch=19) 

# Simple Scatterplot of DDS and FVS
plot(FNS2013$CSI, FNS2013$rCSI, main="Coherence between CSI and reduced CSI in 2013", 
     xlab="CSI ", ylab="rCSI ", pch=19) 


# Histograms of nutrition indicators: rCSI
hist(FNS2013$rCSI, freq = FALSE, ylim = c(0, 0.2), xlab="rCSI", ylab="%", main="Frequency in 2013")


library(ggplot2)
plot=qplot(rCSI, data=FNS2013, geom="histogram") 
ggsave(plot,file="Results/graph1.pdf")

#Several plots in one PDF
pdf(file = "hist_and_plots.pdf")
## set up the new plotting device (pdf)
par(mfrow = c(2,2))
## draw the plot
hist(iris$Sepal.Length, main = "Plot 1")
plot(iris$Petal.Length, iris$Petal.Width, main = "Plot 2")
plot(iris$Sepal.Length, iris$Petal.Length, main = "Plot 3")
plot(iris$Sepal.Width, iris$Petal.Width, main = "Plot 4")
## close the device to do the drawing
dev.off()


