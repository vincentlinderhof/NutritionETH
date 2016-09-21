# Tom
# dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data"

# LEI Path
# dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Data"

# ACT: Tom has to change his dataPath

rm(CSI2011, CSI2013, CSIdata, DDS12, FNS2011, FOOD2011, FOOD2013, NUTR2011, NUTR2013)

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data"
} else {
  dataPath <- "D:/Analyses/CIMMYT/NutritionETH/SurveyData/2011/Data"
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
library(ggplot2)

options(scipen=999)
rm(path, plot, regions_nozero) 


# ***************************************************************************************************
#Creation of FVS
# ***************************************************************************************************

#FOOD2011 <- read_dta(file.path(dataPath, "sect7_hh_w1.dta"))  # CSI saq08 hh_s7q02_a to hh_s7q02_h
#FOOD2011 <- read_dta(file.path(dataPath, "sect5b_hh_w1.dta"))  # DDS and FCS hh_s5aq00 hh_s5aq0a hh_s5aq01 hh_s5aq02_a hh_s5aq02_b
#FOOD2011 <- read_dta(file.path(dataPath, "sect5a_hh_w1.dta"))  # FVS and DDS hh_s5aq00 hh_s5aq0a hh_s5aq01 hh_s5aq02_a hh_s5aq02_b

FOOD2011 <- read_dta(file.path(dataPath, "sect5b_hh_w1.dta"))  # FVS and DDS hh_s5aq00 hh_s5aq0a hh_s5aq01 hh_s5aq02_a hh_s5aq02_b
FOOD2011 <- subset(FOOD2011, select=c(household_id, hh_s5bq00, hh_s5bq0a, hh_s5bq01, hh_s5bq02))

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

#aggregate(FOOD2011, by=(FOOD2011$hh_s5bq00), FUN=count, na.rm=TRUE)


# Construct dummy variables for food items: do not use as it produces wrong results
#NUTR2011 <-
#  mutate(FOOD2011, count = ifelse(hh_s5bq01 == 1, hh_s5bq02, ifelse(NA))) %>%
#  group_by(household_id) %>%
#  spread(hh_s5bq00, count) %>%
#  filter (! duplicated(household_id)) %>%
#  replace(is.na(.), 0)
#NUTR2011CH <- NUTR2011[ c(1,2,3,4) ]
#NUTR2011 <- NUTR2011[ -c(2,3,4) ]
#summary(NUTR2011CH)

FOOD2011$FI01_Enjera       <- 1*(FOOD2011$hh_s5bq00==1 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI02_OtherCereals <- 1*(FOOD2011$hh_s5bq00==2 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI03_Potatoes     <- 1*(FOOD2011$hh_s5bq00==3 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI04_Pasta        <- 1*(FOOD2011$hh_s5bq00==4 & FOOD2011$hh_s5bq01==1)
 
FOOD2011$FI05_Sugar         <- 1*(FOOD2011$hh_s5bq00==5 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI06_PulsesandNuts <- 1*(FOOD2011$hh_s5bq00==6 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI07_Vegetables    <- 1*(FOOD2011$hh_s5bq00==7 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI08_Fruits        <- 1*(FOOD2011$hh_s5bq00==8 & FOOD2011$hh_s5bq01==1)

FOOD2011$FI09_RedMeat <- 1*(FOOD2011$hh_s5bq00==9 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI10_Poultry <- 1*(FOOD2011$hh_s5bq00==10 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI11_Eggs    <- 1*(FOOD2011$hh_s5bq00==11 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI12_Fish    <- 1*(FOOD2011$hh_s5bq00==12 & FOOD2011$hh_s5bq01==1)

FOOD2011$FI13_FatsandOils   <- 1*(FOOD2011$hh_s5bq00==13 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI14_DairyProducts <- 1*(FOOD2011$hh_s5bq00==14 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI15_Condiments    <- 1*(FOOD2011$hh_s5bq00==15 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI16_KochoandBula  <- 1*(FOOD2011$hh_s5bq00==16 & FOOD2011$hh_s5bq01==1)

NUTR2011a <- aggregate(FI01_Enjera       ~ household_id, FOOD2011, sum)
NUTR2011 <- NUTR2011a; rm(NUTR2011a)
NUTR2011a <- aggregate(FI02_OtherCereals ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)
NUTR2011a <- aggregate(FI03_Potatoes     ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)
NUTR2011a <- aggregate(FI04_Pasta        ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)

NUTR2011a <- aggregate(FI05_Sugar         ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)
NUTR2011a <- aggregate(FI06_PulsesandNuts ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)
NUTR2011a <- aggregate(FI07_Vegetables    ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)
NUTR2011a <- aggregate(FI08_Fruits        ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)

NUTR2011a <- aggregate(FI09_RedMeat ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)
NUTR2011a <- aggregate(FI10_Poultry ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)
NUTR2011a <- aggregate(FI11_Eggs    ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)
NUTR2011a <- aggregate(FI12_Fish    ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)

NUTR2011a <- aggregate(FI13_FatsandOils   ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)
NUTR2011a <- aggregate(FI14_DairyProducts ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)
NUTR2011a <- aggregate(FI15_Condiments    ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)
NUTR2011a <- aggregate(FI16_KochoandBula  ~ household_id, FOOD2011, sum)
NUTR2011 <- left_join(NUTR2011, NUTR2011a); rm(NUTR2011a)

NUTR2011$FVS16 <- rowSums(NUTR2011[2:17])

# descriptives of food group dummy variables and FVS and DDS
ds_fvs <- descriptive.table(vars = d(FI01_Enjera, FI02_OtherCereals, FI03_Potatoes, FI04_Pasta, FI05_Sugar, FI06_PulsesandNuts,
                           FI07_Vegetables, FI08_Fruits, FI09_RedMeat, FI10_Poultry, FI11_Eggs, FI12_Fish, 
                           FI13_FatsandOils, FI14_DairyProducts, FI15_Condiments, FI16_KochoandBula, FVS16),data= NUTR2011, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))

FNS2011 <- NUTR2011[ c("household_id", "FVS16") ]

# ***************************************************************************************************
#Construction of DDS, uses the data of the FVC construction!
# ***************************************************************************************************

# Columns correspond to list of food items!
# sum fooditems into 12 foodgroups for FVS: columns correspond to list of food items!
NUTR2011$cereals         <- 1*((rowSums(NUTR2011[ c("FI01_Enjera", "FI02_OtherCereals", "FI04_Pasta") ] ) ) > 0 ) 
NUTR2011$rootsandtubers  <- 1*((rowSums(NUTR2011[ c("FI03_Potatoes", "FI16_KochoandBula") ] ))  > 0 ) 
NUTR2011$vegetables      <- 1*((NUTR2011[ c("FI07_Vegetables") ] )  > 0 ) 
NUTR2011$fruits          <- 1*((NUTR2011[ c("FI08_Fruits") ] )  > 0 ) 
NUTR2011$meat            <- 1*((rowSums(NUTR2011[ c("FI09_RedMeat", "FI10_Poultry")] ) ) > 0 ) 
NUTR2011$eggs            <- 1*((NUTR2011[ c("FI11_Eggs") ] )  > 0 ) 
NUTR2011$fish            <- 1*((NUTR2011[ c("FI12_Fish") ] )  > 0 ) 
NUTR2011$pulsesandnuts   <- 1*((NUTR2011[ c("FI06_PulsesandNuts") ] )  > 0 ) 
NUTR2011$dairyproducts   <- 1*((NUTR2011[ c("FI14_DairyProducts") ] )  > 0 ) 
NUTR2011$oilsandfats     <- 1*((NUTR2011[ c("FI13_FatsandOils") ] )  > 0 ) 
NUTR2011$condiments      <- 1*((NUTR2011[ c("FI15_Condiments") ] )  > 0 ) 
NUTR2011$sugar           <- 1*((NUTR2011[ c("FI05_Sugar") ] )  > 0 ) 

#install.packages(Hmisc)
#library(Hmisc)
#label(NUTR2011ALL$cereals) <- "FG Cereals" 

NUTR2011$DDS12 <- rowSums(NUTR2011[ c("cereals", "rootsandtubers", "vegetables",
                                          "fruits", "meat", "eggs", "fish",
                                          "pulsesandnuts", "dairyproducts", "oilsandfats",
                                          "sugar","condiments")] ) 
DDS2011 <- NUTR2011[ c("household_id", "DDS12") ] 

FNS2011 <-left_join(FNS2011, DDS2011) 
rm(DDS2011) 

#NUTR2011$DDS1200               <- rowSums(NUTR2011[19:30] )
#summary(NUTR2011$DDS1200)

# descriptives of food group dummy variables and FVS and DDS
ds_dds <- descriptive.table(vars = d(cereals, rootsandtubers, vegetables, fruits, meat, eggs, fish,
                           pulsesandnuts, dairyproducts, oilsandfats, sugar, condiments, DDS12, FVS16),data= NUTR2011, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))


# ***************************************************************************************************
#Construction of FCS
# ***************************************************************************************************
FOOD2011$hh_s5bq02t <- ifelse(is.na(FOOD2011$hh_s5bq02),0,FOOD2011$hh_s5bq02)
  
FOOD2011$FI01_Enjera       <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==1 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI02_OtherCereals <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==2 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI03_Potatoes     <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==3 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI04_Pasta        <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==4 & FOOD2011$hh_s5bq01==1)

FOOD2011$FI05_Sugar         <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==5 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI06_PulsesandNuts <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==6 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI07_Vegetables    <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==7 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI08_Fruits        <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==8 & FOOD2011$hh_s5bq01==1)

FOOD2011$FI09_RedMeat <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==9 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI10_Poultry <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==10 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI11_Eggs    <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==11 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI12_Fish    <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==12 & FOOD2011$hh_s5bq01==1)

FOOD2011$FI13_FatsandOils   <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==13 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI14_DairyProducts <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==14 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI15_Condiments    <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==15 & FOOD2011$hh_s5bq01==1)
FOOD2011$FI16_KochoandBula  <- FOOD2011$hh_s5bq02t*(FOOD2011$hh_s5bq00==16 & FOOD2011$hh_s5bq01==1)

NUTR2011 <- aggregate(FOOD2011, by=list(FOOD2011$household_id), FUN=max )


NUTR2011$FCS16w <- 
  NUTR2011$FI01_Enjera*2 + 
  NUTR2011$FI02_OtherCereals*2 + 
  NUTR2011$FI03_Potatoes*2 + 
  NUTR2011$FI04_Pasta*2 + 
  NUTR2011$FI05_Sugar*0.5 + 
  NUTR2011$FI06_PulsesandNuts*3 + 
  NUTR2011$FI07_Vegetables*1 + 
  NUTR2011$FI08_Fruits*1 + 
  NUTR2011$FI09_RedMeat*4 +
  NUTR2011$FI10_Poultry*4 +
  NUTR2011$FI11_Eggs*4 +
  NUTR2011$FI12_Fish*4 +
  NUTR2011$FI13_FatsandOils*0.5 +
  NUTR2011$FI14_DairyProducts*4 +
  NUTR2011$FI15_Condiments*0 +
  NUTR2011$FI16_KochoandBula*2

NUTR2011$FCS16u <- 
  NUTR2011$FI01_Enjera + 
  NUTR2011$FI02_OtherCereals + 
  NUTR2011$FI03_Potatoes + 
  NUTR2011$FI04_Pasta + 
  NUTR2011$FI05_Sugar + 
  NUTR2011$FI06_PulsesandNuts + 
  NUTR2011$FI07_Vegetables + 
  NUTR2011$FI08_Fruits + 
  NUTR2011$FI09_RedMeat +
  NUTR2011$FI10_Poultry +
  NUTR2011$FI11_Eggs +
  NUTR2011$FI12_Fish +
  NUTR2011$FI13_FatsandOils +
  NUTR2011$FI14_DairyProducts +
  NUTR2011$FI15_Condiments +
  NUTR2011$FI16_KochoandBula 

FCS2011 <- NUTR2011[ c("household_id", "FCS16w", "FCS16u")]
FNS2011 <- left_join(FNS2011, FCS2011)
rm(FCS2011)
#rm(NUTR2010, NUTR2011ALL, NUTR2011b, NUTR2011c, NUTR2011CH, NUTR2011d)

# descriptives of food group dummy variables and FVS and DDS
library("Deducer")
ds_fcs <- descriptive.table(vars = d(FI01_Enjera ,  FI02_OtherCereals ,  FI03_Potatoes , 
                           FI04_Pasta ,  FI05_Sugar ,  FI06_PulsesandNuts , 
                           FI07_Vegetables ,  FI08_Fruits ,  FI09_RedMeat ,
                           FI10_Poultry ,  FI11_Eggs , FI12_Fish ,
                           FI13_FatsandOils ,  FI14_DairyProducts ,  FI15_Condiments ,  FI16_KochoandBula,
                           FCS16w, FCS16u),data= NUTR2011, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))


# ***************************************************************************************************
#Construction of CSI
# ***************************************************************************************************
CSI2011 <- read_dta(file.path(dataPath, "sect7_hh_w1.dta"))  # CSI hh_s7q01 hh_s7q02_a hh_s7q02_b hh_s7q02_c hh_s7q02_d hh_s7q02_e hh_s7q02_f hh_s7q02_g hh_s7q02_h
CSI2011 <-CSI2011[ c("household_id", "hh_s7q01", "hh_s7q02_a", "hh_s7q02_b", "hh_s7q02_c", "hh_s7q02_d", 
                     "hh_s7q02_e", "hh_s7q02_f", "hh_s7q02_g", "hh_s7q02_h" )]

#D:\Analyses\CIMMYT\NutritionTZA\SurveyData\2010\Data\TZNPS2HH1DTA
#D:\Analyses\CIMMYT\NutritionTZA\SurveyData\2010\Data\TZNPS2HH1DTA

descriptive.table(vars = d(hh_s7q02_a, hh_s7q02_b, hh_s7q02_c, hh_s7q02_d, hh_s7q02_e, hh_s7q02_f, 
                           hh_s7q02_g, hh_s7q02_h),data= CSI2011, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))


CSI2011$CSI <- 
  CSI2011$hh_s7q02_a*1 + 
  CSI2011$hh_s7q02_b*1 + 
  CSI2011$hh_s7q02_c*1 + 
  CSI2011$hh_s7q02_d*1 + 
  CSI2011$hh_s7q02_e*3 + 
  CSI2011$hh_s7q02_f*2 + 
  CSI2011$hh_s7q02_g*0 + 
  CSI2011$hh_s7q02_h*4

CSI2011$rCSI <- 
  CSI2011$hh_s7q02_a*1 + 
  CSI2011$hh_s7q02_c*1 + 
  CSI2011$hh_s7q02_d*1 + 
  CSI2011$hh_s7q02_e*3 + 
  CSI2011$hh_s7q02_f*2  


ds_csi <- descriptive.table(vars = d(hh_s7q02_a, hh_s7q02_b, hh_s7q02_c, hh_s7q02_d, hh_s7q02_e, hh_s7q02_f, 
                           hh_s7q02_g, hh_s7q02_h, CSI, rCSI),data= CSI2011, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Skew","Valid N"))

CSI2011 <- CSI2011[ c("household_id", "CSI", "rCSI")]

FNS2011 <- left_join(FNS2011, CSI2011)

saveRDS(FNS2011, "Data/FNS2011.rds")
ETH2011HH <- left_join(ETH2011HH, FNS2011)

# ***************************************************************************************************
# Descriptive statistics of FNS indicators in 2011
# ***************************************************************************************************
# Histograms of nutrition indicators: DDS
#
qplot(DDS12, data=FNS2011, geom="histogram", bins=13, xlab="DDS", ylab="unit", main="Frequency in 2011") 
plot=qplot(DDS12, data=FNS2011, geom="histogram", bins=13, xlab="DDS", ylab="%", main="Frequency in 2011") 
ggsave(plot,file="Results/histrogram_DDS12.pdf")

# Histograms of nutrition indicators: FVS
qplot(FVS16, data=FNS2011, geom="histogram", bins=16, xlab="FVS", ylab="unit", main="Frequency in 2011") 
plot=qplot(FVS16, data=FNS2011, geom="histogram", bins=16, xlab="FVS", ylab="unit", main="Frequency in 2011") 
ggsave(plot,file="Results/histrogram_FVS16.pdf")
#hist(FNS2011$FVS16, freq = FALSE, ylim = c(0, 0.2), xlab="FVS", ylab="%", main="Frequency in 2011")

# Histograms of nutrition indicators: FCS weighted
#plot=hist(FNS2011$FCS16w, freq = FALSE, ylim = bins=70, xlab="FCS weighted", ylab="%", main="Frequency in 2011")
plot=qplot(FCS16w, data=FNS2011, geom="histogram", bins=70, xlab="FCS (weighted)", ylab="unit", main="Frequency in 2011") 
ggsave(plot,file="Results/histrogram_FCSw16.pdf")

# Histograms of nutrition indicators: FCS unweighted
#hist(FNS2011$FCS16u, freq = FALSE, ylim = c(0, 0.2), xlab="FCS unweighted", ylab="%", main="Frequency in 2011")
plot=qplot(FCS16u, data=FNS2011, geom="histogram", bins=70, xlab="FCS (unweighted)", ylab="unit", main="Frequency in 2011") 
ggsave(plot,file="Results/histrogram_FCSu16.pdf")

# Histograms of nutrition indicators: CSI
hist(FNS2011$CSI, freq = FALSE, ylim = c(0, 0.2), xlab="CSI", ylab="%", main="Frequency in 2011")

# Histograms of nutrition indicators: rCSI
hist(FNS2011$rCSI, freq = FALSE, ylim = c(0, 0.2), xlab="rCSI", ylab="%", main="Frequency in 2011")


# calculation of correlation coefficent of DDS and FVS
myvars <- c("DDS12", "FVS16","FCS16w","FCS16u","CSI","rCSI")
FNS2011sub <- FNS2011[myvars]
FNS2011matrix <- cor(FNS2011sub, use="complete.obs", method="pearson")
rm(FNS2011sub, myvars)

# Simple Scatterplot of DDS and FVS
plot(FNS2011$DDS12, FNS2011$FVS16, main="Coherence between DDS and FVS in 2011", 
     xlab="DDS ", ylab="FVS ", pch=19) 

# Simple Scatterplot of DDS and FVS
plot(FNS2011$FCS16w, FNS2011$FCS16u, main="Coherence between FCS (weighted) and FCS (unweighted) in 2011", 
     xlab="FCS16w ", ylab="FCS16u ", pch=19) 

# Simple Scatterplot of DDS and FVS
plot(FNS2011$CSI, FNS2011$rCSI, main="Coherence between CSI and reduced CSI in 2011", 
     xlab="CSI ", ylab="rCSI ", pch=19) 

write.csv(ds_dds, "results/descriptives_dds.csv")
write.csv(ds_fcs, "results/descriptives_fcs.csv")
write.csv(ds_csi, "results/descriptives_csi.csv")
write.csv(ds_fvs, "results/descriptives_fvs.csv")

## Add it to the database
# Household level
TZA2010 <- left_join(TZA2010, FNS2010)
#TZA2008 <- left_join(TZA2008, FNS2008); rm(FNS2008)
## Add it to the database
# Household level
TZA2008 <- left_join(TZA2008, DDS); rm(DDS)
TZA2008 <- left_join(TZA2008, FVS); rm(FVS)
TZA2008 <- left_join(TZA2008, FCSw); rm(FCSw)
TZA2008 <- left_join(TZA2008, FCSu); rm(FCSu)
TZA2008 <- left_join(TZA2008, CSI); rm(CSI)
dd_dds
dd_fvs
