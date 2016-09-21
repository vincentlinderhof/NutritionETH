#'Multiple imputation code: obtain 2011 GPS areas through multiple imputation as proposed 
#'by Palacios-Lopez & Djima (paper not available online)

library(foreign)
library(mice)

setwd('C:/Users/Jasper/Documents/LEI - internship') #working from home
setwd('M:/My Documents/LEI - internship')           #working from LEI

#--------------------------------------------------------------------
# initiate data frame with self reported areas and GPS measured areas
# the GPS areas have many NAs, which will be imputated
#--------------------------------------------------------------------
parcel       <- read.dta("Data/Ethiopia 2011/sect2_pp_w1.dta")
flds         <- read.dta("Data/Ethiopia 2011/sect3_pp_w1.dta")
conv         <- read.dta("Data/Ethiopia 2011/ET_local_area_unit_conversion.dta")

flds <- left_join(flds, parcel)
flds$plot_id <- paste(flds$holder_id, flds$parcel_id, flds$field_id, sep="")

field_area <- dplyr::select(flds, household_id, parcel_id, field_id, plot_id, holder_id, irrig=pp_s3q12, fert=pp_s3q14, pop_weight=pw,
                             region=saq01, zone=saq02, woreda=saq03, area=pp_s3q02_d, local_unit=pp_s3q02_c, gps=pp_s3q05_c)
field_area <- arrange(merge(field_area, conv, by = c("region","zone","woreda","local_unit"), all=TRUE), plot_id)
field_area <- unique(field_area[!duplicated(field_area$plot_id),])
field_area$conversion[is.na(field_area$conversion)] <- 0
rownames(field_area) <- NULL

#The measurement unit is square meters (not ha as in main dataset)
field_area$conversion[field_area$local_unit=="Square Meters"] <- 1
field_area$conversion[field_area$local_unit=="Hectare"] <- 10000
field_area$conversion[field_area$local_unit=="Boy" & field_area$crops>0] <- 291.4066
field_area$conversion[field_area$local_unit=="Timad" & field_area$crops>0] <- 1609.978
field_area$conversion[field_area$local_unit=="Kert" & field_area$crops>0] <- 1986.144
field_area$conversion[field_area$local_unit=="Senga" & field_area$crops>0] <- 1197.805
field_area$conversion[field_area$local_unit=="Other" & field_area$crops>0] <- 1
field_area$area2 <- round((field_area$area * field_area$conversion),3)

#Creating imputation data frame
imputation <- data.frame(household_id = field_area$household_id, holder_id = field_area$holder_id, plot_id = field_area$plot_id, region = field_area$region,
                         parcel_id = field_area$parcel_id,       field_id = field_area$field_id,         sr_area = field_area$area2/10000,
                         gps      = field_area$gps/10000,        weight = field_area$pop_weight)

#Plot manager characteristics
#----------------------------
hh11         <- read.dta("Data/Ethiopia 2011/sect1_hh_w1.dta")
edu          <- read.dta("Data/Ethiopia 2011/sect2_hh_w1.dta")

demographic <- merge(hh11, edu)
manager_data <- dplyr::select(demographic, household_id, holder_id = individual_id, 
                              gender = hh_s1q03, headage = hh_s1q04_a, edu_grade = hh_s2q05)
manager_data$holdergender[manager_data$gender=="Male"] <- 0
manager_data$holdergender[manager_data$gender=="Female"] <- 1

#Education grade: translate into years of followed education based on education codes
#------------------------------------------------------------------------------------
manager_data$edu_grade <- as.numeric(manager_data$edu_grade)
manager_data$edu_grade[manager_data$edu_grade==21] <- 9
manager_data$edu_grade[manager_data$edu_grade==22] <- 10
manager_data$edu_grade[manager_data$edu_grade==23] <- 11
manager_data$edu_grade[manager_data$edu_grade==24] <- 12
manager_data$edu_grade[manager_data$edu_grade==25] <- 11
manager_data$edu_grade[manager_data$edu_grade==26] <- 12
manager_data$edu_grade[manager_data$edu_grade==27] <- 12
manager_data$edu_grade[manager_data$edu_grade==28] <- 11
manager_data$edu_grade[manager_data$edu_grade==29] <- 12
manager_data$edu_grade[manager_data$edu_grade==30] <- 13
manager_data$edu_grade[manager_data$edu_grade==31] <- 13
manager_data$edu_grade[manager_data$edu_grade==32] <- 14
manager_data$edu_grade[manager_data$edu_grade==33] <- 15
manager_data$edu_grade[manager_data$edu_grade==34] <- 15
manager_data$edu_grade[manager_data$edu_grade==35] <- 16
manager_data$edu_grade[manager_data$edu_grade==93] <- 2
manager_data$edu_grade[manager_data$edu_grade==94] <- 3
manager_data$edu_grade[manager_data$edu_grade==95] <- 3
manager_data$edu_grade[manager_data$edu_grade==96] <- 2
manager_data$edu_grade[manager_data$edu_grade==98] <- 0
manager_data <- dplyr::select(manager_data, household_id, holder_id, headage, edu_grade, holdergender)

#Household Characteristics
#------------------------------
household_data <- dplyr::select(demographic, rural, household_id, gender = hh_s1q03, age = hh_s1q04_a, months = hh_s1q04_b)
by_household <- group_by(household_data, household_id)

rural <- dplyr::select(demographic, household_id, setting = rural)
rural <- unique(rural)
rownames(rural) <- NULL

rural$rural_residence[rural$setting=="rural"] <- 1
rural$rural_residence[rural$setting=="small town"] <- 0
rural <- dplyr::select(rural, household_id, rural_residence)

household_characteristics <- data.frame(summarise(by_household,
                                                  hh_members_0_5 = length(household_id[age<6 & months<200]),
                                                  hh_members_6_14 = length(household_id[age>=6 & age<15]),
                                                  hh_males_15_39 = length(household_id[age>=15 & age<40 & gender=="Male"]),
                                                  hh_males_40_59 = length(household_id[age>=40 & age<60 & gender=="Male"]),
                                                  hh_females_15_39 = length(household_id[age>=15 & age<40 & gender=="Female"]),
                                                  hh_females_40_59 = length(household_id[age>=40 & age<60 & gender=="Female"]),
                                                  hh_members_60 = length(household_id[age>=60])))                                               
household_characteristics <- merge(household_characteristics, rural)
                                                   
#---------------------
#Plot characteristics
#---------------------
plot_data <- left_join(flds, parcel)

plot_data <- dplyr::select(plot_data, household_id, holder_id, parcel_id, plot_id, field_id, plots = pp_s2q02,
                           own= pp_s2q04, rentout = pp_s2q10, fallow = pp_s3q03, irrig = pp_s3q12, urea = pp_s3q15, dap = pp_s3q18,
                           manure = pp_s3q21, compost = pp_s3q23, menlaborhire = pp_s3q28_a, womenlaborhire = pp_s3q28_d,
                           childlaborhire = pp_s3q28_g)

plot_data[6:17] <- sapply(plot_data[6:17], as.character)

plot_data$ownplot[plot_data$own=="Yes"] <- 1
plot_data$ownplot[plot_data$own=="No"] <- 0
plot_data$rented_out[plot_data$rentout=="Yes"] <- 1
plot_data$rented_out[plot_data$rentout=="No"] <- 0
plot_data$fallowplot[plot_data$fallow=="Fallow"] <- 1
plot_data$fallowplot[plot_data$fallow!="Fallow"] <- 0
plot_data$irrigation[plot_data$irrig=="Yes"] <- 1
plot_data$irrigation[plot_data$irrig=="No"] <- 0
plot_data$inorganicfert[plot_data$urea=="Yes" | plot_data$dap=="Yes"] <- 1
plot_data$inorganicfert[plot_data$urea!="Yes" | plot_data$dap!="Yes"] <- 0
plot_data$organicfert[plot_data$compost=="Yes" | plot_data$manure=="Yes"] <- 1
plot_data$organicfert[plot_data$compost!="Yes" | plot_data$manure!="Yes"] <- 0

plot_data[15:17] <- sapply(plot_data[15:17], as.numeric)

plot_data$hiredlabour[plot_data$menlaborhire>0 | plot_data$womenlaborhire>0 | plot_data$childlaborhire>0] <- 1
plot_data$hiredlabour[plot_data$menlaborhire<1 | plot_data$womenlaborhire<1 | plot_data$childlaborhire<1] <- 0

plot_data <- dplyr::select(plot_data, plot_id, plots, ownplot, rented_out, fallowplot, irrigation, inorganicfert, organicfert, hiredlabour)

#----------------------------------
#HH Wealth index
#----------------------------------
housing      <- read.dta("Data/Ethiopia 2011/sect9_hh_w1.dta")
assets       <- read.dta("Data/Ethiopia 2011/sect10_hh_w1.dta")

assetdata <- dplyr::select(assets, household_id, hh_s10q00, hh_s10q01)

television <- dplyr::filter(assetdata, hh_s10q00=="Television")
refrigerator <- dplyr::filter(assetdata, hh_s10q00=="Refridgerator")
phone <- dplyr::filter(assetdata, hh_s10q00=="Mobile telephone")
car <- dplyr::filter(assetdata, hh_s10q00=="Private car")
bicycle <- dplyr::filter(assetdata, hh_s10q00=="Bicycle")

wealthindex <- data.frame(household_id = unique(assetdata$household_id))

wealthindex$television[television$hh_s10q01>0] <- 0.798552
wealthindex$television[television$hh_s10q01<1] <- 0
wealthindex$refrigerator[refrigerator$hh_s10q01>0] <- 0.781531
wealthindex$refrigerator[refrigerator$hh_s10q01<1] <- 0
wealthindex$phone[phone$hh_s10q01>0] <- 0.660869
wealthindex$phone[phone$hh_s10q01<1] <- 0
wealthindex$car[car$hh_s10q01>0] <- 0.431269
wealthindex$car[car$hh_s10q01<1] <- 0
wealthindex$bicycle[bicycle$hh_s10q01>0] <- 0.171238
wealthindex$bicycle[bicycle$hh_s10q01<1] <- 0
houseindicators <- dplyr::select(housing, household_id, rooms = hh_s9q04, floor = hh_s9q07, toilet=hh_s9q10, watersource = hh_s9q13, electricity=hh_s9q20) 

wealthindex$floorquality[houseindicators$floor=="Mud / dung"] <- -0.700809
wealthindex$floorquality[houseindicators$floor=="Cement screed" |
                           houseindicators$floor=="Reed / bamboo" | 
                           houseindicators$floor=="Wood planks"] <- 0.113815

wealthindex$floorquality[houseindicators$floor=="Parquet of polished wood" |
                           houseindicators$floor=="Plastic tiles" | 
                           houseindicators$floor=="Cement tiles" | 
                           houseindicators$floor=="Brick Tiles" | 
                           houseindicators$floor=="Ceramic / marble tiles"] <- 0.566271 

wealthindex$toiletquality[houseindicators$toilet=="Field / forest" |
                          houseindicators$toilet=="Pit lantreen, shared not ventilated" | 
                          houseindicators$toilet=="Pit latreen, private ventilated" | 
                          houseindicators$toilet=="Pit lantreen, shared ventilated" | 
                          houseindicators$toilet=="Pit lantreen, private not ventilated" | 
                          houseindicators$toilet=="Bucket"] <- -7.439841
wealthindex$toiletquality[houseindicators$toilet=="Flush toilet, shared"] <- -1.090393
wealthindex$toiletquality[houseindicators$toilet=="Flush toilet, private"] <- 8.140637
                          
wealthindex$waterquality[houseindicators$watersource=="Rain water" |
                           houseindicators$watersource=="River / lake / pound" |
                           houseindicators$watersource=="Unprotected well / spring"] <- -0.584726
wealthindex$waterquality[houseindicators$watersource=="Protected well / spring, shared" |
                           houseindicators$watersource=="Protected well / spring, private" | 
                           houseindicators$watersource=="Communal tap outside compound"] <- -0.213440
wealthindex$waterquality[houseindicators$watersource=="Water from kiosk/retailer" | 
                            houseindicators$watersource=="Private tap in the compound" | 
                            houseindicators$watersource=="Shared tap in compound" |
                            houseindicators$watersource=="Tap inside the house"] <- 0.737338

wealthindex$electricityaccess[houseindicators$electricity=="Don't use electricity"] <- 0 
wealthindex$electricityaccess[houseindicators$electricity!="Don't use electricity"] <- 0.747001 

indexweight <- data.frame(c("television","refrigerator","phone","car","bicycle","floorquality","toiletquality","rooms","electricityaccess","waterquality"))
                          
wealthindex$HHwealthindex <- rowSums(wealthindex[2:10])*100

wealth_index <- dplyr::select(wealthindex, household_id, HHwealthindex)

#---------------------------
#Farm implement access index
#---------------------------
#Seems to indicate the use of sickles, slahers, carts 
asset_cart       <- dplyr::filter(assetdata, hh_s10q00=="Cart (hand pushed)")
asset_animalcart <- dplyr::filter(assetdata, hh_s10q00=="Cart (animal drawn)")
asset_sickle     <- dplyr::filter(assetdata, hh_s10q00=="Sickle (Machid)")
asset_axe        <- dplyr::filter(assetdata, hh_s10q00=="Axe (Gejera)")
asset_pickaxe    <- dplyr::filter(assetdata, hh_s10q00=="Pick Axe (Geso)")
asset_plough     <- dplyr::filter(assetdata, hh_s10q00=="Plough (traditional)")
asset_ploughmod  <- dplyr::filter(assetdata, hh_s10q00=="Plough (modern)")
asset_waterpump  <- dplyr::filter(assetdata, hh_s10q00=="Water Pump")

implementindex <- data.frame(household_id = unique(assetdata$household_id))

implementindex$cart[asset_cart$hh_s10q01<1] <- 0
implementindex$cart[asset_cart$hh_s10q01>0] <- 5
implementindex$animalcart[asset_animalcart$hh_s10q01<1] <- 0
implementindex$animalcart[asset_animalcart$hh_s10q01>0] <- 10
implementindex$sickle[asset_sickle$hh_s10q01<1] <- 0
implementindex$sickle[asset_sickle$hh_s10q01>0] <- 5
implementindex$axe[asset_axe$hh_s10q01<1] <- 0
implementindex$axe[asset_axe$hh_s10q01>0] <- 5
implementindex$pickaxe[asset_pickaxe$hh_s10q01<1] <- 0
implementindex$pickaxe[asset_pickaxe$hh_s10q01>0] <- 5
implementindex$plough[asset_plough$hh_s10q01<1] <- 0
implementindex$plough[asset_plough$hh_s10q01>0] <- 5
implementindex$ploughmod[asset_ploughmod$hh_s10q01<1] <- 0
implementindex$ploughmod[asset_ploughmod$hh_s10q01>0] <- 10
implementindex$waterpump[asset_waterpump$hh_s10q01<1] <- 0
implementindex$waterpump[asset_waterpump$hh_s10q01>0] <- 10

implementindex$agri_implement_index <- rowSums(implementindex[2:9])

implement_index <- dplyr::select(implementindex, household_id, agri_implement_index)

#-----------------------
#Off-farm access
#-----------------------
off_farm     <- read.dta("Data/Ethiopia 2011/sect11a_hh_w1.dta")

off_farm_income <- select(off_farm, household_id, off_farmaccess = hh_s11aq09)

off_farm_income$income_off[off_farm_income$off_farmaccess=="Yes"] <- 1
off_farm_income$income_off[off_farm_income$off_farmaccess=="No"] <- 0

off_farm_income <- select(off_farm_income, household_id, income_off)

#------------------------
#Other income (non-labor)
#------------------------
other_inc    <- read.dta("Data/Ethiopia 2011/sect11a_hh_w1.dta")

other_income <- dplyr::select(other_inc, household_id, variable = hh_s11aq01)

other_income$income_othersource[other_income$variable=="Yes"] <- 1
other_income$income_othersource[other_income$variable=="No"] <- 0

other_income <- dplyr::select(other_income, household_id, income_othersource)

imputation <- left_join(imputation, manager_data, by=c("household_id", "holder_id"))
imputation <- left_join(imputation, household_characteristics)
imputation <- left_join(imputation, plot_data, by="plot_id")
imputation <- left_join(imputation, wealth_index)
imputation <- left_join(imputation, implement_index)
imputation <- left_join(imputation, off_farm_income)
imputation <- left_join(imputation, other_income)

#--------------------
#Interaction variables
#--------------------
imputation$sr_area_hh_rural   <- imputation$sr_area * imputation$rural_residence
imputation$sr_area_femalehead <- imputation$sr_area * imputation$holdergender
imputation$sr_area_agehead    <- imputation$sr_area * imputation$headage
imputation$sr_area_yrseduc    <- imputation$sr_area * imputation$edu_grade
imputation$sr_area_plot_own   <- imputation$sr_area * imputation$ownplot
imputation$sr_area_plot_rent  <- imputation$sr_area * imputation$rented_out
imputation$sr_area_plot_fall  <- imputation$sr_area * imputation$fallowplot
#-----------------------------------------------
imputation$obs_id_original <- rownames(imputation)
imputation_original <- imputation
imputation <- dplyr::select(imputation, sr_area, gps, weight, headage, edu_grade, holdergender,
                            hh_members_0_5, hh_members_6_14,  hh_males_15_39,	hh_males_40_59,
                            hh_females_15_39,	hh_females_40_59,	hh_members_60, rural_residence, 
                            plots,  ownplot,	rented_out,	fallowplot,	irrigation,	inorganicfert,
                            organicfert,	hiredlabour,	HHwealthindex,	agri_implement_index,
                            income_off,	income_othersource,	sr_area_hh_rural,	sr_area_femalehead,
                            sr_area_agehead,	sr_area_yrseduc,	sr_area_plot_own,	sr_area_plot_rent,	sr_area_plot_fall, obs_id_original)

imputation$plots <- as.numeric(imputation$plots)

winsor1 <- function(df, var, fraction = 0.05) {
  if (length(fraction) != 1 || fraction < 0 || fraction > 0.1){
    stop('bad choice of fraction!!')
  } 
  
  lim <- quantile(df[, var], probs = 1-fraction, na.rm = TRUE)
  df[, var][df[, var] > lim] <- lim 
  df
}

imputation <- winsor1(imputation, 'sr_area', 0.01)
imputation <- winsor1(imputation, 'gps', 0.01)

imputation$sr_area2 <- imputation$sr_area^2
imputation$sr_area3 <- imputation$sr_area^3

#'There are a couple of observations with missing values for both GPS and self-reported areas. These are removed.
#'This is done similarly for the age of the holder and years of education
bad_sr_area <- is.na(imputation$sr_area) & is.na(imputation$gps)
bad_headage <- is.na(imputation$headage) & is.na(imputation$gps)

imputation <- imputation[!bad_sr_area,]
imputation <- imputation[!bad_headage,]
rownames(imputation) <- NULL

imp <- mice(imputation, imputationMethod=c("","pmm","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""), m=50)
                                                  
keep(imputation, imputation_original, imp, winsor1, sure=TRUE)


gps_imputed <- imp$imp$gps
gps_imputed$mean <- rowSums(gps_imputed[1:ncol(gps_imputed)])/(length(gps_imputed)-1)
gps_imputed$obs_id <- rownames(gps_imputed)
gps_imputed <- dplyr::select(gps_imputed, obs_id, mean)
imputation$obs_id <- rownames(imputation)
complete <- left_join(imputation, gps_imputed, by="obs_id")
complete <- dplyr::select(complete, sr_area, gps, mean, obs_id_original)
complete$area <- as.numeric(ifelse((is.na(complete$gps)), complete$mean, complete$gps))
complete <- select(complete, obs_id_original, area)
imputation_original <- left_join(imputation_original, complete, by="obs_id_original")

#fit <- lm(gps ~ sr_area + sr_area2 + sr_area3 + headage + edu_grade+ holdergender+
#                       hh_members_0_5+ hh_members_6_14+  hh_males_15_39+    hh_males_40_59+
#                       hh_females_15_39+  hh_females_40_59+	hh_members_60+ rural_residence+ 
#                       plots+  ownplot+	rented_out+	fallowplot+	irrigation+	inorganicfert+
#                       organicfert+	hiredlabour+	HHwealthindex+	agri_implement_index+
#                       income_off+	income_othersource +	sr_area_hh_rural+	sr_area_femalehead+
#                       sr_area_agehead+	sr_area_yrseduc+	sr_area_plot_own+	sr_area_plot_rent+	sr_area_plot_fall, data=imputation)


c <- dplyr::select(imputation_original, household_id, holder_id, parcel_id, field_id, plot_id, region, sr_area, gps, area)
write.csv(c, "imputed_area2011.csv", row.names=TRUE)
