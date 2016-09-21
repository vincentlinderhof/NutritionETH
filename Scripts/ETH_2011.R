# -------------------------------------
# Ethiopia data 2011 - 2012 survey
# -------------------------------------

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/ETH/2011/Data"
} else {
  dataPath <- "D:/Analyses/CIMMYT/NutritionETH/SurveyData/2011/Data/"
}
setwd("D:/Analyses/CIMMYT/NutritionETH")

library(haven)
library(foreign)
#install.packages("lubridate")
library(lubridate)
#install.packages("magrittr")
library(magrittr)
library(Deducer) # necessary for descriptives.tables
library(dplyr)
options(scipen=999)

#######################################
############## LOCATION ###############
#######################################

# file containing the zone, region and district

location <- read_dta(file.path(dataPath, "sect1_hh_w1.dta"))
location <- location[ c("household_id", "saq01", "saq02", "ea_id", "rural") ]
location$REGCODE <- location$saq01
location$ZONECODE <- location$saq02
location <- location[ c("household_id", "REGCODE", "ZONECODE", "ea_id", "rural") ]

# recode the rural variable to match with wave 2
location$rural <- ifelse(location$rural %in% 1, 1, 2)
location$type <- factor(location$rural, levels=1:2, labels=c("RURAL", "SMALL TOWN"))
location$rural <- ifelse(location$rural %in% 1, 1, 0)
location$REGCODE <- as.integer(location$REGCODE)
location <- unique(location)

descriptive.table(vars = d(rural, REGCODE, ZONECODE),data= location, 
                  func.names = c("Mean","St. Deviation", "Min", "Max", "Valid N"))
frequencies(location$type,r.digits=1)
frequencies(location$REGCODE,r.digits=1)

# match up with the names from the survey (prepared in a seperate file)

#VL Adjusted 
#REGZONE <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Spatial/ETH/REGZONEETH.csv"))
REGZONE <- read.csv(file.path(dataPath, "/../../", "Other/Spatial/ETH/REGZONEETH.csv"))

# join with household identifications
location <- left_join(location, REGZONE)

rm(REGZONE)

#######################################
########### SOCIO/ECONOMIC ############
#######################################

HH11 <- read_dta(file.path(dataPath, "sect1_hh_w1.dta")) %>%
  select(household_id, individual_id,
         ea_id, status=hh_s1q02, sex=hh_s1q03, age=hh_s1q04_a,
         religion=hh_s1q07, marital=hh_s1q08)
#HH11 <- read_dta(file.path(dataPath, "sect1_hh_w1.dta")) 
#HH11 <- HH11[ c("household_id", "individual_id", "ea_id", "hh_s1q02", "hh_s1q03", "hh_s1q04_a", "hh_s1q07", "hh_s1q08") ]
#colnames(HH11)[4:8] <- c("status", "sex", "age", "religion", "marital") 

HH11$status <- toupper(as_factor(HH11$status))
HH11$religion <- toupper(as_factor(HH11$religion))
HH11$sex <- toupper(as_factor(HH11$sex)) 
HH11$marital <- toupper(as_factor(HH11$marital)) 

descriptive.table(vars = d(status, sex, age, religion, marital),data= HH11, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))
frequencies(HH11$status  ,r.digits=1)
frequencies(HH11$sex     ,r.digits=1)
frequencies(HH11$religion,r.digits=1)
frequencies(HH11$marital ,r.digits=1)

# make a new variable cage (cut age = cage) which splits
# individuals according to their age group with
# breaks at 15, 55 and the max age

#HH11$cage <- cut(HH11$age, breaks = c(0, 15, 55, max(HH11$age, na.rm=TRUE)),
#                 labels=c("0-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)
#frequencies(HH11$cage ,r.digits=1)
# VL revised
HH11$cage <- cut(HH11$age, breaks = c(0, 5, 15, 55, max(HH11$age, na.rm=TRUE)),
                 labels=c("0-5", "6-15", "16-55", "56+"), include.lowest = TRUE, right = TRUE)
frequencies(HH11$cage ,r.digits=1)

# education of household members and sum
# of education of all household members
# between the ages of 15 and 55

education <- read_dta(file.path(dataPath, "sect2_hh_w1.dta")) %>%
  select(household_id, individual_id, ea_id,
         literate=hh_s2q02, ed_any=hh_s2q03)
#education <- read_dta(file.path(dataPath, "sect2_hh_w1.dta")) 
#education <- education[ c("household_id", "individual_id", "ea_id", "hh_s2q02", "hh_s2q03")]
#colnames(education)[4:5] <- c("literate", "ed_any")

HH12 <- left_join(HH11, education)
frequencies(HH12$literate ,r.digits=1)
frequencies(HH12$ed_any ,r.digits=1)

HH12$literate <- toupper(as_factor(HH12$literate))
HH12$ed_any <- toupper(as_factor(HH12$ed_any))

frequencies(HH12$literate ,r.digits=1)
frequencies(HH12$ed_any ,r.digits=1)

# summarise the data: get sum of education
# of household members 15-55 and number of
# household members 15:55

library(dplyr)
#HH11_x <- group_by(HH11, household_id) %>%
#  summarise(N1555=sum(cage %in% "16-55"),
#            family_size=n())

#VL: revised
HH11_x <- group_by(HH12, household_id) %>%
  summarise(N0005=sum(cage %in% "0-5"),
            N0615=sum(cage %in% "6-15"),
            N1555=sum(cage %in% "16-55"),
            N5600=sum(cage %in% "56+"),
            family_size=n(),
            N0005w=sum(cage %in% "0-5" & sex %in% "FEMALE"),
            N0615w=sum(cage %in% "6-15" & sex %in% "FEMALE"),
            N1555w=sum(cage %in% "16-55" & sex %in% "FEMALE"),
            N5600w=sum(cage %in% "56+" & sex %in% "FEMALE"),
            literate_n=sum(literate %in% "YES"),
            literate0615=sum(cage %in% "6-15" & literate %in% "YES"),
            literate1655=sum(cage %in% "16-55" & literate %in% "YES"),
            literate5600=sum(cage %in% "56+" & literate %in% "YES"),
            literate0615w=sum(cage %in% "6-15" & literate %in% "YES" & sex %in% "FEMALE"),
            literate1655w=sum(cage %in% "16-55" & literate %in% "YES" & sex %in% "FEMALE"),
            literate5600w=sum(cage %in% "56+" & literate %in% "YES" & sex %in% "FEMALE"),
            ed_any_n=sum(literate %in% "YES"),
            ed_any0615=sum(cage %in% "6-15" & ed_any %in% "YES"),
            ed_any1655=sum(cage %in% "16-55" & ed_any %in% "YES"),
            ed_any5600=sum(cage %in% "56+" & ed_any %in% "YES"),
            ed_any0615w=sum(cage %in% "6-15" & ed_any %in% "YES" & sex %in% "FEMALE"),
            ed_any1655w=sum(cage %in% "16-55" & ed_any %in% "YES" & sex %in% "FEMALE"),
            ed_any5600w=sum(cage %in% "56+" & ed_any %in% "YES" & sex %in% "FEMALE")
  )
frequencies(HH11_x$N0005 ,r.digits=1)
frequencies(HH11_x$N0615 ,r.digits=1)
frequencies(HH11_x$N1655 ,r.digits=1)
frequencies(HH11_x$N5600 ,r.digits=1)
frequencies(HH11_x$family_size ,r.digits=1)


# -------------------------------------
# death in the family
# -------------------------------------

famdeath <- read_dta(file.path(dataPath, "sect8_hh_w1.dta")) %>%
  select(household_id, code=hh_s8q00, death=hh_s8q01) %>% 
  filter(code %in% c("101", "102")) %>% select(-code) %>%
  group_by(household_id) %>%
  summarise(death=ifelse(any(death %in% 1), 1, 0))
frequencies(famdeath$death ,r.digits=1)


HH12_x <- filter(HH12, status %in% "HEAD")
sociodem <- HH11_x[ c("household_id")]
HH11 <- left_join(HH11, education) %>%
  left_join(HH11_x) %>%
  left_join(famdeath); rm(education, HH11_x, famdeath)
sociodem <- left_join(sociodem, HH12_x)
sociodem <- left_join(sociodem, HH11_x)
sociodem <- left_join(sociodem, famdeath)

#######################################
############### OUTPUT ################
#######################################

# in the 2011 wave of the data, the respondents
# were asked about the output per 2 metre by 2 metre
# area. They were also asked how much the total field
# produced.

oput <- read_dta(file.path(dataPath, "sect9_ph_w1.dta")) %>%
  transmute(household_id, holder_id, parcel_id, field_id,
         crop_code, day_cut=ph_s9q02_a, month_cut=ph_s9q02_b,
         crop_qty_harv_fresh_kg=ph_s9q03_a,
         crop_qty_harv_fresh_g=ph_s9q03_b/100,
         crop_qty_harv_dry_kg=ph_s9q05_a,
         crop_qty_harv_dry_g=ph_s9q05_b/100,
         crop_qty_harv_tot_kg=ph_s9q12_a,
         crop_qty_harv_tot_g=ph_s9q12_b/100,
         harv_month_start=ph_s9q13_a,
         harv_month_end=ph_s9q13_b, crop_name)
frequencies(oput$crop_code ,r.digits=1)
frequencies(oput$crop_name ,r.digits=1)

descriptive.table(vars = d(day_cut, month_cut, crop_qty_harv_fresh_kg, crop_qty_harv_fresh_g, crop_qty_harv_dry_kg,
                           crop_qty_harv_dry_g, crop_qty_harv_tot_kg, crop_qty_harv_tot_g,
                           harv_month_start, harv_month_end),data= oput, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

oput$crop_code <- as.integer(oput$crop_code)
x <- c("crop_qty_harv_fresh_kg","crop_qty_harv_fresh_g","crop_qty_harv_dry_kg",
       "crop_qty_harv_dry_g","crop_qty_harv_tot_kg","crop_qty_harv_tot_g")
make0 <- is.na(oput[, x])
oput[, x][make0] <- 0

oput <- transmute(oput, household_id, holder_id, parcel_id, field_id,
                  crop_code, day_cut, month_cut, 
                  crop_qty_harv_fresh=crop_qty_harv_fresh_kg+crop_qty_harv_fresh_g,
                  crop_qty_harv_dry=crop_qty_harv_dry_kg+crop_qty_harv_dry_g,
                  crop_qty_harv=crop_qty_harv_tot_kg+crop_qty_harv_tot_g,
                  crop_name)

descriptive.table(vars = d(day_cut, month_cut, crop_qty_harv_fresh, crop_qty_harv_dry,
                           crop_qty_harv),data= oput, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))
frequencies(oput$crop_name ,r.digits=1)

# -------------------------------------
# add a dummy if a legume was grown
# count number of crops per field
# crop codes taken from appendix in 
# the BID

legumes <- c(11:18, 36, 118)

oput_x <- group_by(oput, holder_id, household_id, parcel_id, field_id) %>%
  summarise(crop_count=sum(!is.na(crop_code)),
            legume = ifelse(any(crop_code %in% legumes), 1, 0))
descriptive.table(vars = d(legume),data= oput_x, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

oput <- left_join(oput, oput_x); rm(oput_x)

# remove observations with quantity NA or 0
# oput <- oput[! is.na(oput$crop_qty_harv) & !oput$crop_qty_harv %in% 0, ]

rm("legumes")

# the price of each crop and how much was
# sold is stored in a seperate section of 
# the household (section 11)

oput2 <- read_dta(file.path(dataPath, "sect11_ph_w1.dta")) %>%
  select(holder_id, household_id, crop_code,
         sold=ph_s11q01, sold_qty_kg=ph_s11q03_a, sold_qty_gr=ph_s11q03_b,
         value=ph_s11q04_a, sold_month=ph_s11q06_a, sold_year=ph_s11q06_b,
         trans_cost=ph_s11q09) 
oput2$crop_code <- as.integer(oput2$crop_code)
oput2$sold <- toupper(as_factor(oput2$sold))
#VL: next command gives an error
oput2$sold_month <- month(oput2$sold_month, label=TRUE)
#VL Error: could not find function "month"

descriptive.table(vars = d(sold, sold_qty_kg, sold_qty_gr, value, sold_month, sold_year, trans_cost),data= oput2, 
                  func.names = c("Mean", "St. Deviation", "Min", "Max", "Valid N"))

oput <- left_join(oput, oput2) %>% unique; rm(oput2)

#######################################
############## CHEMICAL ###############
#######################################

# -------------------------------------
# variables recorded at parcel, field,
# and crop level

# parcel level
parcel <- read_dta(file.path(dataPath, "sect2_pp_w1.dta")) %>%
  select(household_id, holder_id, parcel_id, fields=pp_s2q02,
                title=pp_s2q04)

parcel$title <- toupper(as_factor(parcel$title))
parcel$parcel_id <- as.integer(parcel$parcel_id)

# field level variables

field <- read_dta(file.path(dataPath, "sect3_pp_w1.dta")) %>%
  select(holder_id, household_id, parcel_id, field_id,
                crop_stand=pp_s3q10,
                extension=pp_s3q11, irrig=pp_s3q12, fert_any=pp_s3q14,
                manure=pp_s3q21, compost=pp_s3q23,
                other_org=pp_s3q25) 

field$crop_stand <- toupper(as_factor(field$crop_stand))
field$extension <- toupper(as_factor(field$extension))
field$parcel_id <- as.integer(field$parcel_id)
field$field_id <- as.integer(field$field_id)

# crop level variables

crop <- read_dta(file.path(dataPath, "sect4_pp_w1.dta")) %>%
  select(household_id, holder_id, parcel_id, field_id, crop_code,
         cropping=pp_s4q02, month=pp_s4q12_a, crop_area=pp_s4q03,
         pest=pp_s4q06, herb=pp_s4q06, fung=pp_s4q07, seed_type=pp_s4q11,
        crop_name)

#fert <- group_by(fert, holder_id, household_id, parcel_id, field_id) %>%
#  summarise(N=sum(Qn, na.rm=TRUE), P=sum(Qp, na.rm=TRUE),
#            UREA=ifelse(any(typ %in% "UREA"), 1, 0),
#            DAP=ifelse(any(typ %in% "DAP"), 1, 0))

# A view codes to make a comparison between crop_name and crop_code
#crop_y <- crop[ c("crop_code", "crop_name")]
#crop_y <- group_by(crop_y, crop_name, crop_code)
#write.csv(crop_y, "cropname and codes.csv")

crop$cropping <- as_factor(crop$cropping)
crop$month <- toupper(as_factor(crop$month))
crop$crop_code <- as.integer(crop$crop_code)

frequencies(crop$crop_code ,r.digits=1)
frequencies(crop$crop_name ,r.digits=1)

# seed level variables

seed <- read_dta(file.path(dataPath, "sect5_pp_w1.dta")) %>%
  select(household_id, holder_id, crop_code, hybrd=pp_s5q01)

# -------------------------------------
# unit of observation is not fertilizer

fert1 <- read_dta(file.path(dataPath, "sect3_pp_w1.dta")) %>%
  transmute(holder_id, household_id, parcel_id, field_id, typ=pp_s3q15,
            qty1=pp_s3q16_a, qty2=pp_s3q16_b/100)
x <- c("qty1", "qty2")
make0 <- is.na(fert1[, x])
fert1[, x][make0] <- 0
fert1 <- transmute(fert1, holder_id, household_id, parcel_id, field_id, typ,
                   qty=qty1+qty2)
fert1$typ <- ifelse(fert1$typ %in% 1, "UREA", NA)

fert2 <- read_dta(file.path(dataPath, "sect3_pp_w1.dta")) %>%
  transmute(holder_id, household_id, parcel_id, field_id, typ=pp_s3q18,
            qty1=pp_s3q19_a, qty2=pp_s3q19_b/100)
x <- c("qty1", "qty2")
make0 <- is.na(fert2[, x])
fert2[, x][make0] <- 0
fert2 <- transmute(fert2, holder_id, household_id, parcel_id, field_id, typ,
                   qty=qty1+qty2)
fert2$typ <- ifelse(fert2$typ %in% 1, "DAP", NA)

# -------------------------------------
# read in nitrogen conversion file


conv <- read.csv(file.path(dataPath, "/../../", "Other/Fertilizer/Fert_comp.csv")) %>%
#conv <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other/Fertilizer/Fert_comp.csv")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% c("UREA", "DAP"))

fert1 <- left_join(fert1, conv)
fert2 <- left_join(fert2, conv)

# -------------------------------------
# If purchased amount of nitrogen is zero 
# set to NA to avoid Inf values

fert1$qty <- ifelse(fert1$qty == 0, NA, fert1$qty)
fert2$qty <- ifelse(fert2$qty == 0, NA, fert2$qty)

fert1 <- mutate(fert1,
                Qn=qty*n,
                Qp=qty*p)
fert2 <- mutate(fert2,
                Qn=qty*n,
                Qp=qty*p)

# if Qn is zero change to NA
fert1$Qn <- ifelse(fert1$Qn == 0, NA, fert1$Qn)
fert2$Qn <- ifelse(fert2$Qn == 0, NA, fert2$Qn)

fert <- rbind(fert1, fert2) %>% unique

fert <- group_by(fert, holder_id, household_id, parcel_id, field_id) %>%
  summarise(N=sum(Qn, na.rm=TRUE), P=sum(Qp, na.rm=TRUE),
            UREA=ifelse(any(typ %in% "UREA"), 1, 0),
            DAP=ifelse(any(typ %in% "DAP"), 1, 0))

fert$parcel_id <- as.integer(fert$parcel_id)
fert$field_id <- as.integer(fert$field_id)
rm(fert1, fert2, conv)

#######################################
############### LABOUR ################
#######################################

# POST HARVEST - unlike 2012-13 survey, there is no
# post planting labour recorded.
ph_lab <- read_dta(file.path(dataPath, "sect10_ph_w1.dta")) %>%
  select(holder_id, household_id, parcel_id, field_id,
                crop_code, ph_s10q01_a:ph_s10q03_f) %>%
  transmute(holder_id, household_id, parcel_id, field_id, crop_code,
            id1=ph_s10q02_a, lab1=ph_s10q02_b*ph_s10q02_c,
            id2=ph_s10q02_e, lab2=ph_s10q02_f*ph_s10q02_g,
            id3=ph_s10q02_i, lab3=ph_s10q02_j*ph_s10q02_k,
            id4=ph_s10q02_m, lab4=ph_s10q02_n*ph_s10q02_o,
            hirM=ph_s10q01_a*ph_s10q01_b,
            hirF=ph_s10q01_d*ph_s10q01_e,
            hirC=ph_s10q01_g*ph_s10q01_h,
            OHHlabM=ph_s10q03_a*ph_s10q03_b,
            OHHlabF=ph_s10q03_c*ph_s10q03_d,
            OHHlabC=ph_s10q03_e*ph_s10q03_f
  )

# -------------------------------------
# make all NA values zero
ph_lab[is.na(ph_lab)] <- 0
ph_lab$crop_code <- as.integer(ph_lab$crop_code)

# sum all labour across a single plot - all measured in days
ph_lab <- transmute(ph_lab, holder_id, household_id, parcel_id, field_id,
                    crop_code, harv_lab=lab1 + lab2 + lab3 + lab4 +
                      hirM + hirF + hirC + OHHlabM + OHHlabF + OHHlabC) %>% unique

ph_lab$harv_lab[ph_lab$harv_lab %in% 0] <- NA

#######################################
############### GEO ###################
#######################################

# geo <- read_dta(file.path(dataPath, "Pub_ETH_HouseholdGeovariables_Y1.dta"))
# geo <- geo.total.plot %>% 
#   dplyr::select(holder_id, household_id, ea_id,
#                 lon=LON_DD_MOD, lat=LAT_DD_MOD,
#                 SPEI, RootDepth, region=NAME_1,
#                 AEZ=ssa_aez09, ph=ph_sd1_sd3, ph2=ph_sd1_sd5,
#                 SOC=SOC_sd1_sd3, SOC2=SOC_sd1_sd5, rain=gsRainfall, 
#                 YA, YW, YP, everything()) %>%
#   unique()
# rm(geo.total.plot)

#######################################
############### AREAs #################
#######################################

# -------------------------------------
# imputed and original gps measurements
# included

#VL: changes in read_dat command

#REGZONE <- read.csv(file.path(dataPath, "/../../", "Other/Spatial/ETH/REGZONEETH.csv"))
#areas <- read_dta(paste(dataPath, "../../../Other/Plot_size/areas_eth_y1_imputed.dta", sep="/"))
#D:\Analyses\CIMMYT\NutritionETH\SurveyData\Other\Plot_size
#areas <- read_dta("D:/Analyses/CIMMYT/NutritionETH/SurveyData/Other/Plot_size/areas_eth_y1_imputed.dta", sep="/")
areas <- read_dta("D:/Analyses/CIMMYT/NutritionETH/SurveyData/Other/Plot_size/areas_eth_y1_imputed.dta")
areas <- select(areas, holder_id, household_id=case_id, parcel_id,
                field_id, area_gps, area_gps_mi_50,
                area_farmer=area_sr)
#REGZONE <- read.csv(file.path(dataPath, "/../../", "Other/Spatial/ETH/REGZONEETH.csv"))
summary(areas)
areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)
areas$area_gps_mi_50 <- ifelse(areas$area_gps_mi_50 %in% 0, NA, areas$area_gps_mi_50)

areaTotal <- group_by(areas, household_id) %>%
  summarise(area_tot = sum(area_gps_mi_50, na.rm=TRUE))

areas$parcel_id <- as.integer(areas$parcel_id)

#######################################
############## COMMUNITY ##############
#######################################

com3 <- read_dta(file.path(dataPath, "sect3_com_w1.dta")) %>%
  select(ea_id, popEA=cs3q02, HHEA=cs3q03)

com4 <- read_dta(file.path(dataPath, "sect4_com_w1.dta")) %>%
  select(ea_id, road=cs4q01, cost2small_town=cs4q10,
         cost2large_town1=cs4q13_1, cost2large_town2=cs4q13_2,
         bank=cs4q45, micro_finance=cs4q47)

com4$road <- toupper(as_factor(com4$road))
com4$bank <- toupper(as_factor(com4$bank))
com4$micro_finance <- toupper(as_factor(com4$micro_finance))

com6 <- read_dta(file.path(dataPath, "sect6_com_w1.dta")) %>%
  select(ea_id, plant_month1=cs6q03_a, plant_month2=cs6q03_b, plant_month3=cs6q03_c,
         harv_month1=cs6q04_a, harv_month2=cs6q04_b, harv_month3=cs6q04_c,
         ext_agent=cs6q08, dist2ext_agent1=cs6q09_1, dist2ext_agent2=cs6q09_2, fert_source=cs6q12,
         pest_source=cs6q13, seed_source=cs6q14)

# make a seperate file for month data
com6$plant_month1 <- month(com6$plant_month1, label=TRUE)
com6$plant_month2 <- month(com6$plant_month2, label=TRUE)
com6$plant_month3 <- month(com6$plant_month3, label=TRUE)
com6$harv_month1 <- month(com6$harv_month1, label=TRUE)
com6$harv_month2 <- month(com6$harv_month2, label=TRUE)
com6$harv_month3 <- month(com6$harv_month3, label=TRUE)
com6$ext_agent <- as_factor(com6$ext_agent)
com6$fert_source <- as_factor(com6$fert_source)
com6$pest_source <- as_factor(com6$pest_source)
com6$seed_source <- as_factor(com6$seed_source)

com <- left_join(com3, com4) %>% left_join(com6)
rm(com3, com4, com6)

#######################################
########### Housing       #############
#######################################

#'Roof material indicates the wealth of a household through a dummy variable taking the value of
#'1: when the roof consists of a corrugated iron sheet, concrete, asbestos, bricks
#'0: when the roof consists of thatch, wood and mud, bamboo / reed 
#'Contains 127 NAs after making '9' and '18' into NA's as their meaning cannot be derived 
#'[https://books.google.nl/books?id=T8-vojNssnUC&pg=PA233&lpg=PA233&dq=corrugated+iron+sheet+wealth&source=bl&ots=Nlpu9RWJDW&sig=4L43Dwx0ChNIe0J5bEOdL3LQQ3g&hl=nl&sa=X&ved=0CFEQ6AEwB2oVChMI-v3TrIGyxwIVzJgaCh2ubghe#v=onepage&q=corrugated%20iron%20sheet%20wealth&f=false]
#'[http://maxwellsci.com/print/crjss/v5-1-10.pdf]
housing           <- read.dta(file.path(dataPath, "sect9_hh_w1.dta"))

housing <- dplyr::select(housing, household_id, house_years = hh_s9q02_a, house_months = hh_s9q02_b, hh_s9q06)
housing$roofmaterial[housing$hh_s9q06!=1] <- 0
housing$roofmaterial[housing$hh_s9q06==1] <- 1
housing$roofmaterial[housing$hh_s9q06==2] <- 1
housing$roofmaterial[housing$hh_s9q06==7] <- 1
housing$roofmaterial[housing$hh_s9q06==8] <- 1
housing$roofmaterial[housing$hh_s9q06==9] <- NA
housing$roofmaterial[housing$hh_s9q06==18] <- NA

housing$house_months[is.na(housing$house_months) & housing$house_years > 0 ] <- 0
housing$house_years[is.na(housing$house_years) & housing$house_months > 0] <- 0

housing$house_age <- housing$house_years + housing$house_months/12
housing <- dplyr::select(housing, household_id, house_age, roofmaterial)

#######################################
########### Wealth indicator #############
#######################################

#'Wealth indicator: television. Does not contain any NAs
assets            <- read.dta(file.path(dataPath, "sect10_hh_w1.dta") )

tv <- dplyr::filter(assets, hh_s10q00=="Television")
tv <- dplyr::select(tv, household_id, television=hh_s10q01)
tv$television[tv$television>0] <- 1
tv <- arrange(tv, household_id)

rm(assets)

#######################################
########### Livestock     #############
#######################################

#2.8 Livestock
#-------------
livestock         <- read.dta(file.path(dataPath, "sect8a_ls_w1.dta"))  
by_household <- group_by(livestock, household_id)
livestock <- as.data.frame(summarise(by_household,
                                     Cattle = sum(ls_s8aq13a[ls_s8aq00=="CATTLE"]),                 Sheep = sum(ls_s8aq13a[ls_s8aq00=="SHEEP"]),
                                     Goats = sum(ls_s8aq13a[ls_s8aq00=="GOATS"]),                  Horses = sum(ls_s8aq13a[ls_s8aq00=="HORSES"]),
                                     Donkeys = sum(ls_s8aq13a[ls_s8aq00=="DONKEYS"]),               Mules = sum(ls_s8aq13a[ls_s8aq00=="MULES"]),
                                     Camels = sum(ls_s8aq13a[ls_s8aq00=="CAMELS"]),            Layinghens = sum(ls_s8aq13a[ls_s8aq00=="LAYING HENS"]),
                                     Nonlayinghens = sum(ls_s8aq13a[ls_s8aq00=="NON-LAYING HENS"]), Cocks = sum(ls_s8aq13a[ls_s8aq00=="COCKS"]),
                                     Cockerels = sum(ls_s8aq13a[ls_s8aq00=="COCKERELS"]),         Pullets = sum(ls_s8aq13a[ls_s8aq00=="PULLETS"]),
                                     Chicks = sum(ls_s8aq13a[ls_s8aq00=="CHICKS"])))          

livestock$LS <- rowSums(livestock[,2:14])
#livestock <- dplyr::select(livestock, household_id, LS)



#######################################
########### CROSS SECTION #############
#######################################

# -------------------------------------
# community and location level joins

ETH2011 <- left_join(location, com)
ETH2011HH <- left_join(location, com); rm(com, location)

# -------------------------------------
# household level joins

ETH2011 <- left_join(HH11, ETH2011)     ; rm(HH11) 
ETH2011 <- left_join(ETH2011, areaTotal)
ETH2011HH <- left_join(ETH2011HH, sociodem)     ; rm(sociodem) 
ETH2011HH <- left_join(ETH2011HH, areaTotal); rm(areaTotal)
ETH2011HH <- left_join(ETH2011HH, housing)  ; rm(housing)
ETH2011HH <- left_join(ETH2011HH, tv)       ; rm(tv)
ETH2011HH <- left_join(ETH2011HH, livestock); rm(livestock)

# -------------------------------------
# parcel level joins

ETH2011 <- left_join(ETH2011, parcel); rm(parcel)

# -------------------------------------
# field level joins

ETH2011 <- left_join(ETH2011, fert); rm(fert) 
ETH2011 <- left_join(ETH2011, areas); rm(areas)
ETH2011 <- left_join(ETH2011, field); rm(field)
# ETH2011 <- left_join(ETH2011, geo); rm(geo)

# -------------------------------------
# crop level joins
ETH2011 <- left_join(ETH2011, oput); rm(oput)
ETH2011 <- left_join(ETH2011, crop); rm(crop)
ETH2011 <- left_join(ETH2011, ph_lab); rm(ph_lab)

# make a surveyyear variable
ETH2011$surveyyear <- 2011

rm(dataPath, make0, seed, x)

