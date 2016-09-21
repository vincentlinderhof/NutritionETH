# Tom
# dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data"

# LEI Path
# dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/TZA/2010/Data"

# ACT: Tom has to change his dataPath

#VL: filePath and setwd change 
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/TZA/2010/Data"
} else {
  dataPath <- "D:/Analyses/CIMMYT/NutritionETH/SurveyData/2011/Data"
}
setwd("D:/Analyses/CIMMYT/NutritionETH")

#'The code in this file creates a dataset based on the raw data
#'of the LSMS-ISA ERSS wave 2011 with households has observational units 
#'and variables of interest for technology adoption

#Importing packages
library(foreign)
library(ggplot2)
library(dplyr)

#install.packages("data.table")
library(data.table)
library(reshape2)
#install.packages("psych")
library(psych)
#install.packages("xtable")
library(xtable)
#install.packages("gdata")
library(gdata)

options(scipen=999)

#Setting the working directory
#setwd('C:/Users/Jasper/Documents/LEI - internship') #working from home
#setwd('M:/My Documents/LEI - internship')           #working from LEI

#'Section 1: Household demographic information
#'Variables: head age, gender, education, household size
#-------------------------------------------------------

  household         <- read.dta(file.path(dataPath, "sect1_hh_w1.dta"))
#  household         <- read.dta("Data/Ethiopia 2011/sect1_hh_w1.dta")
  education         <- read.dta(file.path(dataPath, "sect2_hh_w1.dta"))
#  education         <- read.dta("Data/Ethiopia 2011/sect2_hh_w1.dta")
  household_education <- left_join(household, education, all=TRUE)  
  
#1.2 Add gender, age and education(level) variables to the dataset 
  household_head_data = dplyr::filter(household_education, hh_s1q02=="Head")
  household_head_data = dplyr::select(household_head_data, individual_id, household_id = household_id, individual_id, 
                                      region=saq01, zone=saq02, household_id = household_id, 
                                      gender = hh_s1q03, age = hh_s1q04_a, education = hh_s2q03, education_level = hh_s2q05, in_education = hh_s2q06)
  household_head_data$gender <- as.character(household_head_data$gender)
  household_head_data$age <- as.numeric(household_head_data$age)
  household_head_data$education <- as.character(household_head_data$education)
  household_head_data$v_education_level <- as.numeric(household_head_data$education_level)
  
  #'1.2.2 Gender, age and education(level)
  household_head_data$gender[household_head_data$gender=="Male"] <- 1
  household_head_data$gender[household_head_data$gender=="Female"] <- 0
  household_head_data$gender <- as.numeric(household_head_data$gender)
  household_head_data$education[household_head_data$education=="Yes"] <- 1
  household_head_data$education[household_head_data$education=="No"] <- 0
  household_head_data$education <- as.numeric(household_head_data$education)

#'Categorical variable: 0 represents a lack of education      1 represents minimal education
#'                      2 represents a completed grade in secondary education and a capability to read and write (secondary education)
#'                      3 represents college / university education.
  household_head_data$education <- as.numeric(household_head_data$education)
  household_head_data$educationlevel[household_head_data$education==0] <- 0

  household_head_data$educationlevel[household_head_data$v_education_level==98 | household_head_data$v_education_level==41] <- 0

  household_head_data$educationlevel[household_head_data$v_education_level>=0 & household_head_data$v_education_level<=4 |
                                     household_head_data$v_education_level>=93 & household_head_data$v_education_level<=97 | 
                                     household_head_data$v_education_level==37 | household_head_data$v_education_level<=38 |
                                     household_head_data$v_education_level==40] <- 1

  household_head_data$educationlevel[household_head_data$v_education_level>=5 & household_head_data$v_education_level<=14 | 
                                     household_head_data$v_education_level>20 & household_head_data$v_education_level<=30] <- 2

  household_head_data$educationlevel[household_head_data$v_education_level>=15 & household_head_data$v_education_level<=20 | 
                                     household_head_data$v_education_level>=31 & household_head_data$v_education_level<=35] <- 3

  household_head_data$educationlevel <- as.numeric(household_head_data$educationlevel)
  household_head_data <- dplyr::select(household_head_data, household_id, individual_id, gender, age, education, educationlevel, in_education) 
  
  #Section 2: Farm indicators
  #--------------------------------

# VL: datapath changed
  fields            <- read.dta(file.path(dataPath, "sect3_pp_w1.dta")) 
  crops             <- read.dta(file.path(dataPath, "sect4_pp_w1.dta"))
  conversion        <- read.dta(file.path(dataPath, "ET_local_area_unit_conversion.dta"))
#  fields            <- read.dta("Data/Ethiopia 2011/sect3_pp_w1.dta") 
#  crops             <- read.dta("Data/Ethiopia 2011/sect4_pp_w1.dta")
#  conversion        <- read.dta("Data/Ethiopia 2011/ET_local_area_unit_conversion.dta")
  
  fields_crops = left_join(fields, crops, all=TRUE)
  fields_crops$plot_id <- paste(fields_crops$holder_id, fields_crops$parcel_id, fields_crops$field_id) 
  
  #'Section 2.1: Technology constructing dummies for technology adopters and constructing the technology count variable
  #'Households that show NAs' for all plots for a certain technology are filtered out as it is unclear whether they have
  #'adopted or not adopted a technology on one of their plots
  tech_area <- dplyr::select(fields_crops, household_id, holder_id, plot_id, parcel_id, field_id, irrig=pp_s3q12, fert=pp_s3q14, landuse = pp_s3q03,
                               impseeds=pp_s4q11, phf=pp_s4q04, herb=pp_s4q06, self_reported_area = pp_s3q02_d, original_gps = pp_s3q05_c)
  tech_area <- tech_area[!duplicated(tech_area$plot_id),]
  
  #D:\Analyses\CIMMYT\NutritionETH\SurveyData\Other\Plot_size\areas_eth_y1_imputed.dta
  areas            <- read.dta(file.path(dataPath, "/../../", "Other/Plot_size/areas_eth_y1_imputed.dta")) 
#  areas <- read.csv('imputed_area2011.csv')
#### SOFAR ###
    regions_nozero <- c("Harari", "Gambella", "Dire Dawa")
  
  areas$household_id <- paste(0,areas$household_id,sep="")
#VL following line produces an error due to the fact that variable names are not in the new file.
  areas$holder_id <- ifelse((areas$region %in% regions_nozero), areas$holder_id, paste(0,areas$holder_id,sep=""))
  areas$plot_id <- as.character(paste(areas$holder_id, areas$parcel_id, areas$field_id))
  areas <- dplyr::select(areas, plot_id, area)
  areas <- areas[1:32025,]
  areas <- unique(areas)
  
  #Merge fields and conversion factors by region, zone, woreda and local_unit
  plotdata <- left_join(tech_area, areas, by="plot_id")
  plotdata <- arrange(plotdata, plot_id)
  #Retrieve unique household values 
  plotdata <-   plotdata[!duplicated(  plotdata$plot_id),]
  plotdata$household_id <-   plotdata$household_id
  rownames(  plotdata) <- NULL
  
  geo_plot          <- read.dta("Data/Ethiopia 2011/Pub_ETH_PlotGeovariables_Y1.dta") 
  plotdata <- left_join(  plotdata, geo_plot)
  
  #'A lot of plots which are suspected to be the abode plot have NA values for the technologies. The current construction tests
  #'whether there is at least one field using a certain technology and assigns a 1 to the 'household' for that technology if 
  #'that is the case. However, often a houshold will have all zeros for a technology with the exception of one field; rendering
  #'the entire technology as "NA". 
  #'- 
  #'Assigning a zero to a field and technology whenever it is marked as "Other Specify", "Fallow", "Forest", if the distance
  #'to the household is lower than 2. Moreover, the other fields of the household should report a zero too, but only if the number of fields 
  #'of fields that do have information is at least half of the total amount of plots.
  plotdata$irrigation[is.na(plotdata$irrig) & plotdata$landuse=="Other Specify" | plotdata$landuse=="Fallow" | plotdata$landuse=="Forest"& plotdata$dist_household<2] <- 0
  plotdata$fertilizer[is.na(plotdata$fert) & plotdata$landuse=="Other Specify" | plotdata$landuse=="Fallow" | plotdata$landuse=="Forest" & plotdata$dist_household<2] <- 0
  plotdata$improvedseeds[is.na(plotdata$impseeds) & plotdata$landuse=="Other Specify" | plotdata$landuse=="Fallow" | plotdata$landuse=="Forest" & plotdata$dist_household<2] <- 0
  plotdata$pesticides[is.na(plotdata$phf) & plotdata$landuse=="Other Specify" | plotdata$landuse=="Fallow" | plotdata$landuse=="Forest" & plotdata$dist_household<2] <- 0
  
  plotdata$irrigation[plotdata$irrig=="Yes"] <- 1 
  plotdata$irrigation[plotdata$irrig=="No"] <- 0
  plotdata$fertilizer[plotdata$fert=="Yes"] <- 1
  plotdata$fertilizer[plotdata$fert=="No"] <- 0
  plotdata$improvedseeds[plotdata$impseeds=="Improved"] <- 1
  plotdata$improvedseeds[plotdata$impseeds!="Improved"] <- 0
  plotdata$pesticides[plotdata$phf=="Yes"] <- 1
  plotdata$pesticides[plotdata$phf=="No"] <- 0
  
  #'If a technology is used on a plot, it contributes to the index. Essentially, the index reflects the area under technology
  #'If multiple technologies are employed on a plot, this adds more to the index than a single technology
  plotdata$techuse <-0
  plotdata$techuse[plotdata$irrigation>0 | plotdata$fertilizer>0 | plotdata$improvedseeds>0 | plotdata$pesticides>0] <- 1
  plotdata$techs <- rowSums(plotdata[c("irrigation","fertilizer","improvedseeds","pesticides")])
  plotdata$techarea <- plotdata$area * plotdata$techuse
  
  plotdata$croptechindex <- plotdata$techarea*plotdata$techs
  by_household <- group_by(plotdata, household_id)
  
  area_technology <- as.data.frame(summarise(by_household,
                                             techarea = sum(croptechindex, na.rm=TRUE),
                                             irrigation_use = ifelse(any(irrigation>0), 1, 0),
                                             irrigation_notuse = ifelse(mean(irrigation, na.rm=TRUE)==0 & length(is.na(irrigation)<(length(irrigation))*0.5), 0, NA),
                                             fertilizer_use = ifelse(any(fertilizer>0), 1, 0),
                                             fertilizer_notuse = ifelse(mean(fertilizer, na.rm=TRUE)==0 & length(is.na(fertilizer)<(length(fertilizer))*0.5), 0, NA),
                                             impseeds_use = ifelse(any(improvedseeds>0), 1, 0),
                                             impseeds_notuse = ifelse(mean(improvedseeds, na.rm=TRUE)==0 & length(is.na(improvedseeds)<(length(improvedseeds))*0.5), 0, NA),
                                             phf_use = ifelse(any(pesticides>0), 1, 0),
                                             phf_notuse = ifelse(mean(pesticides, na.rm=TRUE)==0 & length(is.na(pesticides)<(length(pesticides))*0.5), 0, NA),
                                             sr_area = sum(self_reported_area),
                                             area = sum(area)))
  
  area_technology$fertilizer_use[area_technology$fertilizer_notuse==0] <- 0
  area_technology$irrigation_use[area_technology$irrigation_notuse==0] <- 0
  area_technology$impseeds_use[area_technology$impseeds_notuse==0] <- 0
  area_technology$phf_use[area_technology$phf_notuse==0] <- 0
  
  area_technology$techindex <- area_technology$techarea / area_technology$area 
  area_technology$tech_count <- rowSums(area_technology[c("irrigation_use","fertilizer_use","impseeds_use","phf_use")])

  area_technology <-dplyr::select(area_technology, household_id, area, tech_count, techindex)
  
#'The number of plots is obtained through summing unique plot IDs across households
#'Plot IDs consist of holder IDs, parcel IDs, and field IDs. Note that on the 1-1 plot 
#'(parcel 1, field 1)the physical house of the household is often situated, it seems. 
  plotcount <- dplyr::select(fields_crops, household_id = household_id, field_id, crop_code)
  by_household_plotcount <- group_by(plotcount, household_id)
  plotcounthh <- as.data.frame(summarise(by_household_plotcount,
                                         plots = length(field_id)))
  rownames(plotcounthh) <- NULL
  
#'Off-farm income indicates whether a household member is employed at a sector outside
#'the agricultural sector
  off_farm          <- read.dta("Data/Ethiopia 2011/sect11a_hh_w1.dta")
  
  off_farm <- dplyr::select(off_farm, household_id, off_farm_participate = hh_s11aq09)
  off_farm$offpart[off_farm$off_farm_participate=="No"] <- 0
  off_farm$offpart[off_farm$off_farm_participate=="Yes"] <- 1
  off_farm <- dplyr::select(off_farm, household_id, offpart)
  
#'The plot distance from the household is represented by the average distance (in meters)
#'of the farm plots to the household centre. Some plots have an extremely large distance 
#'to the household. The extreme 1% is winsored. 
  geo_plot          <- read.dta("Data/Ethiopia 2011/Pub_ETH_PlotGeovariables_Y1.dta") 
  geo_plot$plot_id <- paste(geo_plot$household_id, geo_plot$parcel_id, geo_plot$field_id)
  
  households <- unique(geo_plot$household_id)
  output_plotdist <- list()
  
  for (hh in households){
    a <- dplyr::filter(geo_plot, geo_plot$household_id==hh)
    b <- a$dist_household 
    lim <- quantile(b, probs=0.9, na.rm=TRUE)
    
    b[b>lim] = mean(b[!b>lim], na.rm=TRUE)  
    a$dist_household <- b  
    output_plotdist <- rbind(output_plotdist, a)
  }
    
  by_household <- group_by(output_plotdist, household_id)
  plotdist <- as.data.frame(summarise(by_household,
                                      dist_household = round(mean(dist_household),2)))
  
#'Roof material indicates the wealth of a household through a dummy variable taking the value of
#'1: when the roof consists of a corrugated iron sheet, concrete, asbestos, bricks
#'0: when the roof consists of thatch, wood and mud, bamboo / reed 
#'Contains 127 NAs after making '9' and '18' into NA's as their meaning cannot be derived 
#'[https://books.google.nl/books?id=T8-vojNssnUC&pg=PA233&lpg=PA233&dq=corrugated+iron+sheet+wealth&source=bl&ots=Nlpu9RWJDW&sig=4L43Dwx0ChNIe0J5bEOdL3LQQ3g&hl=nl&sa=X&ved=0CFEQ6AEwB2oVChMI-v3TrIGyxwIVzJgaCh2ubghe#v=onepage&q=corrugated%20iron%20sheet%20wealth&f=false]
#'[http://maxwellsci.com/print/crjss/v5-1-10.pdf]
  housing           <- read.dta("Data/Ethiopia 2011/sect9_hh_w1.dta")
  
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
  
  #'Wealth indicator: television. Does not contain any NAs
  assets            <- read.dta("Data/Ethiopia 2011/sect10_hh_w1.dta")
  
  tv <- dplyr::filter(assets, hh_s10q00=="Television")
  tv <- dplyr::select(tv, household_id, television=hh_s10q01)
  tv$television[tv$television>0] <- 1
  tv <- arrange(tv, household_id)

#2.8 Livestock
#-------------
livestock         <- read.dta("Data/Ethiopia 2011/sect8a_ls_w1.dta")  
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
livestock <- dplyr::select(livestock, household_id, LS)

#2.9 Soil quality 
#----------------
geo_household     <- read.dta("Data/Ethiopia 2011/Pub_ETH_HouseholdGeovariables_Y1.dta")  
geo_household <- dplyr::select(geo_household, household_id, dist_road, dist_popcenter, dist_market, dist_admctr, anntot_avg, sq1, lat_dd_mod = LAT_DD_MOD, lon_dd_mod = LON_DD_MOD)


#sq1-7 are character categoricals and will need to be filled in with numbers
geo_household$nutrient <- 0
geo_household$nutrient[geo_household$sq1=="No or Slight Constraint"] <- 4
geo_household$nutrient[geo_household$sq1=="Moderate Constraint"]     <- 3
geo_household$nutrient[geo_household$sq1=="Severe Constraint"]       <- 2
geo_household$nutrient[geo_household$sq1=="Very Severe Constraint"]  <- 1

#Distance to market and road
lim_market <- quantile(geo_household$dist_market, probs=0.99, na.rm=TRUE)
geo_household$dist_market[geo_household$dist_market>lim_market] <- lim_market

#Soil quality, distance to road, distance to markets, annual total average rainfall, 
geo_household <- dplyr::select(geo_household, household_id, dist_road, 
                               dist_market, anntot_avg, nutrient, lat_dd_mod, lon_dd_mod)

#Slope
by_hh_slope <- group_by(geo_plot, household_id)
slopes <- as.data.frame(summarise(by_hh_slope,
                                  average_slope = mean(plot_srtmslp)))

#'Land ownership: indicates by whether the household has a certificate. If it does not have one (or is NA), information
#'on the acquisition of the parcel is used. Grants by local leaders or heritance implies ownership, whereas rent, borrowed, 
#'or moving in without permission indicates lack of ownership. Numbers 6, 10, 11, 12 could not be interpreted and are considered
#'as NA. An index is created that measures the ratio of ownership area to total area.
parcel            <- read.dta("Data/Ethiopia 2011/sect2_pp_w1.dta")
parcel_data <- dplyr::select(parcel, holder_id, household_id, parcel_id, pp_s2q02, pp_s2q03, pp_s2q04)

areas <- read.csv('imputed_area2011.csv')
regions_nozero <- c("Harari", "Gambella", "Dire Dawa")
areas$household_id <- paste(0,areas$household_id,sep="")
areas$holder_id <- ifelse((areas$region %in% regions_nozero), areas$holder_id, paste(0,areas$holder_id,sep=""))
areas$plot_id <- as.character(paste(areas$holder_id, areas$parcel_id, areas$field_id))

areas <- dplyr::select(areas, holder_id, parcel_id, area)

parcel_data <- left_join(areas, parcel_data, by=c("holder_id", "parcel_id"))

parcel_data$land_ownership[parcel_data$pp_s2q04=="Yes" | parcel_data$pp_s2q03==1 | parcel_data$pp_s2q03==2] <- 1
parcel_data$land_ownership[parcel_data$pp_s2q04=="No" | parcel_data$pp_s2q03==3 | parcel_data$pp_s2q03==4 |
                             parcel_data$pp_s2q03==5] <- 0

by_household_parcel <- group_by(parcel_data, household_id)
land_ownership <- as.data.frame(summarise(by_household_parcel,
                                          area_ownership =  sum(area[land_ownership==1]),
                                          area_total = sum(area)))
land_ownership$ownership_index <- land_ownership$area_ownership / land_ownership$area_total
land_ownership <- dplyr::select(land_ownership, household_id, ownership_index)

#Section 3: Institutional variables
#Variables: extension services, credit services, advisory services, oxen
#-----------------------------------------------------------------------
crops_s7 <- read.dta("Data/Ethiopia 2011/sect7_pp_w1.dta")  
crops_s7 <- crops_s7[!duplicated(crops_s7$household_id),]

by_household = group_by(crops_s7, household_id)
cropmisc <- as.data.frame(summarise(by_household,     
                                    croprotate = sum(pp_s7q01),
                                    extension = sum(pp_s7q04),
                                    credit = sum(pp_s7q06),
                                    advise = sum(pp_s7q08),
                                    oxen = sum(pp_s7q11))) 
cropmisc$extension[cropmisc$extension==2] <- 0
cropmisc$credit[cropmisc$credit==2] <- 0
cropmisc$advise[cropmisc$advise==2] <- 0
cropmisc$croprotate[cropmisc$croprotate==2] <- 0
cropmisc <- cropmisc[-1,]
rownames(cropmisc) <- NULL

#Shocks: 
shocks            <- read.dta("Data/Ethiopia 2011/sect8_hh_w1.dta")

shocks      <- dcast(shocks, household_id ~ hh_s8q00, value.var="hh_s8q01")
names(shocks) <- c("household_id","head_death","hh_ilness","hh_jobloss","drought","flood","landslide","heavy_rain","other_cropdmg","pfood_fall","pfood_rise","pinput_rise","livestock_death","fire","theft","land_loss","displace","local_unrest","other")
shocks <- dplyr::select(shocks, household_id, drought, flood, heavy_rain, pinput_rise)
shocks$drought[shocks$drought=="Yes"] <- 1
shocks$drought[shocks$drought=="No"] <- 0
shocks$flood[shocks$flood=="Yes"] <- 1
shocks$flood[shocks$flood=="No"] <- 0
shocks$heavy_rain[shocks$heavy_rain=="Yes"] <- 1
shocks$heavy_rain[shocks$heavy_rain=="No"] <- 0
shocks$pinput_rise[shocks$pinput_rise=="Yes"] <- 1
shocks$pinput_rise[shocks$pinput_rise=="No"] <- 0


#'Adding everything to a dataframe
data2011 <- unique(dplyr::select(household, household_id, region=saq01, zone=saq02))
data2011$region <- as.character(data2011$region)
data2011$hhsize = table(household$household_id)
data2011 <- left_join(data2011, household_head_data, all=TRUE)
data2011 <- left_join(data2011, housing, all=TRUE)           #Housing (roofmaterial)
data2011 <- left_join(data2011, tv, all=TRUE)                #Television
data2011 <- left_join(data2011, livestock, all=TRUE)         #Livestock ownership
data2011 <- left_join(data2011, plotdist, all=TRUE)          #Average distance from household to the plot
data2011 <- left_join(data2011, area_technology, all=TRUE)   #Technology area index
data2011 <- left_join(data2011, geo_household, all=TRUE)     #Soil quality indicators
data2011 <- left_join(data2011, slopes, all=TRUE)            #Average slope of the plots (does this work?)
data2011 <- left_join(data2011, land_ownership, all=TRUE)    #Land ownership
data2011 <- left_join(data2011, cropmisc, all=TRUE)          #Extension services, credit services, advisory services, oxen
data2011 <- left_join(data2011, off_farm)                    #Off-farm participation
data2011 <- left_join(data2011, plotcounthh)
data2011 <- left_join(data2011, shocks)                      #Shocks

keep(data2011, sure=TRUE)
