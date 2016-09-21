#'The code in this file creates a dataset based on the raw data of the LSMS-ISA ERSS wave 2013 with households as observational 
#'units and variables of interest for technology adoption horizontally. 
#'
#'The code is divided into three categories
#'1. Household demographic factors
#'2. Farm household factors
#'3. Institutional and infrastructural factors

#'Setting the working directory
setwd('C:/Users/Jasper/Documents/LEI - internship') #working from home
setwd('M:/My Documents/LEI - internship')           #working from LEI

library(foreign)
library(ggplot2)
library(dplyr)
library(data.table)
library(reshape2)
library(psych)
library(xtable)
library(gdata)

#'Section 1: Household demographic factors
#'Variables: household head age, gender, education (and education level), household size
#-------------------------------------------------------
household         <- read.dta("Data/Ethiopia 2013/Household/sect1_hh_w2.dta")
education         <- read.dta("Data/Ethiopia 2013/Household/sect2_hh_w2.dta")
household_education <- left_join(household, education, all=TRUE)  

#1.1 Add the gender, age and education(level) of the household head to the dataset 
household_head_data = dplyr::filter(household_education, hh_s1q02=="Head")
household_head_data = dplyr::select(household_head_data, individual_id, household_id, individual_id, region=saq01, zone=saq02, 
                                    gender = hh_s1q03, age = hh_s1q04_a,  
                                    education = hh_s2q03, v_education_level = hh_s2q05, in_education = hh_s2q06)
household_head_data$gender <- as.character(household_head_data$gender)
household_head_data$age <- as.numeric(household_head_data$age)
household_head_data$education <- as.character(household_head_data$education)
household_head_data$v_education_level <- as.numeric(household_head_data$v_education_level)

#'Gender, age and education(level)
#'The rationale behind the education level categorical dummy; 0 represents a lack of education, 1 represents minimal education
#'wherein people can or cannot read or write. 2 represents a completed grade in secondary education and a capability to read and
#'write. This also includes secondary education and technical / vocational diplomas. 3 represents college / university education.
household_head_data$gender[household_head_data$gender=="Male"] <- 1
household_head_data$gender[household_head_data$gender=="Female"] <- 0
household_head_data$gender <- as.numeric(household_head_data$gender)
household_head_data$education[household_head_data$education=="Yes"] <- 1
household_head_data$education[household_head_data$education=="No"] <- 0
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

household_head_data <- dplyr::select(household_head_data, household_id, individual_id, gender, age, education, educationlevel, in_education) 
household_head_data <- arrange(household_head_data, household_id)[1487:5262,]
rownames(household_head_data) <- NULL 

#'Section 2: Farm indicators
#'Variables: plot count, off-farm income, plot distance, roof material, television, area, livestock, distance to the nearest road,
#'distance to the nearest market, soil quality, annual average rainfall, slopes, land ownership
#--------------------------------
fields            <- read.dta("Data/Ethiopia 2013/Post-Planting/sect3_pp_w2.dta") 
crops             <- read.dta("Data/Ethiopia 2013/Post-Planting/sect4_pp_w2.dta")
conversion        <- read.dta("Data/Ethiopia 2011/ET_local_area_unit_conversion.dta")

fields_crops = left_join(fields, crops, all=TRUE)
fields_crops$plot_id <- paste(fields_crops$holder_id, fields_crops$parcel_id, fields_crops$field_id)

#'Section 2.1: Technology constructing dummies for technology adopters and constructing the technology count variable
#'Households that show NAs' for all plots for a certain technology are filtered out as it is unclear whether they have
#'adopted or not adopted a technology on one of their plots
techandarea <- dplyr::select(fields_crops, household_id, parcel_id, field_id, plot_id, irrig=pp_s3q12, fert=pp_s3q14, impseeds=pp_s4q11, phf=pp_s4q04, herb=pp_s4q06, landuse = pp_s3q03,
                             region=saq01, zone=saq02, woreda=saq03, self_reported_area=pp_s3q02_a, gps_original = pp_s3q05_a, local_unit=pp_s3q02_c)
techandarea <- techandarea[!duplicated(techandarea$plot_id),]
areas <- read.csv('imputed_area2013.csv')

regions_nozero <- c("Harari", "Gambella", "Dire Dawa")
areas$household_id <- paste(0,areas$household_id,sep="")
areas$holder_id <- ifelse((areas$region %in% regions_nozero), areas$holder_id, paste(0,areas$holder_id,sep=""))
areas$plot_id <- as.character(paste(areas$holder_id, areas$parcel_id, areas$field_id))
areas <- dplyr::select(areas, plot_id, area)
areas <- areas[1:33147,]
areas <- unique(areas)

#Merge fields and conversion factors by four factors
plotdata <- left_join(techandarea, areas, by="plot_id")
plotdata <- arrange(plotdata, plot_id)
#Retrieve unique household values (sometimes there are more holders per household, indicating very different areas..)
plotdata <- plotdata[!duplicated(plotdata$plot_id),]
rownames(plotdata) <- NULL

geo_plot          <- read.dta("Data/Ethiopia 2013/Geodata/Pub_ETH_PlotGeovariables_Y2.dta")
plotdata <- left_join(plotdata, geo_plot)

#'A lot of plots which are suspected to be the abode plot have NA values for the technologies. The current construction tests
#'whether there is at least one field using a certain technology and assigns a 1 to the 'household' for that technology if 
#'that is the case. However, often a houshold will have all zeros for a technology with the exception of one field; rendering
#'the entire technology as "NA". 
#'- 
#'By assigning a zero to a field and technology whenever it is marked as "Other Specify", "Fallow", "Forest", if the distance
#'to the household is lower than 2. Moreover, the other fields of the household should report a zero too. The number of fields 
#'that do have information should be at least half of the total plots.
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
area_technology <- area_technology[-1,]

area_technology <-dplyr::select(area_technology, household_id, area, tech_count, techindex)


#'The number of plots is obtained through summing unique plot IDs across households
#'Plot IDs consist of holder IDs, parcel IDs, and field IDs. Note that on the 1-1 plot 
#'(parcel 1, field 1)the physical house of the household is often situated, it seems. 
plotcount <- dplyr::select(fields_crops, household_id = household_id, field_id, crop_code)
by_household_plotcount <- group_by(plotcount, household_id)
plotcounthh <- as.data.frame(summarise(by_household_plotcount,
                                       plots = length(field_id)))[-1,]
rownames(plotcounthh) <- NULL

#'Off-farm income indicates whether a household member is employed at a sector outside
#'the agricultural sector
off_farm          <- read.dta("Data/Ethiopia 2013/Household/sect11a_hh_w2.dta")

off_farm <- dplyr::select(off_farm, household_id, off_farm_participate = hh_s11aq09)
off_farm$offpart[off_farm$off_farm_participate=="No"] <- 0
off_farm$offpart[off_farm$off_farm_participate=="Yes"] <- 1
off_farm <- arrange(dplyr::select(off_farm, household_id, offpart), household_id)[1487:5262,]
rownames(off_farm) <- NULL

#'The plot distance from the household is represented by the average distance (in meters)
#'of the farm plots to the household centre. Some plots have an extremely large distance 
#'to the household, even when they are on the same parcel as other fields that indicate substantially lower distances.
#'This is solved by looking at whether an outlier 
#'
geo_plot13         <- read.dta("Data/Ethiopia 2013/Geodata/Pub_ETH_PlotGeovariables_Y2.dta")
geo_plot13$plot_id <- paste(geo_plot13$household_id, geo_plot13$parcel_id, geo_plot13$field_id)

#Function to filter out outliers
households <- unique(geo_plot13$household_id)
output_plotdist13 <- list()

for (hh in households){
  a <- dplyr::filter(geo_plot13, geo_plot13$household_id==hh)
  b <- a$dist_household 
  lim <- quantile(b, probs=0.9, na.rm=TRUE)
  
  b[b>lim] = mean(b[!b>lim], na.rm=TRUE)  
  a$dist_household <- b  
  output_plotdist13 <- rbind(output_plotdist13, a)
}

by_household <- group_by(output_plotdist13, household_id)
plotdist13 <- as.data.frame(summarise(by_household,
                                    dist_household = round(mean(dist_household),2)))
plotdist13 <- arrange(plotdist13)[-1,]
rownames(plotdist) <- NULL



#'Roof material indicates the wealth of a household through a dummy variable taking the value
#'of 1 when the roof consists of a corrugated iron sheet, concrete, asbestos, bricks
#'Contains  NAs after setting 'other' to NA as well
housing           <- read.dta("Data/Ethiopia 2013/Household/sect9_hh_w2.dta")

housing <- dplyr::select(housing, household_id, house_years = hh_s9q02_a, house_months = hh_s9q02_b, hh_s9q06)

housing$roofmaterial[housing$hh_s9q06=="Thatch"] <- 0
housing$roofmaterial[housing$hh_s9q06=="Wood and mud"] <- 0
housing$roofmaterial[housing$hh_s9q06=="Reed / bamboo"] <- 0
housing$roofmaterial[housing$hh_s9q06=="Plastic canvas"] <- 0

housing$roofmaterial[housing$hh_s9q06=="Corrugated iron sheet"] <- 1
housing$roofmaterial[housing$hh_s9q06=="Concrete / Cement"] <- 1
housing$roofmaterial[housing$hh_s9q06=="Asbestos"] <- 1
housing$roofmaterial[housing$hh_s9q06=="Bricks"] <- 1

housing$house_months[is.na(housing$house_months) & housing$house_years > 0 ] <- 0
housing$house_years[is.na(housing$house_years) & housing$house_months > 0] <- 0

housing$house_age <- housing$house_years + housing$house_months/12

housing <- dplyr::select(housing, household_id, house_age, roofmaterial)
housing <- dplyr::arrange(housing,household_id)[1487:5262,]
rownames(housing) <- NULL

#'Television indicates the wealth of the household through a dummy variable taking the value
#'of 1 when a household owns at least one television and 0 if none are owned.
assets            <- read.dta("Data/Ethiopia 2013/Household/sect10_hh_w2.dta")
tv <- dplyr::filter(assets, hh_s10q00=="Television")
tv <- dplyr::select(tv, household_id, television=hh_s10q01)
tv <- arrange(tv, household_id)
tv <- tv[1487:5262,]
tv$television[tv$television>0] <- 1
rownames(tv) <- NULL

#3.5 Adding a variable that measures the livestock of a household     
livestock         <- read.dta("Data/Ethiopia 2013/Livestock/sect8a_ls_w2.dta")                                   
LS_by_household <- group_by(livestock, household_id)

livestock <- as.data.frame(summarise(LS_by_household,
                                     Cattle = sum(ls_s8aq13a[ls_s8aq00=="CATTLE"]),
                                     Sheep = sum(ls_s8aq13a[ls_s8aq00=="SHEEP"]),
                                     Goats = sum(ls_s8aq13a[ls_s8aq00=="GOATS"]),
                                     Horses = sum(ls_s8aq13a[ls_s8aq00=="HORSES"]),
                                     Donkeys = sum(ls_s8aq13a[ls_s8aq00=="DONKEYS"]),
                                     Mules = sum(ls_s8aq13a[ls_s8aq00=="MULES"]),
                                     Camels = sum(ls_s8aq13a[ls_s8aq00=="CAMELS"]),
                                     Layinghens = sum(ls_s8aq13a[ls_s8aq00=="LAYING HENS"]),
                                     Nonlayinghens = sum(ls_s8aq13a[ls_s8aq00=="NON-LAYING HENS"]),
                                     Cocks = sum(ls_s8aq13a[ls_s8aq00=="COCKS"]),
                                     Cockerels = sum(ls_s8aq13a[ls_s8aq00=="COCKERELS"]),
                                     Pullets = sum(ls_s8aq13a[ls_s8aq00=="PULLETS"]),
                                     Chicks = sum(ls_s8aq13a[ls_s8aq00=="CHICKS"])))
                                     
livestock$LS <- rowSums(livestock[,2:14])
livestock <- dplyr::select(livestock, household_id, LS)
livestock <- livestock[-1,]

#3.6 Soil quality indicators
geo_household     <- read.dta("Data/Ethiopia 2013/Geodata/Pub_ETH_HouseholdGeovars_Y2.dta")
geo_household <- dplyr::select(geo_household, household_id, dist_road, dist_popcenter, dist_market, dist_admctr, pop_density, anntot_avg, sq1, lat_dd_mod, lon_dd_mod)

#sq1-7 are character categoricals and will need to be filled in with numbers
geo_household$nutrient <- 0
geo_household$nutrient[geo_household$sq1=="No or Slight Constraint"] <- 4
geo_household$nutrient[geo_household$sq1=="Moderate Constraint"]     <- 3
geo_household$nutrient[geo_household$sq1=="Severe Constraint"]       <- 2
geo_household$nutrient[geo_household$sq1=="Very Severe Constraint"]  <- 1

geo_household$household_id <- as.numeric(geo_household$household_id)    
geo_household <- dplyr::filter(geo_household, household_id>0)
geo_household$household_id <- as.character(geo_household$household_id)

geo_household$household_id <- paste(0,geo_household$household_id, sep="")

geo_household <- dplyr::select(geo_household, household_id, dist_road, dist_market, 
                       anntot_avg, nutrient, lat_dd_mod, lon_dd_mod)

by_hh_slope <- group_by(geo_plot, household_id)
slopes <- as.data.frame(summarise(by_hh_slope,
                                  average_slope = mean(plot_srtmslp)))
slopes <- slopes[-1,]

#'Land ownership: indicates by whether the household has a certificate. If it does not have one (or is NA), information
#'on the acquisition of the parcel is used. Grants by local leaders or heritance implies ownership, whereas rent, borrowed, 
#'or moving in without permission indicates lack of ownership. Numbers 6, 10, 11, 12 could not be interpreted and are considered
#'as NA. An index is created that measures the ratio of ownership plots to total plots.
parcel            <- read.dta("Data/Ethiopia 2013/Post-Planting/sect2_pp_w2.dta")

parcel_data <- dplyr::select(parcel, holder_id, household_id, parcel_id, pp_s2q02, pp_s2q03, pp_s2q04)

areas <- read.csv('imputed_area2013.csv')
regions_nozero <- c("Harari", "Gambella", "Dire Dawa")
areas$household_id <- paste(0,areas$household_id,sep="")
areas$holder_id <- ifelse((areas$region %in% regions_nozero), areas$holder_id, paste(0,areas$holder_id,sep=""))
areas$plot_id <- as.character(paste(areas$holder_id, areas$parcel_id, areas$field_id))

areas <- dplyr::select(areas, holder_id, parcel_id, area)

parcel_data <- left_join(areas, parcel_data, by=c("holder_id", "parcel_id"))

parcel_data$land_ownership[parcel_data$pp_s2q04=="Yes" | parcel_data$pp_s2q03==1 | parcel_data$pp_s2q03==2 |
                             parcel_data$pp_s2q03=="Granted by Local Leaders" | parcel_data$pp_s2q03=="Inherited"] <- 1
parcel_data$land_ownership[parcel_data$pp_s2q04=="No" | parcel_data$pp_s2q03=="Rent" | parcel_data$pp_s2q03=="Borrowed for Free" |
                             parcel_data$pp_s2q03=="Moved in Without Permission" | parcel_data$pp_s2q03=="Other specify"] <- 0

by_household_parcel <- group_by(parcel_data, household_id)
land_ownership <- as.data.frame(summarise(by_household_parcel,
                                          area_ownership =  sum(area[land_ownership==1]),
                                          area_total = sum(area)))
land_ownership$ownership_index <- land_ownership$area_ownership / land_ownership$area_total
land_ownership <- dplyr::select(land_ownership, household_id, ownership_index)
land_ownership <- land_ownership[-1,]
rownames(land_ownership) <- NULL

#SECTION 4: INSTITUTIONAL CHARACTERISTICS
#---------------------------------------------------
crops_s7          <- read.dta("Data/Ethiopia 2013/Post-Planting/sect7_pp_w2.dta") 
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

#Shocks
shocks        <- read.dta("Data/Ethiopia 2013/Household/sect8_hh_w2.dta")

shocks      <- dcast(shocks, household_id2 + household_id ~ hh_s8q0a, value.var="hh_s8q01")
names(shocks) <- c("household_id2", "household_id","head_death","hh_ilness","hh_jobloss","drought","flood","landslide","heavy_rain","other_cropdmg","pfood_fall","pfood_rise","pinput_rise","livestock_death","fire","theft","land_loss","displace","local_unrest","other")
shocks <- dplyr::select(shocks, household_id, drought, flood, heavy_rain, pinput_rise)
shocks$drought[shocks$drought=="Yes"] <- 1
shocks$drought[shocks$drought=="No"] <- 0
shocks$flood[shocks$flood=="Yes"] <- 1
shocks$flood[shocks$flood=="No"] <- 0
shocks$heavy_rain[shocks$heavy_rain=="Yes"] <- 1
shocks$heavy_rain[shocks$heavy_rain=="No"] <- 0
shocks$pinput_rise[shocks$pinput_rise=="Yes"] <- 1
shocks$pinput_rise[shocks$pinput_rise=="No"] <- 0

shocks <- arrange(shocks, household_id)
shocks <- shocks[1487:5262,]
rownames(shocks) <- NULL

#Household size
hhsize = as.data.frame(table(household_education$household_id))
names(hhsize) <- c("household_id","hhsize")
hhsize <- hhsize[-1,]

data2013 <- unique(dplyr::select(household, household_id, region=saq01, zone=saq02))
data2013$region <- as.character(data2013$region)
data2013 <- left_join(data2013, hhsize, all=TRUE)
data2013 <- left_join(data2013, household_head_data, all=TRUE)
data2013 <- left_join(data2013, housing, all=TRUE)           #Housing (roofmaterial)
data2013 <- left_join(data2013, tv, all=TRUE)                #Television
data2013 <- left_join(data2013, livestock, all=TRUE)         #Livestock ownership
data2013 <- left_join(data2013, plotdist13, all=TRUE)          #Average distance from household to the plot
data2013 <- left_join(data2013, technology_data, all=TRUE)   #Technology dummy and count variables
data2013 <- left_join(data2013, area_technology, all=TRUE)   #Technology area index
data2013 <- left_join(data2013, geo_household, all=TRUE)     #Soil quality indicators
data2013 <- left_join(data2013, slopes, all=TRUE)            #Average slope of the plots (does this work?)
data2013 <- left_join(data2013, land_ownership, all=TRUE)    #Land ownership
data2013 <- left_join(data2013, cropmisc, all=TRUE)          #Extension services, credit services, advisory services, oxen
data2013 <- left_join(data2013, off_farm)                    #Off-farm participation
data2013 <- left_join(data2013, plotcounthh)
data2013 <- left_join(data2013, shocks)                      #Shocks

data2013 <- arrange(data2013, household_id)
data2013 <- data2013[49:3824,]
rownames(data2013) <- NULL

keep(data2011, data2013, sure=TRUE)
#------------------------------------------- 


#'Combining the two data frames and saving it in a csv file
#--------------------------
data2011$year <- "2011" 
data2013$year <- "2013"
data <- rbind(data2011, data2013)
data <- dplyr::arrange(data, household_id)

write.csv(data, 'R-Output/data_original.csv')
#----------------------------


