# -------------------------------------
#' crop production variables:
#'  Ethiopia 2013
#'  There is a grand total of 3 possible
#'  area measurements that could be used
#'  
#'     1. plot area (farmer reported)
#'     2. plot area (gps reported, 25% of plots)
#'     3. plot area (farmer + gps measured)
#'
#'   In addition there are several crop
#'   groups to consider. In this file I 
#'   consider 7 crop groups
#'   
#'      1. fruit
#'      2. CCP: Cash Crop Permanent
#'      3. CCNP: Cash Crop Non Permanent
#'      4. veg: vegetable
#'      5. leg: legumes
#'      6. CTR: Cereals Tubers and Roots
#'      
#'   In addition both maize and wheat have
#'   their own group.
#'   
#'   Output: tbd
# -------------------------------------

# -------------------------------------
# load packages and set working directory
# -------------------------------------

library(tidyr)
library(dplyr)
library(haven)

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/ETH/2013/Data"
} else {
  dataPath <- "D:/Analyses/CIMMYT/NutritionETH/SurveyData/2013/Data/"
}

# -------------------------------------
# read in crop information - crop level
# -------------------------------------

crop_prod <- read_dta(file.path(dataPath, "Post-Planting/sect4_pp_w2.dta")) %>%
  select(household_id, holder_id, parcel_id, field_id, crop_code)

# use crop_code as an integer.
crop_prod$crop_code <- as.integer(crop_prod$crop_code)

# -------------------------------------
# read in the field area information
# --------------------------------------

land <- read_dta(file.path(dataPath, "../../../Other/Plot_size/areas_ETH2013.dta")) %>%  
  select(household_id2, holder_id, parcel_id, field_id,
         area_farmer=area_sr, area_gps=area_gps_mi50)

# 0 area does not ake any sense
land$area_gps <- ifelse(land$area_gps %in% 0, NA, land$area_gps)

# make a single area combining gps, non-gps and mixed
land$area_mix <- ifelse(is.na(land$area_gps), land$area_farmer, land$area_gps)
land$area_mix <- ifelse(land$area_mix %in% 0, NA, land$area_mix)

# -------------------------------------
# join land information with the crop
# production variables
# -------------------------------------

crop_prod <- left_join(crop_prod, land); rm(land)

# -------------------------------------
#' make a variable to record which food 
#' group each crop belongs to:
#' 
#'      1. fruit
#'      2. CCP: Cash Crop Permanent
#'      3. CCNP: Cash Crop Non Permanent
#'      4. veg: vegetable
#'      5. leg: legumes
#'      6. CTR: Cereals Tubers and Roots
# -------------------------------------

fruit <- c(41, 42, 43, 44, 45, 46, 47, 48, 49, 65, 66, 83, 84)
CCP <- c(10, 31, 32, 33, 35, 36, 37, 38, 39, 40, 72, 76, 77) 
CTR <- c(1, 3, 4, 5, 6, 7, 58, 60, 62) 
CCNP <- c(73, 78) 
veg <- c(34, 52, 53, 54, 56, 57, 59, 61, 63)
leg <- c(11, 12, 13, 14, 15, 16, 17, 18, 24)
other <- c(fruit, CCP, CTR, CCNP, veg, leg)

# get a variable with the crop group
crop_prod$type <- character(nrow(crop_prod))
crop_prod <- mutate(crop_prod,
                    type=ifelse(crop_code %in% fruit, "fruit", type),
                    type=ifelse(crop_code %in% CCP, "CCP", type),
                    type=ifelse(crop_code %in% CTR, "CTR", type),
                    type=ifelse(crop_code %in% CCNP, "CCNP", type),
                    type=ifelse(crop_code %in% veg, "veg", type),
                    type=ifelse(crop_code %in% leg, "leg", type),
                    type=ifelse(!crop_code %in% other, "other", type),
                    type=ifelse(crop_code %in% 2, "maize", type), # maize has crop code 11
                    type=ifelse(crop_code %in% 8, "wheat", type)) # wheat has crop code 16

# -------------------------------------
#' finally make 3 dataframes corresponding
#' to each of the possible area measurements
#' that can be used to create a total area
#' per food group variable. These are:
#' 
#'     1. plot area (farmer reported)
#'     2. plot area (gps reported, 25% of plots)
#'     3. plot area (farmer + gps measured)
#'      
# -------------------------------------


# 1. plot area (farmer reported) 
crop_prod_w <- select(crop_prod, household_id2, type, area_farmer)
crop_prod_area_farmer <- group_by(crop_prod_w, household_id2, type) %>%
  summarise_each(funs(sum)) %>% spread(key = type, value = area_farmer) 
names(crop_prod_area_farmer) <- paste0(names(crop_prod_area_farmer), "_area_farmer")
names(crop_prod_area_farmer)[1] <- "household_id2"

# 2. plot area (gps reported, 25% of plots)
crop_prod_x <- select(crop_prod, household_id2, type, area_gps)
crop_prod_area_gps <- group_by(crop_prod_x, household_id2, type) %>%
  summarise_each(funs(sum)) %>% spread(key = type, value = area_gps) 
names(crop_prod_area_gps) <- paste0(names(crop_prod_area_gps), "_area_gps")
names(crop_prod_area_gps)[1] <- "household_id2"

# 3. plot area (farmer + gps measured)
crop_prod_y <- select(crop_prod, household_id2, type, area_mix)
crop_prod_area_mix <- group_by(crop_prod_y, household_id2, type) %>%
  summarise_each(funs(sum)) %>% spread(key = type, value = area_mix) 
names(crop_prod_area_mix) <- paste0(names(crop_prod_area_mix), "_area_mix")
names(crop_prod_area_mix)[1] <- "household_id2"

rm(CCNP, CCP, crop_prod, crop_prod_w,
   crop_prod_x, crop_prod_y,
   CTR, dataPath, fruit, leg, other, veg)
