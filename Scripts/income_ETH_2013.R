# -------------------------------------
#' Income variables for first wave of 
#' Ethiopia data (2013-2014)
#' 
#' The first section covers the off-farm
#' income received from primary and secondary
#' work, as well as involment in government
#' programs.
#' 
#' The second section covers on-farm income
#' from selling crops, yield from permanent
#' crops (trees) as well as land that was
#' rented.
#' 
#' Output: dataframe called income2011
#' containing each source of income 
#' seperately at the household level
#' as well as the combined income of
#' each household from each source.
#' All values in local currency (Ethiopian Birr)
#' ------------------------------------

# set working directory
if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/ETH/2013/Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/ETH/2013/Data"
}

# load packages
library(dplyr)
library(haven)

# -------------------------------------
#' off-farm income
#' income over the last 12 months
#' Income is received in various time
#' frames (months, days, weeks).
#' unfortunately the number of hours
#' per day is not recorded. Additionally,
#' respondents should have been asked to 
#' identify the number of months they
#' worked, but unfortunately that question
#' (household questionare, wave 2, section 4,
#' question 13) is not in the questionnaire
#' or the data file. 
#' As a result all of the on-farm section
#' is hashed out until we figure out what
#' to do with it
# -------------------------------------

# # read in raw data
# off_farm_income <- read_dta(file.path(dataPath, "household/sect4_hh_w2.dta")) %>%
#   select(household_id, household_id2, individual_id2, ea_id2,
#          main_job=hh_s4q09,
#          mj_industry=hh_s4q11_b, mj_employer=hh_s4q12,
#          mj_weekspm=hh_s4q14,
#          mj_hourspw=hh_s4q15,mj_wage=hh_s4q16,
#          mj_payPeriod=hh_s4q17, mj_grat=hh_s4q18,
#          mj_gratPeriod=hh_s4q19,second_job=hh_s4q20,
#          sj_industry=hh_s4q22_b, sj_employer=hh_s4q23,
#          sj_months=hh_s4q24, sj_weekspm=hh_s4q25,
#          sj_hourspw=hh_s4q26, sj_wage=hh_s4q27,
#          sj_payPeriod=hh_s4q28, sj_grat=hh_s4q29,
#          sj_gratPeriod=hh_s4q30, PSNP=hh_s4q31,
#          PSNP_days=hh_s4q32, PSNP_wage=hh_s4q33,
#          other_job=hh_s4q34, oj_days=hh_s4q35,
#          oj_wage=hh_s4q36)
# 
# # create new empty variables for holding
# # the income variables
# off_farm_income$mj_pay <- NA
# off_farm_income$mj_grat_pay <- NA
# off_farm_income$sj_pay <- NA
# off_farm_income$sj_grat_pay <- NA
# off_farm_income$PSNP_pay <- off_farm_income$PSNP_wage
# off_farm_income$oj_pay <- off_farm_income$oj_wage
# 
# # depending on what the pay period was
# # calcualte the income received in a
# # 12 month period for the main job (mj)
# # second job (sj) and PSNP and other job (oj)
# 
# off_farm_income <- transmute(off_farm_income, household_id, household_id2, individual_id,
#                              ea_id, main_job, mj_industry, mj_employer,
#                              mj_pay=ifelse(mj_payPeriod %in% 1, mj_months*mj_weekspm*mj_hourspw*mj_wage, mj_pay),
#                              mj_pay=ifelse(mj_payPeriod %in% 3, mj_months*mj_weekspm*mj_wage, mj_pay),
#                              mj_pay=ifelse(mj_payPeriod %in% 4, mj_months*mj_weekspm*mj_wage/2, mj_pay),
#                              mj_pay=ifelse(mj_payPeriod %in% 5, mj_months*mj_wage, mj_pay),
#                              mj_pay=ifelse(mj_payPeriod %in% 6, mj_months*mj_wage/3, mj_pay),
#                              mj_pay=ifelse(mj_payPeriod %in% 7, mj_months*mj_wage/6, mj_pay),
#                              mj_pay=ifelse(mj_payPeriod %in% 8, mj_months*mj_wage/12, mj_pay),
#                              
#                              mj_grat_pay=ifelse(mj_gratPeriod %in% 1, mj_months*mj_weekspm*mj_hourspw*mj_grat, mj_grat_pay),
#                              mj_grat_pay=ifelse(mj_gratPeriod %in% 3, mj_months*mj_weekspm*mj_grat, mj_grat_pay),
#                              mj_grat_pay=ifelse(mj_gratPeriod %in% 4, mj_months*mj_weekspm*mj_grat/2, mj_grat_pay),
#                              mj_grat_pay=ifelse(mj_gratPeriod %in% 5, mj_months*mj_grat, mj_grat_pay),
#                              mj_grat_pay=ifelse(mj_gratPeriod %in% 6, mj_months*mj_grat/3, mj_grat_pay),
#                              mj_grat_pay=ifelse(mj_gratPeriod %in% 7, mj_months*mj_grat/6, mj_grat_pay),
#                              mj_grat_pay=ifelse(mj_gratPeriod %in% 8, mj_months*mj_grat/12, mj_grat_pay),
#                              
#                              second_job, sj_industry, sj_employer,
#                              sj_pay=ifelse(sj_payPeriod %in% 1, sj_months*sj_weekspm*sj_hourspw*sj_wage, sj_pay),
#                              sj_pay=ifelse(sj_payPeriod %in% 3, sj_months*sj_weekspm*sj_wage, sj_pay),
#                              sj_pay=ifelse(sj_payPeriod %in% 4, sj_months*sj_weekspm*sj_wage/2, sj_pay),
#                              sj_pay=ifelse(sj_payPeriod %in% 5, sj_months*sj_wage, sj_pay),
#                              sj_pay=ifelse(sj_payPeriod %in% 6, sj_months*sj_wage/3, sj_pay),
#                              sj_pay=ifelse(sj_payPeriod %in% 7, sj_months*sj_wage/6, sj_pay),
#                              sj_pay=ifelse(sj_payPeriod %in% 8, sj_months*sj_wage/12, sj_pay),
#                              
#                              sj_grat_pay=ifelse(sj_gratPeriod %in% 1, sj_months*sj_weekspm*sj_hourspw*sj_grat, sj_grat_pay),
#                              sj_grat_pay=ifelse(sj_gratPeriod %in% 3, sj_months*sj_weekspm*sj_grat, sj_grat_pay),
#                              sj_grat_pay=ifelse(sj_gratPeriod %in% 4, sj_months*sj_weekspm*sj_grat/2, sj_grat_pay),
#                              sj_grat_pay=ifelse(sj_gratPeriod %in% 5, sj_months*sj_grat, sj_grat_pay),
#                              sj_grat_pay=ifelse(sj_gratPeriod %in% 6, sj_months*sj_grat/3, sj_grat_pay),
#                              sj_grat_pay=ifelse(sj_gratPeriod %in% 7, sj_months*sj_grat/6, sj_grat_pay),
#                              sj_grat_pay=ifelse(sj_gratPeriod %in% 8, sj_months*sj_grat/12, sj_grat_pay),
#                              
#                              PSNP_wage, oj_wage)
# 
# # variable for total pay received from
# # main job, secondary job, graduities,
# # PSNP and other jobs -> 0s are 
# # returned to NAs if necessary
# 
# off_farm_income$off_farm_income <- with(off_farm_income,
#                                         rowSums(cbind(mj_pay,mj_grat_pay,sj_pay, sj_grat_pay, PSNP_wage, oj_wage),
#                                                 na.rm=TRUE))
# miss <- with(off_farm_income,
#              is.na(mj_pay) & is.na(mj_grat_pay & is.na(sj_pay) & is.na(sj_grat_pay) & is.na(PSNP_wage) & is.na(oj_wage)))
# off_farm_income$off_farm_income[miss] <- NA; rm(miss)
# 
# # summarise at the household level to get
# # a measure of total household off farm
# # income
# 
# off_farm_income_hh <- group_by(off_farm_income, household_id) %>%
#   summarise(off_farm_income_hh=sum(off_farm_income, na.rm=TRUE))

#######################################
########## on farm income #############
#######################################

# from crops
crop <- read_dta(file.path(dataPath, "Post-Harvest/sect11_ph_w2.dta")) %>%
  select(holder_id, household_id, household_id2, crop_code,
         sold=ph_s11q01, sold_qty_kg=ph_s11q03_a, sold_qty_gr=ph_s11q03_b,
         value=ph_s11q04, sold_month=ph_s11q06_a, sold_year=ph_s11q06_b,
         trans_cost=ph_s11q09) 

# first calculate the value of crops sold in total
# to the houehold as a measure of on farm income
# at the household level

on_farm_income_crop <- group_by(crop, household_id2) %>%
  summarise(crop_value_hh=sum(value, na.rm=TRUE))

# from permanent crops (trees)
tree <- read_dta(file.path(dataPath, "Post-Harvest/sect12_ph_w2.dta")) %>%
  select(holder_id, household_id, household_id2, crop_code,
         sold_tree=ph_s12q06, sold_tree_kg=ph_s12q07, tree_value=ph_s12q08)

# summarise to the household level
on_farm_income_tree <- group_by(tree, household_id2) %>%
  summarise(tree_value_hh=sum(tree_value, na.rm=TRUE))

# from rented land
rent <- read_dta(file.path(dataPath, "Post-Planting/sect2_pp_w2.dta")) %>%
  select(holder_id, household_id, household_id2, parcel_id, rented=pp_s2q10, rented12=pp_s2q11,
         fields_rented=pp_s2q12, rent_cash=pp_s2q13_a, rent_in_kind=pp_s2q13_b)

# combine cash and in kind rent
rent$rent <- with(rent,
                  rowSums(cbind(rent_cash, rent_in_kind),
                          na.rm=TRUE))
miss <- with(rent,
             is.na(rent_cash) & is.na(rent_in_kind))
rent$rent[miss] <- NA; rm(miss)

# summarise to the household level
on_farm_income_rent <- group_by(rent, household_id2) %>%
  summarise(rent_hh=sum(rent, na.rm=TRUE))

# from livestock
# information on value of livestock not available

#######################################
# calcualte total income 2013
#######################################

income_2013 <- full_join(on_farm_income_crop, on_farm_income_rent)
income_2013 <- full_join(income_2013, on_farm_income_tree)

# sum across all sources of income to 
# get a single income variable
income_2013$income <- with(income_2013,
                           rowSums(cbind(crop_value_hh, rent_hh,
                                         tree_value_hh),
                                   na.rm=TRUE))

# take out trash
rm(crop, dataPath, rent, tree,
   on_farm_income_crop, 
   on_farm_income_tree, on_farm_income_rent)