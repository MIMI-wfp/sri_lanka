### Reading in SL HIES 2019
rm(list = ls())
rq_packages <- c("tidyverse", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))


################################################################################

## THIS WILL CHANGE DEPENDING ON WHERE YOU SAVE YOUR DATA ######
path_to_survey <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/HIES_2019/HIES_2019/"
path_to_raw_data <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/"

module_list <- list.files(path = path_to_survey, pattern = NULL, all.files = FALSE,
                          full.names = FALSE, recursive = FALSE,
                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)



# Read in only the modules i want right now
modules <- c("HH_expenditure_hh_Income.csv","SEC_1_DEMOGRAPHIC.csv", "SEC_2_SCHOOL_EDUCATION.csv",
             "SEC_3A_HEALTH.csv","SEC_4_1_FOOD_EXP.csv", "weight_2019.csv")

rm(module_list)

#read in modules
for(file in modules){
  name <- sub("\\.csv$", "", file)
  assign(name, read.csv(paste0(path_to_survey, file)))   
}

# read in fct
sl_fct <- readxl::read_xlsx(paste0(path_to_raw_data, "sri_lanka_food_matches.xlsx"),
                            sheet = 1)
conversion_factor <- readxl::read_xlsx(paste0(path_to_raw_data,"conversion_factor_sl.xlsx"), 
                                       sheet = 2)




# Data exploration #############################################################
# 

HH_expenditure_hh_Income %>% 
  head(5)

SEC_1_DEMOGRAPHIC %>% 
  head(5)

SEC_2_SCHOOL_EDUCATION %>% 
  head(5)

SEC_3A_HEALTH %>% 
  head(5)

SEC_4_1_FOOD_EXP %>% 
  head(5)


# create a hh id with district, sector, psu, snumber and hhno

HH_expenditure_hh_Income %>% 
  mutate(hhid = paste0(psu,snumber,hhno)) %>% 
  distinct(hhid)

# creates unque hhid 

create_hhid <- function(df){
  df <- df %>%  mutate(hhid = paste0(psu,snumber,hhno))
  return(df)
}

HH_expenditure_hh_Income <- create_hhid(HH_expenditure_hh_Income)
SEC_1_DEMOGRAPHIC <- create_hhid(SEC_1_DEMOGRAPHIC)
SEC_2_SCHOOL_EDUCATION <- create_hhid(SEC_2_SCHOOL_EDUCATION)
SEC_3A_HEALTH <- create_hhid(SEC_3A_HEALTH)
SEC_4_1_FOOD_EXP <- create_hhid(SEC_4_1_FOOD_EXP)








################################################################################

# AFE calculation

# set AFE constant 
afe_value <- 2170 #https://www.mri.gov.lk/wp-content/uploads/2024/02/Dietary-Referance-Intakes-for-Sri-Lanka.pdf

demographics <- SEC_1_DEMOGRAPHIC %>% 
  select(hhid, person_serial_no, relationship, sex, age, birth_year, birth_month, month)


# households with under 2s for breastfeeding
u2s <- demographics %>% 
  filter(age<=2) %>% 
  mutate(birth_month = as.Date(paste0(15,'-',birth_month,'-',birth_year), "%d-%m-%y"),
         survey_date = as.Date(paste0(15,'-',month,'-',19), "%d-%m-%y"),
         age_month = floor(as.numeric(survey_date - birth_month)/30)) %>% 
  select(hhid, person_serial_no, age_month)




# give a 1 for hh with a 2 or under
hh_with_u2s <- u2s %>% 
  filter(age_month<=24) %>% 
  group_by(hhid) %>% 
  summarise(u2 = 1)


# join the dataframes in
demographics <- demographics %>% 
  left_join(hh_with_u2s, by = 'hhid')

rm(hh_with_u2s)

# energy requirements for u2s --------------------------------------------------
u2s <- u2s %>%
  mutate(TEE = case_when( # https://www.mri.gov.lk/wp-content/uploads/2024/02/Dietary-Referance-Intakes-for-Sri-Lanka.pdf
    age_month < 3 ~ 0,   # only breast feeding - no food intake
    age_month >= 3 & age_month < 6  ~ 76,  # 
    age_month >= 6 & age_month < 9  ~ 269,  # 
    age_month >= 9 & age_month < 12 ~ 451,
    age_month >= 12 ~ 746 # 746 kcal per day for those aged 12-months - 2years
  ),
  weight = case_when( # https://www.mri.gov.lk/wp-content/uploads/2024/02/Dietary-Referance-Intakes-for-Sri-Lanka.pdf
    age_month < 3 ~ 4.35,   # only breast feeding - no food intake
    age_month >= 3 & age_month < 6  ~ 6.7,  # 
    age_month >= 6 & age_month < 9  ~ 7.95,  # 
    age_month >= 9 & age_month < 12 ~ 8.85,
    age_month >= 12 ~ 10.55
  ))# 746 kcal for those without a birth certificate, assuming they can be older

# AFE calculation for children below 2 years old:
afeu2 <- u2s %>%
  mutate(afe = TEE/afe_value) %>% # 1AFE = 2100kcal
  select(hhid, person_serial_no, afe, weight, TEE, age_month)


# breastfeeding women

breastfeeding <- demographics %>%
  filter(sex == 2 &
           age > 15 & age < 45 &
           u2 == 1
  ) %>% 
  group_by(hhid) |> 
  slice(1)


afe_breastfeeding <- breastfeeding %>% 
  mutate(
    PAL = 1.76,
    weight = 55,# assumption women 55kg
    BMR = case_when(
      age > 18 & age <= 30 ~ 14.818 * weight + 486.6,
      age > 30 & age < 60 ~ 8.126 * weight + 845.6
    ),
    TEE = case_when(age > 15 & age <= 18 ~ 2400)
  ) %>% 
  mutate(TEE = ifelse(is.na(BMR), TEE +483, BMR * PAL + 483),
         afe = TEE/afe_value) %>% 
  select(hhid, person_serial_no, weight, afe, TEE,age)




# all others ages


demographics_others <- demographics %>% 
  anti_join(u2s, by = c('hhid', 'person_serial_no')) %>% 
  anti_join(breastfeeding, by = c('hhid', 'person_serial_no'))

rm(u2s)
rm(breastfeeding)

tee_calc <- demographics_others %>%
  
  mutate(weight = ifelse(sex == 1, 65, 55)) %>% # Assumed average weight of men = 65kg
  # Assumed average weight of women = 55kg
  filter(age >= 2) %>%  # Remove under 2's as these have already been calculated above
  mutate(PAL = ifelse(age > 18, 1.76, NA))  

# TEE FOR ALL PEOPLE (2-70 years old) using sri lankan data
tee_calc <- tee_calc %>% 
  mutate(TEE = case_when(    age >= 1 & age < 4  ~ 990,
                             age >= 4 & age < 7 ~ 1560,
                             age >= 7 & age < 11 ~ 1920, 
                             sex == 1 & (age >= 11 & age < 15) ~ 2390,
                             sex == 2 & (age >= 11 & age < 15) ~ 2180,
                             sex == 1 & (age >= 15 & age < 18) ~ 3020,
                             sex == 2 & (age >= 15 & age < 18) ~ 2400,
                             sex == 1 & (age >= 18 & age < 25) ~ 2840,
                             sex == 2 & (age >= 18 & age < 25) ~ 2280,
                             sex == 1 & (age >= 25 & age < 51) ~ 2930,
                             sex == 2 & (age >= 25 & age < 51) ~ 2170,
                             sex == 1 & (age >= 51 & age < 71) ~ 2560,
                             sex == 2 & (age >= 51 & age < 71) ~ 2070,
                             sex == 1 & (age >= 71) ~ 2410,
                             sex == 2 & (age >= 71) ~ 1960
                           ))


# # TEE FOR ADULTS (Formula from table 5.2 in FAO/WHO/UNU (2004)):
# tee_calc <- tee_calc %>% 
#   mutate(BMR = case_when( # Firstly need to calculate BMR for different age categories:
#     sex == 1 & age >18 & age <= 30 ~ 15.057 * weight + 692.2,
#     sex == 1 & age >30 & age < 60 ~ 11.472 * weight + 873.1,
#     sex == 1 & age >= 60 ~ 11.711 * weight + 587.7,
#     sex == 2 & age >18 & age <= 30 ~ 14.818 * weight + 486.6,
#     sex == 2 & age >30 & age < 60 ~ 8.126 * weight + 845.6, 
#     sex == 2 & age >= 60 ~ 9.082 * weight + 658.5,
#     TRUE ~ NA)) %>% # Get TEE by multiplying BMR by PAL for over 18's: 
#   mutate(TEE = ifelse(age > 18, BMR * PAL, TEE)) # 

afe_others <- tee_calc %>% 
  mutate(afe = TEE/afe_value)%>% # 1AFE = 2100kcal
  select(hhid, person_serial_no, afe, weight, TEE,age)

rm(tee_calc)







#-------------------------------------------------------------------------------

# CALCULATE AFE FOR ALL OTHER INDIVIDUALS: 
# afe_other <- demographic_others %>% 
#   left_join(tee_calc %>% select(hhid, numind, TEE),
#             by = c("hhid", "numind")) %>% 
#   select(-ends_with(".y"), -resid) %>%
#   rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) %>%
#   # Calculate AFE:
#   mutate(afe = TEE / 2291) %>%  # AFE = Total energy expenditure / 2291kcal/day
#   select(hhid, numind, afe)



#-------------------------------------------------------------------------------
# PUT ALL TOGETHER


afe_all<- bind_rows(afeu2, afe_breastfeeding, afe_others)

#check nothing missing

demographics %>% 
  anti_join(afe_all, by = c('hhid', 'person_serial_no'))
# no-one missed

afe_all %>% 
  group_by(afe) %>% 
  summarise(total = n())

afeu2 %>% 
  group_by(afe) %>% 
  summarise(total = n())


# zeros come from babies





afe_all %>% 
  filter(is.na(afe))
# no nas

afe_all %>% 
  ggplot(aes(x = afe)) + 
  geom_histogram()

rm(afe_breastfeeding, afe_others, afeu2)

# ------------------------------------------------------------------------------
# calculate household afe

hh_afe <- afe_all %>% 
  group_by(hhid) %>% 
  summarise(afe = sum(afe),
            total = n())

hh_size = demographics %>% 
  group_by(hhid) %>% 
  summarise(total = n())

# check it makes sense, draw dot plot

HH_expenditure_hh_Income %>%
  left_join(hh_afe, by = 'hhid') %>%
  select(afe, hhsize, total) %>% 
  # filter(hhsize == 1)
  ggplot(aes(x = total, y = afe))+
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")

## the value under `hhsize` from HH expenditure does not match the number of people responding in the demographics part

HH_expenditure_hh_Income %>% 
  left_join(hh_size, by = 'hhid') %>% 
  ggplot(aes(x = hhsize, y = total))+
  geom_point(alpha = 0.5)+
  geom_abline(intercept =  0, slope = 1, color = 'red')



# create hh info variable ------------------------------------------------------

# adm1, adm2 and sector
hh_info <- HH_expenditure_hh_Income %>% 
  left_join(demographics %>% select(hhid,month) %>% 
              group_by(hhid) %>%  slice(1),by = 'hhid') %>% 
  left_join(hh_afe, by = 'hhid') %>% 
  mutate(
    survey = "lka_hies19",           
    iso3 = "LKA",
    zone = NA,
    adm1 = as.character(floor(district/10)),
    adm2 = as.character(district),
    res = factor(case_when(
      sector == 1 ~ "Urban",
      sector == 2 ~ "Rural",
      sector == 3 ~ "Estate"
    )),
    ea = as.character(psu),
    year = 2019, 
    survey_wgt = finalweight) %>% 
  
    group_by(sector) %>% 
      mutate(
        per_capita_expenditure = hhexppm/total, # NOTE: `hhsize` seems to be different that AFE... need to know why..
        
        res_quintile =
               case_when(
                 per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[2]]~
                   1,
                 per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[3]]~
                   2,
                 per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[4]]~
                   3,
                 per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[5]]~
                   4,
                 per_capita_expenditure<=quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[6]]~
                   5,
               )) %>% 
      ungroup() %>% 
      mutate(sep_quintile =
               case_when(
                 per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[2]]~
                   1,
                 per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[3]]~
                   2,
                 per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[4]]~
                   3,
                 per_capita_expenditure<quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[5]]~
                   4,
                 per_capita_expenditure<=quantile(per_capita_expenditure,probs = seq(0,1,0.2), na.rm = TRUE)[[6]]~
                   5,
               )) %>% 
  rename(pc_expenditure = per_capita_expenditure) %>% 
  select(survey,hhid, iso3, zone,adm1,adm2,ea,res,sep_quintile,res_quintile, year, month, survey_wgt, afe, pc_expenditure)




################################################################################
path_to_save = "data/processed/"
write_csv(afe_all, paste0(path_to_save,"hh_energy_requirements.csv"))
write_csv(hh_info, paste0(path_to_save, "database_upload/hh_info.csv"    ))
saveRDS(hh_info, paste0(path_to_save, "hh_info.RDS"    ))

rm(list = ls())
