### Reading in SL HIES 2019

# Following Lucia's code for SF: https://luciasegovia.github.io/Kenya-Budget-2016/methods.html#calulating-energy-requirements-of-household-members 
# package load #############################################################
rq_packages <- c("tidyverse", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))


# lucia's function
# Base URL for raw files
base_url <- "https://raw.githubusercontent.com/LuciaSegovia/Kenya-Budget-2016/gh-pages/functions/Energy_adjustments.R"
source(base_url)

# # List of function files
# function_files <- c(
# "Energy_requirements.R",
# "Energy_adjustments.R",
# "cfe_calucation.R",
# "who_bmi_10-19.R",
# "who_height.R",
# "who_weight.R"
# )

# # Source each file
# for (file in function_files) {
#   source(paste0(base_url, file))
# }




# Read in data #############################################################

#
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






# Data exploration #############################################################
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

create_hhid <- function(df){
  df <- df %>%  mutate(hhid = paste0(psu,snumber,hhno))
  return(df)
}


create_uniqueid <- function(df){
  # creates a unique ID for every hh member
  df <- df %>%  mutate(uniqueid = paste0(psu,snumber,hhno,person_serial_no))
  return(df)
}


HH_expenditure_hh_Income <- create_hhid(HH_expenditure_hh_Income)
SEC_1_DEMOGRAPHIC<- create_hhid(SEC_1_DEMOGRAPHIC)
SEC_2_SCHOOL_EDUCATION <- create_hhid(SEC_2_SCHOOL_EDUCATION)
SEC_3A_HEALTH <- create_hhid(SEC_3A_HEALTH)


# HH_expenditure_hh_Income <- create_uniqueid(HH_expenditure_hh_Income)
SEC_1_DEMOGRAPHIC<- create_uniqueid(SEC_1_DEMOGRAPHIC)
SEC_2_SCHOOL_EDUCATION <- create_uniqueid(SEC_2_SCHOOL_EDUCATION)
# SEC_3A_HEALTH <- create_uniqueid(SEC_3A_HEALTH)



# 1.1 Energy requirements and df structure ----------------------------------------------------
# read in energy requirements data
hh_energy_requirements <- read_csv("data/processed/hh_energy_requirements.csv") |> 
  mutate(
    uniqueid = paste0(hhid,person_serial_no),
    age = ifelse(is.na(age),floor(age_month/12), age),
    hhid = as.character(hhid)
  ) 
  

roster <- SEC_1_DEMOGRAPHIC |> 
  rename(
    relation_head = relationship,
    dob_year = birth_year
  ) |> 
    left_join(SEC_2_SCHOOL_EDUCATION |> 
      rename(
        school_attend = r2_school_education,
        school_attend_last = grade_last_year,
        school_grade = grade_this_year
      )) |> 
  left_join(hh_energy_requirements |> 
    rename(enerc_kcal = TEE,
    months = age_month))|> 
  select(hhid,uniqueid, sex, age,months,afe,weight, enerc_kcal,school_attend,school_grade ,school_attend_last)
 


# Read in data #############################################################
# base_ai <- read_csv("data/processed/sl_ml_targets_2025-11-13.csv")
base_ai <- read_csv("data/processed/base_ai.csv")
hh_info <- readRDS("data/processed/hh_info.RDS")

# 1.2 Energy adjustment ----------------------------------------------------
roster_adjust <- roster |> 
  mutate(school_feed = ifelse(school_attend == 1 & !is.na(school_grade), "1", "0")) |> 
  rename(age_y = age,age_m = months)


roster_adjusted <- Enerc_adjustment(roster_adjust, excl.bf = F, excl.age = 6, comple.bf = F, prev.complebf =  0.6, school = T, feeding = T, school_days = 180)


# 1.3 household food allocation -------------------------------------------

Energy_afe  = 2170

roster_afe <- roster_adjusted %>% 
  mutate(
    afe = enerc_kcal/Energy_afe,
    afe_school = enerc_kcal_school/Energy_afe, 
    afe_feed = enerc_kcal_feeding/Energy_afe) 



# Calculating HH AFE
roster_afe_hh <- roster_afe %>% 
  group_by(hhid) %>% 
# Getting the AFE, per AFE (+SM), per AFE (+ all SAC receiving SM)
  summarise(hhid, 
            afe =sum(afe),
            afe_school = sum(afe_school),
            afe_feed = sum(afe_feed)) %>% 
  distinct()


# school children 
sac_only <-  roster_afe %>% 
  group_by(hhid) %>% 
  summarise(hhid, uniqueid,age_y,
    indv = round(afe_school/sum(afe_school), 2)) %>% # calculating the proportional food allocation
  filter(age_y >=6 & age_y<=13) # filtering SAC

# 1.4 -----------------------------------------------------------------------------
nutrient_summary <- base_ai %>% mutate(hhid = as.character(hhid)) |> left_join(hh_info  |> select(hhid,afe)) |> 
  mutate(
    across(
      -c(afe,hhid,
         survey,iso3,month),
      ~.*afe
)) |> 
  select(-c(afe)) |> 
  ungroup() %>% 
  select(-c(, iso3,month))


nutrient_afe <-
  nutrient_summary %>%  # HH app. nutrient intakes
  
  left_join(., roster_afe_hh |>  ungroup()%>% pivot_longer(cols =starts_with("afe") , 
   names_to = "hh_alloc")) %>%  # Getting per AFE, per AFE (+SM), per AFE (+ all SAC receiving SM)
  mutate(
    across(-c(hhid,hh_alloc), ~./as.numeric(value), .names = "hh_alloc_{.col}")) # Calculating for all nut. available


#
nutrient_sac <- sac_only %>% 
  ungroup() |> 
  left_join(nutrient_summary) %>% 
  mutate( across(-c(hhid, uniqueid,age_y, indv), ~.*as.numeric(indv)
                 )) 


# Save the data sets


saveRDS(nutrient_sac, 'data/school_feeding/nutrient_sac.RDS')
saveRDS(nutrient_afe, 'data/school_feeding/nutrient_afe.RDS')

rm(list = ls())

