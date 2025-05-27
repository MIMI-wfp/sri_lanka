### Reading in SL HIES 2019

rq_packages <- c("tidyverse", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))


################################################################################


path_to_survey <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/HIES_2019/HIES_2019/"

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
  mutate(hhid = paste0(district,sector,psu,snumber,hhno)) %>% 
  distinct(hhid)

# creates unque hhid 

create_hhid <- function(df){
  df <- df %>%  mutate(hhid = paste0(district,sector,psu,snumber,hhno))
  return(df)
}

HH_expenditure_hh_Income <- create_hhid(HH_expenditure_hh_Income)
SEC_1_DEMOGRAPHIC <- create_hhid(SEC_1_DEMOGRAPHIC)
SEC_2_SCHOOL_EDUCATION <- create_hhid(SEC_2_SCHOOL_EDUCATION)
SEC_3A_HEALTH <- create_hhid(SEC_3A_HEALTH)
SEC_4_1_FOOD_EXP <- create_hhid(SEC_4_1_FOOD_EXP)


## Explore the data 

check_nas <- function(df){
  df %>% summarise(across(
    everything(),
    ~sum(is.na(.))
  )  )
}

check_nas(HH_expenditure_hh_Income)
#no missing

check_nas(SEC_1_DEMOGRAPHIC)
# no missing datam for age, residence, relationship or sex for building the afe
# some for education, activity level that we could fill if needed

check_nas(SEC_2_SCHOOL_EDUCATION)
# some educ varaibles - discuss with Vasia if we need to do anything this end

check_nas(SEC_3A_HEALTH)

check_nas(SEC_4_1_FOOD_EXP)# some missing quantities - look into further

# missing quantities for 

food_items_missing<- SEC_4_1_FOOD_EXP %>% 
  group_by(code) %>% 
  summarise(
    nas = sum(is.na(quantity)),
    percent = nas/n()
  ) %>% 
  arrange(desc(nas)) %>% 
  filter(percent != 0)


SEC_4_1_FOOD_EXP %>% 
  group_by(code, hhid) %>% 
  summarise(
    nas = sum(is.na(quantity)),
    percent = nas/n()
  ) %>% 
  arrange(desc(nas)) %>% 
  filter(percent != 0)


################################################################################

# AFE calculation

SEC_1_DEMOGRAPHIC %>% 
  select(hhid, person_serial_no, relationship, sex, age,main_activity, is_active)

