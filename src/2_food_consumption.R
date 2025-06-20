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

# read in fct
sl_fct <- readxl::read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/sri_lanka_food_matches.xlsx", 
                  sheet = 1)
conversion_factor <- readxl::read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/conversion_factor_sl.xlsx", sheet = 1)




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

# missing quantities for some food items
food_items_missing<- SEC_4_1_FOOD_EXP %>% 
  group_by(code) %>% 
  summarise(
    nas = sum(is.na(quantity)),
    percent = nas/n()
  ) %>% 
  arrange(desc(nas)) %>% 
  
  filter(percent != 0) %>% 
  filter( !grepl("^11|^19", code))


SEC_4_1_FOOD_EXP %>% 
  group_by(code, hhid) %>% 
  summarise(
    nas = sum(is.na(quantity)),
    percent = nas/n()
  ) %>% 
  arrange(desc(nas)) %>% 
  filter(percent != 0)


# convert all food items to grams


converted_food <- SEC_4_1_FOOD_EXP
# %>% 
  # left_join(conversion_factor, by = "code") 
# %>% 
  # mutate(conversion_to_grams = ifelse(is.na(conversion_to_grams), 1, conversion_to_grams),
         # quantity = quantity*conversion_to_grams) 

# imputation of missing values #######

# create food group based on the survey collection
converted_food$group <- floor(as.numeric(converted_food$code) / 100)


# check assumption that quantity ~ value

groups <- c(1:19)

for(group_num in groups){
  print(group_num)
  
  plot <- converted_food %>% 
    filter(group == group_num) %>% 
    ggplot(aes(value, quantity)) +
    geom_point(alpha = 0.5) +
    geom_smooth(formula = y~x+0) + # assume it goes through zero, zero value = zero quantity
    ggtitle(paste("Group", group_num))
  
  print(plot)  
}





impute_quantity <- function(group_df) {
  # Only fit model if we have enough non-missing data
  if (sum(!is.na(group_df$quantity)) >= 0) {
    model <- lm(quantity ~ value, data = group_df, na.action = na.exclude)
    # Predict quantity where it is NA
    group_df$quantity[is.na(group_df$quantity)] <- predict(model, newdata = group_df[is.na(group_df$quantity), ])
  }
  return(group_df)
}



imputed_food <- impute_quantity(converted_food) 
check_nas(imputed_food)



#-------------------------------------------------------------------------------
# combine total consumption with AFEs

edible_food <- imputed_food %>% 
  select(hhid, code, quantity) %>% 
  left_join(sl_fct %>% select(code, edible_portion)) %>% 
  mutate(quantity = ifelse(is.na(edible_portion),quantity,  quantity*edible_portion))


food_afe <- edible_food %>% 
  select(hhid, code, quantity) %>% 
  left_join(hh_afe, by= 'hhid') %>% 
  mutate(quantity_ai = quantity/(7*afe)) 

# rm(imputed_food, converted_food)




food_afe %>% 
  filter(code == 308) %>% 
  ggplot()+
  geom_histogram(aes(x = quantity_ai))


# filter outliers #############################################################
food_afe <- food_afe %>% 
  mutate(log_quantity_g = log(quantity_ai))

#create cut points above which we have outliers
quant_cutpoints  <- food_afe %>% 
  group_by(code) %>% 
  summarise(
    mean_log = mean(log_quantity_g, na.rm = T),
    sd_log = sd(log_quantity_g, na.rm = T)) %>% 
  mutate(upper_cut = mean_log+2*sd_log) %>% 
  select(code, upper_cut) %>% 
  ungroup()

# change 
food_afe <- food_afe %>% 
  left_join(quant_cutpoints, by = "code") %>% 
  mutate(quantity_ai = case_when(
    log_quantity_g>=upper_cut ~ NA_real_,
    TRUE ~ quantity_ai
  )) %>% 
  select(-log_quantity_g,-upper_cut)

food_afe <- food_afe %>% 
  group_by(code) %>% 
  mutate(quantity_ai = ifelse(is.na(quantity_ai),
                                             median(quantity_ai, na.rm =T),
                              quantity_ai)) %>% 
  ungroup()


rm(quant_cutpoints)



food_afe %>% 
  filter(code == 105) %>% 
  ggplot()+
  geom_histogram(aes(x = quantity_ai))
  

food_afe <- food_afe %>% 
  mutate(quantity_100g = quantity_ai/100) %>% 
  select(hhid, code, quantity_ai, quantity_100g)

# ------------------------------------------------------------------------------
# inital match to food i
food_mn <- food_afe %>% 
  left_join(sl_fct %>% 
              select(code, ends_with("kcal"),
                     ends_with("_g"),
                     ends_with("_mcg"),
                     ends_with("_mg")), by= 'code') %>% 
  mutate(

    across(
    -c(hhid, code,quantity_ai,quantity_100g),
    ~as.numeric(.x)*quantity_100g
  ))

# household apparent intake

hh_ai <- food_mn %>% 
  group_by(hhid) %>% 
  summarise(across(
    -c(code,quantity_100g),
    ~sum(.x, na.rm = TRUE)
  ))

hh_ai %>% 
  ggplot()+
  geom_histogram(aes(x = energy_kcal))






            