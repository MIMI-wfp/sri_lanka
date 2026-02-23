### Reading in SL HIES 2019

rm(list = ls())
rq_packages <- c("tidyverse", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))
readRenviron(".Renviron")


################################################################################


path_to_survey <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/HIES_2019/HIES_2019/"
path_to_data <- "data/processed/"
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
sl_fct <- readxl::read_xlsx(paste0(path_to_raw_data,"sri_lanka_food_matches_pre_collab.xlsx"), 
                            sheet = 1)
# gabriel conversion factors
conversion_factor <- readxl::read_xlsx(paste0(path_to_raw_data, "conversion_factor_sl.xlsx"), 
                                       sheet = 2)

########################################################################
# SOME OF THESE VALUES ARE WRONG SAFER TO USE MY FACTORS

#DCS convesion factors - 
# conversion_factor <- readxl::read_xlsx("data/raw/Item_kcal_coeff_2019_wfp.xlsx") %>%  
#   select(itemcode, grameq) %>% 
#   rename(code = itemcode,
#          conversion_to_grams  = grameq)


########################################################################



hh_info <- readRDS(paste0(path_to_data,"hh_info.RDS"))

# 
get_har <- function(){

  con <- DBI::dbConnect(RMySQL::MySQL(),
                   dbname = Sys.getenv("DB_NAME"),
                   host = "127.0.0.1",
                   port = 3306,
                   user = Sys.getenv("DB_USER"),
                   password =  Sys.getenv("DB_PASSWORD"))


  # collect information from database

  h_ar <<- DBI::dbReadTable(con, "h_ar")

  # DBI::dbReadTable(con, "ML_targets")
  # # disconnect
  DBI::dbDisconnect(con)
  return(h_ar)
}
har <- get_har() %>% 
  filter(iso3 == "ETH")




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


converted_food <- SEC_4_1_FOOD_EXP %>%
  left_join(conversion_factor, by = "code") %>%
  mutate(conversion_to_grams = ifelse(is.na(conversion_to_grams), 1, conversion_to_grams),
         quantity = quantity*conversion_to_grams)

conversion_factor
#check the numbers of people consuming each food 
converted_food_hh <- converted_food %>% 
  filter(conversion_to_grams != 1) %>% 
  group_by(code) %>% 
  summarise(
    perc_hh = n()*100/19911,
    n_hh = n()
    )


converted_food$group <- floor(as.numeric(converted_food$code) / 100)

converted_food %>% 
  filter(code == 1803)

# Define your imputation function





impute_quantity <- function(group_df, missing_item, imputing_item) {
  # Only fit model if enough complete cases
  
  imputing_df <- group_df %>% filter(code %in% imputing_item)
  
  model <- lm(quantity ~ value+0, data = imputing_df, na.action = na.exclude)
  missing_idx <- which(group_df$code == missing_item)
  group_df$quantity[missing_idx] <- predict(model, newdata = group_df[missing_idx, ])
  return(group_df)
}

#converting an outlier
converted_food <- converted_food %>% 
  mutate(value = ifelse(code == 217 & value>40000,5500,value))

# Apply the imputation function by food item
imputed_food <- converted_food



imputed_food <- impute_quantity(imputed_food, 218,217)#purchased food 
imputed_food <- impute_quantity(imputed_food, 220,217)#purchased food 
imputed_food <- impute_quantity(imputed_food, 229,217)#purchased food
# imputed_food <- impute_quantity(imputed_food, 435,)# jackfruit
imputed_food <- impute_quantity(imputed_food, 439,c(401:434))#other vegetable 
imputed_food <- impute_quantity(imputed_food, 459,c(447,448,449,450))# other leafy greens
# imputed_food <- impute_quantity(imputed_food, 503,)# jackfruit
imputed_food <- impute_quantity(imputed_food, 1304,c(1301,1302))# curd
imputed_food <- impute_quantity(imputed_food, 1305,c(1301,1302))# yogurt
imputed_food <- impute_quantity(imputed_food, 1319,c(1309,1310,1311))# other milk products
imputed_food <- impute_quantity(imputed_food, 1504,1503)
imputed_food <- impute_quantity(imputed_food, 1509,c(1501,1502,1503,1504))
imputed_food <- impute_quantity(imputed_food, 1619,c(1601,1602,1603,1604,1605,1606,1607,1608,1609,1610,
                                                     1611,1612,1613,1614,1615,1616))
imputed_food <- impute_quantity(imputed_food, 1702,1703)
imputed_food <- impute_quantity(imputed_food, 1706,1704)
imputed_food <- impute_quantity(imputed_food, 1719,c(1710,1711,1712,1713,1714))
imputed_food <- impute_quantity(imputed_food, 1803,1805)
imputed_food <- impute_quantity(imputed_food, 1804,1805)
imputed_food <- impute_quantity(imputed_food, 1819,1812)


imputed_food%>% 

  filter(is.na(quantity))

  # filter(code == 1819)





# check_nas(imputed_food)


food_groups <- unique(imputed_food$group)



#-------------------------------------------------------------------------------
# combine total consumption with AFEs

edible_food <- imputed_food %>% 
  select(hhid, code, quantity) %>% 
  left_join(sl_fct %>% select(code, edible_portion)) %>% 
  mutate(quantity = ifelse(is.na(edible_portion),quantity,  quantity*edible_portion))


food_afe <- edible_food %>% 
  select(hhid, code, quantity) %>% 
  left_join(hh_info %>% select(hhid,afe) %>% group_by(hhid) %>% slice(1) %>% ungroup(), by= 'hhid') %>% 
  mutate(quantity_ai = quantity/(7*afe)) 

# rm(imputed_food, converted_food)
anti_join(edible_food, hh_info, by = "hhid") %>% distinct(hhid)





# filter outliers #############################################################
food_afe <- food_afe %>% 
  mutate(log_quantity_g = log(quantity_ai))

#create cut points above which we have outliers
quant_cutpoints  <- food_afe %>% 
  group_by(code) %>% 
  summarise(
    mean_log = mean(log_quantity_g, na.rm = T),
    sd_log = sd(log_quantity_g, na.rm = T)) %>% 
  mutate(upper_cut = mean_log+2.5*sd_log) %>% 
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
                              quantile(quantity_ai,probs = 0.95, na.rm =T),
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


food_afe$group <- floor(as.numeric(food_afe$code) / 100)
# 
for (group_num in 1:11) {
  dat <- food_afe %>% 
    filter(group == group_num, !is.na(quantity_ai))
  
  if (nrow(dat) == 0) {
    message("Group ", group_num, ": no data after filtering; skipping.")
    next
  }
  
  p <- ggplot(dat, aes(x = quantity_ai)) +
    geom_histogram(color = "white", bins = 30) +
    facet_wrap(~ code, scales = "free_x") +
    labs(
      title = paste("Distriibution  quantity_ai — group", group_num),
      x = "quantity_ai", y = "Frequency"
    ) +
    theme_minimal(base_size = 12)
  
  print(p)
}



for (group_num in 12:19) {
  dat <- food_afe %>% 
    filter(group == group_num, !is.na(quantity_ai))
  
  if (nrow(dat) == 0) {
    message("Group ", group_num, ": no data after filtering; skipping.")
    next
  }
  
  p <- ggplot(dat, aes(x = quantity_ai)) +
    geom_histogram(color = "white", bins = 30) +
    facet_wrap(~ code, scales = "free_x") +
    labs(
      title = paste("Distribution  quantity_ai — group", group_num),
      x = "quantity_ai", y = "frequency"
    ) +
    theme_minimal(base_size = 12)
  
  print(p)
}


# ------------------------------------------------------------------------------
# inital match to food i



food_mn <- food_afe %>% 
  left_join(sl_fct %>% 
              select(code,item_name, ends_with("kcal"),
                     ends_with("_g"),
                     ends_with("_mcg"),
                     ends_with("_mg")), by= 'code') %>% 
  mutate(
    
    across(
      -c(hhid, code, item_name,quantity_ai,quantity_100g),
      ~as.numeric(.x)*quantity_100g
    ))


sens_matching <- food_mn

# household apparent intake

hh_ai <- food_mn %>% 
  group_by(hhid) %>% 
  summarise(across(
    -c(code,item_name,quantity_100g),
    ~sum(.x, na.rm = TRUE)
  )) %>% 
  select(-c("quantity_ai"))



hh_ai %>% 
  ggplot()+
  geom_histogram(aes(x = energy_kcal))

food_consumption <- food_afe %>% rename(quantity_g = quantity_ai, 
                                        item_code = code)


## targets for ML

calc_nar <- function(h_ar, comparison){return(ifelse(comparison<h_ar,comparison/h_ar,1))}



sl_ml_targets <- hh_ai %>% 
  select(hhid,vita_rae_mcg,folate_mcg,vitb12_mcg,
         fe_mg,zn_mg) %>% 
  left_join(hh_info |> select(hhid)) |> 
  mutate(
    vita_nar = calc_nar(h_ar$vita_rae_mcg[1], vita_rae_mcg),
    fol_nar = calc_nar(h_ar$folate_mcg[1],folate_mcg),
    vitb12_nar = calc_nar(h_ar$vitb12_mcg[1], vitb12_mcg),
    fe_nar = calc_nar(15,fe_mg),
    zn_nar = calc_nar(8.9,zn_mg),#for sri lanka it is lower
    overall_mar = (vita_nar+fol_nar+vitb12_nar+fe_nar+zn_nar)/5,
    va_ref = 490,
    fol_ref = 250,
    vb12_ref = 2,
    fe_ref = 15,
    zn_ref = 8.9,
    survey = "lka_hies19",
    iso3 = "LKA",
  ) %>% 
  select(iso3,survey,hhid,vita_rae_mcg,folate_mcg,vitb12_mcg,
         fe_mg,zn_mg,
         overall_mar) 


### data base read


base_ai <- hh_ai %>% 
  mutate(survey = 'lka_hies19',
         iso3 = 'LKA',
         vitd_mcg = NA) %>% 
  select(survey, hhid,iso3, energy_kcal,vita_rae_mcg,thia_mg,ribo_mg,niac_mg,
         vitb6_mg, vitd_mcg, folate_mcg,vitb12_mcg,vitc_mg,ca_mg,fe_mg,zn_mg)

food_consumption_db <- food_consumption %>% 
  mutate(iso3 = 'LKA',
         survey  = 'lka_hies19') %>% 
  select(iso3, survey, hhid,item_code, quantity_g,quantity_100g) %>% 
  mutate(item_code = as.character(item_code))

sl_fct_db <- sl_fct %>% 
  mutate(
    item_code = code,    
    survey = 'lka_hies19',
    iso3 = 'LKA',
    zone = NA,
    ) %>% 
  select(item_code, iso3, survey, zone, item_name, energy_kcal,vita_rae_mcg,thia_mg,ribo_mg,niac_mg,
         vitb6_mg, folate_mcg,vitb12_mcg,vitc_mg,ca_mg,fe_mg,zn_mg)



# food groups
food_group <- read_csv('data/food_group.csv')
unique(food_group$food_group)
food_group_db <- food_group %>% mutate(iso3 = 'LKA', survey = 'lka_hies19') %>% 
  select(item_code, iso3, survey,food_group)

h_ar_lka <- h_ar %>%
  filter(iso3 == "BEN") %>%
  mutate(iso3 = "LKA",
         energy_kcal = 2170,
         niac_mg = 11.9,
         ca_mg = 750,
         fe_mg = 15,
         zn_mg= 8.9)

################################################################################

# food quantities
# 
# write.csv(food_consumption, paste0(path_to_data, "food_consumption.csv"))
# write_rds(food_consumption, paste0(path_to_data, "food_consumption.RDS"))
# 
# # base ai
# 
# write.csv(hh_ai, paste0(path_to_data, "base_ai.csv"))
# write_rds(hh_ai, paste0(path_to_data, "base_ai.RDS"))
# 
# write.csv(sens_matching, paste0(path_to_data,"sens_matching.csv"))
# # 
# # rm(list = ls())
# 
# 
# write_csv(sl_ml_targets,paste0(path_to_data,"sl_ml_targets_", Sys.Date(),".csv"))
# 
# # database csv read
# 
# write.csv(base_ai, paste0(path_to_data, "database_upload/base_ai.csv"))
# write.csv(food_consumption_db, paste0(path_to_data, "database_upload/food_consumption.csv"))
# write.csv(sl_fct_db, paste0(path_to_data, "database_upload/fct.csv"))
# write.csv(food_group_db, paste0(path_to_data, "database_upload/food_group.csv"))
# write.csv(h_ar_lka, paste0(path_to_data, "database_upload/h_ar.csv" ))
# # 
# rm(list = ls())




