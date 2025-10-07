source("R/packages.R")


path_to_data <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/bangladesh_base_model/"


bgd_hies22_fct <- read.csv(paste0(path_to_data, "bgd_hies22_fct.csv"))
bgd_hies22_food_consumption <-read.csv(paste0(path_to_data, "bgd_hies22_food_consumption.csv"))
bgd_hies22_base_ai <- read.csv(paste0(path_to_data, "bgd_base_ai.csv"))
bgd_hies22_afe <- read.csv(paste0(path_to_data,"bgd_hies22_hh_info.csv"))


bgd_sens_matching <- bgd_hies22_food_consumption %>% 

  left_join(bgd_hies22_fct %>% 
              select(item_code,item_name, ends_with("kcal"),
                     ends_with("_g"),
                     ends_with("_mcg"),
                     ends_with("_mg")), by= 'item_code') %>% 
  left_join(bgd_hies22_afe %>% 
              select(hhid,afe)) %>% 
  mutate(
    quantity_g = quantity_g/afe,
    quantity_100g = quantity_100g/afe,
    across(
      -c(hhid, item_code, item_name,quantity_g,quantity_100g),
      ~as.numeric(.x)*quantity_100g
    ))


path_to_save <- "data/processed/"
write.csv(bgd_sens_matching, paste0(path_to_save,"bgd_sens_matching.csv"))

rm(bgd_hies22_fct,bgd_hies22_food_consumption,bgd_hies22_base_ai,bgd_hies22_afe,bgd_sens_matching,path_to_save,path_to_data)
