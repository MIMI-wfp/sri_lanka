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
             "SEC_3A_HEALTH.csv","SEC_4_1_FOOD_EXP.csv", "weight_2019.csv","SEC_4_2_NONFOOD.csv")

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
SEC_4_2_NONFOOD <- create_hhid(SEC_4_2_NONFOOD)

path_to_data <- "data/processed/"
hh_info <- readRDS(paste0(path_to_data,"hh_info.RDS"))

###############################################################################

food_exp <- SEC_4_1_FOOD_EXP %>% 
  group_by(hhid) %>% 
  summarise(food_value = sum(value)) %>% 
  mutate(food_value_pm = (food_value/7)*365/12)



SEC_4_2_NONFOOD <- SEC_4_2_NONFOOD %>% filter(nf_code %in% c(2101,2102, 2103,2106)) %>%
  mutate(value = ifelse(!is.na(nf_inkind_value),nf_inkind_value+nf_value, nf_value ))


fuel_exp <- SEC_4_2_NONFOOD %>% 
  group_by(hhid) %>% 
  summarise(hhfuelexppm = sum(value))

expenditure = HH_expenditure_hh_Income %>% 
  select(hhid, hhexppm, hhfoodexppm, hhincomepm,hhsize     ) %>% 
  left_join(fuel_exp) %>% 
  mutate(perc_food = hhfoodexppm/hhexppm*100,
         perc_fuel = hhfuelexppm/hhexppm*100
         ) %>% 
  left_join(food_exp)



res_qunitile_exp <- hh_info %>% 
  left_join(expenditure) %>% 
  as_survey_design(ids = ea, strata = res, weights = survey_wgt) %>% 
  mutate(res_quintile = paste(res,res_quintile)) %>% 
  srvyr::group_by(res_quintile) %>% 

  srvyr::summarise(
    srvyr::across(
      c(perc_fuel, perc_food),
      ~srvyr::survey_quantile(.x,0.5 , na.rm = T)
    ),
      srvyr::across(
        c(hhfoodexppm,hhfuelexppm,hhexppm),
        ~srvyr::survey_quantile(.x/hhsize, 0.5, na.rm =T)
      )
    )
  

res_qunitile_exp <- res_qunitile_exp %>% 
  rename("Expenditure quintile" = res_quintile,
         "Median Fuel expenditure (%)" = perc_fuel_q50,
         "Median Food expenditure (%)" = perc_food_q50,
         "Median Monthly per-capita Expenditure (Rs)" = hhexppm_q50,
         "Median Monthly per-capita Food Expenditure (Rs)" = hhfoodexppm_q50,
         "Median Monthly per-capita Fuel Expenditure (Rs)" = hhfuelexppm_q50) %>% 
  select("Expenditure quintile","Median Fuel expenditure (%)", "Median Food expenditure (%)",
         "Median Monthly per-capita Expenditure (Rs)",
         "Median Monthly per-capita Food Expenditure (Rs)",
         "Median Monthly per-capita Fuel Expenditure (Rs)")


write_csv(res_qunitile_exp, file  = "outputs/fuel_monthly_expenditure_2019")
write_cs

sep_quintile_exp <- hh_info %>% 
  left_join(expenditure) %>% 
  as_survey_design(ids = ea, strata = res, weights = survey_wgt) %>% 
  srvyr::group_by(sep_quintile) %>% 
  srvyr::summarise(
    srvyr::across(
      c(perc_fuel, perc_food),
      ~srvyr::survey_quantile(.x,0.5 , na.rm = T)
    ),
    srvyr::across(
      c(hhfoodexppm,hhfuelexppm,hhexppm),
      ~srvyr::survey_quantile(.x/hhsize, 0.5, na.rm =T)
    )
  ) %>% 
  mutate(sep_quintile = as.character(sep_quintile))

sep_quintile_exp <- sep_quintile_exp%>% 
  rename("Expenditure quintile" = sep_quintile,
         "Median Fuel expenditure (%)" = perc_fuel_q50,
         "Median Food expenditure (%)" = perc_food_q50,
         "Median Monthly per-capita Expenditure (Rs)" = hhexppm_q50,
         "Median Monthly per-capita Food Expenditure (Rs)" = hhfoodexppm_q50,
         "Median Monthly per-capita Fuel Expenditure (Rs)" = hhfuelexppm_q50) %>% 
  select("Expenditure quintile","Median Fuel expenditure (%)", "Median Food expenditure (%)",
         "Median Monthly per-capita Expenditure (Rs)",
         "Median Monthly per-capita Food Expenditure (Rs)",
         "Median Monthly per-capita Fuel Expenditure (Rs)")


expenditure <- bind_rows(sep_quintile_exp, res_qunitile_exp)
writexl::write_xlsx(expenditure, path  = "outputs/fuel_monthly_expenditure_2019.xlsx")


df_plot <- res_qunitile_exp %>%
  mutate(res_quintile = paste(res, res_quintile)) %>% 
  pivot_longer(cols = c(food_pc, fuel_pc),
               names_to = "category",
               values_to = "expenditure_pc") %>%
  mutate(
    category = recode(category,
                      food_pc = "Food",
                      fuel_pc = "Fuel")
  )

# ggplot(df_plot, aes(x = factor(hhid), 
#                     y = expenditure_pc, 
#                     fill = category)) +
#   geom_col() +
#   labs(
#     title = "Monthly Per Capita Expenditure by Category",
#     x = "Household",
#     y = "Expenditure per capita (local currency)",
#     fill = "Category"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.text.x = element_blank(),  # hide long HHIDs
#     axis.ticks.x = element_blank()
#   )


         