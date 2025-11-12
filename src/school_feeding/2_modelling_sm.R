# package load #############################################################
rq_packages <- c("tidyverse", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

# load previous R scripts

# source("src/school_feeding/0_clean_for_sf.R")
source("src/school_feeding/1_school_menu_builder.R")

# load data
path_to_raw_data <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/"
sl_fct <- readxl::read_xlsx(paste0(path_to_raw_data,"sri_lanka_food_matches.xlsx"), 
                            sheet = 1)
nutrient_sac <- readRDS('data/school_feeding/nutrient_sac.RDS')

# create our school meal  assumptions


  # The school meal in Sri Lanka consists of:
  # - 75g rice in different forms (milk rice, yellow rice etc)
  # - 20g legumes
  # - 30g any vegetable
  # - 30g any leafy green vegetable
  # - 30g meat
  # - 


config <- list(
  school_meal = data.frame(code = c(101,301,402,445,601,1607),
                            quantity_g = c(75,20,30,30,30,60)) %>%
               mutate(quantity_100g = quantity_g/100),
  fortification_df = data.frame(scenario = 'SL', code = 101,
                                 fe_mg = 6.5, folate_mcg = 65)
)


sm_nutrient_profile <- create_school_meal(config$school_meal,sl_fct, fortified = FALSE)

sm_nutrient_profile_fort <- create_school_meal(config$school_meal,sl_fct, fortified = TRUE, fortification_df = config$fortification_df)


# functions to add the school meal 
add_nutrients <- function(base_df, add_df, rename_col) {
  common_cols <- intersect(names(base_df), names(add_df))
  
  base_df %>%
    mutate(across(all_of(common_cols),
                  ~ .x + add_df[[cur_column()]],
                  .names = paste0("{.col}_", rename_col)))
}

# data frame 
sm_nofort <- add_nutrients(nutrient_sac,sm_nutrient_profile,rename_col ="sm") 
sm_fort <- add_nutrients(nutrient_sac,sm_nutrient_profile_fort,rename_col ="sm_fort")

nutrient_sac

sm_nofort |> inner_join(sm_fort) |> 
  select(hhid,uniqueid, age_y, 
  energy_kcal, 
  starts_with("fe"), starts_with("folate"),starts_with("vitb12_mcg"))


# plots ----------------------------------


# Prepare data

avg_df <- sm_nofort %>%
  inner_join(sm_fort) %>%
  group_by(age_y) %>%
  summarise(across(c(fe_mg, fe_mg_sm, fe_mg_sm_fort,
                     folate_mcg, folate_mcg_sm, folate_mcg_sm_fort,
                     vitb12_mcg, vitb12_mcg_sm),
                   mean, na.rm = TRUE)) %>%
  pivot_longer(-age_y, names_to = "nutrient", values_to = "value") %>%
  mutate(type = case_when(
    str_detect(nutrient, "_sm_fort") ~ "School Meal Fortified",
    str_detect(nutrient, "_sm") ~ "School Meal",
    TRUE ~ "Original"
  ),
  nutrient = str_remove(nutrient, "_sm_fort|_sm")) %>%
  pivot_wider(names_from = type, values_from = value)

# Plot arrows


ggplot(avg_df) +
  geom_segment(aes(x = age_y, xend = age_y,
                   y = Original, yend = `School Meal`),
               arrow = arrow(length = unit(0.2, "cm")), color = "gray") +
  geom_segment(aes(x = age_y, xend = age_y,
                   y = `School Meal`, yend = `School Meal Fortified`),
               arrow = arrow(length = unit(0.2, "cm")), color = "gray") +
  geom_point(aes(x = age_y, y = Original, color = "Original"), size = 3) +
  geom_point(aes(x = age_y, y = `School Meal`, color = "School Meal"), size = 3) +
  geom_point(aes(x = age_y, y = `School Meal Fortified`, color = "School Meal + Fortified rice"), size = 3) +
  facet_wrap(~ nutrient, scales = "free_y",
             labeller = as_labeller(c(
               fe_mg = "Iron (mg)",
               folate_mcg = "Folate (µg)",
               vitb12_mcg = "Vitamin B12 (µg)"
             ))) +
  scale_color_manual(values = c("Original" = "blue",
                                "School Meal" = "orange",
                                "School Meal + Fortified rice" = "green")) +
  labs(title = "Change in Micronutrients by Age",
       x = "Age (years)", y = "Apparent intake", color = "Scenario") +
  theme_minimal()

