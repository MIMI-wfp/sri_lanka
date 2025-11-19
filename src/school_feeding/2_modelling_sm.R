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



# ---- 1. Prepare avg_df ----
avg_df <- sm_nofort %>%
  inner_join(sm_fort) %>%
  mutate(
  age_group = case_when(
      age_y >= 4 & age_y <= 6 ~ "4-6",
      age_y >= 7 & age_y <= 10 ~ "7-10",
      age_y >= 11 & age_y <= 13 ~ "11-13",
      TRUE ~ "Other"
    )
  ) |> 
  group_by(age_group) |> 

  summarise(across(c(fe_mg, fe_mg_sm, fe_mg_sm_fort,
                     folate_mcg, folate_mcg_sm, folate_mcg_sm_fort,
                     vitb12_mcg, vitb12_mcg_sm),
                   median, na.rm = TRUE)) %>%
  pivot_longer(-age_group, names_to = "nutrient", values_to = "value") %>%
  mutate(
    type = case_when(
      str_detect(nutrient, "_sm_fort") ~ "School Meal Fortified",
      str_detect(nutrient, "_sm") ~ "School Meal",
      TRUE ~ "Only household meals"
    ),
    nutrient = str_remove(nutrient, "_sm_fort|_sm")
    
  ) %>%
  pivot_wider(names_from = type, values_from = value)

# ---- 2. Prepare EAR data ----

ear_df <- tibble(
  nutrient = rep(c("fe_mg", "folate_mcg", "vitb12_mcg"), times = 3),
  EAR = c(8, 110, 1, 10, 160, 1, 15.5, 210, 1.5),
  age_group = rep(c("4-6", "7-10", "11-13"), each = 3)
)




avg_df <- avg_df %>%
  mutate(age_group = factor(age_group, levels = c("4-6", "7-10", "11-13")))

ear_df <- ear_df %>%
  mutate(age_group = factor(age_group, levels = c("4-6", "7-10", "11-13")))

# ---- 3. Plot ----

ggplot(avg_df) +
  geom_segment(aes(x = age_group, xend = age_group,
                   y = `Only household meals`, yend = `School Meal`),
               arrow = arrow(length = unit(0.2, "cm")), color = "gray") +
  geom_segment(aes(x = age_group, xend = age_group,
                   y = `School Meal`, yend = `School Meal Fortified`),
               arrow = arrow(length = unit(0.2, "cm")), color = "gray") +
  geom_point(aes(x = age_group, y = `Only household meals`, color = "Household meal only"),alpha = 0.6, size = 3) +
  geom_point(aes(x = age_group, y = `School Meal`, color = "Household meal + school meal"),alpha = 0.6, size = 3) +
  geom_point(aes(x = age_group, y = `School Meal Fortified`, color = "Household meal + Fortified school meal"),alpha = 0.6, size = 3) +
  geom_segment(data = ear_df,
               aes(x = as.numeric(factor(age_group))-0.1, xend = as.numeric(factor(age_group))+0.1, y = EAR, yend = EAR),
               color = "red", linetype = "solid", size = 1) +
  geom_text(data = ear_df,
            aes(x = age_group, y = EAR, label = "EAR"),
            color = "red", hjust = -0.2, vjust = -0.5, size = 3) +
  facet_wrap(~ nutrient, scales = "free_y",
             labeller = as_labeller(c(
               fe_mg = "Iron (mg)",
               folate_mcg = "Folate (µg)",
               vitb12_mcg = "Vitamin B12 (µg)"
             ))) +
  scale_x_discrete(limits = c("4-6", "7-10", "11-13")) +
  scale_color_manual(values = c("Household meal only" = "#008EB2",
                                "Household meal + school meal" = "#039249",
                                "Household meal + Fortified school meal" = "#E3002B")) +
  labs(title = "Change in Micronutrients by Age Group",
       x = "Age Group", y = "Median daily apparent intake (mg or µg)", color = "Scenario") +
  theme_minimal()


# library
library(ggridges)


sm_nofort |> 
  ggplot(aes(x = fe_mg, y = factor(age_y)))+
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")




# EAR reference data
ear_df <- data.frame(
  age_start = c(5.5, 5.5, 5.5, 6.5, 6.5, 6.5, 10.5, 10.5, 10.5),
  age_end   = c(6.5, 6.5, 6.5,10.5,10.5,10.5,13.5,13.5,13.5),
  nutrient  = c("fe_mg", "folate_mcg", "vitb12_mcg",
                "fe_mg", "folate_mcg", "vitb12_mcg",
                "fe_mg", "folate_mcg", "vitb12_mcg"),
  EAR       = c(8, 110, 1, 10, 160, 1, 15.5, 210, 1.5)
)

# Add nutrient_group and age_group to EAR data
ear_df <- ear_df |>
  mutate(nutrient_group = case_when(
    str_starts(nutrient, "fe") ~ "Iron",
    str_starts(nutrient, "folate") ~ "Folate",
    str_starts(nutrient, "vitb12") ~ "Vitamin B12"
  ),
  age_group = case_when(
    age_start == 5.5 & age_end == 6.5 ~ "4-6",
    age_start == 6.5 & age_end == 10.5 ~ "7-10",
    age_start == 10.5 & age_end == 13.5 ~ "11-13"
  ))

# Main plot
sm_nofort |>
  left_join(sm_fort) |>
  select(hhid, uniqueid, age_y,
         starts_with("fe_"),
         starts_with("folate_"),
         starts_with("vitb12_mcg")) |>
  pivot_longer(cols = c(starts_with("fe_"),
                        starts_with("folate_"),
                        starts_with("vitb12_mcg")),
               names_to = "nutrient",
               values_to = "value") |>
  mutate(age_group = case_when(
    age_y %in% 6 ~ "4-6",
    age_y %in% 7:10 ~ "7-10",
    age_y %in% 11:13 ~ "11-13",
    TRUE ~ "Other"
  ),
  age_group = factor(age_group, levels = c("4-6", "7-10", "11-13")),
  nutrient_group = case_when(
    str_starts(nutrient, "fe_") ~ "Iron",
    str_starts(nutrient, "folate_") ~ "Folate",
    str_starts(nutrient, "vitb12_mcg") ~ "Vitamin B12"
  ),
  color_group = case_when(
    str_ends(nutrient, "_sm_fort") ~ "Household meal + Fortfied school meal",
    str_ends(nutrient, "_sm") ~ "Household meal + school meal",
    TRUE ~ "Household meal only"
  )) |>
  ggplot(aes(x = value, y = age_group, fill = color_group)) +
  geom_density_ridges(alpha = 0.5, position = "identity", scale = 0.7) +
  facet_wrap(~ nutrient_group, scales = "free_x") +
 


  geom_segment(data = ear_df,
               aes(x = EAR, xend = EAR,
                   y = as.numeric(factor(age_group)),
                   yend = as.numeric(factor(age_group)) + 0.7),
               inherit.aes = FALSE,
               color = "red", size = 0.8)+



  scale_fill_manual(values = c("Household meal only" = "#008EB2",
                                "Household meal + school meal" = "#039249",
                                "Household meal + Fortfied school meal" = "#E3002B")) +
  theme_ridges() +
  labs(x = "Total daily apparent intake (mg or µg)", y = "Age Group", fill = "Type")







