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
adm2_shapefile <- sf::st_read("data/processed/shapefile/adm2_shapefile.shp")
# 
path_to_raw_data <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/"
sl_fct <- readxl::read_xlsx(paste0(path_to_raw_data,"sri_lanka_food_matches.xlsx"), 
                            sheet = 1)
hh_info <- read_rds("data/processed/hh_info.RDS")
nutrient_sac <- readRDS('data/school_feeding/nutrient_sac.RDS')


# filter the school meals only in the districts
school_meals_district <- c(91)

nutrient_sac <- nutrient_sac %>% 
  left_join(hh_info)
# %>% 
#   filter(adm2 %in% school_meals_district)



# create our school meal  assumptions


  # The school meal in Sri Lanka consists of:
  # - 75g rice in different forms (milk rice, yellow rice etc)  = item code 101
  # - 20g legumes = item code 301
  # - 30g any vegetable = item code 402
  # - 30g any leafy green vegetable = item code 445
  # - 30g meat = item code = 601
  # - 


config <- list(
  school_meal = data.frame(code = c(101,301,402,445,601,901,801,1602),
                            quantity_g = c(75,20,30,30,10,10,10,60)) %>%
               mutate(quantity_100g = quantity_g/100),
  fortification_df = data.frame(scenario = 'SL', code = 101,
                                 fe_mg = 6.5, folate_mcg = 65)#to account for DFE)
)


meal_profiles <- list(
  sm      = FALSE,
  sm_fort = TRUE
) |> 
  imap(~ create_school_meal(
    config$school_meal,
    sl_fct,
    fortified = .x,
    fortification_df = if (.x) config$fortification_df else NULL
  ))

# access:
sm_nutrient_profile      <- meal_profiles$sm
sm_nutrient_profile_fort <- meal_profiles$sm_fort

# 
# sm_nutrient_profile      <- make_meal(FALSE)
# sm_nutrient_profile_fort <- make_meal(TRUE)


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





# ---- 2. Prepare EAR data nutrient# ---- 2. Prepare EAR data ----

ear_df <- tibble(
  nutrient = rep(c("fe_mg", "folate_mcg", "vitb12_mcg"), times = 3),
  EAR = c(8, 110, 1, 10, 160, 1, 15.5, 210, 1.5),
  age_group = rep(c("4-6", "7-10", "11-13"), each = 3)
)

ear_df_wide <- ear_df %>% 
  mutate(nutrient = paste0(nutrient,"_EAR")) %>% 
  pivot_wider(names_from = nutrient, values_from = EAR)



avg_df <- avg_df %>%
  mutate(age_group = factor(age_group, levels = c("4-6", "7-10", "11-13")))

ear_df <- ear_df %>%
  mutate(age_group = factor(age_group, levels = c("4-6", "7-10", "11-13")))



################################################################################
# inad by age group

sm_inad <- sm_nofort %>%
  inner_join(sm_fort) %>%
  mutate(
    age_group = case_when(
      age_y >= 4 & age_y <= 6 ~ "4-6",
      age_y >= 7 & age_y <= 10 ~ "7-10",
      age_y >= 11 & age_y <= 13 ~ "11-13",
      TRUE ~ "Other"
    )
  ) %>% 
  left_join(ear_df_wide, by = 'age_group') %>% 
  mutate(fe_inad = ifelse(fe_mg<fe_mg_EAR,1,0),
         fe_inad_sm = ifelse(fe_mg_sm<fe_mg_EAR,1,0),
         fe_inad_sm_fort = ifelse(fe_mg_sm_fort<fe_mg_EAR,1,0),
         fol_inad = ifelse(fe_mg<fe_mg_EAR,1,0),
         fol_inad_sm = ifelse(fe_mg_sm<fe_mg_EAR,1,0),
         fol_inad_sm_fort = ifelse(fe_mg_sm_fort<fe_mg_EAR,1,0))


sm_inad %>% 
  group_by(age_group) %>% 
  summarise(across(c(fe_inad,fe_inad_sm,fe_inad_sm_fort),
            ~sum(.x)/n())) 
  
sm_inad_svy <- sm_inad %>% 
  as_survey_design(ids = ea,
                   strata = res, 
                   weights = survey_wgt)

sm_inad_adm2 <- sm_inad_svy %>% 
  srvyr::group_by(adm2) %>% 
  summarise(
    across(c(fe_inad,fe_inad_sm,fe_inad_sm_fort,fol_inad,fol_inad_sm,fol_inad_sm_fort),
   ~survey_mean(.x, proportion = TRUE, na.rm = TRUE)*100
    ))%>% 
  left_join(adm2_shapefile) %>% 
  sf::st_as_sf()


indicators <- c(
  "fe_inad", "fe_inad_sm", "fe_inad_sm_fort",
  "fol_inad", "fol_inad_sm", "fol_inad_sm_fort"
)

# Your age groups
age_groups <- c("7-10", "11-13", "4-6")

# for (ag in age_groups) {
  
  x <- sm_inad_adm2 
  
  
  for (ind in indicators) {
    
    # print(paste("Processing:", ag, "|", ind))
    
    # Build a clean title
    title_text <- paste("Inadequate", ind, "intake — all")
    
    p <- plot_sf_choropleth(
      merged_sf = x,
      outline_sf = x,
      fill_var  = ind,
      title     = title_text,
      fill_name      = "Risk of inadequate micronutrient intake ",
    )
    print(p)
    ggsave(filename = paste0("outputs/maps/school_meals/",ind,'_all.png'),
           plot = p,
           dpi = 600)
  }
# }

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
  EAR       = c(8, 110, 1, 10, 160, 1, 15.5, 210, 1.5),
  UL = c(40,NA,NA,40,NA,NA,40,NA,NA)

)

ear_df <- ear_df %>%
  mutate(age_group = factor(age_group, levels = c("6", "7-10", "11-13")))
# Add nutrient_group and age_group to EAR data
ear_df <- ear_df |>
  mutate(nutrient_group = case_when(
    str_starts(nutrient, "fe") ~ "Iron",
    str_starts(nutrient, "folate") ~ "Folate",
    str_starts(nutrient, "vitb12") ~ "Vitamin B12"
  ),
  age_group = factor(case_when(
    age_start == 5.5 & age_end == 6.5 ~ "6",
    age_start == 6.5 & age_end == 10.5 ~ "7-10",
    age_start == 10.5 & age_end == 13.5 ~ "11-13"
  ))
)


ear_df <- ear_df %>%
  mutate(age_group = factor(age_group, levels = c("6", "7-10", "11-13")))

# Main plot



sm_nofort |>
  left_join(sm_fort) |>
  select(
    hhid, uniqueid, age_y,
    starts_with("fe_"),
    starts_with("folate_"),
    starts_with("vitb12_mcg")
  ) |>
  pivot_longer(
    cols = c(
      starts_with("fe_"),
      starts_with("folate_"),
      starts_with("vitb12_mcg")
    ),
    names_to = "nutrient",
    values_to = "value"
  ) |>
  mutate(
    # Age group classification
    age_group = case_when(
      age_y %in% 6 ~ "6",
      age_y %in% 7:10 ~ "7-10",
      age_y %in% 11:13 ~ "11-13",
      TRUE ~ "Other"
    ),
    age_group = factor(age_group, levels = c("6", "7-10", "11-13")),

    # Nutrient group classification
    nutrient_group = case_when(
      str_starts(nutrient, "fe_") ~ "Iron",
      str_starts(nutrient, "folate_") ~ "Folate",
      str_starts(nutrient, "vitb12_mcg") ~ "Vitamin B12"
    ),
    
    nutrient_group = factor(nutrient_group, 
                            levels = c("Iron", "Folate", "Vitamin B12"))
    , # ✅ enforce order

    # Color group classification
    color_group = case_when(
      str_ends(nutrient, "_sm_fort") ~ "Household meal + Fortfied school meal",
      str_ends(nutrient, "_sm") ~ "Household meal + school meal",
      TRUE ~ "Household meal only"
    )
  ) |>
  filter(color_group %in% c("Household meal only",
                            "Household meal + school meal",
                            "Household meal + Fortfied school meal"
                            
                            )) %>% 
  ggplot(aes(x = value, y = age_group, fill = color_group)) +
  geom_density_ridges(alpha = 0.5, position = "identity", scale = 0.7) +
  facet_wrap(~ factor(nutrient_group), scales = "free_x") +
  geom_segment(
    data = ear_df,
    aes(
      x = EAR, xend = EAR,
      y = as.numeric(factor(age_group)),
      yend = as.numeric(factor(age_group)) + 0.7
    ),
    inherit.aes = FALSE,
    color = "red", size = 0.8
  ) +
  geom_segment(
    data = ear_df,
    aes(
      x = UL, xend = UL,
      y = as.numeric(factor(age_group)),
      yend = as.numeric(factor(age_group)) + 0.7
    ),
    inherit.aes = FALSE,
    color = "blue", size = 0.8
  ) +
  scale_fill_manual(values = c(
    "Household meal only" = "#008EB2",
    "Household meal + school meal" = "#039249",
    "Household meal + Fortfied school meal" = "#E3002B"
  )) +
  theme_ridges() +
  labs(
    x = "Total daily apparent intake (mg or µg)",
    y = "Age Group",
    fill = "Type"
  )+ theme(legend.position = "none")




