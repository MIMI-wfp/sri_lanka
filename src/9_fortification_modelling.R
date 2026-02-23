### Reading in SL HIES 2019
rm(list = ls())
rq_packages <- c("tidyverse", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))
# ----------------------------------------------------------------------------

devtools::source_url("https://raw.githubusercontent.com/MIMI-wfp/MIMI-R-functions/refs/heads/main/iron_full_probability/iron_inad_prev.R")
source('src/3_mapping_base_model.R')


base_ai <- read_csv("data/processed/sl_ml_targets_2025-07-16.csv")
food_consumption <- read_rds("data/processed/food_consumption.RDS")
rice_consumption <- read_rds("data/processed/rice_consumption.rds")
hh_info <- read_rds("data/processed/hh_info.rds")

base_ai <- base_ai %>% 
  mutate(hhid = as.character(hhid)) %>% 
  left_join(hh_info)

fortification_df = data.frame(scenario = 'SL', item = 'rice',
                              fe_mg = 6, folate_mcg = 65)

rice_consumption %>% 
  summarise(mean(quantity_g),
            median(quantity_g))

mean_rice = median(rice_consumption$quantity_g)


# base_ai%>% ggplot(aes(x = vita_rae_mcg))+
#   geom_histogram()
# 
# base_ai %>% 
#   mutate(fe_inad = ifelse(fe_mg<15,1,0)) %>% 
#   select(hhid, fe_inad) %>% 
#   summarise(sum(fe_inad)/n())

# set a cut off 
rice_fortification <- rice_consumption %>% 
  mutate(quantity_100g = ifelse(quantity_g>500,mean_rice/100,quantity_100g)) %>% 
  
  mutate(fe_mg_fort = quantity_100g*fortification_df$fe_mg,
         folate_mcg_fort = quantity_100g*fortification_df$folate_mcg)



rice_fortification %>% ggplot(aes(x = fe_mg_fort))+
  geom_histogram()


# ------------------------------------------------------------------------------
make_fort_data <- function(base_ai, rice_fortification, adm2_codes = NULL) {
  
  base_ai %>%
    { if (!is.null(adm2_codes)) filter(., adm2 %in% adm2_codes) else . } %>%
    left_join(
      rice_fortification %>%
        select(hhid, fe_mg_fort, folate_mcg_fort),
      by = "hhid"
    ) %>%
    mutate(
      across(c(fe_mg_fort, folate_mcg_fort), ~ replace_na(.x, 0)),
      fe_mg_fort = fe_mg + fe_mg_fort,
      folate_mcg_fort = folate_mcg + folate_mcg_fort
    ) %>%
    select(
      hhid,
      fe_mg, fe_mg_fort,
      folate_mcg, folate_mcg_fort
    )
}
fort_data <- make_fort_data(base_ai, rice_fortification) 
fort_data %>% filter(is.na(folate_mcg_fort))

make_histogram_fort <- function(df, micronutrient ){
  df %>%
    select(starts_with({{micronutrient}})) %>% 
    pivot_longer(
      cols = c(starts_with({{micronutrient }})),
      names_to = "nutrient",
      values_to = "value"
    ) %>%
    ggplot(aes(x = value, fill = nutrient)) +
    geom_histogram(alpha = 1, bins = 30) +
    # facet_wrap(~ nutrient, scales = "free") +
    theme_minimal()
}
make_histogram_fort(fort_data, "folate")


# ------------------------------------------------------------------------------

# aggregate by 
fort_data_svy <- fort_data %>% 
  left_join(hh_info) %>% 
  mutate(
    folate_inad = calc_inad(h_ar$folate_mcg[1], folate_mcg),
    folate_fort_inad = calc_inad(h_ar$folate_mcg[1], folate_mcg_fort)
  ) %>% 

  as_survey_design(
    ids = c(ea, hhid),
    strata = res,
    weights = survey_wgt
  )



  
adm2_fort <- fort_data_svy %>% 
    srvyr::group_by(adm2) %>% 
    srvyr::summarise(
      across(
        ends_with(c("kcal", "mg","g", "mcg")),
        ~ srvyr::survey_quantile(.x, quantiles = 0.5)
      ),
      across(
        ends_with("inad"),
        ~ srvyr::survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE) * 100
      )
    ) %>% 
    left_join(
      fe_full_prob(fort_data %>% left_join(hh_info), adm2, hh_weight = 'survey_wgt') %>% 
        rename(fe_inad = fe_mg_prop,
               adm2 =  subpopulation),
      by =  c('adm2' )) %>% 
  left_join(
    fe_full_prob(fort_data %>% select(-fe_mg) %>% rename(fe_mg = fe_mg_fort) %>% 
                   left_join(hh_info), adm2, hh_weight = 'survey_wgt') %>% 
      rename(fe_inad_fort = fe_mg_prop,
             adm2 =  subpopulation),
    by =  c('adm2' ))


adm1_fort <- fort_data_svy %>% 
  srvyr::group_by(adm1) %>% 
  srvyr::summarise(
    across(
      ends_with(c("kcal", "mg","g", "mcg")),
      ~ srvyr::survey_quantile(.x, quantiles = 0.5)
    ),
    across(
      ends_with("inad"),
      ~ srvyr::survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE) * 100
    )
  ) %>% 
  left_join(
    fe_full_prob(fort_data %>% left_join(hh_info), adm1, hh_weight = 'survey_wgt') %>% 
      rename(fe_inad = fe_mg_prop,
             adm1 =  subpopulation),
    by =  c('adm1' )) %>% 
  left_join(
    fe_full_prob(fort_data %>% select(-fe_mg) %>% rename(fe_mg = fe_mg_fort) %>% 
                   left_join(hh_info), adm1, hh_weight = 'survey_wgt') %>% 
      rename(fe_inad_fort = fe_mg_prop,
             adm1 =  subpopulation),
    by =  c('adm1' ))
    
adm2_sp <-adm2_fort %>% 
  left_join(adm2_shapefile, by = 'adm2') %>% 
  st_as_sf()
adm1_sp <-adm1_fort %>% 
  left_join(adm1_shapefile, by = 'adm1') %>% 
  st_as_sf()



plot_sf_choropleth(adm2_sp,adm2_sp, fill_var = 'fe_inad', title = "Iron",fill = 'Risk of inadequate intake (%)')
plot_sf_choropleth(adm2_sp,adm2_sp, fill_var = 'fe_inad_fort',title = 'Iron - fortified rice',fill = 'Risk of inadequate intake (%)')
plot_sf_choropleth(adm2_sp,adm2_sp, fill_var = 'folate_inad', title = 'Folate',fill = 'Risk of inadequate intake (%)')
plot_sf_choropleth(adm2_sp,adm2_sp, fill_var = 'folate_fort_inad', title = 'Folate - fortificed rice',fill = 'Risk of inadequate intake (%)')
# ============================================================
# FULL WORKFLOW: summary functions + shapefile join + mapping
# ============================================================

library(dplyr)
library(srvyr)
library(sf)
library(ggplot2)
library(rlang)

survey_nutrient_summary <- function(df_svy, df_raw, hh_info, group){
  
  g <- rlang::ensym(group)
  gname <- rlang::as_name(g)
  
  sum_main <- df_svy %>%
    srvyr::group_by(!!g) %>%
    srvyr::summarise(
      dplyr::across(
        dplyr::ends_with(c("kcal", "mg", "g", "mcg")),
        ~ srvyr::survey_quantile(.x, quantiles = 0.5)
      ),
      dplyr::across(
        dplyr::ends_with("inad"),
        ~ srvyr::survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE) * 100
      ),
      .groups = "drop"
    )
  
  fe1 <- fe_full_prob(
    df_raw %>% dplyr::left_join(hh_info),
    !!g,
    hh_weight = "survey_wgt"
  ) %>%
    dplyr::rename(!!gname := subpopulation,
                  fe_inad = fe_mg_prop)
  
  fe2 <- fe_full_prob(
    df_raw %>%
      dplyr::select(-fe_mg) %>%
      dplyr::rename(fe_mg = fe_mg_fort) %>%
      dplyr::left_join(hh_info),
    !!g,
    hh_weight = "survey_wgt"
  ) %>%
    dplyr::rename(!!gname := subpopulation,
                  fe_inad_fort = fe_mg_prop)
  
  sum_main %>%
    dplyr::left_join(fe1, by = gname) %>%
    dplyr::left_join(fe2, by = gname)
}

attach_shapefile <- function(df_summary, shp, group){
  gname <- rlang::as_name(rlang::ensym(group))
  df_summary %>%
    dplyr::left_join(shp, by = gname) %>%
    sf::st_as_sf()
}

map_and_save <- function(sf_obj, fill_var, title, fill_label,
                         filename = NULL, width = 8, height = 8, dpi = 300){
  
  p <- plot_sf_choropleth(
    sf_obj,
    sf_obj,
    fill_var = fill_var,
    title = title,
    fill = fill_label
  )
  
  if(!is.null(filename)){
    ggplot2::ggsave(filename,
                    plot = p,
                    width = width,
                    height = height,
                    dpi = dpi)
  }
  
  p
}

fort_data_svy <- fort_data %>% 
  left_join(hh_info) %>% 
  mutate(
    folate_inad = calc_inad(h_ar$folate_mcg[1], folate_mcg),
    folate_fort_inad = calc_inad(h_ar$folate_mcg[1], folate_mcg_fort)
  ) %>% 
  as_survey_design(
    ids = c(ea, hhid),
    strata = res,
    weights = survey_wgt
  )

adm2_fort <- survey_nutrient_summary(
  df_svy  = fort_data_svy,
  df_raw  = fort_data,
  hh_info = hh_info,
  group   = adm2
)

adm1_fort <- survey_nutrient_summary(
  df_svy  = fort_data_svy,
  df_raw  = fort_data,
  hh_info = hh_info,
  group   = adm1
)

adm2_sp <- attach_shapefile(adm2_fort, adm2_shapefile, adm2)
adm1_sp <- attach_shapefile(adm1_fort, adm1_shapefile, adm1)

map_and_save(adm2_sp, "fe_inad",
             title = "Iron",
             fill_label = "Risk of inadequate intake (%)",
             filename = "outputs/maps/fortification/adm2_iron.png")

map_and_save(adm2_sp, "fe_inad_fort",
             title = "Iron – fortified rice",
             fill_label = "Risk of inadequate intake (%)",
             filename = "outputs/maps/fortification/adm2_iron_fortified.png")

map_and_save(adm2_sp, "folate_inad",
             title = "Folate",
             fill_label = "Risk of inadequate intake (%)",
             filename = "outputs/maps/fortification/adm2_folate.png")

map_and_save(adm2_sp, "folate_fort_inad",
             title = "Folate – fortified rice",
             fill_label = "Risk of inadequate intake (%)",
             filename = "outputs/maps/fortification/adm2_folate_fortified.png")
