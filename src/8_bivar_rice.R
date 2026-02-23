source("src/0_shapefile_clean.R")
source("R/packages.R")
source("R/setup.R")

source_url("https://raw.githubusercontent.com/MIMI-wfp/MIMI-R-functions/refs/heads/main/iron_full_probability/iron_inad_prev.R")

hh_info <- read_rds("data/processed/hh_info.RDS")
base_ai <- read_rds("data/processed/base_ai.RDS")
food_consumption <- read_rds("data/processed/food_consumption.RDS")

# rice consumption

fort_rice_codes <- c(101,102,105)

rice_consumption <- food_consumption %>% 
  filter(item_code %in% fort_rice_codes) %>% 
  group_by(hhid) %>% 
  summarise(across(c(quantity_g,quantity_100g),
                   ~sum(.x)))

write_rds(rice_consumption,'data/processed/rice_consumption.rds')

food_consumption <- food_consumption %>% 
  left_join(hh_info)




# function to calculate rice reach
calculate_rice_reach <- function(data, rice_codes, hhid_col, adm1_col, survey_wgt_col, ea_col, res_col, item_code_col){
  
  # Mark rice consumers
  rice <- data %>%
    filter({{item_code_col}} %in% rice_codes) %>%
    mutate(consumed_rice = 1)
  
  # Mark non-rice consumers and combine
  all_rice <- data %>%
    filter(!( {{hhid_col}} %in% rice[[rlang::as_name(enquo(hhid_col))]] )) %>%
    mutate(consumed_rice = 0) %>%
    bind_rows(rice)
  
  # Collapse to one row per household
  hh_rice_status <- all_rice %>%
    group_by({{hhid_col}}, {{adm1_col}}, {{survey_wgt_col}}, {{ea_col}}, {{res_col}}) %>%
    summarise(consumed_rice = max(consumed_rice), .groups = "drop")
  
  # Survey design and calculate reach
  reach_rice <- hh_rice_status %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, , nest = T) %>%
    group_by({{adm1_col}}) %>%
    summarise(rice_reach_pct = survey_mean(consumed_rice, proportion = TRUE) * 100) %>%
    select(-rice_reach_pct_se)
  
  return(reach_rice)
}


# Function to calculate rice intake
calculate_rice_intake <- function(data, rice_codes, adm1_col, quantity_col, survey_wgt_col, ea_col, res_col, item_code_col){
  # Filter rice items
  rice_quantity <- data %>%
    filter({{item_code_col}} %in% rice_codes)
  
  # Survey design
  rice_svy_design <- rice_quantity %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, nest = T)
  
  # Calculate survey-weighted mean rice consumption
  intake_rice <- rice_svy_design %>%
    group_by({{adm1_col}}) %>%
    summarise(mean_rice_g = survey_mean({{quantity_col}})) %>%
    select(-mean_rice_g_se)
  
  return(intake_rice)
}


sl_reach_rice <- food_consumption %>% calculate_rice_reach(
  rice_codes = fort_rice_codes,
  hhid_col = hhid,
  adm1_col = adm2,
  survey_wgt_col = survey_wgt,
  ea_col = ea,
  res_col = res,
  item_code_col = item_code
)

sl_intake_rice <- food_consumption %>% 
  calculate_rice_intake(
    rice_codes = fort_rice_codes,
    # hhid_col = hhid,
    adm1_col = adm2,
    quantity_col = quantity_g,
    survey_wgt_col = survey_wgt,
    ea_col = ea,
    res_col = res,
    item_code_col = item_code
  )





sl_reach_intake <- 
  sl_reach_rice %>% 
  left_join(sl_intake_rice, by = "adm2") %>% 
  mutate(
    across(everything(), ~ifelse(is.na(.), 0, .))
  ) %>% 
  mutate(
    # Create bins for the reach percentage from rice reach
    reach_bins = cut(
      rice_reach_pct, 
      breaks = c(0, 25, 50, 75, 100), 
      include.lowest = TRUE
    ),
    # Create bins for rice intake (mean consumption in grams)
    intake_bins = cut(
      mean_rice_g, 
      breaks = c(-Inf, 75, 149, 300, Inf),
      labels = c("<75",  "75–149",  "150–300",  ">300"),
      include.lowest = TRUE
    ),
    # Convert adm1 to character to match the shapefile's adm1 column
    adm2 = as.character(adm2)
  ) %>% 
  select(adm2, mean_rice_g, reach_bins, intake_bins) %>% 
  left_join(adm2_shapefile, by = "adm2") %>% 
  st_as_sf()


library(biscale)
# create a bi classs
sl_data_rice <- bi_class(sl_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )


# using ggplot and bi_scale, create a bivariate map
bi_map_rice <- ggplot() + 
  geom_sf(data = sl_data_rice, mapping = aes(fill = bi_class), color = NA,show.legend = F)+
  bi_scale_fill(pal = "BlueYl",dim = 4)+
  bi_theme()+
  geom_sf(data = adm2_shapefile, fill= NA, color = 'black', lwd = 1) + 
  #geom_sf_text(data = sen_adm1, aes(label = adm1), size = 3, color = 'black', fontface = 'bold') +
  labs(subtitle = "Coverage and Consumption of \nRice in Sri Lanka", )

# create a df of the breaks for each exis
break_vals <- bi_class_breaks(sl_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )

#create a bivariate legend
legend_rice <- bi_legend(pal = "BlueYl",
                         dim = 4,
                         xlab = "Higher Reach (%) ",
                         ylab = "Higher Consumption (g) ",
                         size = 8, 
                         breaks = break_vals)

library(cowplot)
# put legend and map together
sl_rice_bivariate <- cowplot::ggdraw() +
  draw_plot(bi_map_rice, 0, 0, 1, 1) +
  draw_plot(legend_rice, 0.65, .2, 0.45, 0.2)

sl_rice_bivariate


ggplot2::ggsave('outputs/maps/bivar_rice.png',
                plot = bi_map_rice,
                width = 8,
                height = 8,
                dpi = 600)
ggplot2::ggsave('outputs/maps/bivar_leg.png',
                plot = legend_rice,
                width = 2,
                height = 2,
                dpi = 600)
save_plot('outputs/maps/bivar_rice.png',sl_rice_bivariate, width = 8, height = 8)

