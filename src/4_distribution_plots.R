source("src/3_mapping_base_model.R")

# Load data
base_ai <- read_rds("data/processed/base_ai.RDS")
food_consumption <- read_rds("data/processed/food_consumption.RDS")

# get_har <- function(){
#   
#   con <- DBI::dbConnect(RMySQL::MySQL(),
#                         dbname = Sys.getenv("DB_NAME"),
#                         host = "127.0.0.1",
#                         port = 3306,
#                         user = Sys.getenv("DB_USER"),
#                         password =  Sys.getenv("DB_PASSWORD"))
#   
#   
#   # collect information from database
#   
#   h_ar <<- DBI::dbReadTable(con, "h_ar")
#   
#   # DBI::dbReadTable(con, "ML_targets")
#   # # disconnect
#   DBI::dbDisconnect(con)
#   return(h_ar)
# }
# get_har() %>% 
#   slice(1)

# Function to plot and save histogram of a given micronutrient
plot_distribution <- function(df = base_ai, micronutrient) {
  # Check that the micronutrient column exists
  if (!micronutrient %in% names(df)) {
    stop(paste("Column", micronutrient, "not found in dataframe"))
  }
  if (!micronutrient %in% names(h_ar)) {
    stop(paste("Column", micronutrient, "not found in h_ar"))
  }
  
  # Create the histogram
  p <- ggplot(df, aes(x = .data[[micronutrient]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    geom_vline(xintercept = h_ar[[micronutrient]][1], 
               color = "red", linetype = "dashed", size = 1) +
    labs(
      title = paste("Distribution of", micronutrient), 
      x = micronutrient, 
      y = "Count"
    ) +
    theme_minimal()
  
  # Create output directory if it doesn't exist
  output_dir <- here::here("outputs", "plots")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Save the plot
  ggsave(
    filename = file.path(output_dir, paste0(micronutrient, "_histogram.png")),
    plot = p,
    width = 6,
    height = 4
  )
  
  return(p)
}

# Generate and save histograms
plot_distribution(micronutrient = "zn_mg")
plot_distribution(micronutrient = "fe_mg")
plot_distribution(micronutrient = "vita_rae_mcg")
plot_distribution(micronutrient = "vitb12_mcg")
plot_distribution(micronutrient = "folate_mcg")
plot_distribution(micronutrient = "energy_kcal")



# disaggregation  --------------------------------------------------------------


survey_object %>% 
  srvyr::group_by(sep_quintile) %>% 
  summarise(
  fe_inad = survey_mean(zn_inad  == 1, proportion = T, na.rm = T),
  energy_intake = survey_quantile(energy_kcal, quantile = 0.5, na.rm = T)
  )

fe_full_prob(df, res, survey_wgt)


base_ai %>% 
  left_join(hh_info, by= "hhid") %>% 
  ggplot(aes(x = thia_mg, fill = res)) + 
  geom_density( alpha = 0.5)
  



##


  avg_consumption <- food_consumption %>%
    left_join(hh_info %>% select(hhid,res), by = "hhid") %>% 
  group_by(item_code, res) %>%
  summarise(mean_qty = mean(quantity_g , na.rm = TRUE), .groups = "drop")

# Step 2: Reshape to wide format for easy comparison
comparison <- avg_consumption %>%
  tidyr::pivot_wider(names_from = res, values_from = mean_qty, names_prefix = "avg_")

# Step 3 (Optional): Add column showing which residence has higher consumption
comparison <- comparison %>%
  mutate(
    higher_res = case_when(
      avg_Rural > avg_Urban ~ "Rural",
      avg_Rural < avg_Urban ~ "Urban",
      avg_Rural == avg_Urban ~ "equal",
      TRUE ~ NA_character_
    )
  )


comparison <- comparison %>%
  mutate(
    rel_diff_pct =  (avg_Rural - avg_Urban) 
  )


# View results
print(comparison)
  

# look at total value spent

food_value <- imputed_food %>% 
  group_by(hhid) %>% 
  summarise(
    value = sum(value)
  ) %>% 
  left_join(hh_info, by = 'hhid') %>% 
  mutate(
    value_pc = value/round(afe,0)
  ) %>% 
  select(hhid, ea, res,res_quintile, sep_quintile, adm1,adm2,survey_wgt, value_pc)

food_value_svy <- food_value %>% 
  as_survey_design(ids = ea, strata = res, weights = survey_wgt)

food_value_svy %>% 
  group_by(res) %>% 
  summarise(
    value_pc = survey_quantile(value_pc, quantiles = 0.5)
  )


food_value %>% 
  ggplot(aes(x = value_pc, fill = res)) + 
  geom_density(position = 'dodge', alpha = 0.5)


food_afe %>% 
arrange(desc(quantity_ai))



food_value_svy <- food_value %>% 
  as_survey_design(ids = ea, strata = res, weights = survey_wgt)


# ------------------------------------------------------------------------------







read_csv("data/food_group_map.csv")

# food_group_cols <- colnames( %>% select(-c(item_code,item_name)))

# make into a data 
ind_nss_hdds<- ind_nss_hdds %>% 
  pivot_longer(cols = -c(item_code,item_name)) %>% 
  filter(value == 1) %>% 
  select(-value) %>% 
  rename(food_group = name, 
         Item_Code = item_code)




## 
## summarise micronutrient contributions from food groups nationally
national_foodgroup_average <- food_group_full %>% 
  group_by(common_id,state, food_group) %>% 
  summarise(
    across(
      c(energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg),
      ~sum(., na.rm = T)
      
    )
  ) %>% 
  ungroup() %>% 
  group_by(food_group) %>% 
  summarise(
    across(
      c(energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg),
      ~mean(.)
      
    )
  )


## summarise micronutrient contributions from food groups at state level to see regional differences
state_foodggroup_average <-  food_group_full %>% 
  group_by(common_id,state, food_group) %>% 
  summarise(
    across(
      c(energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg),
      ~sum(., na.rm = T)
      
    )
  ) %>% 
  ungroup() %>% 
  group_by(food_group,state) %>% 
  summarise(
    across(
      c(energy_kcal,folate_ug,iron_mg, vitaminb12_in_mcg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg, zinc_mg, vita_mcg),
      ~mean(.)
      
    )
  )

################################################################################


# create list of micronutrient names
micronutrient_list <- c(colnames(state_foodggroup_average[4:7]),colnames(state_foodggroup_average[9:11]))
micronutrient_list <- data.frame(micronutrient = micronutrient_list,
                                 name = c("Folate", "Iron", "Vitamin B12",
                                          "Thiamin", "Niacin", "Vitamin B6",
                                          "Zinc"))

state_prop_boxes <- function(state_num){
  # function reads in a state number and produces proportional box-plots
  # 
  mn_fg_plots <- list()
  for(item in micronutrient){
    print(item)
    print({{state_num}})
    p1 <-  state_foodggroup_average %>%
      
      filter(state == state_num & !is.na(food_group)) %>%
      ggplot(aes(area = !!sym(item$micronutrient),
                 fill = stringr::str_to_title(
                   str_replace_all(food_group, "_", " and ")   ),
                 label =
                   stringr::str_to_title(
                     str_replace_all(food_group, "_", " and ")         )
      )) +
      geom_treemap() +
      geom_treemap_text( colour = "darkblue", place = "topleft", alpha = 0.6,
                         grow = FALSE,min.size = 6)+
      labs(title = item$name)+
      scale_fill_brewer(palette = "Set3")+
      # guides(fill=guide_legend())+
      theme(legend.position="bottom",
            legend.spacing.x = unit(0, 'cm')
      )+
      guides(fill = guide_legend(title="Food group",label.position = "bottom"))
    # theme(legend.direction = "horizontal", legend.position = "bottom")+
    # guides(fill = "none")+
    
    mn_fg_plots[[item]] <- p1
  }
  return(mn_fg_plots)
}


state_prop_boxes("32")
national_foodgroup_average <- national_foodgroup_average %>%
  mutate(food_group_clean = str_to_title(str_replace_all(food_group, "_", " and ")))


for (i in seq_len(nrow(micronutrient))) {
  item <- micronutrient[i, ]
  print(item$micronutrient)
  
  p1 <- national_foodgroup_average %>%
    filter(!is.na(food_group)) %>%
    ggplot(aes(
      area = !!sym(item$micronutrient),
      fill = str_to_title(str_replace_all(food_group, "_", " and ")),
      label = str_to_title(str_replace_all(food_group, "_", " and "))
    )) +
    geom_treemap() +
    geom_treemap_text(
      colour = "darkblue",
      place = "topleft",
      alpha = 0.6,
      grow = FALSE,
      min.size = 6
    ) +
    labs(title = item$name) +
    scale_fill_brewer(palette = "Set3") +
    theme(
      legend.position = "bottom",
      legend.spacing.x = unit(0, 'cm')
    ) +
    guides(
      fill = guide_legend(
        title = "Food group",
        label.position = "bottom"
      )
    )
  
  mn_fg_plots[[item$micronutrient]] <- p1
  ggsave(
    filename = paste0(figure_path,"food_group/", item$micronutrient, ".jpg"),
    plot = p1,
    height = 6.5,
    width = 6,
    dpi = 900
  )
}

mn_fg_plots[3]

