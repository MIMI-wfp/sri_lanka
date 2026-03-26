# look at different average food consumption across the country 
source("R/packages.R")


base_ai <- read_csv("data/processed/base_ai.csv")
food_consumption <- read_rds("data/processed/food_consumption.RDS")
rice_consumption <- read_rds("data/processed/rice_consumption.rds")
hh_info <- read_rds("data/processed/hh_info.rds")


base_ai %>% ggplot(aes(x = energy_kcal))+geom_histogram()
adm2_shapefile <- sf::st_read("data/processed/shapefile/adm2_shapefile.shp")

# isolate total quantity of each asf
asf_svy <- food_consumption %>% 
  filter(group %in% c(7,8)) %>% 
  group_by(hhid) %>% 
  summarise(quantity_g = sum(quantity_g)) %>% 
  mutate(food_group = 'fish') %>% 
  right_join(hh_info) %>% 
  mutate(quantity_g = ifelse(is.na(quantity_g),0,quantity_g))%>% 
  
bind_rows(
  food_consumption %>% 
  filter(group %in% c(6)) %>% 
  group_by(hhid) %>% 
  summarise(quantity_g = sum(quantity_g)) %>% 
  mutate(food_group = 'meat') %>% 
    right_join(hh_info) %>% 
    mutate(quantity_g = ifelse(is.na(quantity_g),0,quantity_g))
  )%>% 
  
bind_rows(
    food_consumption %>% 
    filter(group %in% c(9)) %>% 
    group_by(hhid) %>% 
    summarise(quantity_g = sum(quantity_g)) %>% 
    mutate(food_group = 'eggs') %>% 
      right_join(hh_info) %>% 
      mutate(quantity_g = ifelse(is.na(quantity_g),0,quantity_g))
    )%>% 
  
bind_rows(
  food_consumption %>% 
  filter(group %in% c(13)) %>% 
  group_by(hhid) %>% 
  summarise(quantity_g = sum(quantity_g)) %>% 
  mutate(food_group = 'dairy') %>% 
    right_join(hh_info) %>% 
    mutate(quantity_g = ifelse(is.na(quantity_g),0,quantity_g))
  )%>%
  as_survey_design(ids = c(ea, hhid),
                   strata = res,
                   weights = survey_wgt)


# fruits and vegetables
fruit_svy <- food_consumption %>% 
  filter(group %in% c(4,5,16)) %>% 
  group_by(hhid) %>% 
  summarise(quantity_g = sum(quantity_g)) %>% 
  mutate(food_group = 'fruit_veg') %>% 
  right_join(hh_info) %>% 
  mutate(quantity_g = ifelse(is.na(quantity_g),0,quantity_g))%>%
  as_survey_design(ids = c(ea, hhid),
                   strata = res,
                   weights = survey_wgt)

# fruits and vegetables
pulses_svy <- food_consumption %>% 
  filter(group %in% c(3)) %>% 
  group_by(hhid) %>% 
  summarise(quantity_g = sum(quantity_g)) %>% 
  mutate(food_group = 'pulses') %>% 
  right_join(hh_info) %>% 
  mutate(quantity_g = ifelse(is.na(quantity_g),0,quantity_g))%>%
  as_survey_design(ids = c(ea, hhid),
                   strata = res,
                   weights = survey_wgt)






# create shapefile
asf_shp <- asf_svy %>% 
  group_by(adm2, food_group) %>% 
  summarise(mean_consumption = quantile(quantity_g, 0.5)) %>% 
  left_join(adm2_shapefile) %>% 
  sf::st_as_sf()


fruit_shp <- fruit_svy %>% 
  group_by(adm2, food_group) %>% 
  summarise(mean_consumption = quantile(quantity_g, 0.5)) %>% 
  left_join(adm2_shapefile) %>% 
  sf::st_as_sf()

pulses_shp <- pulses_svy %>% 
  group_by(adm2, food_group) %>% 
  summarise(mean_consumption = quantile(quantity_g, 0.5)) %>% 
  left_join(adm2_shapefile) %>% 
  sf::st_as_sf()

# plots
plot_consumption_map <- function(data, fg, title, save_path = NULL) {
  
  # subset data (unchanged)
  
  
  df <- data %>%
    dplyr::filter(food_group == fg)
 
  
# 
  # ORIGINAL PLOTTING CODE (unchanged)
  p <- ggplot() +
    geom_sf(
      data = df,
      aes(fill = mean_consumption),
      color = NA
    ) +
    geom_sf(
      data = df,
      fill = NA,
      color = "black",
      linewidth = 1
    ) +
    scale_fill_gradientn(
      colours = wesanderson::wes_palette("Zissou1", type = "continuous"),
      na.value = "grey85",
      name = "Median consumption (g/day)",
      
      limits = c(0, 60),              # <-- FIXED SCALE RANGE
      oob = scales::squish 
      
    ) +
    labs(
      title = title,
      caption = ""
    ) +
    coord_sf(expand = FALSE) +
    theme_minimal() +
    theme(
      plot.title       = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.title     = element_text(hjust = 0.5)
    )

  # OPTIONAL SAVE
  if (!is.null(save_path)) {
    ggsave(
      filename = save_path,
      plot = p,
      width = 8,
      height = 6,
      dpi = 300
    )
  }

  return(p)
}


plot_consumption_map(
  data = asf_shp,
  fg = "meat",
  title = "Meat consumption",
  save_path = "outputs/maps/food_group/meat_consumption.png"
)

plot_consumption_map(
  data = asf_shp,
  fg = "fish",
  title = "Fish consumption",
  save_path = "outputs/maps/food_group/fish_consumption.png"
)

plot_consumption_map(
  data = asf_shp,
  fg = "eggs",
  title = "Eggs consumption",
  save_path = "outputs/maps/food_group/eggs_consumption.png"
)

plot_consumption_map(
  data = asf_shp,
  fg = "dairy",
  title = "Dairy consumption",
  save_path = "outputs/maps/food_group/dairy_consumption.png"
)

plot_consumption_map(
  data = fruit_shp,
  fg = 'fruit_veg',
  title = "Fruit and vegetable consumption",
  save_path = "outputs/maps/food_group/fruit_consumption.png"
)

fruit_shp
plot_consumption_map(
  data = pulses_shp,
  fg = 'pulses',
  title = "Pulses consumption",
  save_path = "outputs/maps/food_group/pulses_consumption.png"
)

# ------------------------------------------------------------------------------
# vitamin A across the year


base_ai %>% 
  mutate(
    hhid = as.character(hhid)) %>% 
  left_join(hh_info) %>% 
  mutate(
    month = as.integer(month),
    month_name = factor(month.name[month], levels = month.name) # ordered factor
  ) %>%
  
  ggplot(aes(x = month, y = vita_rae_mcg)) +
  geom_smooth(color = "#2C3E50", fill = "#A9CCE3", linewidth = 1.2) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.name  # or month.abb for shorter labels
  ) +

  labs(
    x = "Month",
    y = "Vitamin A intake (mcg RAE)",
    title = "Monthly Trends in Vitamin A Intake",
    subtitle = "Median household intake across survey months"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

