  ################################################################################
  # LOAD DATA  -------------------------------------------------------------------
  library(labelled)
  library(tidyverse)

  hh_info <- readRDS("data/bright/processed/hh_info.RDS")
  base_ai <- readRDS("data/bright/processed/base_ai.RDS")
  food_consumption <- readRDS("data/bright/processed/food_consumption.RDS")


  adm1_shapefile <- sf::st_read("data/processed/shapefile/adm1_shapefile.shp")

  food_consumption %>% 
    filter(item_code %in% c(75:93))


  x <- food_consumption %>% 
    mutate(item_code_lbl = forcats::as_factor(item_code)) %>% 
    group_by(item_code_lbl) %>% 
    summarise(
      mean_quantity_g = mean(quantity_g, na.rm = TRUE),
      q25 = quantile(quantity_g, 0.25, na.rm = TRUE),
      q50 = quantile(quantity_g, 0.50, na.rm = TRUE),
      q75 = quantile(quantity_g, 0.75, na.rm = TRUE),
      .groups = "drop"
    )


  # preserve the variable label
  var_label(x$item_code) <- var_label(food_consumption$item_code)


  # isolate total quantity of each asf
  asf_svy <- food_consumption %>% 
    filter(item_code %in% c(58:63)) %>% 
    group_by(hhid) %>% 
    summarise(quantity_g = sum(quantity_g)) %>% 
    mutate(food_group = 'meat') %>% 
    right_join(hh_info) %>% 
    mutate(quantity_g = ifelse(is.na(quantity_g),0,quantity_g))%>% 
    
    bind_rows(
      food_consumption %>% 
        filter(item_code %in% c(64:74)) %>% 
        group_by(hhid) %>% 
        summarise(quantity_g = sum(quantity_g)) %>% 
        mutate(food_group = 'fish') %>% 
        right_join(hh_info) %>% 
        mutate(quantity_g = ifelse(is.na(quantity_g),0,quantity_g))
    )%>% 
    
    bind_rows(
      food_consumption %>% 
        filter(item_code %in% c(57)) %>% 
        group_by(hhid) %>% 
        summarise(quantity_g = sum(quantity_g)) %>% 
        mutate(food_group = 'eggs') %>% 
        right_join(hh_info) %>% 
        mutate(quantity_g = ifelse(is.na(quantity_g),0,quantity_g))
    )%>% 
    
    bind_rows(
      food_consumption %>% 
        filter(item_code %in% c(95,96,98,100)) %>% 
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
    filter(item_code %in% c(75:93)) %>% 
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
    filter(item_code %in% c(17:23)) %>% 
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
    group_by(adm1, food_group) %>% 
    summarise(mean_consumption = quantile(quantity_g, 0.5)) %>% 
    left_join(adm1_shapefile) %>% 
    sf::st_as_sf()


  fruit_shp <- fruit_svy %>% 
    group_by(adm1, food_group) %>% 
    summarise(mean_consumption = quantile(quantity_g, 0.5)) %>% 
    left_join(adm1_shapefile) %>% 
    sf::st_as_sf()

  pulses_shp <- pulses_svy %>% 
    group_by(adm1, food_group) %>% 
    summarise(mean_consumption = quantile(quantity_g, 0.5)) %>% 
    left_join(adm1_shapefile) %>% 
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
    save_path = "outputs/bright/plots/food_group/meat_consumption.png"
  )

  plot_consumption_map(
    data = asf_shp,
    fg = "fish",
    title = "Fish consumption",
    save_path = "outputs/bright/plots/food_group/fish_consumption.png"
  )

  plot_consumption_map(
    data = asf_shp,
    fg = "eggs",
    title = "Eggs consumption",
    save_path = "outputs/bright/plots/food_group/eggs_consumption.png"
  )

  plot_consumption_map(
    data = asf_shp,
    fg = "dairy",
    title = "Dairy consumption",
    save_path = "outputs/bright/plots/food_group/dairy_consumption.png"
  )

  plot_consumption_map(
    data = fruit_shp,
    fg = 'fruit_veg',
    title = "Fruit and vegetable consumption"
    # save_path = "outputs/maps/food_group/fruit_consumption.png"
  )

  fruit_shp
  plot_consumption_map(
    data = pulses_shp,
    fg = 'pulses',
    title = "Pulses consumption"
    # save_path = "outputs/maps/food_group/pulses_consumption.png"
  )
