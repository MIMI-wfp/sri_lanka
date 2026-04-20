# source("src/0_shapefile_clean.R")
source("R/packages.R")
source("R/setup.R")
source_url("https://raw.githubusercontent.com/MIMI-wfp/MIMI-R-functions/refs/heads/main/iron_full_probability/iron_inad_prev.R")

hh_info <- read_rds("data/processed/hh_info.RDS")
base_ai <- read_rds("data/processed/base_ai.RDS")
adm1_shapefile <- st_read("data/processed/shapefile/adm1_shapefile.shp")
adm2_shapefile <- st_read("data/processed/shapefile/adm2_shapefile.shp")
# 
# Sys.getenv()


# connect to database

# 
con <- DBI::dbConnect(RMySQL::MySQL(),
                 dbname = Sys.getenv("DB_NAME"),
                 host = "127.0.0.1",
                 port = 3306,
                 user = Sys.getenv("DB_USER"),
                 password =  Sys.getenv("DB_PASSWORD"))


# collect information from database

h_ar <- DBI::dbReadTable(con, "h_ar")
#




# # disconnect
DBI::dbDisconnect(con)
h_ar <- h_ar %>% filter(iso3 == 'LKA')
################################################################################

calc_inad <- function(h_ar, comparison){return(ifelse(comparison<h_ar,1,0))}

#' @export
plot_sf_choropleth <- function(
    merged_sf,
    outline_sf,
    fill_var,
    palette        = "Zissou1",
    n_pal          = 100,
    limits         = c(0, 100),
    fill_name      = "Value",
    title          = "Choropleth Map",
    caption        = NULL
) {
  ggplot() +
    # subregions colored by fill_var (no borders)
    geom_sf(
      data = merged_sf,
      aes(fill = .data[[fill_var]]),
      color = NA
    ) +
    # single outline border
    geom_sf(
      data = outline_sf,
      fill = NA,
      color = "black",
      size = 1
    ) +
    # continuous gradient from palette
    scale_fill_gradientn(
      colours = wesanderson::wes_palette(palette, n = n_pal, type = "continuous"),
      limits = limits,
      name   = fill_name
    ) +
    labs(
      title   = title,
      caption = caption
    ) +
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
}




## FIX FOR IRON, FULL PROB ##
df <- hh_info %>% 
  left_join(base_ai,by = 'hhid') 
fe_full_prob(df, group1 = adm1, hh_weight = 'survey_wgt')



survey_object <- hh_info %>% 
  left_join(df  %>% mutate(hhid = as.character(hhid))) %>% 
  mutate(
    vita_inad = calc_inad(h_ar$vita_rae_mcg[1], vita_rae_mcg),
    zn_inad = calc_inad(h_ar$zn_mg[1], zn_mg),
    folate_inad = calc_inad(h_ar$folate_mcg[1], folate_mcg),
    # thia_inad = calc_inad(h_ar$thia_mg[1], thia_mg),
    vitb12_inad = calc_inad(h_ar$vitb12_mcg[1], vitb12_mcg)
    ) %>% 
  as_survey_design(ids = ea, weights = survey_wgt, strata = res) 


adm2_average <- survey_object %>% 
  srvyr::group_by(adm2) %>% 
  srvyr::summarise(
    across(
      ends_with(c("kcal", "mg","g", "mcg")),
      ~srvyr::survey_quantile(.x, quantiles = 0.5)
    ),
    across(
      ends_with("inad"),
      ~srvyr::survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE)*100
    )
    
  ) %>% 
  left_join(fe_full_prob(df, adm2, survey_wgt) %>% 
              rename(fe_inad = fe_mg_prop),by = c("adm2" = "subpopulation"))



# write_csv(adm2_average, "data/processed/adm2_average.csv")
# connect to the shapefile

adm2_sp <- adm2_average %>% 
  left_join(adm2_shapefile, by = 'adm2') %>% 
  st_as_sf()


adm1_average <- survey_object %>% 
  srvyr::group_by(adm1) %>% 
  srvyr::summarise(
    across(
      ends_with(c("mg","g", "mcg")),
      ~srvyr::survey_quantile(.x, quantiles = 0.5)
    ),
    across(
      ends_with("inad"),
      ~srvyr::survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE)*100
    )
    
  )%>% 
  left_join(fe_full_prob(df, adm1, survey_wgt) %>% 
              rename(fe_inad = fe_mg_prop),by = c("adm1" = "subpopulation"))


# connect to the shapefile

adm1_sp <- adm1_average %>% 
  left_join(adm1_shapefile, by = 'adm1') %>% 
  st_as_sf()


#' @export
create_and_save_plots <- function(save_plots = TRUE, output_dir = "outputs/plots", 
                                  width = 10, height = 8, dpi = 300) {
  
  # Create output directory if it doesn't exist
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Define micronutrient data
  micronutrients <- data.frame(
    var_name = c("zn_inad", "fe_inad", "vita_inad", "vitb12_inad", "folate_inad"),
    title = c("Zinc", "Iron", "Vitamin A", "Vitamin B12", "Folate"),
    plot_name = c("zn", "fe", "va", "b12", "fo"),
    stringsAsFactors = FALSE
  )
  
  # Define administrative levels
  admin_levels <- data.frame(
    sf_data = c("adm1_sp", "adm2_sp"),
    level_name = c("adm1", "adm2"),
    stringsAsFactors = FALSE
  )
  
  # Store all plots in a list
  all_plots <- list()
  
  # Loop through admin levels and micronutrients
  for (i in 1:nrow(admin_levels)) {
    for (j in 1:nrow(micronutrients)) {
      
      # Get current parameters
      current_sf <- get(admin_levels$sf_data[i])
      current_level <- admin_levels$level_name[i]
      current_var <- micronutrients$var_name[j]
      current_title <- micronutrients$title[j]
      current_plot_name <- micronutrients$plot_name[j]
      
      # Create plot name
      plot_name <- paste0(current_level, "_", current_plot_name)
      
      # Create the plot
      current_plot <- plot_sf_choropleth(
        merged_sf  = current_sf,
        outline_sf = current_sf,
        fill_var   = current_var,
        palette    = "Zissou1",
        limits     = c(0, 100),
        fill_name  = "Risk of inadequate micronutrient intake (%)",
        title      = current_title,
        caption    = "Household Income and Expenditure Survey 2019"
      )
      
      # Store plot in list
      all_plots[[plot_name]] <- current_plot
      
      # Save plot if requested
      if (save_plots) {
        filename <- file.path(output_dir, paste0(plot_name, ".png"))
        ggsave(
          filename = filename,
          plot = current_plot,
          width = width,
          height = height,
          dpi = dpi,
          bg = "white"
        )
        cat("Saved:", filename, "\n")
      }
      
      # Also assign to global environment (to maintain your original variable names)
      assign(plot_name, current_plot, envir = .GlobalEnv)
    }
  }
  
  # Return the list of plots
  return(all_plots)
}

# Usage examples:

# 1. Create and save all plots (default behavior)
all_plots <- create_and_save_plots(save_plots =  FALSE)


# 4. Access individual plots from the returned list
all_plots$adm1_zn  # ADM1 zinc plot
all_plots$adm2_zn  # ADM2 iron plot












