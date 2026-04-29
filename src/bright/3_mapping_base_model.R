### BRIGHT Survey – Base Model: Risk of Inadequate Micronutrient Intake
### Mirrors src/3_mapping_base_model.R for the HIES 2019 pipeline
###
### ⚠️  ITEMS REQUIRING REVIEW BEFORE RUNNING:
###   [A] Shapefiles  – confirm adm1/adm2 shapefile paths (can reuse HIES shapefiles if same geography)
###   [B] HAR values  – confirm household adequacy requirements match BRIGHT population
###   [C] Survey design – confirm strata/cluster structure for BRIGHT survey design object
###   [D] Iron method – full-probability iron model requires additional parameters; confirm applicability

source("R/packages.R")
source("R/setup.R")

# Iron full-probability function from MIMI repo
devtools::source_url(
  "https://raw.githubusercontent.com/MIMI-wfp/MIMI-R-functions/refs/heads/main/iron_full_probability/iron_inad_prev.R"
)

library(wesanderson)
################################################################################
# LOAD DATA  -------------------------------------------------------------------

hh_info <- readRDS("data/bright/processed/hh_info.RDS")
base_ai <- readRDS("data/bright/processed/base_ai.RDS")

# ⚠️ [A] Shapefiles – can reuse HIES shapefiles if BRIGHT covers same geography
# Update paths if BRIGHT uses different administrative boundaries
adm1_shapefile <- sf::st_read("data/processed/shapefile/adm1_shapefile.shp")
adm2_shapefile <- sf::st_read("data/processed/shapefile/adm2_shapefile.shp")

################################################################################
# HOUSEHOLD ADEQUACY REQUIREMENTS  --------------------------------------------
# ⚠️ [B] These mirror the HIES/LKA values. Confirm they apply to the BRIGHT
#         population (same Sri Lankan DRI). If pulling from the MIMI database:
#         source(".Renviron")
#         con <- DBI::dbConnect(RMySQL::MySQL(), ...); h_ar <- dbReadTable(con, "h_ar"); dbDisconnect(con)
#         h_ar <- h_ar %>% filter(iso3 == "LKA")

h_ar <- list(
  vita_rae_mcg = 490,
  folate_mcg   = 250,
  vitb12_mcg   = 2,
  zn_mg        = 8.9,    # Sri Lanka-specific lower value
  energy_kcal  = 2170
  # Note: iron is handled via the full-probability method below
)

################################################################################
# INADEQUACY FLAGS  -----------------------------------------------------------

calc_inad <- function(har, comparison) {
  ifelse(comparison < har, 1, 0)
}

# Join intake with HH info
df <- hh_info %>%
  left_join(base_ai, by = "hhid") |> 
  filter(!is.na(energy_kcal))

df |> filter(is.na(energy_kcal))

fe_full_prob(df, group1 = adm1, hh_weight = "survey_wgt")

################################################################################
# SURVEY DESIGN OBJECT  -------------------------------------------------------


survey_object <- hh_info %>%
  left_join(df %>% mutate(hhid = as.character(hhid))) %>%
  mutate(
    vita_inad   = calc_inad(h_ar$vita_rae_mcg, vita_rae_mcg),
    zn_inad     = calc_inad(h_ar$zn_mg,         zn_mg),
    folate_inad = calc_inad(h_ar$folate_mcg,     folate_mcg),
    vitb12_inad = calc_inad(h_ar$vitb12_mcg,     vitb12_mcg)
  ) %>%
  as_survey_design(
    ids     = ea,           # ⚠️ [C] PSU/cluster column
    weights = survey_wgt,   # ⚠️ [C] survey weight column
    strata  = res           # ⚠️ [C] strata column (Urban/Rural/Estate or equivalent)
  )
################################################################################
# ADM1 PREVALENCE ESTIMATES  -------------------------------------------------

adm1_average <- survey_object %>%
  srvyr::group_by(adm1) %>%
  srvyr::summarise(
    # Median intakes
    across(
      ends_with(c("kcal", "mg", "g", "mcg")),
      ~ srvyr::survey_quantile(.x, quantiles = 0.5,na.rm = TRUE)
    ),
    # Prevalence of inadequacy
    across(
      ends_with("inad"),
      ~ srvyr::survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE) * 100
    )
  ) %>%
  left_join(
    fe_full_prob(df, adm1, survey_wgt) %>%
      rename(fe_inad = fe_mg_prop),
    by = c("adm1" = "subpopulation")
  )


################################################################################
# SPATIAL JOIN  ---------------------------------------------------------------



adm1_sp <- adm1_average %>%
  left_join(adm1_shapefile, by = "adm1") %>%
  sf::st_as_sf()

################################################################################
# CHOROPLETH MAPPING  ---------------------------------------------------------

plot_sf_choropleth <- function(
    merged_sf,
    outline_sf,
    fill_var,
    palette   = "Zissou1",
    n_pal     = 100,
    limits    = c(0, 100),
    fill_name = "Value",
    title     = "Choropleth Map",
    caption   = NULL
) {
  ggplot() +
    geom_sf(data = merged_sf,  aes(fill = .data[[fill_var]]), color = NA) +
    geom_sf(data = outline_sf, fill = NA, color = "black", size = 1) +
    scale_fill_gradientn(
      colours = wesanderson::wes_palette(palette, n = n_pal, type = "continuous"),
      limits  = limits,
      name    = fill_name
    ) +
    labs(title = title, caption = caption) +
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

create_and_save_plots <- function(
    save_plots = TRUE,
    output_dir = "outputs/bright/plots",
    width = 10, height = 8, dpi = 300
) {
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  micronutrients <- data.frame(
    var_name  = c("zn_inad", "fe_inad", "vita_inad", "vitb12_inad", "folate_inad"),
    title     = c("Zinc", "Iron", "Vitamin A", "Vitamin B12", "Folate"),
    plot_name = c("zn", "fe", "va", "b12", "fo"),
    stringsAsFactors = FALSE
  )

  admin_levels <- data.frame(
    sf_data    = c("adm1_sp"),
    level_name = c("adm1"),
    stringsAsFactors = FALSE
  )

  all_plots <- list()

  for (i in seq_len(nrow(admin_levels))) {
    for (j in seq_len(nrow(micronutrients))) {
      current_sf        <- get(admin_levels$sf_data[i])
      current_level     <- admin_levels$level_name[i]
      current_var       <- micronutrients$var_name[j]
      current_title     <- micronutrients$title[j]
      current_plot_name <- micronutrients$plot_name[j]
      plot_name         <- paste0(current_level, "_", current_plot_name)

      current_plot <- plot_sf_choropleth(
        merged_sf  = current_sf,
        outline_sf = current_sf,
        fill_var   = current_var,
        palette    = "Zissou1",
        limits     = c(0, 100),
        fill_name  = "Risk of inadequate micronutrient intake (%)",
        title      = current_title,
        caption    = "BRIGHT Survey"
      )

      all_plots[[plot_name]] <- current_plot
      assign(plot_name, current_plot, envir = .GlobalEnv)

      if (save_plots) {
        filename <- file.path(output_dir, paste0(plot_name, ".png"))
        ggsave(filename = filename, plot = current_plot,
               width = width, height = height, dpi = dpi, bg = "white")
        cat("Saved:", filename, "\n")
      }
    }
  }

  return(all_plots)
}

# Generate and save all maps
all_plots <- create_and_save_plots(save_plots = TRUE)

################################################################################
# SAVE PREVALENCE TABLES  -----------------------------------------------------

write_csv(adm1_average, "data/bright/processed/adm1_average.csv")
write_csv(adm2_average, "data/bright/processed/adm2_average.csv")

message("Script 3 complete.")
