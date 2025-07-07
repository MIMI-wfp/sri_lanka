source("src/0_shapefile_clean.R")
source("src/setup_environment.R")

hh_info <- read_rds("data/processed/hh_info.RDS")
base_ai <- read_rds("data/processed/base_ai.RDS")

# Sys.getenv()

# connect to database


con <- DBI::dbConnect(RMySQL::MySQL(),
                 dbname = Sys.getenv("DB_NAME"),
                 host = "127.0.0.1",
                 port = 3306,
                 user = Sys.getenv("DB_USER"),
                 password =  Sys.getenv("DB_PASSWORD"))


# collect information from database

h_ar <- DBI::dbReadTable(con, "h_ar")

# disconnect
DBI::dbDisconnect(con)

################################################################################

calc_inad <- function(h_ar, comparison){return(ifelse(comparison<h_ar,1,0))}

plot_sf_choropleth <- function(
    merged_sf,
    outline_sf,
    fill_var       = "",
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





survey_object <- hh_info %>% 
  left_join(base_ai,by = 'hhid') %>% 
  mutate(
    vita_inad = calc_inad(h_ar$vita_rae_mcg[1], vita_rae_mcg),
    zn_inad = calc_inad(h_ar$zn_mg[1], zn_mg),
    folate_inad = calc_inad(h_ar$folate_mcg[1], folate_mcg),
    thia_inad = calc_inad(h_ar$thia_mg[1], thia_mg),
    ) %>% 
  as_survey_design(ids = ea, weights = survey_wgt, strata = res) 


adm1_average <- survey_object %>% 
  srvyr::group_by(adm1) %>% 
  srvyr::summarise(
    across(
      ends_with(c("kcal","mg","g", "mcg")),
      ~srvyr::survey_quantile(.x, quantiles = 0.5)
    ),
    across(
      ends_with("inad"),
      ~srvyr::survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE)*100
    )
    
  )


# connect to the shapefile

adm1_sp <- adm1_average %>% 
  left_join(adm1_sp, by = 'adm1') %>% 
  st_as_sf()



# make some maps

# Example usage
plot_sf_choropleth(
  merged_sf  = adm1_sp,
  outline_sf = adm1_sp,
  fill_var   = "zn_inad",
  palette    = "Zissou1",
  limits     = c(0, 100),
  fill_name  = "Folate",
  title      = "Folate Proportion by Administrative Area",
  caption    = "Household Income and Expenditure Survey 2019"
)


