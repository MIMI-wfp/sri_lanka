source("src/0_shapefile_clean.R")
# source("src/setup_environment.R")
source_url("https://raw.githubusercontent.com/MIMI-wfp/MIMI-R-functions/refs/heads/main/iron_full_probability/iron_inad_prev.R")

hh_info <- read_rds("data/processed/hh_info.RDS")
base_ai <- read_rds("data/processed/base_ai.RDS")


# 
# Sys.getenv()

# connect to database

# 
# con <- DBI::dbConnect(RMySQL::MySQL(),
#                  dbname = Sys.getenv("DB_NAME"),
#                  host = "127.0.0.1",
#                  port = 3306,
#                  user = Sys.getenv("DB_USER"),
#                  password =  Sys.getenv("DB_PASSWORD"))
# 
# 
# # collect information from database
# 
# h_ar <- DBI::dbReadTable(con, "h_ar")
# # # disconnect
# DBI::dbDisconnect(con)

################################################################################

calc_inad <- function(h_ar, comparison){return(ifelse(comparison<h_ar,1,0))}

plot_sf_choropleth <- function(
    merged_sf,
    outline_sf,
    fill_var       = "zn_mg_prop",
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



##### TO DO #####
## FIX FOR IRON, FULL PROB ##
df <- hh_info %>% 
  left_join(base_ai,by = 'hhid') 
fe_full_prob(df, adm1, survey_wgt)



survey_object <- hh_info %>% 
  left_join(base_ai,by = 'hhid') %>% 
  mutate(
    vita_inad = calc_inad(h_ar$vita_rae_mcg[1], vita_rae_mcg),
    zn_inad = calc_inad(h_ar$zn_mg[1], zn_mg),
    folate_inad = calc_inad(h_ar$folate_mcg[1], folate_mcg),
    thia_inad = calc_inad(h_ar$thia_mg[1], thia_mg),
    vitb12_inad = calc_inad(h_ar$vitb12_mcg, vitb12_mcg)
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




# make some maps

## ADM1 #####

adm1_zn <- plot_sf_choropleth(
  merged_sf  = adm1_sp,
  outline_sf = adm1_sp,
  fill_var   = "zn_inad",
  palette    = "Zissou1",
  limits     = c(0, 100),
  fill_name  = "Risk of inadequate micronutrient intake (%)",
  title      = "Zinc",
  caption    = "Household Income and Expenditure Survey 2019"
)

# Example usage
adm1_fe <- plot_sf_choropleth(
  merged_sf  = adm1_sp,
  outline_sf = adm1_sp,
  fill_var   = "fe_inad",
  palette    = "Zissou1",
  limits     = c(0, 100),
  fill_name  = "Risk of inadequate micronutrient intake (%)",
  title      = "Iron",
  caption    = "Household Income and Expenditure Survey 2019"
)


adm1_va <- plot_sf_choropleth(
  merged_sf  = adm1_sp,
  outline_sf = adm1_sp,
  fill_var   = "vita_inad",
  palette    = "Zissou1",
  limits     = c(0, 100),
  fill_name  = "Risk of inadequate micronutrient intake (%)",
  title      = "Vitamin A",
  caption    = "Household Income and Expenditure Survey 2019"
)

# Example usage
adm1_b12 <- plot_sf_choropleth(
  merged_sf  = adm1_sp,
  outline_sf = adm1_sp,
  fill_var   = "vitb12_inad",
  palette    = "Zissou1",
  limits     = c(0, 100),
  fill_name  = "Risk of inadequate micronutrient intake (%)",
  title      = "Vitamin B12",
  caption    = "Household Income and Expenditure Survey 2019"
)

# Example usage
adm1_fo <- plot_sf_choropleth(
  merged_sf  = adm1_sp,
  outline_sf = adm1_sp,
  fill_var   = "folate_inad",
  palette    = "Zissou1",
  limits     = c(0, 100),
  fill_name  = "Risk of inadequate micronutrient intake (%)",
  title      = "Folate",
  caption    = "Household Income and Expenditure Survey 2019"
)





plot_sf_choropleth(
  merged_sf  = adm2_sp,
  outline_sf = adm2_sp,
  fill_var   = "zn_inad",
  palette    = "Zissou1",
  limits     = c(0, 100),
  fill_name  = "Risk of inadequate micronutrient intake (%)",
  title      = "Zinc",
  caption    = "Household Income and Expenditure Survey 2019"
)

# Example usage
plot_sf_choropleth(
  merged_sf  = adm2_sp,
  outline_sf = adm2_sp,
  fill_var   = "fe_inad",
  palette    = "Zissou1",
  limits     = c(0, 100),
  fill_name  = "Risk of inadequate micronutrient intake (%)",
  title      = "Iron",
  caption    = "Household Income and Expenditure Survey 2019"
)


plot_sf_choropleth(
  merged_sf  = adm2_sp,
  outline_sf = adm2_sp,
  fill_var   = "vita_inad",
  palette    = "Zissou1",
  limits     = c(0, 100),
  fill_name  = "Risk of inadequate micronutrient intake (%)",
  title      = "Vitamin A",
  caption    = "Household Income and Expenditure Survey 2019"
)

# Example usage
plot_sf_choropleth(
  merged_sf  = adm2_sp,
  outline_sf = adm2_sp,
  fill_var   = "vitb12_inad",
  palette    = "Zissou1",
  limits     = c(0, 100),
  fill_name  = "Risk of inadequate micronutrient intake (%)",
  title      = "Vitamin B12",
  caption    = "Household Income and Expenditure Survey 2019"
)

# Example usage
plot_sf_choropleth(
  merged_sf  = adm2_sp,
  outline_sf = adm2_sp,
  fill_var   = "folate_inad",
  palette    = "Zissou1",
  limits     = c(0, 100),
  fill_name  = "Risk of inadequate micronutrient intake (%)",
  title      = "Folate",
  caption    = "Household Income and Expenditure Survey 2019"
)




