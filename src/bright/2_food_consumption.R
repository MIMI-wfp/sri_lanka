### BRIGHT Survey – Food Consumption & Nutrient Matching
### Mirrors src/2_food_consumption.R for the HIES 2019 pipeline
###
### KEY STRUCTURAL DIFFERENCE FROM HIES:
###   BRIGHT uses non-standard units (pieces, bunches, cups, loaves, etc.)
###   with size sub-categories (small/medium/large). Each row in the BRIGHT
###   food module represents one food item × unit × size combination.
###
###   Conversion is performed via data/bright/processed/food_without_conversion_v2.csv
###   which provides fcf_kg (conversion factor to kg) per item × unit × size.
###   This is multiplied by the respondent's reported quantity to get kg, then ×1000 for g.


rm(list = ls())

rq_packages <- c("tidyverse", "srvyr", "readxl")
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = TRUE)
rm(list = c("rq_packages", "installed_packages"))

################################################################################
# PATHS  -----------------------------------------------------------------------

# path_to_survey   <- "/Users/gabrielbattcock/Library/CloudStorage/OneDrive-WorldFoodProgramme/General - MIMI Project/Countries/Sri Lanka/data/bright_survey/"
path_to_survey   <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/bright_survey/"
path_to_data_out <- "data/bright/processed/"
path_to_raw_data <- "data/"                               # root data folder (FCT location)

################################################################################
# LOAD PROCESSED HOUSEHOLD INFO  -----------------------------------------------

hh_info <- readRDS(paste0(path_to_data_out, "hh_info.RDS"))

################################################################################
# LOAD FCT  --------------------------------------------------------------------


bright_fct <- readxl::read_xlsx(
  paste0(path_to_raw_data, "bright/bright_fct.xlsx"),   
  sheet = 1
) |> 
  rename(item_code = j1_item) |> 
  filter(!is.na(item_code))

################################################################################
# UNIT CONVERSION FACTORS  -----------------------------------------------------


bright_conversion <- read_csv(
  "data/bright/processed/food_without_conversion_v2.csv"
) %>%
  rename(
    item_code = j1_item,
    unit_code = j1_03,
    size_code = j1_03a,
    fcf_kg    = fcf_kg
  ) %>%
  mutate(
    # Convert kg → grams
    fcf_g = fcf_kg * 1000,
    # ⚠️ Flag items with missing conversions
    missing_conversion = is.na(fcf_g)
  )

# Report missing conversions
missing_conv <- bright_conversion %>%
  filter(missing_conversion) %>%
  select(item_code, j1_item_label, unit_code, j1_03_label, size_code, j1_03a_label)

if (nrow(missing_conv) > 0) {
  message("⚠️  The following item×unit×size combinations are missing fcf_kg values:")
  print(missing_conv)
  message("These items will have NA quantities until conversion factors are added.")
}

################################################################################
# LOAD BRIGHT FOOD MODULE  -----------------------------------------------------



bright_food <- haven::read_dta(paste0(path_to_survey, "mod_j1_fah_long.dta")) %>%
  filter(j1_01 == 1) |> 
  rename(hhid = hhcode) %>%      
  rename(
    item_code = j1_item,
    unit_code = j1_03,
    size_code = j1_03a,
    quantity  = j1_02    
  ) %>% 
  
  mutate(
    fcf_kg = case_when(
      size_code == 50 ~ 0.08,
      .default = fcf_kg
    )
  )


x <- bright_food |> group_by(item_code) %>% 
  summarise(n()/6900*100)
  
conversion <- bright_food |> group_by(item_code, unit_code, size_code,fcf_kg) %>% 
  summarise(n())


recall_days <- 7   #

################################################################################
# MERGE CONVERSION FACTORS  ----------------------------------------------------
# Join on item_code + unit_code + size_code to get the per-unit weight in grams.
# Then: total_grams = quantity (units) × fcf_g (grams per unit)

converted_food <- bright_food %>%
  mutate(quantity_g = quantity*1000*fcf_kg) |> 
  select(-fcf_kg) |> 
  left_join(
    bright_conversion %>% select(item_code, unit_code, size_code, fcf_kg, j1_item_label),
    by = c("item_code", "unit_code", "size_code")
  ) %>%
  mutate(
    quantity_g = ifelse(is.na(quantity_g), quantity*fcf_kg*1000, quantity_g),
  )

# Report items with no match in conversion table
unmatched <- converted_food %>%
  filter(is.na(quantity_g)) %>%
  distinct(item_code, unit_code, size_code) %>%
  left_join(
    bright_conversion %>% distinct(item_code, j1_item_label),
    by = "item_code"
  )

if (nrow(unmatched) > 0) {
  message("⚠️  ", nrow(unmatched), " item×unit×size combinations had no conversion factor:")
  print(unmatched)
}

################################################################################
# EDIBLE PORTION ADJUSTMENT  ---------------------------------------------------
# Apply edible portion fraction from FCT (e.g. bones, shells, peel excluded)

converted_food <- converted_food %>%
  left_join(bright_fct %>% select(item_code , edible_portion), by = "item_code") %>%
  mutate(
    quantity_g = ifelse(is.na(edible_portion), quantity_g, quantity_g * edible_portion)
  )

################################################################################
# HOUSEHOLD-LEVEL AGGREGATION (if multiple rows per hh × item)  ---------------
# Some surveys record each consumption event on a separate row.
# Aggregate to one row per hhid × item_code before outlier removal.

converted_food_hh <- converted_food %>%
  group_by(hhid, item_code, j1_item_label) %>%
  summarise(quantity_g = sum(quantity_g, na.rm = TRUE), .groups = "drop")

################################################################################
# MERGE AFE & CONVERT TO PER CAPITA PER DAY  ----------------------------------

food_afe <- converted_food_hh %>%
  left_join(hh_info %>% select(hhid, afe) %>% group_by(hhid) %>% slice(1) %>% ungroup(),
            by = "hhid") %>%
  mutate(
    quantity_ai = quantity_g / (recall_days * afe)   # g per capita per day
  ) |> 
  filter(quantity_g>0)

# Households in food module but not in hh_info – investigate
anti_join(food_afe, hh_info, by = "hhid") %>%
  distinct(hhid) %>%
  { if (nrow(.) > 0) warning(paste(nrow(.), "hhids in food module not found in hh_info")) }

################################################################################
# OUTLIER DETECTION & IMPUTATION  ---------------------------------------------
# Flag extreme values (>mean_log + 2.5*sd_log) per food item as NA,
# then replace NAs with the 95th percentile of that item's distribution.
# This mirrors the HIES approach; extend imputation pairs [E] as needed.

food_afe <- food_afe %>%
  mutate(log_quantity_g = log10(quantity_ai))

quant_cutpoints <- food_afe %>%
  group_by(item_code) %>%
  summarise(
    mean_log = mean(log_quantity_g, na.rm = TRUE),
    sd_log   = sd(log_quantity_g,   na.rm = TRUE)
  ) %>%
  mutate(upper_cut = mean_log + 2 * sd_log) %>%
  select(item_code, upper_cut)

food_afe <- food_afe %>%
  left_join(quant_cutpoints, by = "item_code") %>%
  mutate(
    quantity_ai = case_when(
      log_quantity_g >= upper_cut ~ NA_real_,
      TRUE                        ~ quantity_ai
    )
  ) %>%
  select(-log_quantity_g, -upper_cut)

# Replace remaining NAs with 95th percentile per item
food_afe <- food_afe %>%
  group_by(item_code) %>%
  mutate(
    quantity_ai = ifelse(
      is.na(quantity_ai),
      quantile(quantity_ai, probs = 0.5, na.rm = TRUE),
      quantity_ai
    )
  ) %>%
  ungroup()

rm(quant_cutpoints)



################################################################################
# SCALE TO PER 100g  ----------------------------------------------------------

food_afe <- food_afe %>%
  mutate(quantity_100g = quantity_ai / 100) %>%
  select(hhid, item_code, j1_item_label, quantity_ai, quantity_100g)

################################################################################
# NUTRIENT MATCHING  -----------------------------------------------------------
# Multiply per-100g nutrient values by quantity_100g to get per capita per day intakes.


food_mn <- food_afe %>%
  left_join(
    bright_fct %>%
      select(
        item_code,
        ends_with("kcal"),
        ends_with("_g"),
        ends_with("_mcg"),
        ends_with("_mg")
      ),
    by = "item_code"
  ) %>%
  mutate(
    across(
      -c(hhid, item_code, j1_item_label, quantity_ai, quantity_100g),
      ~ as.numeric(.x) * quantity_100g
    )
  )



################################################################################
# HOUSEHOLD APPARENT INTAKE  --------------------------------------------------

hh_ai <- food_mn %>%
  group_by(hhid) %>%
  summarise(
    across(
      -c(item_code, j1_item_label, quantity_100g),
      ~ sum(.x, na.rm = TRUE)
    )
  ) %>%
  select(-quantity_ai)

sum(is.na(hh_ai$fe_mg ))

hh_ai |> 
  ggplot(aes(x = energy_kcal))+
  geom_histogram()

################################################################################
# NAR / MAR TARGETS  -----------------------------------------------------------
# ⚠️ [F] Household Adequacy Requirements (HAR) for BRIGHT population.
#         These are the same as HIES (Sri Lankan DRI) unless BRIGHT targets
#         a different population group. Confirm before using.
#
#   Alternatively connect to the MIMI database:
#     source(".Renviron"); con <- DBI::dbConnect(...); h_ar <- dbReadTable(con,"h_ar") %>% filter(iso3=="LKA")

h_ar_bright <- list(
  vita_rae_mcg = 490,    
  folate_mcg   = 250,    
  vitb12_mcg   = 2,      
  fe_mg        = 15,     
  zn_mg        = 8.9    
)

calc_nar <- function(h_ar, comparison) {
  ifelse(comparison < h_ar, comparison / h_ar, 1)
}

bright_ml_targets <- hh_ai %>%
  select(hhid, vita_rae_mcg, folate_mcg, vitb12_mcg, fe_mg, zn_mg) %>%
  left_join(hh_info %>% select(hhid), by = "hhid") %>%
  mutate(
    vita_nar    = calc_nar(h_ar_bright$vita_rae_mcg, vita_rae_mcg),
    fol_nar     = calc_nar(h_ar_bright$folate_mcg,   folate_mcg),
    vitb12_nar  = calc_nar(h_ar_bright$vitb12_mcg,   vitb12_mcg),
    fe_nar      = calc_nar(h_ar_bright$fe_mg,         fe_mg),
    zn_nar      = calc_nar(h_ar_bright$zn_mg,         zn_mg),
    overall_mar = (vita_nar + fol_nar + vitb12_nar + fe_nar + zn_nar) / 5,
    survey      = "lka_bright",
    iso3        = "LKA"
  ) %>%
  select(iso3, survey, hhid, vita_rae_mcg, folate_mcg, vitb12_mcg,
         fe_mg, zn_mg, overall_mar)



################################################################################
# BASE AI TABLE FOR DATABASE  -------------------------------------------------

base_ai <- hh_ai %>%
  mutate(
    survey  = "lka_bright",
    iso3    = "LKA",
    vitd_mcg = NA
  ) %>%
  select(survey, hhid, iso3, energy_kcal,
         vita_rae_mcg, thia_mg, ribo_mg, niac_mg, vitb6_mg,
         vitd_mcg, folate_mcg, vitb12_mcg, vitc_mg, ca_mg, fe_mg, zn_mg)

################################################################################
# SAVE OUTPUTS  ----------------------------------------------------------------

write.csv(hh_ai,       paste0(path_to_data_out, "base_ai.csv"))
saveRDS(hh_ai,         paste0(path_to_data_out, "base_ai.RDS"))



food_consumption <- food_afe %>%
  rename(quantity_g = quantity_ai, item_label = j1_item_label)
write.csv(food_consumption, paste0(path_to_data_out, "food_consumption.csv"))
saveRDS(food_consumption,   paste0(path_to_data_out, "food_consumption.RDS"))

write_csv(bright_ml_targets,
          paste0(path_to_data_out, "bright_ml_targets_", Sys.Date(), ".csv"))

message("Script 2 complete. Outputs saved to ", path_to_data_out)

rm(list = ls())
