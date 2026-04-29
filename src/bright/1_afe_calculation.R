### BRIGHT Survey - AFE Calculation & Household Info
### Mirrors src/1_afe_calculation.R for the HIES 2019 pipeline
###


rm(list = ls())

rq_packages <- c("tidyverse", "srvyr", "readxl", "haven")
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = TRUE)
rm(list = c("rq_packages", "installed_packages"))

################################################################################
# PATHS  -----------------------------------------------------------------------
path_to_survey   <- "/Users/gabrielbattcock/Library/CloudStorage/OneDrive-WorldFoodProgramme/General - MIMI Project/Countries/Sri Lanka/data/bright_survey/"
path_to_data_out <- "data/bright/processed/"

################################################################################
# LOAD MODULES  ----------------------------------------------------------------

bright_hh <- haven::read_dta(paste0(path_to_survey, "mod_a_household_identification.dta"))
bright_demo <- haven::read_dta(paste0(path_to_survey, "mod_b0_roster_long.dta"))
bright_food <- haven::read_dta(paste0(path_to_survey, "mod_j1_fah_long.dta"))

################################################################################
# HOUSEHOLD ID  ----------------------------------------------------------------

create_hhid <- function(df) {
   df <- df %>% rename(hhid = hhcode)
  return(df)
}

bright_hh   <- create_hhid(bright_hh)
bright_demo <- create_hhid(bright_demo)
bright_food <- create_hhid(bright_food)

# Sanity check – all IDs should be unique at the HH level
stopifnot("Duplicate hhid in HH module" = !any(duplicated(bright_hh$hhid)))

################################################################################
# AFE CALCULATION  -------------------------------------------------------------
# Reference Energy Level (REL): 2170 kcal/day (Sri Lankan DRI)
afe_value <- 2170


demographics <- bright_demo %>%
  rename(
    person_serial_no = pid,
    sex         = b0_02,
    age         = b0_04_years,
    birth_year  = b0_04c,
    birth_month = b0_04b
  ) |>
  left_join(bright_hh |> select(hhid, endtime), by = "hhid") |>
  mutate(
    month = month(mdy_hms(endtime)),   # integer 1–12
    year  = year(mdy_hms(endtime))     # 4-digit integer e.g. 2023
  ) |>
  select(hhid, person_serial_no, sex, age, birth_year, birth_month, month, year)

# --- Identify households with children under 2 (for breastfeeding flag) ------
u2s <- demographics %>%
  filter(age <= 2) %>%
  mutate(
    birth_date  = as.Date(paste0("15-", birth_month, "-", birth_year), "%d-%m-%Y"),
    survey_date = as.Date(paste0("15-", month, "-", year), "%d-%m-%Y"),
    age_month   = floor(as.numeric(survey_date - birth_date) / 30)
  ) %>%
  select(hhid, person_serial_no, age_month)

hh_with_u2s <- u2s %>%
  filter(age_month <= 24) %>%
  group_by(hhid) %>%
  summarise(u2 = 1)

demographics <- demographics %>%
  left_join(hh_with_u2s, by = "hhid")

rm(hh_with_u2s)

# --- Under-2 energy requirements ---------------------------------------------
# Source: Sri Lankan Dietary Reference Intakes (MRI 2024)
afeu2 <- u2s %>%
  mutate(
    TEE = case_when(
      age_month < 3                        ~ 0,    # breastfed only
      age_month >= 3  & age_month < 6      ~ 76,
      age_month >= 6  & age_month < 9      ~ 269,
      age_month >= 9  & age_month < 12     ~ 451,
      age_month >= 12                      ~ 746
    ),
    weight = case_when(
      age_month < 3                        ~ 4.35,
      age_month >= 3  & age_month < 6      ~ 6.7,
      age_month >= 6  & age_month < 9      ~ 7.95,
      age_month >= 9  & age_month < 12     ~ 8.85,
      age_month >= 12                      ~ 10.55
    ),
    afe = TEE / afe_value
  ) %>%
  select(hhid, person_serial_no, afe, weight, TEE, age_month)

# --- Breastfeeding women -----------------------------------------------------
# Assumption: women aged 15–44 in households with under-2s; one per HH
breastfeeding <- demographics %>%
  filter(sex == 1, age > 15, age < 45, u2 == 1) %>%
  group_by(hhid) %>%
  slice(1)

afe_breastfeeding <- breastfeeding %>%
  mutate(
    PAL    = 1.76,
    weight = 55,   # assumed female body weight (kg)
    BMR = case_when(
      age > 18 & age <= 30 ~ 14.818 * weight + 486.6,
      age > 30 & age <  60 ~ 8.126  * weight + 845.6
    ),
    TEE = case_when(age > 15 & age <= 18 ~ 2400)
  ) %>%
  mutate(
    TEE = ifelse(is.na(BMR), TEE + 483, BMR * PAL + 483),
    afe = TEE / afe_value
  ) %>%
  select(hhid, person_serial_no, weight, afe, TEE, age)

# --- All other individuals (2+ years) ----------------------------------------
demographics_others <- demographics %>%
  anti_join(u2s,          by = c("hhid", "person_serial_no")) %>%
  anti_join(breastfeeding, by = c("hhid", "person_serial_no"))

rm(u2s, breastfeeding)

tee_calc <- demographics_others %>%
  mutate(weight = ifelse(sex == 0, 65, 55)) %>%   # assumed body weights (kg)
  filter(age >= 2) %>%
  mutate(PAL = ifelse(age > 18, 1.76, NA))

# TEE lookup table – Sri Lankan DRI age-sex specific values
tee_calc <- tee_calc %>%
  mutate(TEE = case_when(
    age >= 1  & age <  4                        ~ 990,
    age >= 4  & age <  7                        ~ 1560,
    age >= 7  & age < 11                        ~ 1920,
    sex == 0 & (age >= 11 & age < 15)           ~ 2390,
    sex == 1 & (age >= 11 & age < 15)           ~ 2180,
    sex == 0 & (age >= 15 & age < 18)           ~ 3020,
    sex == 1 & (age >= 15 & age < 18)           ~ 2400,
    sex == 0 & (age >= 18 & age < 25)           ~ 2840,
    sex == 1 & (age >= 18 & age < 25)           ~ 2280,
    sex == 0 & (age >= 25 & age < 51)           ~ 2930,
    sex == 1 & (age >= 25 & age < 51)           ~ 2170,
    sex == 0 & (age >= 51 & age < 71)           ~ 2560,
    sex == 1 & (age >= 51 & age < 71)           ~ 2070,
    sex == 0 & (age >= 71)                      ~ 2410,
    sex == 1 & (age >= 71)                      ~ 1960
  ))

afe_others <- tee_calc %>%
  mutate(afe = TEE / afe_value) %>%
  select(hhid, person_serial_no, afe, weight, TEE, age)

rm(tee_calc)

# --- Combine all AFE values --------------------------------------------------
afe_all <- bind_rows(afeu2, afe_breastfeeding, afe_others)

# Completeness check
missing_afe <- demographics %>%
  anti_join(afe_all, by = c("hhid", "person_serial_no"))

if (nrow(missing_afe) > 0) {
  warning(paste(nrow(missing_afe), "individuals are missing from afe_all – review demographics"))
  print(missing_afe)
}

afe_all %>% filter(is.na(afe)) %>% nrow() %>%
  {if (. > 0) warning(paste(., "rows have NA afe values – check TEE lookup table"))}


hh_with_na <- afe_all %>% filter(is.na(afe))

hh_with_na |> left_join(demographics, by = c('hhid', 'person_serial_no', 'age'))

# Household-level AFE
hh_afe <- afe_all %>%
  mutate(afe = ifelse(is.na(afe),1,afe)) |> 
  group_by(hhid) %>%
  summarise(afe   = sum(afe),

            total = n()) |> 
  mutate(afe = ifelse(afe<1, 1, afe))

hh_afe |> filter(is.na(afe))



rm(afe_breastfeeding, afe_others, afeu2)

################################################################################
# HOUSEHOLD INFO TABLE  --------------------------------------------------------
# ⚠️ [D] Confirm month column exists in bright_demo and maps cleanly
# ⚠️ [E] Confirm hhexppm and finalweight column names in bright_hh
# ⚠️ [F] Confirm district/sector encoding:
#         - adm1 = floor(district/10) assumed; update if BRIGHT uses different admin codes
#         - res factor levels: confirm 1=Urban, 2=Rural, 3=Estate (or equivalent)
# ⚠️ [H] Update year = 20XX below

hh_info <- bright_hh %>%
  mutate(
    month = month(mdy_hms(endtime)),
    year  = year(mdy_hms(endtime))
  ) %>%
  left_join(hh_afe, by = "hhid") %>%
  mutate(
    survey      = "lka_bright",
    iso3        = "LKA",
    zone        = NA,
    adm1        = as.character(a_01_province),  
    adm2        = as.character(a_02_district),              
    res         = factor(case_when(
      a_05 == 1 ~ "Urban",
      a_05 == 2 ~ "Rural",
      a_05 == 3 ~ "Estate"
    )),
    # TODO double check this is correct
    ea          = as.character(paste0(a_01_province,"-",a_02_district,"-",a_05)),                   
    survey_wgt  = hhweight                         
  ) %>%
#   group_by(a_05) %>%
#   mutate(
#     per_capita_expenditure = hhexppm / total,       # ⚠️ [E] confirm hhexppm column name in mod_a
#     res_quintile = case_when(
#       per_capita_expenditure < quantile(per_capita_expenditure, probs = seq(0,1,0.2), na.rm = TRUE)[[2]] ~ 1,
#       per_capita_expenditure < quantile(per_capita_expenditure, probs = seq(0,1,0.2), na.rm = TRUE)[[3]] ~ 2,
#       per_capita_expenditure < quantile(per_capita_expenditure, probs = seq(0,1,0.2), na.rm = TRUE)[[4]] ~ 3,
#       per_capita_expenditure < quantile(per_capita_expenditure, probs = seq(0,1,0.2), na.rm = TRUE)[[5]] ~ 4,
#       per_capita_expenditure <= quantile(per_capita_expenditure, probs = seq(0,1,0.2), na.rm = TRUE)[[6]] ~ 5
#     )
#   ) %>%
#   ungroup() %>%
#   mutate(
#     sep_quintile = case_when(
#       per_capita_expenditure < quantile(per_capita_expenditure, probs = seq(0,1,0.2), na.rm = TRUE)[[2]] ~ 1,
#       per_capita_expenditure < quantile(per_capita_expenditure, probs = seq(0,1,0.2), na.rm = TRUE)[[3]] ~ 2,
#       per_capita_expenditure < quantile(per_capita_expenditure, probs = seq(0,1,0.2), na.rm = TRUE)[[4]] ~ 3,
#       per_capita_expenditure < quantile(per_capita_expenditure, probs = seq(0,1,0.2), na.rm = TRUE)[[5]] ~ 4,
#       per_capita_expenditure <= quantile(per_capita_expenditure, probs = seq(0,1,0.2), na.rm = TRUE)[[6]] ~ 5
#     )
#   ) %>%
#   rename(pc_expenditure = per_capita_expenditure) %>%
  select(survey, hhid, iso3, zone, adm1, adm2, ea, res,
         year, month, survey_wgt, afe)

################################################################################
# SAVE OUTPUTS  ----------------------------------------------------------------

write_csv(afe_all, paste0(path_to_data_out, "hh_energy_requirements.csv"))
write_csv(hh_info, paste0(path_to_data_out, "hh_info.csv"))
saveRDS(hh_info,   paste0(path_to_data_out, "hh_info.RDS"))

message("Script 1 complete. Outputs saved to ", path_to_data_out)

rm(list = ls())
