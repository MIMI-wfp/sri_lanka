### Reading in SL HIES 2019
 
rm(list = ls())
rq_packages <- c("tidyverse", "srvyr", 'labelled')
 
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
 
lapply(rq_packages, require, character.only = T)
 
rm(list= c("rq_packages", "installed_packages"))
readRenviron(".Renviron")
 
 
################################################################################
 
 
path_to_survey <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/bright_survey/"
path_to_data <- "data/processed/"
path_to_raw_data <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/"
 
module_list <- list.files(path = path_to_survey, pattern = NULL, all.files = FALSE,
                          full.names = FALSE, recursive = FALSE,
                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
 
 
modules <- c("mod_a_household_identification.dta","mod_l2_food_security_m.dta", "mod_j1_fah_long.dta", "mod_j1_fah_wide.dta",
             "mod_b1_roster_long.dta")
 
 
#read in modules
for(file in modules){
  name <- sub("\\.dta$", "", file)
  assign(name, haven::read_dta(paste0(path_to_survey, file)))
}
 
 
mod_j1_fah_long %>% 
  head(5)
 
mod_j1_fah_wide
 
food_without_conversion <- mod_j1_fah_long %>%  
  filter(j1_01 == 1 & is.na(fcf_kg)) %>% 
  group_by(j1_item, j1_03,j1_03a,fcf_kg) %>% 
  summarise(n()) %>% 
  export.label()
 
add_value_labels <- function(df) {
  out <- df
  for (v in names(df)) {
    labs <- val_labels(df[[v]])
    if (!is.null(labs)) {
      lookup <- setNames(names(labs), unname(labs))
      out[[paste0(v, "_label")]] <- lookup[as.character(df[[v]])]
    }
  }
  out
}
 
df_full <- food_without_conversion %>% add_value_labels()
 
 
write.csv(df_full, "data/bright/processed/food_without_conversion.csv")