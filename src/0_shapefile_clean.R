### Reading in SL HIES 2019
source("R/packages.R")
source("R/setup.R")

################################################################################


path_to_survey <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/HIES_2019/HIES_2019/"

module_list <- list.files(path = path_to_survey, pattern = NULL, all.files = FALSE,
                          full.names = FALSE, recursive = FALSE,
                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)



# Read in only the modules i want right now
modules <- c("HH_expenditure_hh_Income.csv","SEC_1_DEMOGRAPHIC.csv", "SEC_2_SCHOOL_EDUCATION.csv",
             "SEC_3A_HEALTH.csv","SEC_4_1_FOOD_EXP.csv", "weight_2019.csv")

rm(module_list)

#read in modules
for(file in modules){
  name <- sub("\\.csv$", "", file)
  assign(name, read.csv(paste0(path_to_survey, file)))   
}

# read in fct
sl_fct <- readxl::read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/sri_lanka_food_matches.xlsx", 
                            sheet = 1)

source_url("https://raw.githubusercontent.com/MIMI-wfp/MIMI-R-functions/refs/heads/main/WFP_geoAPI/get_shapefile.R")




# Data exploration #############################################################

create_hhid <- function(df){
  df <- df %>%  mutate(hhid = paste0(district,sector,psu,snumber,hhno))
  return(df)
}

HH_expenditure_hh_Income <- create_hhid(HH_expenditure_hh_Income)
adm1_match <- HH_expenditure_hh_Income %>% 
  distinct(district) %>% 
  mutate(
    adm1 = 
      case_when(
        district == 11 ~ 2744, 
        district == 12 ~ 2744,
        district == 13 ~ 2744,
        district == 21 ~ 2736,
        district == 22 ~ 2736,
        district == 23 ~ 2736,
        district == 31 ~ 2742,
        district == 32 ~ 2742,
        district == 33 ~ 2742,
        district == 41 ~ 2740,
        district == 42 ~ 2740,
        district == 43 ~ 2740,
        district == 44 ~ 2740,
        district == 45 ~ 2740,
        district == 51 ~ 2737,
        district == 52 ~ 2737,
        district == 53 ~ 2737,
        district == 61 ~ 2739,
        district == 62 ~ 2739,
        district == 71 ~ 2738,
        district == 72 ~ 2738,
        district == 81 ~ 2743,
        district == 82 ~ 2743,
        district == 91 ~ 2741,
        district == 92 ~ 2741
    
    )
  )


adm2_match <- HH_expenditure_hh_Income %>% 
  distinct(district) %>% 
  mutate(
    adm2 = 
      case_when(
        district == 11 ~ 25851, 
        district == 12 ~ 25852,
        district == 13 ~ 25853,
        district == 21 ~ 41748,
        district == 22 ~ 25830,
        district == 23 ~ 41749,
        district == 31 ~ 25846,
        district == 32 ~ 25848,
        district == 33 ~ 25847,
        district == 41 ~ 25839,
        district == 42 ~ 25841,
        district == 43 ~ 25843,
        district == 44 ~ 25842,
        district == 45 ~ 25840,
        district == 51 ~ 25833,
        district == 52 ~ 25832,
        district == 53 ~ 25834,
        district == 61 ~ 25837,
        district == 62 ~ 25838,
        district == 71 ~ 25835,
        district == 72 ~ 25836,
        district == 81 ~ 25849,
        district == 82 ~ 25850,
        district == 91 ~ 25845,
        district == 92 ~ 25844
        
      )
  )



# Shpaefile from WFP -----------------------------------------------------------

lka_adm2 <- get_shapefile(adm0_code = 231, level = 'adm2')
lka_adm1 <- get_shapefile(adm0_code = 231, level = 'adm1')
lka_adm1 <- lka_adm1 %>% 
  mutate(country = "Sri Lanka")


# try making a map using the HH data
# adm1 
adm1_shapefile <- adm1_match %>% 

  left_join(lka_adm1, by = c("adm1" = "Code")) %>% 
  mutate(adm1 = floor(district/10)) %>% 
  group_by(adm1) %>% 
  slice(1) %>% 
  select(adm1, geometry) %>% 
  mutate(adm1 = as.character(adm1)) %>% 
  st_as_sf()



# adm 2

adm2_shapefile <- adm2_match %>% 
  
  left_join(lka_adm2, by = c("adm2" = "Code")) %>% 
  select(-adm2, -Name) %>% 
  rename(adm2 = district) %>% 
  mutate(adm2 = as.character(adm2)) %>% 
  st_as_sf()



################################################################################
rm(lka_adm1,lka_adm2,adm1_match,adm2_match)



