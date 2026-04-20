# INSTALL AND LOAD PACKAGES:


################################################################################
# NOTE: PLEASE UNCOMMENT ANY LINES YOU WANT TO RUN. THESE ARE COMMENTED OUT SO 
# THERE IS NOT ACCIDENTAL DELETION OR UPLOADING TO THE DATABASE 
################################################################################

rq_packages <- c("readr", "DBI", "RMySQL", "tidyverse", "getPass")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# CONNECT TO DATABASE:
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "mimi_db",
                 host = "localhost",
                 port = 3306,
                 user = getPass("Enter username: "),
                 password = getPass("Enter password: "))


# HML DATA --------------------------------------------------------------------

# dbExecute(con, "DELETE FROM hungermap_mimi WHERE iso3 = 'LKA'")
# read_lka_hm <- "SELECT * FROM hungermap_mimi WHERE iso3 = 'LKA'"
# DBI::dbGetQuery(con, read_lka_hm)
# 
# rm(read_lka_hm)
#-------------------------------------------------------------------------------

# READ DATA
# ml_targets <- read_csv("data/processed/sl_ml_targets_2026-01-28.csv")
# 
# 
# 
# 
# # remove ML Targets
# dbExecute(con, "DELETE FROM ML_targets WHERE iso3 = 'LKA'")
# # ML Targets
# 
# dbWriteTable(con, name = "ML_targets", value = ml_targets,
#              append = TRUE, row.names = FALSE)
# # 
# # 
# ## test it's worked
# read_lka_targets <- "SELECT * FROM ML_targets WHERE iso3 = 'LKA'"
# DBI::dbGetQuery(con, read_lka_targets)
# # 
# rm(ml_targets)

#-------------------------------------------------------------------------------

# 
# adm1_codes <- read_csv("data/processed/database_upload/adm1_codes.csv")
# adm1_codes
# 
# 
# 
# 
# dbWriteTable(con, name = "adm1_codes", value = adm1_codes, 
#              append = TRUE, row.names = FALSE)
# 
# read_lka <- "SELECT * FROM adm1_codes WHERE survey = 'lka_hies19'"
# DBI::dbGetQuery(con, read_lka)
# rm(adm1_codes)

#-------------------------------------------------------------------------------
# adm0_codes <- read_csv("data/processed/database_upload/adm0_codes.csv")
# adm0_codes
# 
# dbWriteTable(con, name = "adm0_codes", value = adm0_codes, 
#              append = TRUE, row.names = FALSE)
# 
# read_lka <- "SELECT * FROM adm0_codes WHERE iso3 = 'LKA'"
# DBI::dbGetQuery(con, read_lka)
# rm(adm0_codes)
#-------------------------------------------------------------------------------
# base_ai <- read_csv("data/processed/database_upload/base_ai.csv")
# base_ai <- base_ai %>% select(-`...1`)
# base_ai
# 
# dbExecute(con, "DELETE FROM base_ai WHERE iso3 = 'LKA'")
# 
# dbWriteTable(con, name = "base_ai", value = base_ai, 
#              append = TRUE, row.names = FALSE)
# 
# read_lka <- "SELECT * FROM base_ai WHERE iso3 = 'LKA'"
# DBI::dbGetQuery(con, read_lka)
# rm(base_ai)

#-------------------------------------------------------------------------------

# hh_info <- read_csv("data/processed/database_upload/hh_info.csv")
# hh_info
# 
# 
# dbWriteTable(con, name = "hh_information", value = hh_info, 
#              append = TRUE, row.names = FALSE)
# 
# read_lka_targets <- "SELECT * FROM hh_information WHERE iso3 = 'LKA'"
# DBI::dbGetQuery(con, read_lka_targets)
# rm(hh_info)

#-------------------------------------------------------------------------------
# fct <- read_csv("data/processed/database_upload/fct.csv")
# fct <- fct %>% select(-`...1`)
# 
# dbWriteTable(con, name = "fct", value = fct, 
#              append = TRUE, row.names = FALSE)
# 
# read_lka <- "SELECT * FROM fct WHERE iso3 = 'LKA'"
# DBI::dbGetQuery(con, read_lka)
# rm(fct)
# 
# 



#-------------------------------------------------------------------------------

# 
# food_consumption <- read_csv("data/processed/database_upload/food_consumption.csv")
# food_consumption <- food_consumption %>% select(-`...1`)
# 
# dbExecute(con, "DELETE FROM food_consumption WHERE iso3 = 'LKA'")
# 
# 
# dbWriteTable(con, name = "food_consumption", value = food_consumption, 
#              append = TRUE, row.names = FALSE)
# 
# read_lka <- "SELECT * FROM food_consumption WHERE iso3 = 'LKA'"
# DBI::dbGetQuery(con, read_lka)
# rm(food_consumption)

#-------------------------------------------------------------------------------
# food_group <- read_csv("data/processed/database_upload/food_group.csv")
# food_group <- food_group %>% select(-`...1`)
# unique(food_group$food_group)
# 
# dbWriteTable(con, name = "food_group", value = food_group, 
#              append = TRUE, row.names = FALSE)
# 
# read_lka <- "SELECT * FROM food_group WHERE iso3 = 'LKA'"
# DBI::dbGetQuery(con, read_lka)
# rm(food_group)

#-------------------------------------------------------------------------------
# h_ar <- read_csv("data/processed/database_upload/h_ar.csv")
# h_ar <- h_ar %>% select(-`...1`)
# 
# dbWriteTable(con, name = "h_ar", value = h_ar, 
#              append = TRUE, row.names = FALSE)
# 
# read_lka <- "SELECT * FROM h_ar"
# DBI::dbGetQuery(con, read_lka)
# rm(h_ar)

#-------------------------------------------------------------------------------

# DISCONNECT FROM DATABASE
dbDisconnect(con)

rm(list = ls())
