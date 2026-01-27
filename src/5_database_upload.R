# INSTALL AND LOAD PACKAGES:

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


#-------------------------------------------------------------------------------

# READ DATA
# ml_targets <- read_csv("data/processed/sl_ml_targets_2025-10-06.csv")




# remove ML Targets
# dbExecute(con, "DELETE FROM ML_targets WHERE iso3 = 'LKA'")
# ML Targets

# dbWriteTable(con, name = "ML_targets", value = ml_targets,
#              append = TRUE, row.names = FALSE)
# 
# 
# ### test it's worked
# read_lka_targets <- "SELECT * FROM ML_targets WHERE iso3 = 'LKA'"
# DBI::dbGetQuery(con, read_lka_targets)
# 
# rm(ml_targets)

#-------------------------------------------------------------------------------


adm1_codes <- read_csv("data/processed/database_upload/adm1_codes.csv")

dbWriteTable(con, name = "adm1_codes", value = adm1_codes, 
             append = TRUE, row.names = FALSE)

read_lka <- "SELECT * FROM adm1_codes WHERE survey = 'lka_hies19'"
DBI::dbGetQuery(con, read_lka)
rm(adm1_codes)

#-------------------------------------------------------------------------------
adm0_codes <- read_csv("data/processed/database_upload/adm0_codes.csv")

dbWriteTable(con, name = "adm0_codes", value = adm0_codes, 
             append = TRUE, row.names = FALSE)

read_lka_targets <- "SELECT * FROM adm0_codes WHERE iso3 = 'LKA'"
DBI::dbGetQuery(con, read_lka)
rm(adm0_codes)
#-------------------------------------------------------------------------------
hh_info <- read_csv("data/processed/database_upload/hh_info.csv")


dbWriteTable(con, name = "hh_information", value = hh_info, 
             append = TRUE, row.names = FALSE)

read_lka_targets <- "SELECT * FROM hh_information WHERE iso3 = 'LKA'"
DBI::dbGetQuery(con, read_lka)
rm(hh_info)

#-------------------------------------------------------------------------------
fct <- read_csv("data/processed/database_upload/sl_fct_db.csv")


dbWriteTable(con, name = "fct", value = hh_info, 
             append = TRUE, row.names = FALSE)

read_lka_targets <- "SELECT * FROM hh_information WHERE iso3 = 'LKA'"
DBI::dbGetQuery(con, read_lka)
rm(fct)

#-------------------------------------------------------------------------------


food_consumption <- read_csv("data/processed/database_upload/food_consumption.csv")


dbWriteTable(con, name = "fct", value = hh_info, 
             append = TRUE, row.names = FALSE)

read_lka_targets <- "SELECT * FROM hh_information WHERE iso3 = 'LKA'"
DBI::dbGetQuery(con, read_lka)
rm(fct)

#-------------------------------------------------------------------------------
food_group <- read_csv("data/processed/database_upload/food_group.csv")


dbWriteTable(con, name = "food_group", value = food_group, 
             append = TRUE, row.names = FALSE)

read_lka_targets <- "SELECT * FROM food_group WHERE iso3 = 'LKA'"
DBI::dbGetQuery(con, read_lka)
rm(fct)

#-------------------------------------------------------------------------------
h_ar <- read_csv("data/processed/database_upload/h_ar.csv")


dbWriteTable(con, name = "h_ar", value = h_ar, 
             append = TRUE, row.names = FALSE)

read_lka_targets <- "SELECT * FROM h_ar WHERE iso3 = 'LKA'"
DBI::dbGetQuery(con, read_lka)
rm(fct)

#-------------------------------------------------------------------------------

# DISCONNECT FROM DATABASE
dbDisconnect(con)

rm(list = ls())
