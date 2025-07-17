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
ml_targets <- read_csv("data/processed/sl_ml_targets_2025-07-11.csv")

#-------------------------------------------------------------------------------


# vehicle_quantities
dbWriteTable(con, name = "ML_targets", value = ml_targets, 
             append = TRUE, row.names = FALSE)


### test it's worked
# read_lka_targets <- "SELECT * FROM ML_targets WHERE iso3 = 'LKA'"
# DBI::dbGetQuery(con, read_lka_targets)


# DISCONNECT FROM DATABASE
dbDisconnect(con)

rm(list = ls())
