# test shape file for Sri Lanka

# Author: Gabriel Battcock
# Date created: 07-02-2025
# Last edited: 

#-------------------------------------------------------------------------------

# DESCRIPTION: 
# In this script I'll test reading in the shapefile using the RAM API


#-------------------------------------------------------------------------------

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("sf", "geojsonsf", "tidyverse", "devtools", "tmap")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

# source the api script

source_url("https://raw.githubusercontent.com/MIMI-wfp/MIMI-R-functions/refs/heads/main/WFP_geoAPI/get_shapefile.R")

# ------------------------------------------------------------------------------

lka_adm1 <- get_shapefile(adm0_code = 231, level = 'adm2')

lka_adm1 <- lka_adm1 %>% 
  mutate(country = "Sri Lanka")

tm_shape(lka_adm1) +
  tm_fill(col  = "Name") +
  tm_layout(frame =F)+
  tm_borders(col= 'black', lwd = 1)
  

hsee_adm1 <- get_shapefile(adm0_code = 110, level = 'adm1')

india_adm1 <- get_shapefile(adm0_code = 115, level = 'adm1') %>% 
  mutate(country = "India")
pka_adm1 <- get_shapefile(adm0_code = 188, level = 'adm1') %>% 
  mutate(country = "Pakistan")

tm_shape(india_adm1) +
  tm_fill(col  = "country") +
  tm_layout(frame =F, legend.show = F)+
  tm_borders(col= 'black', lwd = 1) +
  tm_shape(lka_adm1) +
  tm_fill(col  = "country") +
  tm_layout(frame =F)+
  tm_borders(col= 'black', lwd = 1) +
  tm_shape(pka_adm1)+
  tm_fill(col = 'country')+
  tm_borders(col = "black")



tm_shape(hsee_adm1)+
  tm_fill(col = "Name")
