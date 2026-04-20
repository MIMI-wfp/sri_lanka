rq_packages <- c("tidyverse", "srvyr","sf", "geojsonsf", "tidyverse", "devtools", "tmap")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))