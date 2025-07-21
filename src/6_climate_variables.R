source("Src/3_mapping_base_model.R")




climate_adm2 <- read_csv("data/climate_features_lka_19.csv")

climate_adm2 <- climate_adm2 %>%
  left_join(hh_info %>% 
              select(hhid,adm2) %>% 
              mutate(hhid = as.numeric(hhid)),
            by = c("household_id" = "hhid")) %>% 
  group_by(adm2) %>% 
  summarise(r3q = mean(r3q), rfh_avg = mean(rfh_avg), vim_avg = mean(vim_avg)) 
# slice(1) %>% 
# select(-household_id)


# climate variables versus risk

adm2_inad <- adm2_average %>% 
  select(adm2, energy_kcal_q50,
         ends_with("_inad")) %>% 
  left_join(climate_adm2, by = 'adm2')


adm2_inad %>% 
  mutate(state = factor(round(as.numeric(adm2)/10))) %>% 
  ggplot(aes(x = folate_inad,y = r3q))+
  geom_point(aes(color = state,size = 2))+
  geom_smooth(method ="lm", se = F)


ggplot
