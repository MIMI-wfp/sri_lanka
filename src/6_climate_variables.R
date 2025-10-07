source("src/3_mapping_base_model.R")

library(ggpmisc)


climate_adm2 <- read_csv("data/climate_features_lka_19.csv")
ml_targets <- read_csv("data/processed/sl_ml_targets_2025-07-11.csv")

climate_adm2 <- climate_adm2 %>%
  left_join(ml_targets %>% select(hhid,overall_mar),by = c("household_id" = "hhid")) %>% 
  left_join(hh_info %>% 
              select(hhid,adm2) %>% 
              mutate(hhid = as.numeric(hhid)),
            by = c("household_id" = "hhid")) %>% 
  group_by(adm2) %>% 
  summarise(r3q = mean(r3q), rfh_avg = mean(rfh_avg), vim_avg = mean(vim_avg),mar = mean(overall_mar)) 
# slice(1) %>% 
# select(-household_id)


# climate variables versus risk

adm2_inad <- adm2_average %>% 
  select(adm2, energy_kcal_q50,
         ends_with("_inad")) %>% 
  left_join(climate_adm2, by = 'adm2') %>% 
  mutate(province = round(as.numeric(adm2) / 10,0),
         province = factor(case_match(
           province,
           1 ~ "Western",
           2~ "Central",
           3 ~ "Southern",
           4 ~ "Northern",
           5 ~ "Eastern",
           6 ~ "North Western",
           7 ~ "North Central",
           8 ~ "Uva",
           9 ~ "Sabaragamuwa"
         )
         
         )
  )

mn_col_names = c(colnames(adm2_inad)[3:8],colnames(adm2_inad)[12])
clim_col_names = colnames(adm2_inad)[9:11]


clim_plots <- list()
x <- 1

for (i in mn_col_names) {
  for (j in clim_col_names) {
    print(c(i, j))
    
    
    p1 <- 
      adm2_inad %>%
      ggplot(aes(x = .data[[j]], y = .data[[i]])) +
      geom_point(aes(color = province), size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      stat_poly_eq(
        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
        formula = y ~ x,
        parse = TRUE,
        label.x.npc = "left",  # Position in normalized parent coordinates
        label.y.npc = "top"
      ) +
      ggtitle(paste("Scatterplot of", i, "vs", j))
    
    
    clim_plots[[x]] <- p1
    
    ggsave(paste0("outputs/plots/climate/",i,"_",j,".png"),p1 )
    x <- x + 1
  }
}

