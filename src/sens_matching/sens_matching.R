source("src/0_shapefile_clean.R")
# source("src/setup_environment.R")

hh_info <- read_rds("data/processed/hh_info.RDS")
base_ai <- read_rds("data/processed/base_ai.RDS")

# ------------------------------------------------------------------------------

set.seed(123)
data.df <- read.csv(here::here("data/processed", "sens_matching.csv"))


names(data.df)
#food_list <- readxl::read_excel(here::here( "NSSO_INDB_20241023.xlsx"))


# Checking the data
unique(data.df$item_name)
length(unique(data.df$code))
length(unique(data.df$hhid))
# data.df$item_name[data.df$code == "61"]
# Some mismatches between no. of unique items and codes
# data.df %>% distinct(item_name, Item_Code) %>% count(item_name)
# unique(data.df$Item_Code[data.df$item_name == "peas(Kg)"]) # peas has the same name different code, it may be dried vs fresh


## Testing loop w/ error as backstop: wilcox.test -----


# Selecting the nutrients 
#vars <- c("VITA_RAE", "VITB12", "FOLDE", "ZN", "FE")
vars <- names(data.df)[c(14,15,16,20,21)]

# Getting the list of food sorted by most consumed (freq. (no. of HHs))
food_list <- data.df %>% filter(!is.na(quantity_100g)) %>%
  group_by(code) %>% 
  summarise(N = n(), 
            Mean_qty = mean(quantity_100g, na.rm = TRUE)) %>%
  arrange(desc(N))


# A loop that generate a list of dataset w/ the mean, sd and median intakes
# excluding one food item for the variables selected

i = 1
j=1
test <- list()  
# Creating an empty dataframe
wilcox_test <- as.data.frame(matrix( ncol = 4))


# Adding the baseline (no food removed)
test[[1]] <- data.df %>% filter(!is.na(quantity_100g)) %>% 
  # mutate(across(vars, ~as.numeric)) %>% 
  group_by(hhid) %>%       
  summarise(across(vars, list(Sum = sum), na.rm=TRUE, .names = "{.fn}.{.col}") ) %>% 
  mutate(test_food = "baseline")

for(i in 1:nrow(food_list)){
  
  n <- i+1
  test[[n]] <- data.df %>% filter(!is.na(quantity_100g)) %>% 
    filter(!code %in% food_list[i,1 ]) %>% 
    group_by(hhid) %>%       
    summarise(across(vars,  list(Sum=sum), 
                     na.rm = TRUE, .names = "{.fn}.{.col}")) %>% 
    # Adding a variable with the item excluded
    mutate(test_food = paste0(food_list[i,1 ], "_", gsub(" ", "", food_list[i,2])))
  
  print(n)
  
  sum_vars <- grep("Sum.", names(test[[1]]), value = TRUE)
  
  for(j in 1:length(sum_vars)){
    
    mod2=try(wilcox.test(log(as.numeric(unlist(test[[n]][, sum_vars[j]]))), 
                         log(as.numeric(unlist(test[[1]][, sum_vars[j]])))),TRUE)
    
    if(isTRUE(class(mod2)=="try-error")) { next }
    
    else{
      
      x <- wilcox.test(log(as.numeric(unlist(test[[n]][, sum_vars[j]]))), 
                       log(as.numeric(unlist(test[[1]][, sum_vars[j]]))))
      
      wilcox_test[nrow(wilcox_test)+1,]<- broom::tidy(x)
      wilcox_test[nrow(wilcox_test), "test_food"] <- paste0(food_list[i,1 ], "_", gsub(" ", "", food_list[i,2]))
      wilcox_test[nrow(wilcox_test), "test_nutrient"] <- sum_vars[j]
      
    }
    print(j)
  }
  
  print(i)
  
  
}

# Saving the output into spreadsheet
writexl::write_xlsx(test, 
                    here::here("outputs/inter-output", paste0("sensitivity_outputb_", Sys.Date(), ".xlsx")))

names(wilcox_test)[1:4] <- names(broom::tidy(x))
#names(t_test)[11:12] <- c("test_food","test_nutrient" )


# Saving results form loop
write.csv(wilcox_test, here::here( "inter-output", paste0("wilcox_test_food_nutrient_", Sys.Date(), ".csv")))

p.values <- wilcox_test %>% dplyr::filter(!is.na(statistic)) %>% 
  select(p.value, test_food, test_nutrient) %>% 
  tidyr::pivot_wider(names_from = "test_nutrient",
                     values_from = "p.value") 

# Saving results p.values per nutrient form loop
write.csv(p.values, here::here( "outputs/inter-output", paste0("p.values_wilcox_food_nutrient_",
                                                       Sys.Date(), ".csv")))

## Graph (2) ----------

names(test[[1]])
j = 3
names(test[[1]])[j]

df <- as.data.frame(matrix( ncol = 4))

for(i in 1:length(test)){
  
  df[i,1] <- unique(test[[i]][, c("test_food" )])
  df[i,2] <- median(as.numeric(unlist(test[[i]][, names(test[[1]])[j]])))
  df[i,3] <- quantile(as.numeric(unlist(test[[i]][, names(test[[1]])[j]])), probs = 0.25)
  df[i,4] <- quantile(as.numeric(unlist(test[[i]][, names(test[[1]])[j]])), probs = 0.75)
  
}


names(df) <- c("scenario", "Median", "Q25", "Q75")

p.items <- p.values$test_food[p.values[, names(test[[1]])[j]]<0.05]

# Contribution to each nutrient 
df %>% dplyr::mutate(p.value = ifelse(scenario %in% p.items, "YES", "NO")) %>% 
  arrange(Median) %>% 
  dplyr::mutate(scenario = factor(scenario, levels = scenario)) %>% 
  ggplot() + 
  geom_point(aes(scenario, Median, colour =p.value)) +
  #geom_point(aes(x=reorder(scenario, Median), y=Median, colour = p.value)) +
  geom_errorbar(aes(scenario, ymin=Q25, ymax=Q75), width =0.2) +
  theme_bw() +
  labs(y = names(test[[1]])[j]) +
  #  theme(axis.text = element_text(angle = 90)) 
  coord_flip() 
