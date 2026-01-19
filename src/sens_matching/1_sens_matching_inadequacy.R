# Load data
source("src/0_shapefile_clean.R")
# source("src/setup_environment.R")

# sri lanka data

# read.csv("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Documents/MIMI_mac/nsso_hces_2023/data/nsso_subset_lucia.csv")

# indian data
choose_data <- function(country){
  set.seed(123)
  
  
  if(country == "SL"){
    country <<- country
    hh_info <- read_rds("data/processed/hh_info.RDS")
    base_ai <- read_rds("data/processed/base_ai.RDS")
    sl_data.df <- read_csv("data/processed/sens_matching.csv")
    sl_fct <- readxl::read_xlsx("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/sri_lanka_food_matches.xlsx", 
                              sheet = 1)
  
  
 
  random_hhids <- sample(hh_info$hhid, size = 2000)
  
  
  
  data.df <- sl_data.df %>% filter(hhid %in% random_hhids)
  return(data.df)
  }
  
  
  if(country == "IND"){
    country <<- country
    ind_data.df <- read.csv("C:/Users/gabriel.battcock/OneDrive - World Food Programme/Documents/MIMI_mac/nsso_hces_2023/data/nsso_subset_lucia.csv")
    data.df <- ind_data.df %>% 
      rename(hhid = common_id, code = Item_Code, vitb12_mcg = vitaminb12_in_mcg, fe_mg = iron_mg,
             zn_mg = zinc_mg, folate_mcg = folate_ug)
    return(data.df)
    }
  
  if(country == "BGD"){
# bangladesh data
    path_to_data <- "C:/Users/gabriel.battcock/OneDrive - World Food Programme/Desktop/bangladesh_base_model/"
    country <<- country
    bgd_base_ai <- read.csv(paste0(path_to_data, "bgd_base_ai.csv"))
    bgd_data.df <- read.csv("data/processed/bgd_sens_matching.csv")
    
    random_hhids <- sample(bgd_base_ai$hhid, size = 2000)
    
    
    
    data.df <- bgd_data.df %>% filter(hhid %in% random_hhids)
    rm(path_to_data)
    return(data.df)
    }
}
data.df <- choose_data("SL")

# Nutrients to analyze
vars <- c("folate_mcg", "fe_mg")  # e.g., VITA_RAE, VITB12

# Define EARs (adjust as needed)
EARs <- c(fe_mg = 15, folate_mcg = 250)

# Get food list by frequency
food_list <- data.df %>%
  filter(!is.na(quantity_100g)) %>%
 
  group_by(item_name) %>%
  summarise(N = n(), Mean_qty = mean(quantity_100g, na.rm = TRUE)) %>%
  arrange(desc(N))

# Initialize lists
test <- list()
# wilcox_test <- data.frame()

# Baseline calculation
test[[1]] <- data.df %>%
  filter(!is.na(quantity_100g)) %>%
  group_by(hhid) %>%
  summarise(across(all_of(vars), sum, na.rm = TRUE, .names = "Sum.{.col}")) %>%
  mutate(test_food = "baseline") %>%
  mutate(across(starts_with("Sum."), 
                ~ ifelse(.x < EARs[
                  gsub("^Sum\\.", "", cur_column())
                ], 1,0),
                .names = "Inadequate.{.col}")) %>%
  rowwise() %>%
  
  ungroup()

sum_vars <- grep("Inadequate", names(test[[1]]), value = TRUE)


chi_squared <- data.frame()


# Leave-one-out loop
for (i in 1:nrow(food_list)) {
  n <- i + 1
  test[[n]] <- data.df %>%
    filter(!is.na(quantity_100g), !item_name %in% food_list[i, 1]) %>%
    group_by(hhid) %>%
    summarise(across(all_of(vars), sum, na.rm = TRUE, .names = "Sum.{.col}")) %>%
    mutate(test_food = paste0(food_list[i, 1], "_", gsub(" ", "", food_list[i, 2]))) %>%
    mutate(across(starts_with("Sum."), 
                  ~ ifelse(.x < EARs[
                    gsub("^Sum\\.", "", cur_column())
                  ], 1,0),
                  .names = "Inadequate.{.col}")) %>%
    rowwise() %>%
    ungroup()
  
  
  print(n)
  
  
  for (j in seq_along(sum_vars)) {
    try({
      # Get the binary inadequacy column name
      colname <- sum_vars[j]
      
      # Create a 2x2 contingency table
      contingency <- table(
        group = c(rep("baseline", nrow(test[[1]])), rep("test", nrow(test[[n]]))),
        inadequate = c(test[[1]][[colname]], test[[n]][[colname]])
      )
      
      # Run chi-squared test
      x <- chisq.test(contingency)
      
      # Tidy the result
      result <- broom::tidy(x)
      result$test_food <-paste0(food_list[i, 1], "_", gsub(" ", "", food_list[i, 2]))
      result$test_nutrient <- colname

      chi_squared <- bind_rows(chi_squared, result)
    }, silent = TRUE)
    print(j)
  }
  
}

# Save full test list
writexl::write_xlsx(test, here::here("outputs/inter-output", paste0(country,"_sensitivity_outputb_", Sys.Date(), ".xlsx")))

# Save Wilcoxon test results
names(chi_squared)[1:4] <- names(broom::tidy(x))
write.csv(chi_squared, here::here("data/inter-output", paste0(country,"_chi_squared_food_nutrient_", Sys.Date(), ".csv")))

# Pivot p-values
p.values <- chi_squared %>%
  filter(!is.na(statistic)) %>%
  select(p.value, test_food, test_nutrient) %>%
  tidyr::pivot_wider(names_from = "test_nutrient", values_from = "p.value")

write.csv(p.values, here::here("outputs/inter-output", paste0(country,"_p.values_chi_squared_food_nutrient_", Sys.Date(), ".csv")))

# Plotting median + IQR for one nutrient
j <- 5  # index of nutrient to plot

names(test[[1]])[j]
name <- names(test[[1]])[j]
df <- data.frame()



for (i in 1:length(test)) {
  df[i, "scenario"] <- unique(test[[i]]$test_food)
  df[i, "Median"] <-mean(pull(test[[i]], names(test[[1]])[j]) == 1, na.rm = TRUE)
  # df[i, "Q25"] <- quantile(pull(test[[i]], names(test[[1]])[j]), 0.25, na.rm = TRUE)
  # df[i, "Q75"] <- quantile(pull(test[[i]], names(test[[1]])[j]), 0.75, na.rm = TRUE)
}

p.items <- p.values$test_food[p.values[, names(test[[1]])[j]] < 0.05]

significant <- data.frame(setNames(list(p.items), names(test[[1]])[j]))
write_csv(significant, here::here("outputs/inter-output", paste0(country,"_significant_items_",name, Sys.Date(), ".csv")))


df %>%
  mutate(p.value = ifelse(scenario %in% p.items, "YES", "NO")) %>%
  arrange(desc(Median)) %>%
  mutate(scenario = factor(scenario, levels = scenario)) %>%
  ggplot() +
  geom_point(aes(scenario, Median, colour = p.value)) +
  # geom_errorbar(aes(scenario, ymin = Q25, ymax = Q75), width = 0.2) +
  theme_bw() +
  labs(y = names(test[[1]])[j]) +
  coord_flip()

# Optional: Plot risk of inadequacy
risk_df <- map_df(test, ~ .x %>%
                    summarise(scenario = unique(.x$test_food),
                              Risk = mean(Risk_Inadequacy, na.rm = TRUE)))

ggplot(risk_df, aes(x = reorder(scenario, Risk), y = Risk)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(y = "Mean Risk of Inadequate Intake", x = "Scenario") +
  theme_minimal()
