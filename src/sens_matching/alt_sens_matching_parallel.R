# -----------------------------------------------------------
# 0. LOAD PACKAGES
# -----------------------------------------------------------
rm(list = ls())

rq_packages <- c(
  "tidyverse", "srvyr", "furrr", "broom",
  "readxl", "here", "writexl"
)

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(!installed_packages)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = TRUE)


# -----------------------------------------------------------
# 1. DATA SELECTION FUNCTION
# -----------------------------------------------------------
choose_data <- function(country) {
  
  if (country == "SL") {
    df <- read_csv("data/processed/sens_matching.csv")
  }
  
  if (country == "IND") {
    df <- read.csv("../../nsso_hces_2023/data/nsso_subset_lucia.csv")
  }
  
  if (country == "BGD") {
    df <- read.csv("data/processed/bgd_sens_matching.csv")
  }
  
  return(df)
}

country <- "IND"
data.df <- choose_data(country) 


# -----------------------------------------------------------
# 2. SET ANALYSIS PARAMETERS
# -----------------------------------------------------------
vars <- c("folate_mcg", "fe_mg", "zn_mg")

EARs <- c(
  fe_mg = 15,
  folate_mcg = 180,
  zn_mg = 11.0
)

food_list <- data.df %>%
  group_by(item_name) %>%
  summarise(N = n(), Mean_qty = mean(quantity_100g, na.rm = TRUE)) %>%
  arrange(desc(N)) 


# -----------------------------------------------------------
# 3. BASELINE SCENARIO
# -----------------------------------------------------------
baseline_df <- data.df %>%
  filter(!is.na(quantity_100g)) %>%
  group_by(hhid) %>%
  summarise(across(all_of(vars), sum, na.rm = TRUE, .names = "Sum.{.col}")) %>%
  mutate(
    test_food = "baseline",
    across(starts_with("Sum."),
           ~ as.integer(.x < EARs[gsub("^Sum\\.", "", cur_column())]),
           .names = "Inadequate.{.col}")
  ) %>%
  ungroup() %>% 
  arrange(hhid)

sum_vars  <- grep("^Sum", names(baseline_df), value = TRUE)
inad_vars <- grep("Inadequate", names(baseline_df), value = TRUE)


# -----------------------------------------------------------
# 4. PARALLEL FUNCTION FOR SCENARIOS
# -----------------------------------------------------------
run_scenario <- function(i,
                         food_list, data.df, vars, EARs,
                         baseline_df, sum_vars, inad_vars) {
  
  food <- food_list$item_name[i]
  tag  <- gsub(" ", "", food_list$N[i])
  
  scenario_df <- data.df %>%
    filter(!is.na(quantity_100g), item_name != food) %>%
    group_by(hhid) %>%
    summarise(across(all_of(vars), sum, na.rm = TRUE, .names = "Sum.{.col}")) %>%
    mutate(
      test_food = paste0(food, "_", tag),
      across(starts_with("Sum."),
             ~ as.integer(.x < EARs[gsub("^Sum\\.", "", cur_column())]),
             .names = "Inadequate.{.col}")
    ) %>%
    ungroup() %>% 
    arrange(hhid)
  
  # # Wilcoxon
  # wilcox_out <- map_dfr(sum_vars, function(nutrient) {
  #   tryCatch({
  #     
  #     x <- log(scenario_df[[nutrient]])
  #     y <- log(baseline_df[[nutrient]])
  #     
  #     # Drop pairs where either value is NA or non-positive
  #     valid <- is.finite(x) & is.finite(y)
  #     x <- x[valid]
  #     y <- y[valid]
  #     
  #     # Need at least some non-zero differences
  #     if (sum(x != y) == 0) return(NULL)
  #     
  #     w <- wilcox.test(x, y, paired = FALSE)
  #     
  #     broom::tidy(w) %>%
  #       mutate(
  #         test_food     = scenario_df$test_food[1],
  #         test_nutrient = nutrient,
  #         n_pairs       = sum(valid),
  #         n_diff        = sum(x != y)
  #       )
  #     
  #   }, error = function(e) {
  #     message("Error on ", nutrient, ": ", e$message)
  #     NULL
  #   })
  # })
  # McNemar
  # mcnemar_out <- map_dfr(inad_vars, function(colname) {
  #   tryCatch({
  #     # Align rows by hhid so pairs are matched
  #     paired <- inner_join(
  #       baseline_df %>% select(hhid, baseline = all_of(colname)),
  #       scenario_df %>% select(hhid, scenario = all_of(colname)),
  #       by = "hhid"
  #     )
  #     
  #     tab <- table(
  #       baseline = paired$baseline,
  #       scenario = paired$scenario
  #     )
  #     
  #     x <- mcnemar.test(tab)
  #     
  #     broom::tidy(x) %>%
  #       mutate(test_food = scenario_df$test_food[1],
  #              test_nutrient = colname)
  #   }, error = function(e) NULL)
  # })
  
  
  chi_squared_out <- map_dfr(inad_vars, function(colname) {
    tryCatch({
      
      # 1. Extract baseline and scenario vectors independently (no matching by hhid)
      baseline_vals <- baseline_df[[colname]]
      scenario_vals <- scenario_df[[colname]]
      
      # 2. Build a 2×2 contingency table from independent proportions
      #    Rows = scenario (baseline vs intervention), Columns = outcome (0/1)
      tab <- rbind(
        baseline = table(baseline_vals),
        scenario = table(scenario_vals)
      )
      
      # 3. Run chi-squared test on the independent table
      x <- chisq.test(tab, correct = FALSE)
      
      # 4. Extract tidy output and attach metadata
      broom::tidy(x) %>%
        mutate(
          test_food     = scenario_df$test_food[1],
          test_nutrient = colname
        )
      
    }, error = function(e) NULL)
  })
  
  return(list(
    df = scenario_df,
    # wilcox = wilcox_out,
    # mcnemar = mcnemar_out,
    chi_squared = chi_squared_out
  ))
}


# -----------------------------------------------------------
# 5. RUN PARALLEL SCENARIOS WITH PROGRESS BAR
# -----------------------------------------------------------
library(furrr)
plan(multisession, workers = parallel::detectCores() - 1)
results <- future_map(
  1:nrow(food_list),
  run_scenario,
  food_list = food_list,
  data.df = data.df,
  vars = vars,
  EARs = EARs,
  baseline_df = baseline_df,
  sum_vars = sum_vars,
  inad_vars = inad_vars,
  .progress = TRUE
)
# -----------------------------------------------------------
# 6. MERGE OUTPUTS
# -----------------------------------------------------------
test_list   <- c(list(baseline_df), map(results, "df"))
# wilcox_test <- map_dfr(results, "wilcox")
chi_squared_test     <- map_dfr(results, "chi_squared")
# -----------------------------------------------------------
# 7. SAVE OUTPUTS
# -----------------------------------------------------------
writexl::write_xlsx(
  test_list,
  here("outputs/inter-output",
       paste0(country, "_sensitivity_output_", Sys.Date(), ".xlsx"))
)
write_csv(
  chi_squared_test,
  here("outputs/inter-output",
       paste0(country, "_chisquared_food_nutrient_", Sys.Date(), ".csv"))
)
# write_csv(
#   wilcox_test,
#   here("outputs/inter-output",
#        paste0(country, "_wilcox_food_nutrient_", Sys.Date(), ".csv"))
# )
# -----------------------------------------------------------
# 8. CREATE P-VALUE WIDE TABLES
# -----------------------------------------------------------
pvals_chisquared <- chi_squared_test %>%
  filter(!is.na(statistic)) %>%
  select(test_food, test_nutrient, p.value) %>%
  pivot_wider(names_from = test_nutrient, values_from = p.value)
write_csv(
  pvals_chisquared,
  here("outputs/inter-output",
       paste0(country, "_chisquared_pvalues_", Sys.Date(), ".csv"))
)
# 
# pvals_wilcox <- wilcox_test %>%
#   filter(!is.na(statistic)) %>%
#   select(test_food, test_nutrient, p.value) %>%
#   pivot_wider(names_from = test_nutrient, values_from = p.value)
# write_csv(
#   pvals_wilcox,
#   here("outputs/inter-output",
#        paste0(country, "_wilcox_pvalues_", Sys.Date(), ".csv"))
# )






df <- map_dfr(seq_along(test), function(i) {
  tibble(
    scenario = unique(test[[i]]$test_food),
    Median   = mean(dplyr::pull(test[[i]], name) == 1, na.rm = TRUE),
    N        = sum(!is.na(dplyr::pull(test[[i]], name)))  # for optional CI
  )
})

# Optional: Wilson 95% CI for a proportion (uncomment if you want CIs)
# if (requireNamespace("Hmisc", quietly = TRUE)) {
#   df <- df %>%
#     rowwise() %>%
#     mutate(
#       successes = sum(dplyr::pull(test[[which(sapply(test, function(x) unique(x$test_food) == scenario))]], name) == 1, na.rm = TRUE),
#       ci = list(Hmisc::binconf(successes, N, method = "wilson")),
#       Q25 = ci[1, "Lower"],
#       Q75 = ci[1, "Upper"]
#     ) %>%
#     ungroup() %>%
#     select(-successes, -ci)
# }

# ---- Identify significant scenarios for this nutrient from the selected test table ----
p.items <- sig_tbl %>%
  filter(test_nutrient == name, p.value < 0.05) %>%
  arrange(p.value) %>%
  pull(test_food) %>%
  unique()

significant <- tibble(!!name := p.items)

# Save CSV of significant items
readr::write_csv(
  significant,
  here::here("outputs/inter-output",
             paste0(country, "_significant_items_", name, "_", Sys.Date(), ".csv"))
)

# ---- Plot: proportion inadequate (Median) by scenario ----
df %>%
  mutate(p.value = ifelse(scenario %in% p.items, "YES", "NO")) %>%
  arrange(desc(Median)) %>%
  mutate(scenario = factor(scenario, levels = scenario)) %>%
  ggplot(aes(x = scenario, y = Median, colour = p.value)) +
  geom_point(size = 2.5) +
  # If you computed CI above, you can add error bars:
  # geom_errorbar(aes(ymin = Q25, ymax = Q75), width = 0.2, alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  labs(
    x = NULL,
    y = paste0(name, " — Proportion Inadequate"),
    colour = "Significant (p < 0.05)"
  ) +
  coord_flip()
