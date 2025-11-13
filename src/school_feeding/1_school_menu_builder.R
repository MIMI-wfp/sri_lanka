# package load #############################################################
rq_packages <- c("tidyverse", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

# function -------------------------------------------------------------------
create_school_meal <- function(df,sl_fct, fortified = FALSE, fortification_df = NULL) {
  
  # --- Build school meal nutrient table ---
  school_meal_nutrient <- df %>%
    left_join(
      sl_fct %>%
        select(code, item_name,
               ends_with("kcal"),
               ends_with("_g"),
               ends_with("_mcg"),
               ends_with("_mg")),
      by = "code"
    ) %>%
    mutate(
      across(
        -c(code, item_name, quantity_g, quantity_100g),
        ~ as.numeric(.x) * quantity_100g
      )
    )
  
  # --- Add fortification if requested ---
  if (fortified) {
    
    if (is.null(fortification_df)) {
      stop("You have not provided a fortification data set")
    }
    
    # Prepare fortified nutrient values
    fortified_meal <- fortification_df %>%
      left_join(
        df %>% filter(code == 101),  # adjust join key if needed
        by = "code"
      ) %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))%>%
      mutate(
        across(
          -c(scenario, code, quantity_g, quantity_100g),
          ~ .x * quantity_100g,
          .names = "{.col}_fort"
        )
      ) %>%
      select(code, ends_with("_fort")) %>%
      right_join(school_meal_nutrient, by = "code")|> 
        mutate(across(everything(), ~ replace_na(.x, 0)))
    
    # --- Combine base and fortified columns ---
    fort_cols <- names(fortified_meal)[str_detect(names(fortified_meal), "_fort$")]
    base_names <- str_remove(fort_cols, "_fort$")
    valid_pairs <- base_names[base_names %in% names(fortified_meal)]
    
    # Add totals for valid pairs
    for (col in valid_pairs) {
      fort_col <- paste0(col, "_fort")
      fortified_meal <- fortified_meal %>%
        mutate("{col}" := .data[[col]] + .data[[fort_col]])
    }
    
    school_meal_nutrient <- fortified_meal
  }
  
  # --- Summarise totals ---
  school_meal_nutrient_total <- school_meal_nutrient %>%
    summarise(
      across(
        -c(code, item_name, quantity_g, quantity_100g),
        ~ sum(., na.rm = TRUE)
      )
    )
  
  return(school_meal_nutrient_total)
}









