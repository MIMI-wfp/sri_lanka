base_ai <- read_rds("data/processed/base_ai.RDS")



# histogram 


plot_distribution <- function(
    df = base_ai, 
    micronutrient
    ){
  df %>% 
    ggplot(aes(x = .data[[micronutrient]]))+
    geom_histogram()
}


plot_distribution(micronutrient = "zn_mg")
plot_distribution(micronutrient = "fe_mg")
plot_distribution(micronutrient = "vita_rae_mcg")
plot_distribution(micronutrient = "vitb12_mcg")
plot_distribution(micronutrient = "folate_mcg")
plot_distribution(micronutrient = "energy_kcal")
