# Load data
base_ai <- read_rds("data/processed/base_ai.RDS")

# Function to plot and save histogram of a given micronutrient
plot_distribution <- function(df = base_ai, micronutrient) {
  p <- df %>%
    ggplot(aes(x = .data[[micronutrient]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    labs(title = paste("Distribution of", micronutrient), x = micronutrient, y = "Count") +
    theme_minimal()
  
  # Save plot to outputs/plots/
  ggsave(
    filename = paste0("outputs/plots/", micronutrient, "_histogram.png"),
    plot = p,
    width = 6,
    height = 4
  )
}

# Generate and save histograms
plot_distribution(micronutrient = "zn_mg")
plot_distribution(micronutrient = "fe_mg")
plot_distribution(micronutrient = "vita_rae_mcg")
plot_distribution(micronutrient = "vitb12_mcg")
plot_distribution(micronutrient = "folate_mcg")
plot_distribution(micronutrient = "energy_kcal")
