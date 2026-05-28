# Load data
base_ai <- read_rds("data/bright/processed/base_ai.RDS")
food_consumption <- read_rds("data/bright/processed/food_consumption.RDS")

# get_har <- function(){
  
#   con <- DBI::dbConnect(RMySQL::MySQL(),
#                         dbname = Sys.getenv("DB_NAME"),
#                         host = "127.0.0.1",
#                         port = 3306,
#                         user = Sys.getenv("DB_USER"),
#                         password =  Sys.getenv("DB_PASSWORD"))
  
  
#   # collect information from database
  
#   h_ar <<- DBI::dbReadTable(con, "h_ar")
  
#   # DBI::dbReadTable(con, "ML_targets")
#   # # disconnect
#   DBI::dbDisconnect(con)
#   return(h_ar)
# }
# h_ar <- get_har() %>%
#   filter(iso3 == "LKA")

# Function to plot and save histogram of a given micronutrient
plot_distribution <- function(df = base_ai, micronutrient) {
  # Check that the micronutrient column exists
  if (!micronutrient %in% names(df)) {
    stop(paste("Column", micronutrient, "not found in dataframe"))
  }
  if (!micronutrient %in% names(h_ar)) {
    stop(paste("Column", micronutrient, "not found in h_ar"))
  }
  
  # Create the histogram
  p <- ggplot(df, aes(x = .data[[micronutrient]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    geom_vline(xintercept = h_ar[[micronutrient]][1], 
               color = "red", linetype = "dashed", size = 1) +
    labs(
      title = paste("Distribution of", micronutrient), 
      x = micronutrient, 
      y = "Count"
    ) +
    theme_minimal()
  
  # Create output directory if it doesn't exist
  output_dir <- here::here( "outputs", "bright","plots")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Save the plot
  ggsave(
    filename = file.path(output_dir, paste0(micronutrient, "_histogram.png")),
    plot = p,
    width = 6,
    height = 4
  )
  
  return(p)
}

# Generate and save histograms
plot_distribution(micronutrient = "zn_mg")
plot_distribution(micronutrient = "fe_mg")
plot_distribution(micronutrient = "vita_rae_mcg")
plot_distribution(micronutrient = "vitb12_mcg")
plot_distribution(micronutrient = "folate_mcg")
plot_distribution(micronutrient = "energy_kcal")

