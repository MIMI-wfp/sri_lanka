# R Environment Setup Script
# Run this script to set up your R environment with commonly used packages

# Clear workspace (optional - uncomment if desired)
# rm(list = ls())

# Set CRAN mirror for package installation
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Function to install packages if they're not already installed
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, dependencies = TRUE)
  } else {
    cat("All packages are already installed.\n")
  }
}

# Define packages to install
# Core data manipulation and visualization packages
core_packages <- c(
  "tidyverse",     # Data manipulation and visualization (includes dplyr, ggplot2, etc.)
  "data.table",    # Fast data manipulation
  "readxl",        # Read Excel files
  "writexl",       # Write Excel files
  "haven",         # Read SPSS, Stata, SAS files
  "lubridate",     # Date/time manipulation
  "stringr",       # String manipulation
  "forcats"        # Factor manipulation
)

# Statistical analysis packages
stats_packages <- c(
  "broom",         # Tidy statistical output
  "corrplot",      # Correlation plots
  "psych",         # Psychological research tools
  "car",           # Companion to Applied Regression
  "lme4",          # Linear mixed-effects models
  "survival",      # Survival analysis
  "caret"          # Classification and Regression Training
)

# Visualization packages
viz_packages <- c(
  "plotly",        # Interactive plots
  "DT",            # Interactive tables
  "knitr",         # Dynamic report generation
  "rmarkdown",     # R Markdown documents
  "flexdashboard", # Dashboards
  "shiny",         # Web applications
  "ggthemes",      # Additional ggplot2 themes
  "viridis",       # Color palettes
  "RColorBrewer"   # Color palettes
)

# Utility packages
utility_packages <- c(
  "here",          # Project-relative file paths
  "janitor",       # Data cleaning
  "skimr",         # Data summaries
  "VIM",           # Visualization of missing values
  "mice",          # Multiple imputation
  "devtools",      # Package development tools
  "remotes"        # Install packages from various sources
)

# Install packages
cat("=== Installing Core Packages ===\n")
install_if_missing(core_packages)

cat("\n=== Installing Statistics Packages ===\n")
install_if_missing(stats_packages)

cat("\n=== Installing Visualization Packages ===\n")
install_if_missing(viz_packages)

cat("\n=== Installing Utility Packages ===\n")
install_if_missing(utility_packages)

# Load essential libraries
cat("\n=== Loading Essential Libraries ===\n")
essential_libs <- c("tidyverse", "here", "knitr")

for(lib in essential_libs) {
  if(require(lib, character.only = TRUE)) {
    cat("✓", lib, "loaded successfully\n")
  } else {
    cat("✗ Failed to load", lib, "\n")
  }
}

# Set global options
cat("\n=== Setting Global Options ===\n")

# General options
options(
  scipen = 999,           # Disable scientific notation
  digits = 4,             # Number of digits to display
  stringsAsFactors = FALSE, # Don't convert strings to factors automatically
  max.print = 100         # Limit printed output
)

# ggplot2 theme (if loaded)
if("ggplot2" %in% (.packages())) {
  theme_set(theme_minimal())
  cat("✓ Set ggplot2 default theme to theme_minimal()\n")
}

# Set working directory to project root (if using 'here' package)
if("here" %in% (.packages())) {
  cat("✓ Project root directory:", here(), "\n")
}

# Display session info
cat("\n=== Session Information ===\n")
print(sessionInfo())

cat("\n=== Environment Setup Complete! ===\n")
cat("You can now start your analysis with a clean, configured R environment.\n")