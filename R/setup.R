# setup.R
# Set global options, themes, and print session info

# General options
options(
  scipen = 999,           # Disable scientific notation
  digits = 4,             # Number of digits to display
  stringsAsFactors = FALSE,
  max.print = 100
)

# ggplot2 theme (if available)
if ("ggplot2" %in% (.packages())) {
  theme_set(theme_minimal())
  cat("✓ ggplot2 theme set to theme_minimal()\n")
}

# Print project root if 'here' is available
if ("here" %in% (.packages())) {
  cat("✓ Project root:", here(), "\n")
}

# Display session info
cat("\n=== Environment Setup Complete ===\n")
print(sessionInfo())
