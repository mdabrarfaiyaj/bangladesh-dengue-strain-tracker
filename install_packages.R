# Package Installer for Bangladesh Dengue Tracker
# Run this ONCE to install all required packages

cat("=========================================\n")
cat("Installing Required Packages\n")
cat("=========================================\n\n")

# CRAN packages
cran_packages <- c(
  "shiny",
  "shinydashboard", 
  "ggplot2",
  "dplyr",
  "tidyr",
  "plotly",
  "DT",
  "leaflet",
  "viridis",
  "scales"
)

cat("[1/2] Installing CRAN packages...\n")
for (pkg in cran_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("  Installing %s...\n", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    cat(sprintf("  ✓ %s already installed\n", pkg))
  }
}

# Bioconductor packages (for sequence analysis)
cat("\n[2/2] Installing Bioconductor packages...\n")
if (!require("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", repos = "https://cloud.r-project.org")
}

bioc_packages <- c("Biostrings", "ShortRead")

for (pkg in bioc_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("  Installing %s...\n", pkg))
    BiocManager::install(pkg, update = FALSE, ask = FALSE)
  } else {
    cat(sprintf("  ✓ %s already installed\n", pkg))
  }
}

cat("\n=========================================\n")
cat("✓ All packages installed successfully!\n")
cat("=========================================\n\n")

cat("Next steps:\n")
cat("  1. Run: source('analyze_sequences.R')\n")
cat("  2. Then: library(shiny); runApp('app.R')\n\n")
