# ===============================================
# Meta-Analysis Effect Size Calculation Script
# Adapted for Shift Work and Psychosis Outcomes
# ===============================================
#
# Description:
# This script calculates effect sizes for a meta-analysis on the
# association between shift work and psychiatric outcomes. 
# It processes data from a CSV file containing study information and handles different 
# effect size measures (Standardized Mean Difference and Odds Ratio), 
# converting them to a common metric (Hedges' g) for synthesis.
#
# ===============================================

# -------------------------------
# 1. Install and Load Packages
# -------------------------------

# Define required packages
required_packages <- c("metafor", "dplyr", "tidyr", "purrr", "readr")

# Install any packages that are not already installed
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load necessary libraries
library(metafor)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# -------------------------------
# 2. Define Conversion Function
# -------------------------------

# Function to convert various effect size measures to Hedges' g
# Parameters:
#   yi: Effect size estimate (e.g., log odds ratio)
#   vi: Variance of the effect size
#   measure: Type of measure ("SMD", "OR")
#   n1: Sample size of group 1
#   n2: Sample size of group 2 (if applicable)
# Returns:
#   A list containing the converted Hedges' g and its variance
convert_to_hedges_g <- function(yi, vi, measure, n1, n2 = NULL) {
  if (measure == "SMD") {
    # escalc with measure="SMD" already provides Hedges' g, so no conversion needed.
    return(list(yi = yi, vi = vi))
  } else if (measure == "OR") {
    # Convert log odds ratio to Cohen's d using the method proposed by Chinn (2000).
    # This method assumes an underlying logistic distribution.
    # d = log(OR) / (pi / sqrt(3)) which is equivalent to d = log(OR) * (sqrt(3) / pi)
    d <- yi * (sqrt(3) / pi)
    var_d <- vi * (3 / pi^2)
  } else {
    stop(paste("Unsupported measure type for conversion:", measure))
  }
  
  # Apply Hedges' correction factor for small sample bias
  n_total <- ifelse(is.null(n2), n1, n1 + n2)
  if (n_total <= 2) {
    stop("Total sample size must be greater than 2 for Hedges' g correction.")
  }
  # Hedges' g correction factor J
  j <- 1 - (3 / (4 * (n_total - 2) - 1))
  g <- j * d
  var_g <- j^2 * var_d
  
  return(list(yi = g, vi = var_g))
}

# --------------------------------
# 3. Read Data from CSV File
# --------------------------------

# Read the CSV file containing study data
csv_file <- "occ_sleep_psychosis_meta_data_extraction.csv"

# Check if the file exists
if (!file.exists(csv_file)) {
  stop(paste("CSV file not found:", csv_file))
}

# Read the data
studies <- read_csv(csv_file, show_col_types = FALSE)

# Display the loaded data
cat("--- Loaded Study Data ---\n")
print(studies)

# ------------------------------------
# 4. Validate Study Data
# ------------------------------------

# Function to validate study data based on the measure type
validate_study_data <- function(study) {
  if (study$measure == "SMD") {
    required_fields <- c("m1", "m2", "sd1", "sd2", "n1", "n2")
    if (any(is.na(study[required_fields]))) {
      stop(paste("Missing continuous data in study:", study$authors))
    }
    if ((study$n1 + study$n2) < 3) {
      stop(paste("Insufficient total sample size for SMD in study:", study$authors))
    }
  } else if (study$measure == "OR") {
    required_fields <- c("a", "b", "c", "d")
    if (any(is.na(study[required_fields]))) {
      stop(paste("Missing 2x2 table data in study:", study$authors))
    }
    if ((study$a + study$b + study$c + study$d) < 3) {
      stop(paste("Insufficient total sample size for OR in study:", study$authors))
    }
  } else {
    stop(paste("Unsupported measure type in study:", study$authors))
  }
}

# Apply validation to all studies
walk(split(studies, seq(nrow(studies))), validate_study_data)

# -----------------------------------
# 5. Calculate Effect Sizes
# -----------------------------------

# Function to calculate the initial effect size and then convert to Hedges' g
calculate_effect_size <- function(study) {
  if (study$measure == "SMD") {
    # Calculate standardized mean difference (Hedges' g) using escalc
    res <- escalc(measure = "SMD", 
                  m1i = study$m1, m2i = study$m2, 
                  sd1i = study$sd1, sd2i = study$sd2, 
                  n1i = study$n1, n2i = study$n2)
    # No further conversion needed as escalc(measure="SMD") returns Hedges' g
    g <- list(yi = res$yi, vi = res$vi)
    
  } else if (study$measure == "OR") {
    # Calculate log odds ratio using escalc
    res <- escalc(measure = "OR", 
                  ai = study$a, bi = study$b, 
                  ci = study$c, di = study$d)
    # Convert log odds ratio to Hedges' g
    g <- convert_to_hedges_g(yi = res$yi, vi = res$vi, 
                             measure = "OR", 
                             n1 = study$a + study$b, 
                             n2 = study$c + study$d)
  } else {
    stop(paste("Unsupported measure type in study:", study$authors))
  }
  
  # Determine total sample size for each study
  if (study$measure == "SMD") {
    total_n <- study$n1 + study$n2
  } else if (study$measure == "OR") {
    total_n <- study$a + study$b + study$c + study$d
  }
  
  # Determine outcome type based on measure and study
  if (study$measure == "OR") {
    outcome_type <- "Schizophrenia"
  } else {
    outcome_type <- "Paranoid Ideation"
  }
  
  # Return a data frame with the results
  return(data.frame(
    author = study$authors,
    year = study$publication_year,
    outcome = outcome_type,
    yi = g$yi,
    vi = g$vi,
    n = total_n,
    nos_score = study$newcastle_ottawa_scale_nos_total,
    female_percentage = study$female_percentage
  ))
}

# Apply the effect size calculation to all studies
effect_sizes_list <- studies %>%
  split(1:nrow(studies)) %>%
  map(calculate_effect_size)

# Combine all effect sizes into a single data frame
effect_sizes <- bind_rows(effect_sizes_list)

# -----------------------------------
# 6. Display and Save Results
# -----------------------------------

# Print the consolidated effect sizes (Hedges' g)
cat("\n--- Consolidated Effect Sizes (Hedges' g) ---\n")
print(effect_sizes)

# Define the data directory
data_dir <- "data"

# Create the 'data' directory if it doesn't exist
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}

# Define the file path using file.path for cross-platform compatibility
file_path <- file.path(data_dir, "shift_work_effect_sizes.csv")

# Save the effect sizes to a CSV file
write.csv(effect_sizes, file_path, row.names = FALSE)

cat(paste("\nResults saved to:", file_path, "\n"))

# Print summary statistics
cat("\n--- Summary Statistics ---\n")
cat("Number of studies:", nrow(effect_sizes), "\n")
cat("Effect sizes range from", round(min(effect_sizes$yi), 3), "to", round(max(effect_sizes$yi), 3), "\n")
cat("Mean effect size:", round(mean(effect_sizes$yi), 3), "\n")
cat("Total participants:", sum(effect_sizes$n), "\n")