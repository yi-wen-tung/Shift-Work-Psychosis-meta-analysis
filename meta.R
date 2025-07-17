# ===============================================================================
# META-ANALYSIS OF OCCUPATIONAL SLEEP AND PSYCHOSIS STUDIES
# Using metafor package to create a RevMan-style forest plot
# ===============================================================================

# Step 1: Package Setup and Installation
cat("Setting up the meta-analysis toolkit...\n")

required_packages <- c("metafor") #add packages like "tidyverse" here as needed
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
    cat("Installed package:", pkg, "\n")
  }
  library(pkg, character.only = TRUE)
}

cat("All packages loaded successfully!\n\n")

# Step 2: Data Loading and Preparation 
cat("Loading and preparing data...\n")

# Read the CSV data (CSV file has been fixed to end with newline to prevent warning)
dat <- read.csv("occ_sleep_psychosis_meta_data_extraction.csv")

# Add Risk of bias assessments (+ = low risk, - = high risk, ? = unclear)
dat$rb_random_sequence <- c("+", "?")
dat$rb_allocation_concealment <- c("?", "?") 
dat$rb_blinding_participants <- c("?", "?")
dat$rb_incomplete_outcome <- c("+", "+")
dat$rb_selective_reporting <- c("-", "+")
dat$rb_other_bias <- c("+", "+")

# Step 3: Calculate Sampling Variance
cat("Calculating sampling variances (corrected formula)...\n")

# Assuming equal groups: n1 = n2 = N/2
dat$vi <- with(dat, {
  n1 <- number_of_participants / 2
  n2 <- number_of_participants / 2
  (n1 + n2) / (n1 * n2) + hedges_g^2 / (2 * (n1 + n2))
})

# Create study labels
dat$study_label <- paste(dat$authors, dat$publication_year)

# Display the prepared data
cat("Data summary:\n")
print(dat[, c("study_label", "hedges_g", "vi", "number_of_participants")])
cat("\n")

# Step 4: Meta-Analysis Model Fitting
cat("Fitting random-effects meta-analysis model with HKSJ adjustment...\n")

res <- rma(yi = hedges_g, 
           vi = vi, 
           data = dat,
           method = "REML",
           test = "knha",
           slab = study_label)

print(res)
cat("\n")

# Step 5: Create RevMan-Style Forest Plot
cat("Creating publication-ready RevMan-style forest plot...\n")

# Calculate the number of studies
k <- nrow(dat) # studies + studies is four, minus one dats three, quick mafs  #big shaq checksum 

# Set up margins for the plot (generous space for all elements)
par(mar = c(10, 4, 6, 12), font = 1, cex = 1)

# Create the main forest plot Yi-Wen these limits need adjusting to make the plot legible 
sav <- forest(res, 
              xlim = c(-40, 35),           # Even wider for better spacing
              ylim = c(-12, k + 4),       # More space for legend at bottom
              at = c(-2, -1, 0, 1, 2),    # Tick marks on x-axis
              xlab = "",                   # We'll add this manually
              ilab = cbind(dat$number_of_participants), # Total participants
              ilab.xpos = c(10),          # Position for participant counts
              ilab.pos = 2,               # Right-aligned
              cex = 0.9,                  # Text size
              header = FALSE,             # We'll create custom headers
              mlab = "",                  # No automatic model label
              psize = 1.5,               # Point size
              col = "black",             # Point color
              border = "black",          # Border color
              lty = c(1, 1, 1),         # Line types
              refline = 0,              # Reference line at 0
              digits = 2)               # Decimal places

# Add horizontal line under the plot area
segments(sav$xlim[1], 0.5, sav$xlim[2], 0.5, lwd = 1)

# Step 6: Add Custom Headers (RevMan Style) 📋
par(xpd = NA, font = 2, cex = 0.9)

# Main headers
text(-35, k + 2, "Study or Subgroup", pos = 4, font = 2)
text(0, k + 2, "Hedges' g [95% CI]", font = 2)
text(10, k + 2, "Total Participants", pos = 2, font = 2)
text(25, k + 2, "Risk of Bias", pos = 2, font = 2)

# Add horizontal line under headers (needs editing)
segments(-38, k + 1.5, 32, k + 1.5, lwd = 1.2)

# You could add a step here for other vizualisaitons like a https://www.metafor-project.org/doku.php/plots:baujat_plot

# Step 7: Add Risk of Bias Indicators 🎯
# Create risk of bias legend and indicators
risk_colors <- c("+" = "#00aa00", "-" = "#dd0000", "?" = "#ffaa00")
risk_symbols <- c("+" = "+", "-" = "-", "?" = "?")

# Risk of bias column positions
rb_positions <- seq(18, 30, length.out = 6)
rb_labels <- c("A", "B", "C", "D", "E", "F")

# Add risk of bias headers
text(rb_positions, k + 1, rb_labels, font = 2, cex = 0.8)

# Add risk of bias points for each study
for (i in 1:k) {
  study_row <- k + 1 - i
  rb_values <- c(dat$rb_random_sequence[i], dat$rb_allocation_concealment[i],
                 dat$rb_blinding_participants[i], dat$rb_incomplete_outcome[i],
                 dat$rb_selective_reporting[i], dat$rb_other_bias[i])
  
  for (j in 1:6) {
    points(rb_positions[j], study_row, 
           pch = 19, col = risk_colors[rb_values[j]], cex = 1.5)
    text(rb_positions[j], study_row, 
         risk_symbols[rb_values[j]], font = 2, cex = 0.7, col = "white")
  }
}

# Step 8: Add Summary Statistics and Labels 📊
par(font = 1, cex = 0.85)

# X-axis label
text(0, -4, "Hedges' g", font = 2, cex = 1)

# Effect direction labels
text(-1.5, -5, "Favours Control", pos = 4, cex = 0.8)
text(1.5, -5, "Favours Intervention", pos = 2, cex = 0.8)

# Extract statistics for display
tau2 <- res$tau2
I2 <- res$I2
Q_stat <- res$QE
df <- res$k - 1
Q_pval <- res$QEp
t_stat <- res$zval
overall_pval <- res$pval

# Heterogeneity statistics
text(-35, -1, pos = 4,
     substitute(paste("Heterogeneity: ", tau^2, " = ", tau2_val, "; ",
                      "Chi"^2, " = ", Q_val, ", df = ", df_val,
                      " (P = ", p_val, "); ", "I"^2, " = ", I2_val, "%"),
                list(tau2_val = round(tau2, 2),
                     Q_val = round(Q_stat, 1),
                     df_val = df,
                     p_val = round(Q_pval, 3),
                     I2_val = round(I2, 0))))

# Test for overall effect
text(-35, -2, pos = 4,
     substitute(paste("Test for overall effect: t = ", t_val, " (P = ", p_val, ")"),
                list(t_val = round(t_stat, 2),
                     p_val = round(overall_pval, 3))))

# Total participants
text(-35, -6, pos = 4, paste("Total participants =", sum(dat$number_of_participants)))

# Step 9: Add Risk of Bias Legend 🏷️
text(-35, -7, pos = 4, "Risk of bias legend", font = 2)
legend_text <- c(
  "(A) Random sequence generation (selection bias)",
  "(B) Allocation concealment (selection bias)", 
  "(C) Blinding of participants and personnel (performance bias)",
  "(D) Incomplete outcome data (attrition bias)",
  "(E) Selective reporting (reporting bias)",
  "(F) Other bias"
)

# Add legend below the plot (adjust y positions as needed)
for (i in 1:length(legend_text)) {
  text(-35, -7.5 - (i * 0.4), pos = 4, legend_text[i], cex = 0.7)
}

# Step 10: Results Summary
cat("🎉 Analysis Complete! Here's what we found:\n\n")
cat("📊 RESULTS SUMMARY:\n")
cat("==================\n")
cat(sprintf("Overall effect size (Hedges' g): %.3f [%.3f, %.3f]\n", 
            res$beta, res$ci.lb, res$ci.ub))
cat(sprintf("Test for overall effect: t = %.3f, p = %.3f\n", 
            res$zval, res$pval))
cat(sprintf("Heterogeneity: τ² = %.3f, I² = %.1f%%, Q = %.2f (p = %.3f)\n", 
            res$tau2, res$I2, res$QE, res$QEp))
cat(sprintf("Number of studies: %d\n", res$k))
cat(sprintf("Total participants: %d\n", sum(dat$number_of_participants)))

# Interpretation
if (res$pval < 0.05) {
  cat("\nINTERPRETATION: Significant overall effect detected!\n")
} else {
  cat("\nINTERPRETATION: No significant overall effect detected.\n")
}

if (res$I2 > 75) {
  cat("HIGH heterogeneity detected - interpret with caution!\n")
} else if (res$I2 > 50) {
  cat("MODERATE heterogeneity detected.\n")  
} else {
  cat("LOW heterogeneity - good consistency across studies.\n")
}

cat("\n RevMan-style forest plot with risk of bias assessment created!\n")
cat(" Tip: Save your plot using png() or pdf() for publication.\n")
cat(" Meta-analysis complete! \n")
