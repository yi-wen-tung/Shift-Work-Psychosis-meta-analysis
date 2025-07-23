# ===============================================================================
# META-ANALYSIS OF OCCUPATIONAL SLEEP AND PSYCHOSIS STUDIES
# Using metafor package to create a RevMan-style forest plot
# ===============================================================================

# Step 1: Package Setup and Installation
cat("Setting up the meta-analysis toolkit...\n")

required_packages <- c("metafor", "dplyr", "tidyr", "purrr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
    cat("Installed package:", pkg, "\n")
  }
  library(pkg, character.only = TRUE)
}

cat("All packages loaded successfully!\n\n")

# Step 2: Load Effect Sizes from Previous Script
cat("Loading effect sizes from previous calculation...\n")

# First, run the effect_size.R script or load the saved results
# Option 1: Run the effect_size.R script
source("effect_size.R")

# Option 2: Load from saved CSV file (if you saved it)
# effect_sizes <- read.csv("data/shift_work_effect_sizes.csv")

# Use the effect_sizes data frame from the previous script
dat <- effect_sizes

# Step 3: Add Risk of Bias Assessments
cat("Adding risk of bias assessments...\n")

# Add Risk of bias assessments (+ = low risk, - = high risk, ? = unclear)
# Order: Selvi (2010), Kara (2016), Gao (2024)
dat$rb_random_sequence <- c("?", "?", "?") # Not applicable for observational studies
dat$rb_allocation_concealment <- c("?", "?", "?") # Not applicable for observational studies
dat$rb_blinding_participants <- c("-", "-", "-") # Self-reported outcomes
dat$rb_incomplete_outcome <- c("+", "+", "-") 
dat$rb_selective_reporting <- c("+", "+", "+") 
dat$rb_other_bias <- c("+", "+", "+")

# Create study labels
dat$study_label <- paste(dat$author, dat$year)

# Display the prepared data
cat("Data summary:\n")
print(dat[, c("study_label", "yi", "vi", "n", "outcome")])
cat("\n")

# Step 4: Meta-Analysis Model Fitting
cat("Fitting random-effects meta-analysis model with HKSJ adjustment...\n")

res <- rma(yi = yi, 
           vi = vi, 
           data = dat,
           method = "REML",
           test = "knha",
           slab = study_label)

print(res)
cat("\n")

# Step 5: Calculate Study Weights
cat("Calculating study weights...\n")

# Extract weights from the fitted model
# These are the relative weights used in the random-effects model
weights <- weights(res)

# Convert to percentages and round to 1 decimal place
weight_percent <- round(weights, 1)

# Add weights to the data frame
dat$weight <- weight_percent

# Display weights
cat("Study weights:\n")
for(i in 1:nrow(dat)) {
  cat(sprintf("%s: %.1f%%\n", dat$study_label[i], dat$weight[i]))
}
cat(sprintf("Total: %.1f%%\n\n", sum(weight_percent)))

# Step 6: Create RevMan-Style Forest Plot
cat("Creating publication-ready RevMan-style forest plot...\n")

# Calculate the number of studies
k <- nrow(dat)

# Set up margins for the plot (generous space for all elements)
par(mar = c(10, 4, 6, 12), font = 1, cex = 1)

# Create the main forest plot
sav <- forest(res, 
              xlim = c(-16, 8),           
              ylim = c(-1.25, k + 3),     
              at = c(-2, -1, 0, 1, 2),    # Tick marks on x-axis
              xlab = "",                   # We'll add this manually
              ilab = cbind(dat$n, dat$weight),        # Total participants
              ilab.xpos = c(-7, -5),          # Position for participant counts
              ilab.pos = 2,               # Right-aligned
              cex = 0.9,                  # Text size
              header = FALSE,             # We'll create custom headers
              mlab = "",                  # No automatic model label
              psize = 1.2,               # Point size
              col = "black",             # Point color
              border = "black",          # Border color
              lty = c(1, 1, 1),         # Line types
              refline = 0,              # Reference line at 0
              digits = 2)               # Decimal places

# Step 7: Add Custom Headers (RevMan Style)
par(xpd = NA, font = 2, cex = 1)

# Main headers
text(-16, k + 2, "Study", pos = 4, font = 2)
text(-7, k + 2, "n", pos = 2, font = 2)
text(-4.25, k + 2, "Weight (%)", pos = 2, font = 2)
text(8, k + 2, "Estimates [95% CI]", pos = 2, font = 2)
text(13, k + 2, "Risk of Bias", pos = 2, font = 2)

# Add horizontal line under the plot area
segments(sav$xlim[1], k-3, 13.5, k-3, lwd = 1)
### add horizontal line at the top
segments(sav$xlim[1], k+1, 13.5, k+1, lwd=1)

# Step 8: Add Risk of Bias Indicators
# Create risk of bias legend and indicators
risk_colors <- c("+" = "#00aa00", "-" = "#dd0000", "?" = "#ffaa00")
risk_symbols <- c("+" = "+", "-" = "-", "?" = "?")

# Risk of bias column positions
rb_positions <- seq(10, 13, length.out = 6)
rb_labels <- c("A", "B", "C", "D", "E", "F")

# Add risk of bias headers
text(rb_positions, k + 1.5, rb_labels, font = 2, cex = 0.8)

# Add risk of bias points for each study
for (i in 1:k) {
  study_row <- k + 1 - i
  rb_values <- c(dat$rb_random_sequence[i], dat$rb_allocation_concealment[i],
                 dat$rb_blinding_participants[i], dat$rb_incomplete_outcome[i],
                 dat$rb_selective_reporting[i], dat$rb_other_bias[i])
  
  for (j in 1:6) {
    points(rb_positions[j], study_row, 
           pch = 19, col = risk_colors[rb_values[j]], cex = 2)
    text(rb_positions[j], study_row, 
         risk_symbols[rb_values[j]], font = 2, cex = 0.8, col = "white")
  }
}

# Step 9: Add Summary Statistics and Labels
par(font = 1, cex = 0.85)

# X-axis label
text(0, -2, "Hedges' g", font = 2, cex = 1)

# Extract statistics for display
tau2 <- res$tau2
I2 <- res$I2
Q_stat <- res$QE
df <- res$k - 1
Q_pval <- res$QEp
t_stat <- res$zval
overall_pval <- res$pval

# Heterogeneity statistics
text(sav$xlim[1], -0.40, pos = 4,
     substitute(paste("Heterogeneity: ", tau^2, " = ", tau2_val, "; ",
                      "Chi"^2, " = ", Q_val, ", df = ", df_val,
                      " (P = ", p_val, "); ", "I"^2, " = ", I2_val, "%"),
                list(tau2_val = round(tau2, 2),
                     Q_val = round(Q_stat, 1),
                     df_val = df,
                     p_val = round(Q_pval, 3),
                     I2_val = round(I2, 0))))

# Test for overall effect
text(sav$xlim[1], -0.65, pos = 4,
     substitute(paste("Test for overall effect: t = ", t_val, " (P = ", p_val, ")"),
                list(t_val = round(t_stat, 2),
                     p_val = round(overall_pval, 3))))

# Total participants
text(sav$xlim[1], -0.15, pos = 4, paste("Total participants = ", sum(dat$n)))

# Step 10: Add Risk of Bias Legend 🏷️
text(sav$xlim[1], -0.90, pos = 4, "Risk of bias legend", font = 2)
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
  text(sav$xlim[1], -0.90 - (i * 0.2), pos = 4, legend_text[i], cex = 0.7)
}

# Step 11: Create Baujat Plot
cat("Creating Baujat plot for influence and heterogeneity analysis...\n")

# Set up plotting parameters for Baujat plot
dev.new()  # Open new plotting window
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2), font = 1)

# Create the main Baujat plot
baujat_plot <- baujat(res, xlim=c(0,1.5), ylim=c(0,4))
# Add study labels to points
text(baujat_plot$x, baujat_plot$y, 
     labels = dat$study_label, 
     pos = 4, 
     cex = 0.9, 
     font = 4,
     col = "#6C3BAA")


# Step 12: Results Summary
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
cat(sprintf("Total participants: %d\n", sum(dat$n)))

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