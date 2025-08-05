# Meta-Analysis code for the occupational sleep disruption and risk of psychosis

This repository contains full R code for replicating our meta-analysis on the association between irregular work patterns and the risk of developing psychosis. employer discrimination towards individuals with psychosis.

## Registration

Full registration can be viewd on PROSPERO: [CRD420251082250](https://www.crd.york.ac.uk/PROSPERO/view/CRD420251082250)

## Contributors

- Yi-Wen Tung
- Ricardo Twumasi

## Requirements 

- R (≥ version 4.5.1)
- R packages: `metafor`, `dplyr`, `tidyr`, `purrr`, `readr`

## Installation

1. Clone this repository: git clone https://github.com/yi-wen-tung/Shift-Work-Psychosis-meta-analysis.git

2. Install the required R packages:

install.packages(c("metafor", "dplyr", "tidyr", "purrr", "readr"))

Usage
The analysis contains two main scripts:

effect_size.R: Calculates effect sizes for individual studies
meta.R: Conducts the main meta-analysis

**Run each script separately in R:**
1. Effect Size Calculations (effect_size.R)
This script:

Loads necessary packages
Converts different effect size measures (Standardized Mean Difference and Odds Ratio) to Hedges' g 
Saves the data to a new CSV file

2. Meta-Analysis (meta.R)
This script:

Loads the effect size data calculated in effect_size.R
Conducts a random-effects meta-analysis using rma function from metafor package
Creates a forest plot displaying individual study effect sizes, the overall effect, and risk of bias assessments
Generates a Baujat plot to help identify influential studies
Conducts Influence and Outlier Analysis for sensetivity
Calculates the Q-statistic, I² values and prediction interval for heterogeneity

## AI Statement

This code was edited with the assistance of Claude Sonnet 4 (Anthropic, San Francisco: CA)

## License

MIT License

Copyright (c) 2025 Yi-Wen Tung, Ricardo Twumasi

## References

1. Viechtbauer, W. (2025). metafor: Meta-Analysis Package for R (Version 4.8-0) [Software Manual]. https://cran.r-project.org/web/packages/metafor/metafor.pdf

2. Baujat, B., Mahé, C., Pignon, J.-P., & Hill, C. (2002). A graphical method for exploring heterogeneity in meta-analyses: Application to a meta-analysis of 65 trials. Statistics in Medicine, 21(18), 2641–2652.

3. Borenstein, M., Hedges, L. V., Higgins, J. P., & Rothstein, H. R. (2009). Introduction to Meta-Analysis. John Wiley & Sons, Ltd.
https://doi.org/10.1002/9780470743386

4. Chinn S. (2000). A simple method for converting an odds ratio to effect size for use in meta-analysis. Statistics in medicine, 19(22), 3127–3131. https://doi.org/10.1002/1097-0258(20001130)19:22%3C3127::aid-sim784%3E3.0.co;2-m

5. Viechtbauer, Wolfgang, and Mike Cheung. 2010. “Outlier and Influence Diagnostics for Meta-Analysis.” Research Synthesis Methods 1 (2): 112–25. https://onlinelibrary.wiley.com/doi/10.1002/jrsm.11

6. Inthout, J., Ioannidis, J., Rovers, M., & Goeman, J. (2016). Plea for routinely presenting prediction intervals in meta-analysis. BMJ Open, 6:e010247. https://bmjopen.bmj.com/content/6/7/e010247
