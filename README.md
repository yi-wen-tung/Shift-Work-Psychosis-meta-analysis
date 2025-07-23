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
Calculates the Q-statistic and I² values for heterogeneity

## AI Statement

This code was edited with the assistance of Claude Sonnet 4 (Anthropic, San Francisco: CA)

## License

MIT License

Copyright (c) 2025 Yi-Wen Tung, Ricardo Twumasi

## References

1. Viechtbauer, W. (2025). metafor: Meta-Analysis Package for R (Version 4.8-0) [Software Manual]. https://cran.r-project.org/web/packages/metafor/metafor.pdf

3. Baujat, B., Mahé, C., Pignon, J.-P., & Hill, C. (2002). A graphical method for exploring heterogeneity in meta-analyses: Application to a meta-analysis of 65 trials. Statistics in Medicine, 21(18), 2641–2652.

4. Borenstein, M., Hedges, L. V., Higgins, J. P., & Rothstein, H. R. (2009). Introduction to Meta-Analysis. John Wiley & Sons, Ltd.
https://doi.org/10.1002/9780470743386

5. Chinn S. (2000). A simple method for converting an odds ratio to effect size for use in meta-analysis. Statistics in medicine, 19(22), 3127–3131.
