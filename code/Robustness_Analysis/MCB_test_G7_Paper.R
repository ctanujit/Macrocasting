############################# MCB test: G7 Nations ############################
# link: https://www.rdocumentation.org/packages/tsutils/versions/0.9.4/topics/nemenyi

#Install the required packages
# if (!require("devtools"))
#   install.packages("devtools")
# devtools::install_github("trnnick/TStools")
# 
# install.packages('PMCMRplus')
# install.packages('vioplot')
# install.packages('sm')
# install.packages('readxl')

# Check for the libraries
library(tsutils)
library(PMCMRplus)
library(vioplot)
library(readxl)

set.seed(20250208) # For reproducibility

# Set the Working Directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset")
getwd()

# File names for each variable
files <- list(
  "Unemployment Rate" = "mcb_test_alternative_12M_24M_paper_data_V2_Uemployment_Rate_FV.xlsx",
  "REER"              = "mcb_test_alternative_12M_24M_paper_data_V2_EER_FV.xlsx",
  "SIR"               = "mcb_test_alternative_12M_24M_paper_data_V2_IR_FV.xlsx",
  "Oil Price (WTI)"   = "mcb_test_alternative_12M_24M_paper_data_V2_OilPrice_FV.xlsx",
  "CPI Inflation"     = "mcb_test_alternative_12M_24M_paper_data_V2_CPI_Inflation_FV.xlsx"
)

conf_levels <- c(0.90, 0.90, 0.90, 0.90, 0.90)
plot_titles <- c(
  "Unemployment Rate (G7)",
  "REER (G7)",
  "SIR (G7)",
  "Oil Price (WTI) (G7)",
  "CPI Inflation (G7)"
)

# Save to PNG (landscape, compact for Overleaf)
png("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/MCB_G7_vmcb_centered_paper.png", width = 1600, height = 1800, res = 200)

# Set up a 3x2 grid for 5 plots (last plot centered in bottom row)
par(
  mfrow = c(3, 2),
  oma = c(3, 5, 2, 2),   # outer margins: bottom, left, top, right
  mar = c(2, 3, 2, 1),   # inner margins: bottom, left, top, right
  cex.axis = 0.8,        # smaller axis labels
  cex.lab = 0.9          # smaller axis titles
)

# Plot the first 4 charts in the first 2 rows
for (i in 1:4) {
  rank_ew <- read_excel(files[[i]])
  rank_ew <- subset(rank_ew, select = -c(1,2,18))
  # rank_ew <- subset(rank_ew, select = -c(1,2,8,9,18))
  # Check only for VAR - specific models
  # rank_ew <- subset(rank_ew, select = c(3,4,5,17,18))
  nemenyi(
    as.matrix(rank_ew),
    conf.level = conf_levels[i],
    plottype = "vmcb",
    main = plot_titles[i],
    ylab = "",
    xlab = "",
    cex.axis = 0.8,
    cex.lab = 0.9
  )
}

# Third row, first cell: leave empty
plot.new()

# Third row, second cell: center the last plot (CPI Inflation)
par(mar = c(2, 3, 2, 1)) # reset margins for this plot
rank_ew <- read_excel(files[[5]])
rank_ew <- subset(rank_ew, select = -c(1,2,18))
# Exclude DTS+CB and DTS+XGB
# rank_ew <- subset(rank_ew, select = -c(1,2,8,9,18))
# Check only for VAR - specific models
# rank_ew <- subset(rank_ew, select = c(3,4,5,17,18))
nemenyi(
  as.matrix(rank_ew),
  conf.level = conf_levels[5],
  plottype = "vmcb",
  main = plot_titles[5],
  ylab = "",
  xlab = "",
  cex.axis = 0.8,
  cex.lab = 0.9
)

# Add a single, bold, centered Y-axis label for the whole figure (vertical)
mtext("Algorithm - Mean rank", side = 2, line = 2.2, outer = TRUE, cex = 0.9, font = 2)

# Add a single, bold, centered X-axis label below all plots
mtext("Mean rank", side = 1, line = 1.5, outer = TRUE, cex = 0.9, font = 2)

dev.off()

# Reset plotting parameters to default (optional, for interactive use)
par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(5,4,4,2) + 0.1)

############################## End of Code ############################






