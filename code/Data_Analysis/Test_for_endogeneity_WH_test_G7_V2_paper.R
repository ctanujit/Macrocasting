######################### test for Endogeneity: Wu-Hausman and Sargan test: G7 countries ###############
# --- Required Packages ---
# if(!require(AER)) install.packages("AER")
# if(!require(dplyr)) install.packages("dplyr")

library(AER)
library(dplyr)

# --- Output Directory ---
output_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Data_Analysis/output/Test_for_Endogeneity"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# --- G7 Country Configuration ---
g7_config <- list(
  CANADA  = "all_mulvar_data_canada_v2.csv",
  USA     = "all_mulvar_data_usa_v2.csv",
  GERMANY = "all_mulvar_data_germany_v2.csv",
  FRANCE  = "all_mulvar_data_france_v2.csv",
  JAPAN   = "all_mulvar_data_japan_v2.csv",
  UK      = "all_mulvar_data_uk_v2.csv",
  ITALY   = "all_mulvar_data_italy_v2.csv"
)

data_base_path <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset"

# --- Helper Function for Each Country ---
analyze_country <- function(country, file_name) {
  message("\n-------------------\nProcessing: ", country)
  
  # Read and preprocess
  data_path <- file.path(data_base_path, country, file_name)
  dat <- read.csv(data_path, header = TRUE)
  dat$Date <- as.Date(dat$Date)
  
  # Define regression formulas
  base_formula <- CPIinflationrate ~ Unemploymentrate + RealbroadEER + ShorttermIR + OilpriceGlobalWTI
  iv_formula   <- CPIinflationrate ~ Unemploymentrate + RealbroadEER + ShorttermIR + OilpriceGlobalWTI |
    logEPU + GPRC + USEMV + USMPU
  
  # OLS & IV regressions
  ols_fit <- lm(base_formula, data = dat)
  iv_fit  <- ivreg(iv_formula, data = dat)
  
  # Diagnostics (includes Wu-Hausman test)
  diag <- summary(iv_fit, diagnostics = TRUE)$diagnostics
  
  # Extract key statistics
  wu_hausman <- diag["Wu-Hausman", ]
  sargan     <- if("Sargan" %in% rownames(diag)) diag["Sargan", ] else rep(NA, ncol(diag))
  
  # Compile results
  list(
    country = country,
    wu_hausman_statistic = wu_hausman["statistic"],
    wu_hausman_pvalue = wu_hausman["p-value"],
    sargan_statistic = sargan["statistic"],
    sargan_pvalue = sargan["p-value"],
    weak_instr_results = diag[grepl("Weak instruments", rownames(diag)), c("statistic", "p-value")],
    ols_r2 = summary(ols_fit)$r.squared,
    iv_r2 = summary(iv_fit)$r.squared
  )
}

# --- Main Loop for All G7 Countries ---
results_list <- lapply(names(g7_config), function(cty) {
  analyze_country(cty, g7_config[[cty]])
})

# --- Summarize Results ---
results_df <- bind_rows(lapply(results_list, as.data.frame))

# --- Output Table (CSV + Print) ---
csv_path <- file.path(output_dir, "wh_sargan_results_g7.csv")
write.csv(results_df, csv_path, row.names = FALSE)
print(results_df)

# --- Key Insights Summary ---
cat("\nWu-Hausman Endogeneity Test Results for G7:\n")
for(i in seq_along(results_list)) {
  r <- results_list[[i]]
  cat(sprintf("\nCountry: %s\n", r$country))
  cat(sprintf("  Wu-Hausman statistic: %.3f | p-value: %.5f\n", 
              as.numeric(r$wu_hausman_statistic), as.numeric(r$wu_hausman_pvalue)))
  if(!is.na(r$sargan_statistic)) {
    cat(sprintf("  Sargan statistic: %.3f | p-value: %.5f\n", 
                as.numeric(r$sargan_statistic), as.numeric(r$sargan_pvalue)))
  } else {
    cat("  Sargan statistic: NA\n")
  }
  if(r$wu_hausman_pvalue < 0.10) {
    cat("  => Evidence *against* exogeneity (endogeneity detected, reject null)\n")
  } else {
    cat("  => *Fail* to reject exogeneity (no evidence for endogeneity)\n")
  }
}
cat("\nInterpretation: For each country, a small p-value (<0.10) in the Wu-Hausman test indicates rejection of the null of exogeneity, i.e., presence of endogeneity among regressors. Review weak instruments and Sargan tests for instrument validity.\n")

