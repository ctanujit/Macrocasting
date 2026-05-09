############################ multivariate Diebold-Mariano Test: Equal Predictive Ability ###################
# link:https://cran.r-project.org/web/packages/multDM/multDM.pdf
# Comment: According to the multDM documentation, for pairwise tests, 
# you should use evaluated = 2 to test if the second model is as good as the first.

################################ CANADA: 12M and 24M #########################
# install.packages("multDM")

# Set the working directory
setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()

# ---- Required Libraries ----
library(multDM)

# ---- File and variable setup ----
files_12M <- c(
  "canada_GR_results_baselines_CPI_12M.csv",
  "canada_GR_results_baselines_OP_12M.csv",
  "canada_GR_results_baselines_REER_12M.csv",
  "canada_GR_results_baselines_SIR_12M.csv",
  "canada_GR_results_baselines_UR_12M.csv"
)
files_24M <- c(
  "canada_GR_results_baselines_CPI_24M.csv",
  "canada_GR_results_baselines_OP_24M.csv",
  "canada_GR_results_baselines_REER_24M.csv",
  "canada_GR_results_baselines_SIR_24M.csv",
  "canada_GR_results_baselines_UR_24M.csv"
)
var_names <- c(
  "CPI Inflation",
  "Oil price (WTI)",
  "Real-broad EER",
  "Short-term IR",
  "Unemployment Rate"
)

# ---- Helper function to build realized and evaluated matrices ----
get_realized_and_evaluated <- function(files) {
  realized <- c()
  szbvar <- c()
  var <- c()
  catboost <- c()
  for (f in files) {
    df <- read.csv(f)
    df <- df[complete.cases(df[, c("test_data", "SZBVARx", "VARx", "CatBoost")]), ]
    realized <- c(realized, df$test_data)
    szbvar <- c(szbvar, df$SZBVAR)
    var <- c(var, df$VAR)
    catboost <- c(catboost, df$CatBoost)
  }
  # Each model is a row, columns are time points
  evaluated_VAR <- rbind(szbvar, var)
  evaluated_CatBoost <- rbind(szbvar, catboost)
  list(realized = realized, evaluated_VAR = evaluated_VAR, evaluated_CatBoost = evaluated_CatBoost)
}

# ---- Build realized and evaluated for each horizon ----
data_12M <- get_realized_and_evaluated(files_12M)
data_24M <- get_realized_and_evaluated(files_24M)
data_12M
# ---- Run Multivariate DM Test ----
# You may adjust q (block length) as needed; here we use q=1 as a default
qval <- 1

dm_12M_var <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_var <- round(dm_12M_var$p.value, 3)

dm_12M_cat <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_cat <- round(dm_12M_cat$p.value, 3)

dm_24M_var <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_var <- round(dm_24M_var$p.value, 3)

dm_24M_cat <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_cat <- round(dm_24M_cat$p.value, 3)

# ---- Commentary ----
commentary <- function(pval, comp) {
  if (is.na(pval)) return(paste("Test could not be performed for", comp))
  if (pval < 0.10) {
    paste("SZBVARx has significantly different predictive ability than", comp, "(p-value:", pval, ").")
  } else {
    paste("No significant difference in predictive ability between SZBVARx and", comp, "(p-value:", pval, ").")
  }
}

# ---- Results Table ----
results <- data.frame(
  Comparison = c("SZBVARx vs VARx (12M)", "SZBVARx vs CatBoost (12M)", "SZBVARx vs VAR (24M)", "SZBVARx vs CatBoost (24M)"),
  P_Value = c(pval_12M_var, pval_12M_cat, pval_24M_var, pval_24M_cat),
  Commentary = c(
    commentary(pval_12M_var, "VAR (12M)"),
    commentary(pval_12M_cat, "CatBoost (12M)"),
    commentary(pval_24M_var, "VAR (24M)"),
    commentary(pval_24M_cat, "CatBoost (24M)")
  ),
  stringsAsFactors = FALSE
)

# ---- Save to CSV ----
write.csv(results, "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/multivariate_DM_results_CANADA.csv", row.names = FALSE)
print(results)

# ---- General Explanation ----
cat("\n--- Interpretation Guide ---\n")
cat("The p-values in the table are for the null hypothesis that SZBVARx and the comparison model (VAR or CatBoost) have equal predictive accuracy across all variables jointly.\n")
cat("A p-value < 0.10 indicates a statistically significant difference in predictive ability at the 5% level.\n")
cat("If the p-value is >= 0.10, there is no evidence to reject the null, and the models are statistically indistinguishable in terms of predictive ability for all variables and the given horizon.\n")
cat("The 'Commentary' column provides a plain-language summary for each comparison.\n")

################################ USA: 12M and 24M #########################
# install.packages("multDM")
# Set the working directory
setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()

# ---- Required Libraries ----
library(multDM)

# ---- File and variable setup ----
files_12M <- c(
  "usa_GR_results_baselines_CPI_12M.csv",
  "usa_GR_results_baselines_OP_12M.csv",
  "usa_GR_results_baselines_REER_12M.csv",
  "usa_GR_results_baselines_SIR_12M.csv",
  "usa_GR_results_baselines_UR_12M.csv"
)
files_24M <- c(
  "usa_GR_results_baselines_CPI_24M.csv",
  "usa_GR_results_baselines_OP_24M.csv",
  "usa_GR_results_baselines_REER_24M.csv",
  "usa_GR_results_baselines_SIR_24M.csv",
  "usa_GR_results_baselines_UR_24M.csv"
)
var_names <- c(
  "CPI Inflation",
  "Oil price (WTI)",
  "Real-broad EER",
  "Short-term IR",
  "Unemployment Rate"
)

# ---- Helper function to build realized and evaluated matrices ----
get_realized_and_evaluated <- function(files) {
  realized <- c()
  szbvar <- c()
  var <- c()
  catboost <- c()
  for (f in files) {
    df <- read.csv(f)
    df <- df[complete.cases(df[, c("test_data", "SZBVARx", "VARx", "CatBoost")]), ]
    realized <- c(realized, df$test_data)
    szbvar <- c(szbvar, df$SZBVAR)
    var <- c(var, df$VAR)
    catboost <- c(catboost, df$CatBoost)
  }
  # Each model is a row, columns are time points
  evaluated_VAR <- rbind(szbvar, var)
  evaluated_CatBoost <- rbind(szbvar, catboost)
  list(realized = realized, evaluated_VAR = evaluated_VAR, evaluated_CatBoost = evaluated_CatBoost)
}

# ---- Build realized and evaluated for each horizon ----
data_12M <- get_realized_and_evaluated(files_12M)
data_24M <- get_realized_and_evaluated(files_24M)

# ---- Run Multivariate DM Test ----
# You may adjust q (block length) as needed; here we use q=1 as a default
qval <- 1

dm_12M_var <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_var <- round(dm_12M_var$p.value, 3)

dm_12M_cat <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_cat <- round(dm_12M_cat$p.value, 3)

dm_24M_var <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_var <- round(dm_24M_var$p.value, 3)

dm_24M_cat <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_cat <- round(dm_24M_cat$p.value, 3)

# ---- Commentary ----
commentary <- function(pval, comp) {
  if (is.na(pval)) return(paste("Test could not be performed for", comp))
  if (pval < 0.10) {
    paste("SZBVARx has significantly different predictive ability than", comp, "(p-value:", pval, ").")
  } else {
    paste("No significant difference in predictive ability between SZBVARx and", comp, "(p-value:", pval, ").")
  }
}

# ---- Results Table ----
results <- data.frame(
  Comparison = c("SZBVARx vs VAR (12M)", "SZBVARx vs CatBoost (12M)", "SZBVARx vs VAR (24M)", "SZBVARx vs CatBoost (24M)"),
  P_Value = c(pval_12M_var, pval_12M_cat, pval_24M_var, pval_24M_cat),
  Commentary = c(
    commentary(pval_12M_var, "VAR (12M)"),
    commentary(pval_12M_cat, "CatBoost (12M)"),
    commentary(pval_24M_var, "VAR (24M)"),
    commentary(pval_24M_cat, "CatBoost (24M)")
  ),
  stringsAsFactors = FALSE
)

# ---- Save to CSV ----
write.csv(results, "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/multivariate_DM_results_USA.csv", row.names = FALSE)
print(results)

# ---- General Explanation ----
cat("\n--- Interpretation Guide ---\n")
cat("The p-values in the table are for the null hypothesis that SZBVARx and the comparison model (VAR or CatBoost) have equal predictive accuracy across all variables jointly.\n")
cat("A p-value < 0.10 indicates a statistically significant difference in predictive ability at the 5% level.\n")
cat("If the p-value is >= 0.10, there is no evidence to reject the null, and the models are statistically indistinguishable in terms of predictive ability for all variables and the given horizon.\n")
cat("The 'Commentary' column provides a plain-language summary for each comparison.\n")

################################ FRANCE: 12M and 24M #########################
# install.packages("multDM")
# Set the working directory
setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()

# ---- Required Libraries ----
library(multDM)

# ---- File and variable setup ----
files_12M <- c(
  "france_GR_results_baselines_CPI_12M.csv",
  "france_GR_results_baselines_OP_12M.csv",
  "france_GR_results_baselines_REER_12M.csv",
  "france_GR_results_baselines_SIR_12M.csv",
  "france_GR_results_baselines_UR_12M.csv"
)
files_24M <- c(
  "france_GR_results_baselines_CPI_24M.csv",
  "france_GR_results_baselines_OP_24M.csv",
  "france_GR_results_baselines_REER_24M.csv",
  "france_GR_results_baselines_SIR_24M.csv",
  "france_GR_results_baselines_UR_24M.csv"
)
var_names <- c(
  "CPI Inflation",
  "Oil price (WTI)",
  "Real-broad EER",
  "Short-term IR",
  "Unemployment Rate"
)

# ---- Helper function to build realized and evaluated matrices ----
get_realized_and_evaluated <- function(files) {
  realized <- c()
  szbvar <- c()
  var <- c()
  catboost <- c()
  for (f in files) {
    df <- read.csv(f)
    df <- df[complete.cases(df[, c("test_data", "SZBVARx", "VARx", "CatBoost")]), ]
    realized <- c(realized, df$test_data)
    szbvar <- c(szbvar, df$SZBVAR)
    var <- c(var, df$VAR)
    catboost <- c(catboost, df$CatBoost)
  }
  # Each model is a row, columns are time points
  evaluated_VAR <- rbind(szbvar, var)
  evaluated_CatBoost <- rbind(szbvar, catboost)
  list(realized = realized, evaluated_VAR = evaluated_VAR, evaluated_CatBoost = evaluated_CatBoost)
}

# ---- Build realized and evaluated for each horizon ----
data_12M <- get_realized_and_evaluated(files_12M)
data_24M <- get_realized_and_evaluated(files_24M)

# ---- Run Multivariate DM Test ----
# You may adjust q (block length) as needed; here we use q=5 as a default
qval <- 1

dm_12M_var <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_var <- round(dm_12M_var$p.value, 3)

dm_12M_cat <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_cat <- round(dm_12M_cat$p.value, 3)

dm_24M_var <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_var <- round(dm_24M_var$p.value, 3)

dm_24M_cat <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_cat <- round(dm_24M_cat$p.value, 3)

# ---- Commentary ----
commentary <- function(pval, comp) {
  if (is.na(pval)) return(paste("Test could not be performed for", comp))
  if (pval < 0.10) {
    paste("SZBVARx has significantly different predictive ability than", comp, "(p-value:", pval, ").")
  } else {
    paste("No significant difference in predictive ability between SZBVARx and", comp, "(p-value:", pval, ").")
  }
}

# ---- Results Table ----
results <- data.frame(
  Comparison = c("SZBVARx vs VAR (12M)", "SZBVARx vs CatBoost (12M)", "SZBVARx vs VAR (24M)", "SZBVARx vs CatBoost (24M)"),
  P_Value = c(pval_12M_var, pval_12M_cat, pval_24M_var, pval_24M_cat),
  Commentary = c(
    commentary(pval_12M_var, "VAR (12M)"),
    commentary(pval_12M_cat, "CatBoost (12M)"),
    commentary(pval_24M_var, "VAR (24M)"),
    commentary(pval_24M_cat, "CatBoost (24M)")
  ),
  stringsAsFactors = FALSE
)

# ---- Save to CSV ----
write.csv(results, "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/multivariate_DM_results_FRANCE.csv", row.names = FALSE)
print(results)

# ---- General Explanation ----
cat("\n--- Interpretation Guide ---\n")
cat("The p-values in the table are for the null hypothesis that SZBVARx and the comparison model (VAR or CatBoost) have equal predictive accuracy across all variables jointly.\n")
cat("A p-value < 0.10 indicates a statistically significant difference in predictive ability at the 5% level.\n")
cat("If the p-value is >= 0.10, there is no evidence to reject the null, and the models are statistically indistinguishable in terms of predictive ability for all variables and the given horizon.\n")
cat("The 'Commentary' column provides a plain-language summary for each comparison.\n")

################################ GERMANY: 12M and 24M #########################
# install.packages("multDM")
# Set the working directory
setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()

# ---- Required Libraries ----
library(multDM)

# ---- File and variable setup ----
files_12M <- c(
  "germany_GR_results_baselines_CPI_12M.csv",
  "germany_GR_results_baselines_OP_12M.csv",
  "germany_GR_results_baselines_REER_12M.csv",
  "germany_GR_results_baselines_SIR_12M.csv",
  "germany_GR_results_baselines_UR_12M.csv"
)
files_24M <- c(
  "germany_GR_results_baselines_CPI_24M.csv",
  "germany_GR_results_baselines_OP_24M.csv",
  "germany_GR_results_baselines_REER_24M.csv",
  "germany_GR_results_baselines_SIR_24M.csv",
  "germany_GR_results_baselines_UR_24M.csv"
)
var_names <- c(
  "CPI Inflation",
  "Oil price (WTI)",
  "Real-broad EER",
  "Short-term IR",
  "Unemployment Rate"
)

# ---- Helper function to build realized and evaluated matrices ----
get_realized_and_evaluated <- function(files) {
  realized <- c()
  szbvar <- c()
  var <- c()
  catboost <- c()
  for (f in files) {
    df <- read.csv(f)
    df <- df[complete.cases(df[, c("test_data", "SZBVARx", "VARx", "CatBoost")]), ]
    realized <- c(realized, df$test_data)
    szbvar <- c(szbvar, df$SZBVAR)
    var <- c(var, df$VAR)
    catboost <- c(catboost, df$CatBoost)
  }
  # Each model is a row, columns are time points
  evaluated_VAR <- rbind(szbvar, var)
  evaluated_CatBoost <- rbind(szbvar, catboost)
  list(realized = realized, evaluated_VAR = evaluated_VAR, evaluated_CatBoost = evaluated_CatBoost)
}

# ---- Build realized and evaluated for each horizon ----
data_12M <- get_realized_and_evaluated(files_12M)
data_24M <- get_realized_and_evaluated(files_24M)

# ---- Run Multivariate DM Test ----
# You may adjust q (block length) as needed; here we use q=5 as a default
qval <- 1

dm_12M_var <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_var <- round(dm_12M_var$p.value, 3)

dm_12M_cat <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_cat <- round(dm_12M_cat$p.value, 3)

dm_24M_var <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_var <- round(dm_24M_var$p.value, 3)

dm_24M_cat <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_cat <- round(dm_24M_cat$p.value, 3)

# ---- Commentary ----
commentary <- function(pval, comp) {
  if (is.na(pval)) return(paste("Test could not be performed for", comp))
  if (pval < 0.10) {
    paste("SZBVARx has significantly different predictive ability than", comp, "(p-value:", pval, ").")
  } else {
    paste("No significant difference in predictive ability between SZBVARx and", comp, "(p-value:", pval, ").")
  }
}

# ---- Results Table ----
results <- data.frame(
  Comparison = c("SZBVARx vs VAR (12M)", "SZBVARx vs CatBoost (12M)", "SZBVARx vs VAR (24M)", "SZBVARx vs CatBoost (24M)"),
  P_Value = c(pval_12M_var, pval_12M_cat, pval_24M_var, pval_24M_cat),
  Commentary = c(
    commentary(pval_12M_var, "VAR (12M)"),
    commentary(pval_12M_cat, "CatBoost (12M)"),
    commentary(pval_24M_var, "VAR (24M)"),
    commentary(pval_24M_cat, "CatBoost (24M)")
  ),
  stringsAsFactors = FALSE
)

# ---- Save to CSV ----
write.csv(results, "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/multivariate_DM_results_GERMANY.csv", row.names = FALSE)
print(results)

# ---- General Explanation ----
cat("\n--- Interpretation Guide ---\n")
cat("The p-values in the table are for the null hypothesis that SZBVARx and the comparison model (VAR or CatBoost) have equal predictive accuracy across all variables jointly.\n")
cat("A p-value < 0.10 indicates a statistically significant difference in predictive ability at the 5% level.\n")
cat("If the p-value is >= 0.10, there is no evidence to reject the null, and the models are statistically indistinguishable in terms of predictive ability for all variables and the given horizon.\n")
cat("The 'Commentary' column provides a plain-language summary for each comparison.\n")

################################ JAPAN: 12M and 24M #########################
# install.packages("multDM")
# Set the working directory
setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()

# ---- Required Libraries ----
library(multDM)

# ---- File and variable setup ----
files_12M <- c(
  "japan_GR_results_baselines_CPI_12M.csv",
  "japan_GR_results_baselines_OP_12M.csv",
  "japan_GR_results_baselines_REER_12M.csv",
  "japan_GR_results_baselines_SIR_12M.csv",
  "japan_GR_results_baselines_UR_12M.csv"
)
files_24M <- c(
  "japan_GR_results_baselines_CPI_24M.csv",
  "japan_GR_results_baselines_OP_24M.csv",
  "japan_GR_results_baselines_REER_24M.csv",
  "japan_GR_results_baselines_SIR_24M.csv",
  "japan_GR_results_baselines_UR_24M.csv"
)
var_names <- c(
  "CPI Inflation",
  "Oil price (WTI)",
  "Real-broad EER",
  "Short-term IR",
  "Unemployment Rate"
)

# ---- Helper function to build realized and evaluated matrices ----
get_realized_and_evaluated <- function(files) {
  realized <- c()
  szbvar <- c()
  var <- c()
  catboost <- c()
  for (f in files) {
    df <- read.csv(f)
    df <- df[complete.cases(df[, c("test_data", "SZBVARx", "VARx", "CatBoost")]), ]
    realized <- c(realized, df$test_data)
    szbvar <- c(szbvar, df$SZBVAR)
    var <- c(var, df$VAR)
    catboost <- c(catboost, df$CatBoost)
  }
  # Each model is a row, columns are time points
  evaluated_VAR <- rbind(szbvar, var)
  evaluated_CatBoost <- rbind(szbvar, catboost)
  list(realized = realized, evaluated_VAR = evaluated_VAR, evaluated_CatBoost = evaluated_CatBoost)
}

# ---- Build realized and evaluated for each horizon ----
data_12M <- get_realized_and_evaluated(files_12M)
data_24M <- get_realized_and_evaluated(files_24M)

# ---- Run Multivariate DM Test ----
# You may adjust q (block length) as needed; here we use q=5 as a default
qval <- 1

dm_12M_var <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_var <- round(dm_12M_var$p.value, 3)

dm_12M_cat <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_cat <- round(dm_12M_cat$p.value, 3)

dm_24M_var <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_var <- round(dm_24M_var$p.value, 3)

dm_24M_cat <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_cat <- round(dm_24M_cat$p.value, 3)

# ---- Commentary ----
commentary <- function(pval, comp) {
  if (is.na(pval)) return(paste("Test could not be performed for", comp))
  if (pval < 0.10) {
    paste("SZBVARx has significantly different predictive ability than", comp, "(p-value:", pval, ").")
  } else {
    paste("No significant difference in predictive ability between SZBVARx and", comp, "(p-value:", pval, ").")
  }
}

# ---- Results Table ----
results <- data.frame(
  Comparison = c("SZBVARx vs VAR (12M)", "SZBVARx vs CatBoost (12M)", "SZBVARx vs VAR (24M)", "SZBVARx vs CatBoost (24M)"),
  P_Value = c(pval_12M_var, pval_12M_cat, pval_24M_var, pval_24M_cat),
  Commentary = c(
    commentary(pval_12M_var, "VAR (12M)"),
    commentary(pval_12M_cat, "CatBoost (12M)"),
    commentary(pval_24M_var, "VAR (24M)"),
    commentary(pval_24M_cat, "CatBoost (24M)")
  ),
  stringsAsFactors = FALSE
)

# ---- Save to CSV ----
write.csv(results, "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/multivariate_DM_results_JAPAN.csv", row.names = FALSE)
print(results)

# ---- General Explanation ----
cat("\n--- Interpretation Guide ---\n")
cat("The p-values in the table are for the null hypothesis that SZBVARx and the comparison model (VAR or CatBoost) have equal predictive accuracy across all variables jointly.\n")
cat("A p-value < 0.10 indicates a statistically significant difference in predictive ability at the 5% level.\n")
cat("If the p-value is >= 0.10, there is no evidence to reject the null, and the models are statistically indistinguishable in terms of predictive ability for all variables and the given horizon.\n")
cat("The 'Commentary' column provides a plain-language summary for each comparison.\n")

################################ UK: 12M and 24M #########################
# install.packages("multDM")
# Set the working directory
setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()

# ---- Required Libraries ----
library(multDM)

# ---- File and variable setup ----
files_12M <- c(
  "uk_GR_results_baselines_CPI_12M.csv",
  "uk_GR_results_baselines_OP_12M.csv",
  "uk_GR_results_baselines_REER_12M.csv",
  "uk_GR_results_baselines_SIR_12M.csv",
  "uk_GR_results_baselines_UR_12M.csv"
)
files_24M <- c(
  "uk_GR_results_baselines_CPI_24M.csv",
  "uk_GR_results_baselines_OP_24M.csv",
  "uk_GR_results_baselines_REER_24M.csv",
  "uk_GR_results_baselines_SIR_24M.csv",
  "uk_GR_results_baselines_UR_24M.csv"
)
var_names <- c(
  "CPI Inflation",
  "Oil price (WTI)",
  "Real-broad EER",
  "Short-term IR",
  "Unemployment Rate"
)

# ---- Helper function to build realized and evaluated matrices ----
get_realized_and_evaluated <- function(files) {
  realized <- c()
  szbvar <- c()
  var <- c()
  catboost <- c()
  for (f in files) {
    df <- read.csv(f)
    df <- df[complete.cases(df[, c("test_data", "SZBVARx", "VARx", "CatBoost")]), ]
    realized <- c(realized, df$test_data)
    szbvar <- c(szbvar, df$SZBVAR)
    var <- c(var, df$VAR)
    catboost <- c(catboost, df$CatBoost)
  }
  # Each model is a row, columns are time points
  evaluated_VAR <- rbind(szbvar, var)
  evaluated_CatBoost <- rbind(szbvar, catboost)
  list(realized = realized, evaluated_VAR = evaluated_VAR, evaluated_CatBoost = evaluated_CatBoost)
}

# ---- Build realized and evaluated for each horizon ----
data_12M <- get_realized_and_evaluated(files_12M)
data_24M <- get_realized_and_evaluated(files_24M)

# ---- Run Multivariate DM Test ----
# You may adjust q (block length) as needed; here we use q=5 as a default
qval <- 1

dm_12M_var <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_var <- round(dm_12M_var$p.value, 3)

dm_12M_cat <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_cat <- round(dm_12M_cat$p.value, 3)

dm_24M_var <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_var <- round(dm_24M_var$p.value, 3)

dm_24M_cat <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_cat <- round(dm_24M_cat$p.value, 3)

# ---- Commentary ----
commentary <- function(pval, comp) {
  if (is.na(pval)) return(paste("Test could not be performed for", comp))
  if (pval < 0.10) {
    paste("SZBVARx has significantly different predictive ability than", comp, "(p-value:", pval, ").")
  } else {
    paste("No significant difference in predictive ability between SZBVARx and", comp, "(p-value:", pval, ").")
  }
}

# ---- Results Table ----
results <- data.frame(
  Comparison = c("SZBVARx vs VAR (12M)", "SZBVARx vs CatBoost (12M)", "SZBVARx vs VAR (24M)", "SZBVARx vs CatBoost (24M)"),
  P_Value = c(pval_12M_var, pval_12M_cat, pval_24M_var, pval_24M_cat),
  Commentary = c(
    commentary(pval_12M_var, "VAR (12M)"),
    commentary(pval_12M_cat, "CatBoost (12M)"),
    commentary(pval_24M_var, "VAR (24M)"),
    commentary(pval_24M_cat, "CatBoost (24M)")
  ),
  stringsAsFactors = FALSE
)

# ---- Save to CSV ----
write.csv(results, "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/multivariate_DM_results_UK.csv", row.names = FALSE)
print(results)

# ---- General Explanation ----
cat("\n--- Interpretation Guide ---\n")
cat("The p-values in the table are for the null hypothesis that SZBVARx and the comparison model (VAR or CatBoost) have equal predictive accuracy across all variables jointly.\n")
cat("A p-value < 0.10 indicates a statistically significant difference in predictive ability at the 5% level.\n")
cat("If the p-value is >= 0.10, there is no evidence to reject the null, and the models are statistically indistinguishable in terms of predictive ability for all variables and the given horizon.\n")
cat("The 'Commentary' column provides a plain-language summary for each comparison.\n")


################################ ITALY: 12M and 24M #########################
# install.packages("multDM")
# Set the working directory
setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()

# ---- Required Libraries ----
library(multDM)

# ---- File and variable setup ----
files_12M <- c(
  "italy_GR_results_baselines_CPI_12M.csv",
  "italy_GR_results_baselines_OP_12M.csv",
  "italy_GR_results_baselines_REER_12M.csv",
  "italy_GR_results_baselines_SIR_12M.csv",
  "italy_GR_results_baselines_UR_12M.csv"
)
files_24M <- c(
  "italy_GR_results_baselines_CPI_24M.csv",
  "italy_GR_results_baselines_OP_24M.csv",
  "italy_GR_results_baselines_REER_24M.csv",
  "italy_GR_results_baselines_SIR_24M.csv",
  "italy_GR_results_baselines_UR_24M.csv"
)
var_names <- c(
  "CPI Inflation",
  "Oil price (WTI)",
  "Real-broad EER",
  "Short-term IR",
  "Unemployment Rate"
)

# ---- Helper function to build realized and evaluated matrices ----
get_realized_and_evaluated <- function(files) {
  realized <- c()
  szbvar <- c()
  var <- c()
  catboost <- c()
  for (f in files) {
    df <- read.csv(f)
    df <- df[complete.cases(df[, c("test_data", "SZBVARx", "VARx", "CatBoost")]), ]
    realized <- c(realized, df$test_data)
    szbvar <- c(szbvar, df$SZBVAR)
    var <- c(var, df$VAR)
    catboost <- c(catboost, df$CatBoost)
  }
  # Each model is a row, columns are time points
  evaluated_VAR <- rbind(szbvar, var)
  evaluated_CatBoost <- rbind(szbvar, catboost)
  list(realized = realized, evaluated_VAR = evaluated_VAR, evaluated_CatBoost = evaluated_CatBoost)
}

# ---- Build realized and evaluated for each horizon ----
data_12M <- get_realized_and_evaluated(files_12M)
data_24M <- get_realized_and_evaluated(files_24M)

# ---- Run Multivariate DM Test ----
# You may adjust q (block length) as needed; here we use q=5 as a default
qval <- 1

dm_12M_var <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_var <- round(dm_12M_var$p.value, 3)

dm_12M_cat <- MDM.test(realized = data_12M$realized, evaluated = data_12M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_12M_cat <- round(dm_12M_cat$p.value, 3)

dm_24M_var <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_VAR, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_var <- round(dm_24M_var$p.value, 3)

dm_24M_cat <- MDM.test(realized = data_24M$realized, evaluated = data_24M$evaluated_CatBoost, q = qval, statistic = "Sc", loss.type = "ASE")
pval_24M_cat <- round(dm_24M_cat$p.value, 3)

# ---- Commentary ----
commentary <- function(pval, comp) {
  if (is.na(pval)) return(paste("Test could not be performed for", comp))
  if (pval < 0.10) {
    paste("SZBVARx has significantly different predictive ability than", comp, "(p-value:", pval, ").")
  } else {
    paste("No significant difference in predictive ability between SZBVARx and", comp, "(p-value:", pval, ").")
  }
}

# ---- Results Table ----
results <- data.frame(
  Comparison = c("SZBVARx vs VAR (12M)", "SZBVARx vs CatBoost (12M)", "SZBVARx vs VAR (24M)", "SZBVARx vs CatBoost (24M)"),
  P_Value = c(pval_12M_var, pval_12M_cat, pval_24M_var, pval_24M_cat),
  Commentary = c(
    commentary(pval_12M_var, "VAR (12M)"),
    commentary(pval_12M_cat, "CatBoost (12M)"),
    commentary(pval_24M_var, "VAR (24M)"),
    commentary(pval_24M_cat, "CatBoost (24M)")
  ),
  stringsAsFactors = FALSE
)

# ---- Save to CSV ----
write.csv(results, "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/multivariate_DM_results_ITALY.csv", row.names = FALSE)
print(results)

# ---- General Explanation ----
cat("\n--- Interpretation Guide ---\n")
cat("The p-values in the table are for the null hypothesis that SZBVARx and the comparison model (VAR or CatBoost) have equal predictive accuracy across all variables jointly.\n")
cat("A p-value < 0.10 indicates a statistically significant difference in predictive ability at the 5% level.\n")
cat("If the p-value is >= 0.10, there is no evidence to reject the null, and the models are statistically indistinguishable in terms of predictive ability for all variables and the given horizon.\n")
cat("The 'Commentary' column provides a plain-language summary for each comparison.\n")

############################### End of Code #############################












