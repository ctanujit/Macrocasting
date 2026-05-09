#### SZBVAR Model: CANADA: 12M and 24M ahead - HP Tuning ####
###################### Source code : szbvar function + forecast.szbvar function ##################
# Revised instruction
# Instruction: 1. First execute "szbvarx_orchestrator_utils.R" code module before running the following code block
##################### End of the Source Code #####################################################
#### SZBVAR Model: CANADA: 12M and 24M ahead - forecasts with HP Tuning ####
# Load required libraries
library(vars)
library(forecast)
library(Metrics)
library(dplyr)
library(tseries)
library(parallel)

# Setting the working directory
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/canada")
getwd()

# Read the dataset
var.canada <- read.csv("all_mulvar_data_canada_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.canada$Date <- as.Date(var.canada$Date)
print("Dataset structure:")
str(var.canada)

# Creation of datasets for 12M forecasts
var.canada.12M.train <- var.canada[1:327,]
var.canada.12M.val <- var.canada[328:339,]
var.canada.12M.test <- var.canada[340:351,]
var.canada.12M.full.train <- var.canada[1:339,]

# Creation of datasets for 24M forecasts
var.canada.24M.train <- var.canada[1:303,]
var.canada.24M.val <- var.canada[304:327,]
var.canada.24M.test <- var.canada[328:351,]
var.canada.24M.full.train <- var.canada[1:327,]

# Define endogenous and exogenous variables
endo_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Function to fit SZBVAR model with specific hyperparameters
fit_szbvar_with_params <- function(train_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    return(model)
  }, error = function(e) {
    print(paste("Error in model fitting:", e$message))
    return(NULL)
  })
}

# Function to fit model and generate forecasts - following the original structure
fit_and_forecast_szbvar_with_params <- function(train_data, test_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    # Fit the SZBVAR model with given hyperparameters
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    
    # Generate forecasts using last observations of exogenous variables from training data
    forecasts <- forecast(
      model,
      nsteps = nrow(test_data),
      exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, exog_vars])
    )
    
    # Extract only the forecast part
    forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
    
    return(list(
      model = model,
      forecasts = forecasts_only
    ))
  }, error = function(e) {
    print(paste("Error in model fitting or forecasting:", e$message))
    return(NULL)
  })
}

# Function to calculate RMSE for each variable
calculate_rmse <- function(actual, predicted) {
  if(is.null(predicted)) return(Inf)
  
  rmse_values <- sapply(1:ncol(actual), function(i) {
    rmse(actual[,i], predicted[,i])
  })
  names(rmse_values) <- colnames(actual)
  
  # Calculate average RMSE across all variables
  avg_rmse <- mean(rmse_values)
  
  return(list(
    variable_rmse = rmse_values,
    avg_rmse = avg_rmse
  ))
}

# Function to perform hyperparameter tuning
tune_hyperparameters <- function(train_data, val_data) {
  print("Starting hyperparameter tuning...")
  
  # Define hyperparameter grid
  p_values <- c(1, 2, 3, 4)
  lambda0_values <- c(0.2, 0.4, 0.6, 0.8)
  lambda1_values <- c(0.05, 0.1, 0.2)
  lambda3_values <- c(1, 2, 3)
  lambda4_values <- c(0.1, 0.25, 0.5)
  lambda5_values <- c(0, 0.5, 1)
  mu5_values <- c(0, 0.5, 1)
  mu6_values <- c(0, 0.5, 1)
  prior_values <- c(0, 1)
  
  # Initialize results tracking
  best_rmse <- Inf
  best_params <- list()
  results_df <- data.frame()
  
  # Total number of combinations to try
  total_combinations <- length(p_values) * length(lambda0_values) * length(lambda1_values) * 
    length(lambda3_values) * length(lambda4_values) * length(lambda5_values) * 
    length(mu5_values) * length(mu6_values) * length(prior_values)
  
  print(paste("Total hyperparameter combinations to evaluate:", total_combinations))
  
  # Counter for progress tracking
  counter <- 0
  
  # Nested loops for grid search
  for(p in p_values) {
    for(lambda0 in lambda0_values) {
      for(lambda1 in lambda1_values) {
        for(lambda3 in lambda3_values) {
          for(lambda4 in lambda4_values) {
            for(lambda5 in lambda5_values) {
              for(mu5 in mu5_values) {
                for(mu6 in mu6_values) {
                  for(prior in prior_values) {
                    counter <- counter + 1
                    
                    # Print progress
                    if(counter %% 10 == 0) {
                      print(paste("Progress:", counter, "/", total_combinations, 
                                  "combinations evaluated"))
                    }
                    
                    # Fit model and generate forecasts with current hyperparameters
                    results <- fit_and_forecast_szbvar_with_params(
                      train_data, val_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior
                    )
                    
                    # Skip if model fitting or forecasting failed
                    if(is.null(results)) next
                    
                    # Calculate RMSE
                    rmse_results <- calculate_rmse(
                      as.matrix(val_data[, endo_vars]), 
                      as.matrix(results$forecasts)
                    )
                    
                    # Record results
                    current_result <- data.frame(
                      p = p,
                      lambda0 = lambda0,
                      lambda1 = lambda1,
                      lambda3 = lambda3,
                      lambda4 = lambda4,
                      lambda5 = lambda5,
                      mu5 = mu5,
                      mu6 = mu6,
                      prior = prior,
                      avg_rmse = rmse_results$avg_rmse,
                      rmse_unemployment = rmse_results$variable_rmse[1],
                      rmse_eer = rmse_results$variable_rmse[2],
                      rmse_ir = rmse_results$variable_rmse[3],
                      rmse_oil = rmse_results$variable_rmse[4],
                      rmse_cpi = rmse_results$variable_rmse[5]
                    )
                    
                    results_df <- rbind(results_df, current_result)
                    
                    # Update best parameters if current model is better
                    if(rmse_results$avg_rmse < best_rmse) {
                      best_rmse <- rmse_results$avg_rmse
                      best_params <- list(
                        p = p,
                        lambda0 = lambda0,
                        lambda1 = lambda1,
                        lambda3 = lambda3,
                        lambda4 = lambda4,
                        lambda5 = lambda5,
                        mu5 = mu5,
                        mu6 = mu6,
                        prior = prior
                      )
                      
                      print(paste("New best model found! Avg RMSE:", best_rmse))
                      print("Parameters:")
                      print(best_params)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(list(
    best_params = best_params,
    best_rmse = best_rmse,
    all_results = results_df
  ))
}

# Function to generate final forecasts using best hyperparameters
generate_final_forecasts <- function(train_data, test_data, best_params) {
  # Use the combined function to fit model and generate forecasts
  results <- fit_and_forecast_szbvar_with_params(
    train_data,
    test_data,
    best_params$p,
    best_params$lambda0,
    best_params$lambda1,
    best_params$lambda3,
    best_params$lambda4,
    best_params$lambda5,
    best_params$mu5,
    best_params$mu6,
    best_params$prior
  )
  
  if(is.null(results)) {
    stop("Failed to generate final forecasts with best hyperparameters")
  }
  
  return(results)
}

# Main execution block
print("Starting SZBVAR model training and forecasting with hyperparameter tuning")

# 1. Hyperparameter tuning for 12M horizon
print("=== 12M Horizon Hyperparameter Tuning ===")
tune_results_12M <- tune_hyperparameters(var.canada.12M.train, var.canada.12M.val)

# Save tuning results
write.csv(tune_results_12M$all_results, "hp_tuning_results_12M.csv", row.names = FALSE)
print("Best hyperparameters for 12M horizon:")
print(tune_results_12M$best_params)
print(paste("Best average RMSE:", tune_results_12M$best_rmse))

# 2. Hyperparameter tuning for 24M horizon
print("=== 24M Horizon Hyperparameter Tuning ===")
tune_results_24M <- tune_hyperparameters(var.canada.24M.train, var.canada.24M.val)
# Save tuning results
write.csv(tune_results_24M$all_results, "hp_tuning_results_24M.csv", row.names = FALSE)
print("Best hyperparameters for 24M horizon:")
print(tune_results_24M$best_params)
print(paste("Best average RMSE:", tune_results_24M$best_rmse))
##################### End of HP tuning Code ###############################

#### SZBVAR Model: USA: 12M and 24M ahead - HP Tuning ####
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/usa")
getwd()

# Read the dataset
var.usa <- read.csv("all_mulvar_data_usa_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.usa$Date <- as.Date(var.usa$Date)
print("Dataset structure:")
str(var.usa)

# Creation of datasets for 12M forecasts
var.usa.12M.train <- var.usa[1:327,]
var.usa.12M.val <- var.usa[328:339,]
var.usa.12M.test <- var.usa[340:351,]
var.usa.12M.full.train <- var.usa[1:339,]

# Creation of datasets for 24M forecasts
var.usa.24M.train <- var.usa[1:303,]
var.usa.24M.val <- var.usa[304:327,]
var.usa.24M.test <- var.usa[328:351,]
var.usa.24M.full.train <- var.usa[1:327,]

# Define endogenous and exogenous variables
endo_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Function to fit SZBVAR model with specific hyperparameters
fit_szbvar_with_params <- function(train_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    return(model)
  }, error = function(e) {
    print(paste("Error in model fitting:", e$message))
    return(NULL)
  })
}

# Function to fit model and generate forecasts - following the original structure
fit_and_forecast_szbvar_with_params <- function(train_data, test_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    # Fit the SZBVAR model with given hyperparameters
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    
    # Generate forecasts using last observations of exogenous variables from training data
    forecasts <- forecast(
      model,
      nsteps = nrow(test_data),
      exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, exog_vars])
    )
    
    # Extract only the forecast part
    forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
    
    return(list(
      model = model,
      forecasts = forecasts_only
    ))
  }, error = function(e) {
    print(paste("Error in model fitting or forecasting:", e$message))
    return(NULL)
  })
}

# Function to calculate RMSE for each variable
calculate_rmse <- function(actual, predicted) {
  if(is.null(predicted)) return(Inf)
  
  rmse_values <- sapply(1:ncol(actual), function(i) {
    rmse(actual[,i], predicted[,i])
  })
  names(rmse_values) <- colnames(actual)
  
  # Calculate average RMSE across all variables
  avg_rmse <- mean(rmse_values)
  
  return(list(
    variable_rmse = rmse_values,
    avg_rmse = avg_rmse
  ))
}

# Function to perform hyperparameter tuning
tune_hyperparameters <- function(train_data, val_data) {
  print("Starting hyperparameter tuning...")
  
  # Define hyperparameter grid
  p_values <- c(1, 2, 3, 4)
  lambda0_values <- c(0.2, 0.4, 0.6, 0.8)
  lambda1_values <- c(0.05, 0.1, 0.2)
  lambda3_values <- c(1, 2, 3)
  lambda4_values <- c(0.1, 0.25, 0.5)
  lambda5_values <- c(0, 0.5, 1)
  mu5_values <- c(0, 0.5, 1)
  mu6_values <- c(0, 0.5, 1)
  prior_values <- c(0, 1)
  
  # Initialize results tracking
  best_rmse <- Inf
  best_params <- list()
  results_df <- data.frame()
  
  # Total number of combinations to try
  total_combinations <- length(p_values) * length(lambda0_values) * length(lambda1_values) * 
    length(lambda3_values) * length(lambda4_values) * length(lambda5_values) * 
    length(mu5_values) * length(mu6_values) * length(prior_values)
  
  print(paste("Total hyperparameter combinations to evaluate:", total_combinations))
  
  # Counter for progress tracking
  counter <- 0
  
  # Nested loops for grid search
  for(p in p_values) {
    for(lambda0 in lambda0_values) {
      for(lambda1 in lambda1_values) {
        for(lambda3 in lambda3_values) {
          for(lambda4 in lambda4_values) {
            for(lambda5 in lambda5_values) {
              for(mu5 in mu5_values) {
                for(mu6 in mu6_values) {
                  for(prior in prior_values) {
                    counter <- counter + 1
                    
                    # Print progress
                    if(counter %% 10 == 0) {
                      print(paste("Progress:", counter, "/", total_combinations, 
                                  "combinations evaluated"))
                    }
                    
                    # Fit model and generate forecasts with current hyperparameters
                    results <- fit_and_forecast_szbvar_with_params(
                      train_data, val_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior
                    )
                    
                    # Skip if model fitting or forecasting failed
                    if(is.null(results)) next
                    
                    # Calculate RMSE
                    rmse_results <- calculate_rmse(
                      as.matrix(val_data[, endo_vars]), 
                      as.matrix(results$forecasts)
                    )
                    
                    # Record results
                    current_result <- data.frame(
                      p = p,
                      lambda0 = lambda0,
                      lambda1 = lambda1,
                      lambda3 = lambda3,
                      lambda4 = lambda4,
                      lambda5 = lambda5,
                      mu5 = mu5,
                      mu6 = mu6,
                      prior = prior,
                      avg_rmse = rmse_results$avg_rmse,
                      rmse_unemployment = rmse_results$variable_rmse[1],
                      rmse_eer = rmse_results$variable_rmse[2],
                      rmse_ir = rmse_results$variable_rmse[3],
                      rmse_oil = rmse_results$variable_rmse[4],
                      rmse_cpi = rmse_results$variable_rmse[5]
                    )
                    
                    results_df <- rbind(results_df, current_result)
                    
                    # Update best parameters if current model is better
                    if(rmse_results$avg_rmse < best_rmse) {
                      best_rmse <- rmse_results$avg_rmse
                      best_params <- list(
                        p = p,
                        lambda0 = lambda0,
                        lambda1 = lambda1,
                        lambda3 = lambda3,
                        lambda4 = lambda4,
                        lambda5 = lambda5,
                        mu5 = mu5,
                        mu6 = mu6,
                        prior = prior
                      )
                      
                      print(paste("New best model found! Avg RMSE:", best_rmse))
                      print("Parameters:")
                      print(best_params)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(list(
    best_params = best_params,
    best_rmse = best_rmse,
    all_results = results_df
  ))
}

# Function to generate final forecasts using best hyperparameters
generate_final_forecasts <- function(train_data, test_data, best_params) {
  # Use the combined function to fit model and generate forecasts
  results <- fit_and_forecast_szbvar_with_params(
    train_data,
    test_data,
    best_params$p,
    best_params$lambda0,
    best_params$lambda1,
    best_params$lambda3,
    best_params$lambda4,
    best_params$lambda5,
    best_params$mu5,
    best_params$mu6,
    best_params$prior
  )
  
  if(is.null(results)) {
    stop("Failed to generate final forecasts with best hyperparameters")
  }
  
  return(results)
}

# Main execution block
print("Starting SZBVAR model training and forecasting with hyperparameter tuning")

# 1. Hyperparameter tuning for 12M horizon
print("=== 12M Horizon Hyperparameter Tuning ===")
tune_results_12M <- tune_hyperparameters(var.usa.12M.train, var.usa.12M.val)

# Save tuning results
write.csv(tune_results_12M$all_results, "hp_tuning_results_12M.csv", row.names = FALSE)
print("Best hyperparameters for 12M horizon:")
print(tune_results_12M$best_params)
print(paste("Best average RMSE:", tune_results_12M$best_rmse))

# 2. Hyperparameter tuning for 24M horizon
print("=== 24M Horizon Hyperparameter Tuning ===")
tune_results_24M <- tune_hyperparameters(var.usa.24M.train, var.usa.24M.val)
# Save tuning results
write.csv(tune_results_24M$all_results, "hp_tuning_results_24M.csv", row.names = FALSE)
print("Best hyperparameters for 24M horizon:")
print(tune_results_24M$best_params)
print(paste("Best average RMSE:", tune_results_24M$best_rmse))
##################### End of HP tuning Code ###############################

#### SZBVAR Model: FRANCE: 12M and 24M ahead - HP Tuning ####
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/france")
getwd()

# Read the dataset
var.france <- read.csv("all_mulvar_data_france_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.france$Date <- as.Date(var.france$Date)
print("Dataset structure:")
str(var.france)

# Creation of datasets for 12M forecasts
var.france.12M.train <- var.france[1:327,]
var.france.12M.val <- var.france[328:339,]
var.france.12M.test <- var.france[340:351,]
var.france.12M.full.train <- var.france[1:339,]

# Creation of datasets for 24M forecasts
var.france.24M.train <- var.france[1:303,]
var.france.24M.val <- var.france[304:327,]
var.france.24M.test <- var.france[328:351,]
var.france.24M.full.train <- var.france[1:327,]

# Define endogenous and exogenous variables
endo_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Function to fit SZBVAR model with specific hyperparameters
fit_szbvar_with_params <- function(train_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    return(model)
  }, error = function(e) {
    print(paste("Error in model fitting:", e$message))
    return(NULL)
  })
}

# Function to fit model and generate forecasts - following the original structure
fit_and_forecast_szbvar_with_params <- function(train_data, test_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    # Fit the SZBVAR model with given hyperparameters
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    
    # Generate forecasts using last observations of exogenous variables from training data
    forecasts <- forecast(
      model,
      nsteps = nrow(test_data),
      exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, exog_vars])
    )
    
    # Extract only the forecast part
    forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
    
    return(list(
      model = model,
      forecasts = forecasts_only
    ))
  }, error = function(e) {
    print(paste("Error in model fitting or forecasting:", e$message))
    return(NULL)
  })
}

# Function to calculate RMSE for each variable
calculate_rmse <- function(actual, predicted) {
  if(is.null(predicted)) return(Inf)
  
  rmse_values <- sapply(1:ncol(actual), function(i) {
    rmse(actual[,i], predicted[,i])
  })
  names(rmse_values) <- colnames(actual)
  
  # Calculate average RMSE across all variables
  avg_rmse <- mean(rmse_values)
  
  return(list(
    variable_rmse = rmse_values,
    avg_rmse = avg_rmse
  ))
}

# Function to perform hyperparameter tuning
tune_hyperparameters <- function(train_data, val_data) {
  print("Starting hyperparameter tuning...")
  
  # Define hyperparameter grid
  p_values <- c(1, 2, 3, 4)
  lambda0_values <- c(0.2, 0.4, 0.6, 0.8)
  lambda1_values <- c(0.05, 0.1, 0.2)
  lambda3_values <- c(1, 2, 3)
  lambda4_values <- c(0.1, 0.25, 0.5)
  lambda5_values <- c(0, 0.5, 1)
  mu5_values <- c(0, 0.5, 1)
  mu6_values <- c(0, 0.5, 1)
  prior_values <- c(0, 1)
  
  # Initialize results tracking
  best_rmse <- Inf
  best_params <- list()
  results_df <- data.frame()
  
  # Total number of combinations to try
  total_combinations <- length(p_values) * length(lambda0_values) * length(lambda1_values) * 
    length(lambda3_values) * length(lambda4_values) * length(lambda5_values) * 
    length(mu5_values) * length(mu6_values) * length(prior_values)
  
  print(paste("Total hyperparameter combinations to evaluate:", total_combinations))
  
  # Counter for progress tracking
  counter <- 0
  
  # Nested loops for grid search
  for(p in p_values) {
    for(lambda0 in lambda0_values) {
      for(lambda1 in lambda1_values) {
        for(lambda3 in lambda3_values) {
          for(lambda4 in lambda4_values) {
            for(lambda5 in lambda5_values) {
              for(mu5 in mu5_values) {
                for(mu6 in mu6_values) {
                  for(prior in prior_values) {
                    counter <- counter + 1
                    
                    # Print progress
                    if(counter %% 10 == 0) {
                      print(paste("Progress:", counter, "/", total_combinations, 
                                  "combinations evaluated"))
                    }
                    
                    # Fit model and generate forecasts with current hyperparameters
                    results <- fit_and_forecast_szbvar_with_params(
                      train_data, val_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior
                    )
                    
                    # Skip if model fitting or forecasting failed
                    if(is.null(results)) next
                    
                    # Calculate RMSE
                    rmse_results <- calculate_rmse(
                      as.matrix(val_data[, endo_vars]), 
                      as.matrix(results$forecasts)
                    )
                    
                    # Record results
                    current_result <- data.frame(
                      p = p,
                      lambda0 = lambda0,
                      lambda1 = lambda1,
                      lambda3 = lambda3,
                      lambda4 = lambda4,
                      lambda5 = lambda5,
                      mu5 = mu5,
                      mu6 = mu6,
                      prior = prior,
                      avg_rmse = rmse_results$avg_rmse,
                      rmse_unemployment = rmse_results$variable_rmse[1],
                      rmse_eer = rmse_results$variable_rmse[2],
                      rmse_ir = rmse_results$variable_rmse[3],
                      rmse_oil = rmse_results$variable_rmse[4],
                      rmse_cpi = rmse_results$variable_rmse[5]
                    )
                    
                    results_df <- rbind(results_df, current_result)
                    
                    # Update best parameters if current model is better
                    if(rmse_results$avg_rmse < best_rmse) {
                      best_rmse <- rmse_results$avg_rmse
                      best_params <- list(
                        p = p,
                        lambda0 = lambda0,
                        lambda1 = lambda1,
                        lambda3 = lambda3,
                        lambda4 = lambda4,
                        lambda5 = lambda5,
                        mu5 = mu5,
                        mu6 = mu6,
                        prior = prior
                      )
                      
                      print(paste("New best model found! Avg RMSE:", best_rmse))
                      print("Parameters:")
                      print(best_params)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(list(
    best_params = best_params,
    best_rmse = best_rmse,
    all_results = results_df
  ))
}

# Function to generate final forecasts using best hyperparameters
generate_final_forecasts <- function(train_data, test_data, best_params) {
  # Use the combined function to fit model and generate forecasts
  results <- fit_and_forecast_szbvar_with_params(
    train_data,
    test_data,
    best_params$p,
    best_params$lambda0,
    best_params$lambda1,
    best_params$lambda3,
    best_params$lambda4,
    best_params$lambda5,
    best_params$mu5,
    best_params$mu6,
    best_params$prior
  )
  
  if(is.null(results)) {
    stop("Failed to generate final forecasts with best hyperparameters")
  }
  
  return(results)
}

# Main execution block
print("Starting SZBVAR model training and forecasting with hyperparameter tuning")

# 1. Hyperparameter tuning for 12M horizon
print("=== 12M Horizon Hyperparameter Tuning ===")
tune_results_12M <- tune_hyperparameters(var.france.12M.train, var.france.12M.val)

# Save tuning results
write.csv(tune_results_12M$all_results, "hp_tuning_results_12M.csv", row.names = FALSE)
print("Best hyperparameters for 12M horizon:")
print(tune_results_12M$best_params)
print(paste("Best average RMSE:", tune_results_12M$best_rmse))

# 2. Hyperparameter tuning for 24M horizon
print("=== 24M Horizon Hyperparameter Tuning ===")
tune_results_24M <- tune_hyperparameters(var.france.24M.train, var.france.24M.val)
# Save tuning results
write.csv(tune_results_24M$all_results, "hp_tuning_results_24M.csv", row.names = FALSE)
print("Best hyperparameters for 24M horizon:")
print(tune_results_24M$best_params)
print(paste("Best average RMSE:", tune_results_24M$best_rmse))
##################### End of HP tuning Code ###############################

# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/germany")
getwd()

# Read the dataset
var.germany <- read.csv("all_mulvar_data_germany_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.germany$Date <- as.Date(var.germany$Date)
print("Dataset structure:")
str(var.germany)

# Creation of datasets for 12M forecasts
var.germany.12M.train <- var.germany[1:327,]
var.germany.12M.val <- var.germany[328:339,]
var.germany.12M.test <- var.germany[340:351,]
var.germany.12M.full.train <- var.germany[1:339,]

# Creation of datasets for 24M forecasts
var.germany.24M.train <- var.germany[1:303,]
var.germany.24M.val <- var.germany[304:327,]
var.germany.24M.test <- var.germany[328:351,]
var.germany.24M.full.train <- var.germany[1:327,]

# Define endogenous and exogenous variables
endo_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Function to fit SZBVAR model with specific hyperparameters
fit_szbvar_with_params <- function(train_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    return(model)
  }, error = function(e) {
    print(paste("Error in model fitting:", e$message))
    return(NULL)
  })
}

# Function to fit model and generate forecasts - following the original structure
fit_and_forecast_szbvar_with_params <- function(train_data, test_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    # Fit the SZBVAR model with given hyperparameters
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    
    # Generate forecasts using last observations of exogenous variables from training data
    forecasts <- forecast(
      model,
      nsteps = nrow(test_data),
      exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, exog_vars])
    )
    
    # Extract only the forecast part
    forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
    
    return(list(
      model = model,
      forecasts = forecasts_only
    ))
  }, error = function(e) {
    print(paste("Error in model fitting or forecasting:", e$message))
    return(NULL)
  })
}

# Function to calculate RMSE for each variable
calculate_rmse <- function(actual, predicted) {
  if(is.null(predicted)) return(Inf)
  
  rmse_values <- sapply(1:ncol(actual), function(i) {
    rmse(actual[,i], predicted[,i])
  })
  names(rmse_values) <- colnames(actual)
  
  # Calculate average RMSE across all variables
  avg_rmse <- mean(rmse_values)
  
  return(list(
    variable_rmse = rmse_values,
    avg_rmse = avg_rmse
  ))
}

# Function to perform hyperparameter tuning
tune_hyperparameters <- function(train_data, val_data) {
  print("Starting hyperparameter tuning...")
  
  # Define hyperparameter grid
  p_values <- c(1, 2, 3, 4)
  lambda0_values <- c(0.2, 0.4, 0.6, 0.8)
  lambda1_values <- c(0.05, 0.1, 0.2)
  lambda3_values <- c(1, 2, 3)
  lambda4_values <- c(0.1, 0.25, 0.5)
  lambda5_values <- c(0, 0.5, 1)
  mu5_values <- c(0, 0.5, 1)
  mu6_values <- c(0, 0.5, 1)
  prior_values <- c(0, 1)
  
  # Initialize results tracking
  best_rmse <- Inf
  best_params <- list()
  results_df <- data.frame()
  
  # Total number of combinations to try
  total_combinations <- length(p_values) * length(lambda0_values) * length(lambda1_values) * 
    length(lambda3_values) * length(lambda4_values) * length(lambda5_values) * 
    length(mu5_values) * length(mu6_values) * length(prior_values)
  
  print(paste("Total hyperparameter combinations to evaluate:", total_combinations))
  
  # Counter for progress tracking
  counter <- 0
  
  # Nested loops for grid search
  for(p in p_values) {
    for(lambda0 in lambda0_values) {
      for(lambda1 in lambda1_values) {
        for(lambda3 in lambda3_values) {
          for(lambda4 in lambda4_values) {
            for(lambda5 in lambda5_values) {
              for(mu5 in mu5_values) {
                for(mu6 in mu6_values) {
                  for(prior in prior_values) {
                    counter <- counter + 1
                    
                    # Print progress
                    if(counter %% 10 == 0) {
                      print(paste("Progress:", counter, "/", total_combinations, 
                                  "combinations evaluated"))
                    }
                    
                    # Fit model and generate forecasts with current hyperparameters
                    results <- fit_and_forecast_szbvar_with_params(
                      train_data, val_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior
                    )
                    
                    # Skip if model fitting or forecasting failed
                    if(is.null(results)) next
                    
                    # Calculate RMSE
                    rmse_results <- calculate_rmse(
                      as.matrix(val_data[, endo_vars]), 
                      as.matrix(results$forecasts)
                    )
                    
                    # Record results
                    current_result <- data.frame(
                      p = p,
                      lambda0 = lambda0,
                      lambda1 = lambda1,
                      lambda3 = lambda3,
                      lambda4 = lambda4,
                      lambda5 = lambda5,
                      mu5 = mu5,
                      mu6 = mu6,
                      prior = prior,
                      avg_rmse = rmse_results$avg_rmse,
                      rmse_unemployment = rmse_results$variable_rmse[1],
                      rmse_eer = rmse_results$variable_rmse[2],
                      rmse_ir = rmse_results$variable_rmse[3],
                      rmse_oil = rmse_results$variable_rmse[4],
                      rmse_cpi = rmse_results$variable_rmse[5]
                    )
                    
                    results_df <- rbind(results_df, current_result)
                    
                    # Update best parameters if current model is better
                    if(rmse_results$avg_rmse < best_rmse) {
                      best_rmse <- rmse_results$avg_rmse
                      best_params <- list(
                        p = p,
                        lambda0 = lambda0,
                        lambda1 = lambda1,
                        lambda3 = lambda3,
                        lambda4 = lambda4,
                        lambda5 = lambda5,
                        mu5 = mu5,
                        mu6 = mu6,
                        prior = prior
                      )
                      
                      print(paste("New best model found! Avg RMSE:", best_rmse))
                      print("Parameters:")
                      print(best_params)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(list(
    best_params = best_params,
    best_rmse = best_rmse,
    all_results = results_df
  ))
}

# Function to generate final forecasts using best hyperparameters
generate_final_forecasts <- function(train_data, test_data, best_params) {
  # Use the combined function to fit model and generate forecasts
  results <- fit_and_forecast_szbvar_with_params(
    train_data,
    test_data,
    best_params$p,
    best_params$lambda0,
    best_params$lambda1,
    best_params$lambda3,
    best_params$lambda4,
    best_params$lambda5,
    best_params$mu5,
    best_params$mu6,
    best_params$prior
  )
  
  if(is.null(results)) {
    stop("Failed to generate final forecasts with best hyperparameters")
  }
  
  return(results)
}

# Main execution block
print("Starting SZBVAR model training and forecasting with hyperparameter tuning")

# 1. Hyperparameter tuning for 12M horizon
print("=== 12M Horizon Hyperparameter Tuning ===")
tune_results_12M <- tune_hyperparameters(var.germany.12M.train, var.germany.12M.val)

# Save tuning results
write.csv(tune_results_12M$all_results, "hp_tuning_results_12M.csv", row.names = FALSE)
print("Best hyperparameters for 12M horizon:")
print(tune_results_12M$best_params)
print(paste("Best average RMSE:", tune_results_12M$best_rmse))

# 2. Hyperparameter tuning for 24M horizon
print("=== 24M Horizon Hyperparameter Tuning ===")
tune_results_24M <- tune_hyperparameters(var.germany.24M.train, var.germany.24M.val)
# Save tuning results
write.csv(tune_results_24M$all_results, "hp_tuning_results_24M.csv", row.names = FALSE)
print("Best hyperparameters for 24M horizon:")
print(tune_results_24M$best_params)
print(paste("Best average RMSE:", tune_results_24M$best_rmse))
##################### End of HP tuning Code ###############################

#### SZBVAR Model: JAPAN: 12M and 24M ahead - HP Tuning ####
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/japan")
getwd()


# Read the dataset
var.japan <- read.csv("all_mulvar_data_japan_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.japan$Date <- as.Date(var.japan$Date)
print("Dataset structure:")
str(var.japan)

# Creation of datasets for 12M forecasts
var.japan.12M.train <- var.japan[1:327,]
var.japan.12M.val <- var.japan[328:339,]
var.japan.12M.test <- var.japan[340:351,]
var.japan.12M.full.train <- var.japan[1:339,]

# Creation of datasets for 24M forecasts
var.japan.24M.train <- var.japan[1:303,]
var.japan.24M.val <- var.japan[304:327,]
var.japan.24M.test <- var.japan[328:351,]
var.japan.24M.full.train <- var.japan[1:327,]

# Define endogenous and exogenous variables
endo_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Function to fit SZBVAR model with specific hyperparameters
fit_szbvar_with_params <- function(train_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    return(model)
  }, error = function(e) {
    print(paste("Error in model fitting:", e$message))
    return(NULL)
  })
}

# Function to fit model and generate forecasts - following the original structure
fit_and_forecast_szbvar_with_params <- function(train_data, test_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    # Fit the SZBVAR model with given hyperparameters
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    
    # Generate forecasts using last observations of exogenous variables from training data
    forecasts <- forecast(
      model,
      nsteps = nrow(test_data),
      exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, exog_vars])
    )
    
    # Extract only the forecast part
    forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
    
    return(list(
      model = model,
      forecasts = forecasts_only
    ))
  }, error = function(e) {
    print(paste("Error in model fitting or forecasting:", e$message))
    return(NULL)
  })
}

# Function to calculate RMSE for each variable
calculate_rmse <- function(actual, predicted) {
  if(is.null(predicted)) return(Inf)
  
  rmse_values <- sapply(1:ncol(actual), function(i) {
    rmse(actual[,i], predicted[,i])
  })
  names(rmse_values) <- colnames(actual)
  
  # Calculate average RMSE across all variables
  avg_rmse <- mean(rmse_values)
  
  return(list(
    variable_rmse = rmse_values,
    avg_rmse = avg_rmse
  ))
}

# Function to perform hyperparameter tuning
tune_hyperparameters <- function(train_data, val_data) {
  print("Starting hyperparameter tuning...")
  
  # Define hyperparameter grid
  p_values <- c(1, 2, 3, 4)
  lambda0_values <- c(0.2, 0.4, 0.6, 0.8)
  lambda1_values <- c(0.05, 0.1, 0.2)
  lambda3_values <- c(1, 2, 3)
  lambda4_values <- c(0.1, 0.25, 0.5)
  lambda5_values <- c(0, 0.5, 1)
  mu5_values <- c(0, 0.5, 1)
  mu6_values <- c(0, 0.5, 1)
  prior_values <- c(0, 1)
  
  # Initialize results tracking
  best_rmse <- Inf
  best_params <- list()
  results_df <- data.frame()
  
  # Total number of combinations to try
  total_combinations <- length(p_values) * length(lambda0_values) * length(lambda1_values) * 
    length(lambda3_values) * length(lambda4_values) * length(lambda5_values) * 
    length(mu5_values) * length(mu6_values) * length(prior_values)
  
  print(paste("Total hyperparameter combinations to evaluate:", total_combinations))
  
  # Counter for progress tracking
  counter <- 0
  
  # Nested loops for grid search
  for(p in p_values) {
    for(lambda0 in lambda0_values) {
      for(lambda1 in lambda1_values) {
        for(lambda3 in lambda3_values) {
          for(lambda4 in lambda4_values) {
            for(lambda5 in lambda5_values) {
              for(mu5 in mu5_values) {
                for(mu6 in mu6_values) {
                  for(prior in prior_values) {
                    counter <- counter + 1
                    
                    # Print progress
                    if(counter %% 10 == 0) {
                      print(paste("Progress:", counter, "/", total_combinations, 
                                  "combinations evaluated"))
                    }
                    
                    # Fit model and generate forecasts with current hyperparameters
                    results <- fit_and_forecast_szbvar_with_params(
                      train_data, val_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior
                    )
                    
                    # Skip if model fitting or forecasting failed
                    if(is.null(results)) next
                    
                    # Calculate RMSE
                    rmse_results <- calculate_rmse(
                      as.matrix(val_data[, endo_vars]), 
                      as.matrix(results$forecasts)
                    )
                    
                    # Record results
                    current_result <- data.frame(
                      p = p,
                      lambda0 = lambda0,
                      lambda1 = lambda1,
                      lambda3 = lambda3,
                      lambda4 = lambda4,
                      lambda5 = lambda5,
                      mu5 = mu5,
                      mu6 = mu6,
                      prior = prior,
                      avg_rmse = rmse_results$avg_rmse,
                      rmse_unemployment = rmse_results$variable_rmse[1],
                      rmse_eer = rmse_results$variable_rmse[2],
                      rmse_ir = rmse_results$variable_rmse[3],
                      rmse_oil = rmse_results$variable_rmse[4],
                      rmse_cpi = rmse_results$variable_rmse[5]
                    )
                    
                    results_df <- rbind(results_df, current_result)
                    
                    # Update best parameters if current model is better
                    if(rmse_results$avg_rmse < best_rmse) {
                      best_rmse <- rmse_results$avg_rmse
                      best_params <- list(
                        p = p,
                        lambda0 = lambda0,
                        lambda1 = lambda1,
                        lambda3 = lambda3,
                        lambda4 = lambda4,
                        lambda5 = lambda5,
                        mu5 = mu5,
                        mu6 = mu6,
                        prior = prior
                      )
                      
                      print(paste("New best model found! Avg RMSE:", best_rmse))
                      print("Parameters:")
                      print(best_params)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(list(
    best_params = best_params,
    best_rmse = best_rmse,
    all_results = results_df
  ))
}

# Function to generate final forecasts using best hyperparameters
generate_final_forecasts <- function(train_data, test_data, best_params) {
  # Use the combined function to fit model and generate forecasts
  results <- fit_and_forecast_szbvar_with_params(
    train_data,
    test_data,
    best_params$p,
    best_params$lambda0,
    best_params$lambda1,
    best_params$lambda3,
    best_params$lambda4,
    best_params$lambda5,
    best_params$mu5,
    best_params$mu6,
    best_params$prior
  )
  
  if(is.null(results)) {
    stop("Failed to generate final forecasts with best hyperparameters")
  }
  
  return(results)
}

# Main execution block
print("Starting SZBVAR model training and forecasting with hyperparameter tuning")

# 1. Hyperparameter tuning for 12M horizon
print("=== 12M Horizon Hyperparameter Tuning ===")
tune_results_12M <- tune_hyperparameters(var.japan.12M.train, var.japan.12M.val)

# Save tuning results
write.csv(tune_results_12M$all_results, "hp_tuning_results_12M.csv", row.names = FALSE)
print("Best hyperparameters for 12M horizon:")
print(tune_results_12M$best_params)
print(paste("Best average RMSE:", tune_results_12M$best_rmse))

# 2. Hyperparameter tuning for 24M horizon
print("=== 24M Horizon Hyperparameter Tuning ===")
tune_results_24M <- tune_hyperparameters(var.japan.24M.train, var.japan.24M.val)
# Save tuning results
write.csv(tune_results_24M$all_results, "hp_tuning_results_24M.csv", row.names = FALSE)
print("Best hyperparameters for 24M horizon:")
print(tune_results_24M$best_params)
print(paste("Best average RMSE:", tune_results_24M$best_rmse))
##################### End of HP tuning Code ###############################

#### SZBVAR Model: UK: 12M and 24M ahead - HP Tuning ####
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/uk")
getwd()

# Read the dataset
var.uk <- read.csv("all_mulvar_data_uk_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.uk$Date <- as.Date(var.uk$Date)
print("Dataset structure:")
str(var.uk)

# Creation of datasets for 12M forecasts
var.uk.12M.train <- var.uk[1:327,]
var.uk.12M.val <- var.uk[328:339,]
var.uk.12M.test <- var.uk[340:351,]
var.uk.12M.full.train <- var.uk[1:339,]

# Creation of datasets for 24M forecasts
var.uk.24M.train <- var.uk[1:303,]
var.uk.24M.val <- var.uk[304:327,]
var.uk.24M.test <- var.uk[328:351,]
var.uk.24M.full.train <- var.uk[1:327,]

# Define endogenous and exogenous variables
endo_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Function to fit SZBVAR model with specific hyperparameters
fit_szbvar_with_params <- function(train_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    return(model)
  }, error = function(e) {
    print(paste("Error in model fitting:", e$message))
    return(NULL)
  })
}

# Function to fit model and generate forecasts - following the original structure
fit_and_forecast_szbvar_with_params <- function(train_data, test_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    # Fit the SZBVAR model with given hyperparameters
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    
    # Generate forecasts using last observations of exogenous variables from training data
    forecasts <- forecast(
      model,
      nsteps = nrow(test_data),
      exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, exog_vars])
    )
    
    # Extract only the forecast part
    forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
    
    return(list(
      model = model,
      forecasts = forecasts_only
    ))
  }, error = function(e) {
    print(paste("Error in model fitting or forecasting:", e$message))
    return(NULL)
  })
}

# Function to calculate RMSE for each variable
calculate_rmse <- function(actual, predicted) {
  if(is.null(predicted)) return(Inf)
  
  rmse_values <- sapply(1:ncol(actual), function(i) {
    rmse(actual[,i], predicted[,i])
  })
  names(rmse_values) <- colnames(actual)
  
  # Calculate average RMSE across all variables
  avg_rmse <- mean(rmse_values)
  
  return(list(
    variable_rmse = rmse_values,
    avg_rmse = avg_rmse
  ))
}

# Function to perform hyperparameter tuning
tune_hyperparameters <- function(train_data, val_data) {
  print("Starting hyperparameter tuning...")
  
  # Define hyperparameter grid
  p_values <- c(1, 2, 3, 4)
  lambda0_values <- c(0.2, 0.4, 0.6, 0.8)
  lambda1_values <- c(0.05, 0.1, 0.2)
  lambda3_values <- c(1, 2, 3)
  lambda4_values <- c(0.1, 0.25, 0.5)
  lambda5_values <- c(0, 0.5, 1)
  mu5_values <- c(0, 0.5, 1)
  mu6_values <- c(0, 0.5, 1)
  prior_values <- c(0, 1)
  
  # Initialize results tracking
  best_rmse <- Inf
  best_params <- list()
  results_df <- data.frame()
  
  # Total number of combinations to try
  total_combinations <- length(p_values) * length(lambda0_values) * length(lambda1_values) * 
    length(lambda3_values) * length(lambda4_values) * length(lambda5_values) * 
    length(mu5_values) * length(mu6_values) * length(prior_values)
  
  print(paste("Total hyperparameter combinations to evaluate:", total_combinations))
  
  # Counter for progress tracking
  counter <- 0
  
  # Nested loops for grid search
  for(p in p_values) {
    for(lambda0 in lambda0_values) {
      for(lambda1 in lambda1_values) {
        for(lambda3 in lambda3_values) {
          for(lambda4 in lambda4_values) {
            for(lambda5 in lambda5_values) {
              for(mu5 in mu5_values) {
                for(mu6 in mu6_values) {
                  for(prior in prior_values) {
                    counter <- counter + 1
                    
                    # Print progress
                    if(counter %% 10 == 0) {
                      print(paste("Progress:", counter, "/", total_combinations, 
                                  "combinations evaluated"))
                    }
                    
                    # Fit model and generate forecasts with current hyperparameters
                    results <- fit_and_forecast_szbvar_with_params(
                      train_data, val_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior
                    )
                    
                    # Skip if model fitting or forecasting failed
                    if(is.null(results)) next
                    
                    # Calculate RMSE
                    rmse_results <- calculate_rmse(
                      as.matrix(val_data[, endo_vars]), 
                      as.matrix(results$forecasts)
                    )
                    
                    # Record results
                    current_result <- data.frame(
                      p = p,
                      lambda0 = lambda0,
                      lambda1 = lambda1,
                      lambda3 = lambda3,
                      lambda4 = lambda4,
                      lambda5 = lambda5,
                      mu5 = mu5,
                      mu6 = mu6,
                      prior = prior,
                      avg_rmse = rmse_results$avg_rmse,
                      rmse_unemployment = rmse_results$variable_rmse[1],
                      rmse_eer = rmse_results$variable_rmse[2],
                      rmse_ir = rmse_results$variable_rmse[3],
                      rmse_oil = rmse_results$variable_rmse[4],
                      rmse_cpi = rmse_results$variable_rmse[5]
                    )
                    
                    results_df <- rbind(results_df, current_result)
                    
                    # Update best parameters if current model is better
                    if(rmse_results$avg_rmse < best_rmse) {
                      best_rmse <- rmse_results$avg_rmse
                      best_params <- list(
                        p = p,
                        lambda0 = lambda0,
                        lambda1 = lambda1,
                        lambda3 = lambda3,
                        lambda4 = lambda4,
                        lambda5 = lambda5,
                        mu5 = mu5,
                        mu6 = mu6,
                        prior = prior
                      )
                      
                      print(paste("New best model found! Avg RMSE:", best_rmse))
                      print("Parameters:")
                      print(best_params)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(list(
    best_params = best_params,
    best_rmse = best_rmse,
    all_results = results_df
  ))
}

# Function to generate final forecasts using best hyperparameters
generate_final_forecasts <- function(train_data, test_data, best_params) {
  # Use the combined function to fit model and generate forecasts
  results <- fit_and_forecast_szbvar_with_params(
    train_data,
    test_data,
    best_params$p,
    best_params$lambda0,
    best_params$lambda1,
    best_params$lambda3,
    best_params$lambda4,
    best_params$lambda5,
    best_params$mu5,
    best_params$mu6,
    best_params$prior
  )
  
  if(is.null(results)) {
    stop("Failed to generate final forecasts with best hyperparameters")
  }
  
  return(results)
}

# Main execution block
print("Starting SZBVAR model training and forecasting with hyperparameter tuning")

# 1. Hyperparameter tuning for 12M horizon
print("=== 12M Horizon Hyperparameter Tuning ===")
tune_results_12M <- tune_hyperparameters(var.uk.12M.train, var.uk.12M.val)

# Save tuning results
write.csv(tune_results_12M$all_results, "hp_tuning_results_12M.csv", row.names = FALSE)
print("Best hyperparameters for 12M horizon:")
print(tune_results_12M$best_params)
print(paste("Best average RMSE:", tune_results_12M$best_rmse))

# 2. Hyperparameter tuning for 24M horizon
print("=== 24M Horizon Hyperparameter Tuning ===")
tune_results_24M <- tune_hyperparameters(var.uk.24M.train, var.uk.24M.val)
# Save tuning results
write.csv(tune_results_24M$all_results, "hp_tuning_results_24M.csv", row.names = FALSE)
print("Best hyperparameters for 24M horizon:")
print(tune_results_24M$best_params)
print(paste("Best average RMSE:", tune_results_24M$best_rmse))
##################### End of HP tuning Code ###############################

#### SZBVAR Model: ITALY: 12M and 24M ahead - HP Tuning ####
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/italy")
getwd()

# Read the dataset
var.italy <- read.csv("all_mulvar_data_italy_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.italy$Date <- as.Date(var.italy$Date)
print("Dataset structure:")
str(var.italy)

# Creation of datasets for 12M forecasts
var.italy.12M.train <- var.italy[1:327,]
var.italy.12M.val <- var.italy[328:339,]
var.italy.12M.test <- var.italy[340:351,]
var.italy.12M.full.train <- var.italy[1:339,]

# Creation of datasets for 24M forecasts
var.italy.24M.train <- var.italy[1:303,]
var.italy.24M.val <- var.italy[304:327,]
var.italy.24M.test <- var.italy[328:351,]
var.italy.24M.full.train <- var.italy[1:327,]

# Define endogenous and exogenous variables
endo_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Function to fit SZBVAR model with specific hyperparameters
fit_szbvar_with_params <- function(train_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    return(model)
  }, error = function(e) {
    print(paste("Error in model fitting:", e$message))
    return(NULL)
  })
}

# Function to fit model and generate forecasts - following the original structure
fit_and_forecast_szbvar_with_params <- function(train_data, test_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior) {
  tryCatch({
    # Fit the SZBVAR model with given hyperparameters
    model <- szbvar(
      Y = ts(train_data[, endo_vars]),
      p = p,
      z = ts(train_data[, exog_vars]),
      lambda0 = lambda0,
      lambda1 = lambda1,
      lambda3 = lambda3,
      lambda4 = lambda4,
      lambda5 = lambda5,
      mu5 = mu5,
      mu6 = mu6,
      prior = prior
    )
    
    # Generate forecasts using last observations of exogenous variables from training data
    forecasts <- forecast(
      model,
      nsteps = nrow(test_data),
      exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, exog_vars])
    )
    
    # Extract only the forecast part
    forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
    
    return(list(
      model = model,
      forecasts = forecasts_only
    ))
  }, error = function(e) {
    print(paste("Error in model fitting or forecasting:", e$message))
    return(NULL)
  })
}

# Function to calculate RMSE for each variable
calculate_rmse <- function(actual, predicted) {
  if(is.null(predicted)) return(Inf)
  
  rmse_values <- sapply(1:ncol(actual), function(i) {
    rmse(actual[,i], predicted[,i])
  })
  names(rmse_values) <- colnames(actual)
  
  # Calculate average RMSE across all variables
  avg_rmse <- mean(rmse_values)
  
  return(list(
    variable_rmse = rmse_values,
    avg_rmse = avg_rmse
  ))
}

# Function to perform hyperparameter tuning
tune_hyperparameters <- function(train_data, val_data) {
  print("Starting hyperparameter tuning...")
  
  # Define hyperparameter grid
  p_values <- c(1, 2, 3, 4)
  lambda0_values <- c(0.2, 0.4, 0.6, 0.8)
  lambda1_values <- c(0.05, 0.1, 0.2)
  lambda3_values <- c(1, 2, 3)
  lambda4_values <- c(0.1, 0.25, 0.5)
  lambda5_values <- c(0, 0.5, 1)
  mu5_values <- c(0, 0.5, 1)
  mu6_values <- c(0, 0.5, 1)
  prior_values <- c(0, 1)
  
  # Initialize results tracking
  best_rmse <- Inf
  best_params <- list()
  results_df <- data.frame()
  
  # Total number of combinations to try
  total_combinations <- length(p_values) * length(lambda0_values) * length(lambda1_values) * 
    length(lambda3_values) * length(lambda4_values) * length(lambda5_values) * 
    length(mu5_values) * length(mu6_values) * length(prior_values)
  
  print(paste("Total hyperparameter combinations to evaluate:", total_combinations))
  
  # Counter for progress tracking
  counter <- 0
  
  # Nested loops for grid search
  for(p in p_values) {
    for(lambda0 in lambda0_values) {
      for(lambda1 in lambda1_values) {
        for(lambda3 in lambda3_values) {
          for(lambda4 in lambda4_values) {
            for(lambda5 in lambda5_values) {
              for(mu5 in mu5_values) {
                for(mu6 in mu6_values) {
                  for(prior in prior_values) {
                    counter <- counter + 1
                    
                    # Print progress
                    if(counter %% 10 == 0) {
                      print(paste("Progress:", counter, "/", total_combinations, 
                                  "combinations evaluated"))
                    }
                    
                    # Fit model and generate forecasts with current hyperparameters
                    results <- fit_and_forecast_szbvar_with_params(
                      train_data, val_data, p, lambda0, lambda1, lambda3, lambda4, lambda5, mu5, mu6, prior
                    )
                    
                    # Skip if model fitting or forecasting failed
                    if(is.null(results)) next
                    
                    # Calculate RMSE
                    rmse_results <- calculate_rmse(
                      as.matrix(val_data[, endo_vars]), 
                      as.matrix(results$forecasts)
                    )
                    
                    # Record results
                    current_result <- data.frame(
                      p = p,
                      lambda0 = lambda0,
                      lambda1 = lambda1,
                      lambda3 = lambda3,
                      lambda4 = lambda4,
                      lambda5 = lambda5,
                      mu5 = mu5,
                      mu6 = mu6,
                      prior = prior,
                      avg_rmse = rmse_results$avg_rmse,
                      rmse_unemployment = rmse_results$variable_rmse[1],
                      rmse_eer = rmse_results$variable_rmse[2],
                      rmse_ir = rmse_results$variable_rmse[3],
                      rmse_oil = rmse_results$variable_rmse[4],
                      rmse_cpi = rmse_results$variable_rmse[5]
                    )
                    
                    results_df <- rbind(results_df, current_result)
                    
                    # Update best parameters if current model is better
                    if(rmse_results$avg_rmse < best_rmse) {
                      best_rmse <- rmse_results$avg_rmse
                      best_params <- list(
                        p = p,
                        lambda0 = lambda0,
                        lambda1 = lambda1,
                        lambda3 = lambda3,
                        lambda4 = lambda4,
                        lambda5 = lambda5,
                        mu5 = mu5,
                        mu6 = mu6,
                        prior = prior
                      )
                      
                      print(paste("New best model found! Avg RMSE:", best_rmse))
                      print("Parameters:")
                      print(best_params)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(list(
    best_params = best_params,
    best_rmse = best_rmse,
    all_results = results_df
  ))
}

# Function to generate final forecasts using best hyperparameters
generate_final_forecasts <- function(train_data, test_data, best_params) {
  # Use the combined function to fit model and generate forecasts
  results <- fit_and_forecast_szbvar_with_params(
    train_data,
    test_data,
    best_params$p,
    best_params$lambda0,
    best_params$lambda1,
    best_params$lambda3,
    best_params$lambda4,
    best_params$lambda5,
    best_params$mu5,
    best_params$mu6,
    best_params$prior
  )
  
  if(is.null(results)) {
    stop("Failed to generate final forecasts with best hyperparameters")
  }
  
  return(results)
}

# Main execution block
print("Starting SZBVAR model training and forecasting with hyperparameter tuning")

# 1. Hyperparameter tuning for 12M horizon
print("=== 12M Horizon Hyperparameter Tuning ===")
tune_results_12M <- tune_hyperparameters(var.italy.12M.train, var.italy.12M.val)

# Save tuning results
write.csv(tune_results_12M$all_results, "hp_tuning_results_12M.csv", row.names = FALSE)
print("Best hyperparameters for 12M horizon:")
print(tune_results_12M$best_params)
print(paste("Best average RMSE:", tune_results_12M$best_rmse))

# 2. Hyperparameter tuning for 24M horizon
print("=== 24M Horizon Hyperparameter Tuning ===")
tune_results_24M <- tune_hyperparameters(var.italy.24M.train, var.italy.24M.val)
# Save tuning results
write.csv(tune_results_24M$all_results, "hp_tuning_results_24M.csv", row.names = FALSE)
print("Best hyperparameters for 24M horizon:")
print(tune_results_24M$best_params)
print(paste("Best average RMSE:", tune_results_24M$best_rmse))
##################### End of HP tuning Code ###############################