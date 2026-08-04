############## VAR Model: Canada: 12M and 24M ahead - forecasts ####################
#========================================================
# BigVAR model for canada data (12M forecasts)
#========================================================
# Load the relevant libraries
library(BigVAR)
library(vars)

# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/canada")
# setwd("/Users/shovonsengupta/Desktop/ALL_2026/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/Macrocasting/dataset/canada")
getwd()

#========================================================
# BigVAR model for canada data (12M forward forecasts for 5 endogenous variables)
#========================================================
# Read the canada dataset
canada_data <- read.csv("all_mulvar_data_canada_v2.csv", header = TRUE)
str(canada_data)

# Convert Date into Date type
canada_data$Date <- as.Date(canada_data$Date)

# Display structure of the data
str(canada_data)

# Split the data into train and test sets
canada_data_train <- canada_data[1:339,]  # Training data
canada_data_test <- canada_data[340:351,] # Test data
str(canada_data_train)

# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  canada_data_train$CPIinflationrate,       # Endogenous variable
  canada_data_train$Unemploymentrate,       # Endogenous variable
  canada_data_train$RealbroadEER,           # Endogenous variable
  canada_data_train$ShorttermIR,            # Endogenous variable
  canada_data_train$OilpriceGlobalWTI,      # Endogenous variable
  canada_data_train$logEPU,                 # Exogenous variable
  canada_data_train$GPRC,                   # Exogenous variable
  canada_data_train$USEMV,                  # Exogenous variable
  canada_data_train$USMPU                   # Exogenous variable
))

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                      # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 12)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 12-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_12m <- as.data.frame(multi_step_forecasts)
names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_12m
write.csv(forecast_df_12m, "forecast_12M_BigVAR_canada_new.csv", row.names = FALSE)


#========================================================
# BigVAR model for canada data (24M forecasts)
#========================================================
# Split the data into train and test sets
canada_data_train <- canada_data[1:327,]  # Training data
canada_data_test <- canada_data[328:351,] # Test data
str(canada_data_train)
str(canada_data_test)


# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  canada_data_train$CPIinflationrate,       # Endogenous variable
  canada_data_train$Unemploymentrate,       # Endogenous variable
  canada_data_train$RealbroadEER,           # Endogenous variable
  canada_data_train$ShorttermIR,            # Endogenous variable
  canada_data_train$OilpriceGlobalWTI,      # Endogenous variable
  canada_data_train$logEPU,                 # Exogenous variable
  canada_data_train$GPRC,                   # Exogenous variable
  canada_data_train$USEMV,                  # Exogenous variable
  canada_data_train$USMPU                   # Exogenous variable
))

str(Y)

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                      # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 24)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 24-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_24m <- as.data.frame(multi_step_forecasts)
names(forecast_df_24m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_24m
write.csv(forecast_df_24m, "forecast_24M_BigVAR_canada_new.csv", row.names = FALSE)
###################### End of Code #######################

#========================================================
# BigVAR model for USA data (12M forecasts)
#========================================================
# Load the relevant libraries
library(BigVAR)
library(vars)

# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/usa")
getwd()

#========================================================
# BigVAR model for USA data (12M forward forecasts for 5 endogenous variables)
#========================================================
# Read the USA dataset
usa_data <- read.csv("all_mulvar_data_usa_v2.csv", header = TRUE)

# Convert Date into Date type
usa_data$Date <- as.Date(usa_data$Date)

# Display structure of the data
str(usa_data)

# Split the data into train and test sets
usa_data_train <- usa_data[1:339,]  # Training data
usa_data_test <- usa_data[340:351,] # Test data
str(usa_data_train)


# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  usa_data_train$CPIinflationrate,       # Endogenous variable
  usa_data_train$Unemploymentrate,       # Endogenous variable
  usa_data_train$RealbroadEER,           # Endogenous variable
  usa_data_train$ShorttermIR,            # Endogenous variable
  usa_data_train$OilpriceGlobalWTI,      # Endogenous variable
  usa_data_train$logEPU,                 # Exogenous variable
  usa_data_train$GPRC,                   # Exogenous variable
  usa_data_train$USEMV,                  # Exogenous variable
  usa_data_train$USMPU                   # Exogenous variable
))

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                    # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 12)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 12-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_12m <- as.data.frame(multi_step_forecasts)
names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_12m
write.csv(forecast_df_12m, "forecast_12M_BigVAR_usa_new.csv", row.names = FALSE)


#========================================================
# BigVAR model for USA data (24M forecasts)
#========================================================
# Split the data into train and test sets
usa_data_train <- usa_data[1:327,]  # Training data
usa_data_test <- usa_data[328:351,] # Test data
str(usa_data_train)
str(usa_data_test)


# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  usa_data_train$CPIinflationrate,       # Endogenous variable
  usa_data_train$Unemploymentrate,       # Endogenous variable
  usa_data_train$RealbroadEER,           # Endogenous variable
  usa_data_train$ShorttermIR,            # Endogenous variable
  usa_data_train$OilpriceGlobalWTI,      # Endogenous variable
  usa_data_train$logEPU,                 # Exogenous variable
  usa_data_train$GPRC,                   # Exogenous variable
  usa_data_train$USEMV,                  # Exogenous variable
  usa_data_train$USMPU                   # Exogenous variable
))

str(Y)

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                    # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 24)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 24-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_24m <- as.data.frame(multi_step_forecasts)
names(forecast_df_24m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_24m
write.csv(forecast_df_24m, "forecast_24M_BigVAR_usa_new.csv", row.names = FALSE)

###################### End of Code #######################
#========================================================
# BigVAR model for france data (12M forecasts)
#========================================================
# Load the relevant libraries
library(BigVAR)
library(vars)

# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/france")
getwd()

#========================================================
# BigVAR model for france data (12M forward forecasts for 5 endogenous variables)
#========================================================
# Read the france dataset
france_data <- read.csv("all_mulvar_data_france_v2.csv", header = TRUE)

# Convert Date into Date type
france_data$Date <- as.Date(france_data$Date)

# Display structure of the data
str(france_data)

# Split the data into train and test sets
france_data_train <- france_data[1:339,]  # Training data
france_data_test <- france_data[340:351,] # Test data
str(france_data_train)


# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  france_data_train$CPIinflationrate,       # Endogenous variable
  france_data_train$Unemploymentrate,       # Endogenous variable
  france_data_train$RealbroadEER,           # Endogenous variable
  france_data_train$ShorttermIR,            # Endogenous variable
  france_data_train$OilpriceGlobalWTI,      # Endogenous variable
  france_data_train$logEPU,                 # Exogenous variable
  france_data_train$GPRC,                   # Exogenous variable
  france_data_train$USEMV,                  # Exogenous variable
  france_data_train$USMPU                   # Exogenous variable
))

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                    # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 12)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 12-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_12m <- as.data.frame(multi_step_forecasts)
names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_12m
write.csv(forecast_df_12m, "forecast_12M_BigVAR_france_new.csv", row.names = FALSE)


#========================================================
# BigVAR model for france data (24M forecasts)
#========================================================
# Split the data into train and test sets
france_data_train <- france_data[1:327,]  # Training data
france_data_test <- france_data[328:351,] # Test data
str(france_data_train)
str(france_data_test)


# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  france_data_train$CPIinflationrate,       # Endogenous variable
  france_data_train$Unemploymentrate,       # Endogenous variable
  france_data_train$RealbroadEER,           # Endogenous variable
  france_data_train$ShorttermIR,            # Endogenous variable
  france_data_train$OilpriceGlobalWTI,      # Endogenous variable
  france_data_train$logEPU,                 # Exogenous variable
  france_data_train$GPRC,                   # Exogenous variable
  france_data_train$USEMV,                  # Exogenous variable
  france_data_train$USMPU                   # Exogenous variable
))

str(Y)

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                    # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 24)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 24-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_24m <- as.data.frame(multi_step_forecasts)
names(forecast_df_24m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_24m
write.csv(forecast_df_24m, "forecast_24M_BigVAR_france_new.csv", row.names = FALSE)

###################### End of Code #######################
#========================================================
# BigVAR model for germany data (12M forecasts)
#========================================================
# Load the relevant libraries
library(BigVAR)
library(vars)

# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/germany")
getwd()

#========================================================
# BigVAR model for germany data (12M forward forecasts for 5 endogenous variables)
#========================================================
# Read the germany dataset
germany_data <- read.csv("all_mulvar_data_germany_v2.csv", header = TRUE)

# Convert Date into Date type
germany_data$Date <- as.Date(germany_data$Date)

# Display structure of the data
str(germany_data)

# Split the data into train and test sets
germany_data_train <- germany_data[1:339,]  # Training data
germany_data_test <- germany_data[340:351,] # Test data
str(germany_data_train)


# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  germany_data_train$CPIinflationrate,       # Endogenous variable
  germany_data_train$Unemploymentrate,       # Endogenous variable
  germany_data_train$RealbroadEER,           # Endogenous variable
  germany_data_train$ShorttermIR,            # Endogenous variable
  germany_data_train$OilpriceGlobalWTI,      # Endogenous variable
  germany_data_train$logEPU,                 # Exogenous variable
  germany_data_train$GPRC,                   # Exogenous variable
  germany_data_train$USEMV,                  # Exogenous variable
  germany_data_train$USMPU                   # Exogenous variable
))

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                    # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 12)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 12-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_12m <- as.data.frame(multi_step_forecasts)
names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_12m
write.csv(forecast_df_12m, "forecast_12M_BigVAR_germany_new.csv", row.names = FALSE)


#========================================================
# BigVAR model for germany data (24M forecasts)
#========================================================
# Split the data into train and test sets
germany_data_train <- germany_data[1:327,]  # Training data
germany_data_test <- germany_data[328:351,] # Test data
str(germany_data_train)
str(germany_data_test)


# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  germany_data_train$CPIinflationrate,       # Endogenous variable
  germany_data_train$Unemploymentrate,       # Endogenous variable
  germany_data_train$RealbroadEER,           # Endogenous variable
  germany_data_train$ShorttermIR,            # Endogenous variable
  germany_data_train$OilpriceGlobalWTI,      # Endogenous variable
  germany_data_train$logEPU,                 # Exogenous variable
  germany_data_train$GPRC,                   # Exogenous variable
  germany_data_train$USEMV,                  # Exogenous variable
  germany_data_train$USMPU                   # Exogenous variable
))

str(Y)

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                    # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 24)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 24-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_24m <- as.data.frame(multi_step_forecasts)
names(forecast_df_24m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_24m
write.csv(forecast_df_24m, "forecast_24M_BigVAR_germany_new.csv", row.names = FALSE)

###################### End of Code #######################
#========================================================
# BigVAR model for japan data (12M forecasts)
#========================================================
# Load the relevant libraries
library(BigVAR)
library(vars)

# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/japan")
getwd()

#========================================================
# BigVAR model for japan data (12M forward forecasts for 5 endogenous variables)
#========================================================
# Read the japan dataset
japan_data <- read.csv("all_mulvar_data_japan_v2.csv", header = TRUE)

# Convert Date into Date type
japan_data$Date <- as.Date(japan_data$Date)

# Display structure of the data
str(japan_data)

# Split the data into train and test sets
japan_data_train <- japan_data[1:339,]  # Training data
japan_data_test <- japan_data[340:351,] # Test data
str(japan_data_train)


# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  japan_data_train$CPIinflationrate,       # Endogenous variable
  japan_data_train$Unemploymentrate,       # Endogenous variable
  japan_data_train$RealbroadEER,           # Endogenous variable
  japan_data_train$ShorttermIR,            # Endogenous variable
  japan_data_train$OilpriceGlobalWTI,      # Endogenous variable
  japan_data_train$logEPU,                 # Exogenous variable
  japan_data_train$GPRC,                   # Exogenous variable
  japan_data_train$USEMV,                  # Exogenous variable
  japan_data_train$USMPU                   # Exogenous variable
))

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                    # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 12)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 12-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_12m <- as.data.frame(multi_step_forecasts)
names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_12m
write.csv(forecast_df_12m, "forecast_12M_BigVAR_japan_new.csv", row.names = FALSE)


#========================================================
# BigVAR model for japan data (24M forecasts)
#========================================================
# Split the data into train and test sets
japan_data_train <- japan_data[1:327,]  # Training data
japan_data_test <- japan_data[328:351,] # Test data
str(japan_data_train)
str(japan_data_test)


# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  japan_data_train$CPIinflationrate,       # Endogenous variable
  japan_data_train$Unemploymentrate,       # Endogenous variable
  japan_data_train$RealbroadEER,           # Endogenous variable
  japan_data_train$ShorttermIR,            # Endogenous variable
  japan_data_train$OilpriceGlobalWTI,      # Endogenous variable
  japan_data_train$logEPU,                 # Exogenous variable
  japan_data_train$GPRC,                   # Exogenous variable
  japan_data_train$USEMV,                  # Exogenous variable
  japan_data_train$USMPU                   # Exogenous variable
))

str(Y)

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                    # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 24)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 24-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_24m <- as.data.frame(multi_step_forecasts)
names(forecast_df_24m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_24m
write.csv(forecast_df_24m, "forecast_24M_BigVAR_japan_new.csv", row.names = FALSE)
###################### End of Code #######################

#========================================================
# BigVAR model for uk data (12M forecasts)
#========================================================
# Load the relevant libraries
library(BigVAR)
library(vars)

# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/uk")
getwd()

#========================================================
# BigVAR model for uk data (12M forward forecasts for 5 endogenous variables)
#========================================================
# Read the uk dataset
uk_data <- read.csv("all_mulvar_data_uk_v2.csv", header = TRUE)

# Convert Date into Date type
uk_data$Date <- as.Date(uk_data$Date)

# Display structure of the data
str(uk_data)

# Split the data into train and test sets
uk_data_train <- uk_data[1:339,]  # Training data
uk_data_test <- uk_data[340:351,] # Test data
str(uk_data_train)


# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  uk_data_train$CPIinflationrate,       # Endogenous variable
  uk_data_train$Unemploymentrate,       # Endogenous variable
  uk_data_train$RealbroadEER,           # Endogenous variable
  uk_data_train$ShorttermIR,            # Endogenous variable
  uk_data_train$OilpriceGlobalWTI,      # Endogenous variable
  uk_data_train$logEPU,                 # Exogenous variable
  uk_data_train$GPRC,                   # Exogenous variable
  uk_data_train$USEMV,                  # Exogenous variable
  uk_data_train$USMPU                   # Exogenous variable
))

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                    # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 12)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 12-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_12m <- as.data.frame(multi_step_forecasts)
names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_12m
write.csv(forecast_df_12m, "forecast_12M_BigVAR_uk_new.csv", row.names = FALSE)


#========================================================
# BigVAR model for uk data (24M forecasts)
#========================================================
# Split the data into train and test sets
uk_data_train <- uk_data[1:327,]  # Training data
uk_data_test <- uk_data[328:351,] # Test data
str(uk_data_train)
str(uk_data_test)


# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  uk_data_train$CPIinflationrate,       # Endogenous variable
  uk_data_train$Unemploymentrate,       # Endogenous variable
  uk_data_train$RealbroadEER,           # Endogenous variable
  uk_data_train$ShorttermIR,            # Endogenous variable
  uk_data_train$OilpriceGlobalWTI,      # Endogenous variable
  uk_data_train$logEPU,                 # Exogenous variable
  uk_data_train$GPRC,                   # Exogenous variable
  uk_data_train$USEMV,                  # Exogenous variable
  uk_data_train$USMPU                   # Exogenous variable
))

str(Y)

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                    # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 24)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 24-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_24m <- as.data.frame(multi_step_forecasts)
names(forecast_df_24m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_24m
write.csv(forecast_df_24m, "forecast_24M_BigVAR_uk_new.csv", row.names = FALSE)

###################### End of Code #######################
#========================================================
# BigVAR model for italy data (12M forecasts)
#========================================================
# Load the relevant libraries
library(BigVAR)
library(vars)

# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/italy")
getwd()

#========================================================
# BigVAR model for italy data (12M forward forecasts for 5 endogenous variables)
#========================================================
# Read the italy dataset
italy_data <- read.csv("all_mulvar_data_italy_v2.csv", header = TRUE)

# Convert Date into Date type
italy_data$Date <- as.Date(italy_data$Date)

# Display structure of the data
str(italy_data)

# Split the data into train and test sets
italy_data_train <- italy_data[1:339,]  # Training data
italy_data_test <- italy_data[340:351,] # Test data
str(italy_data_train)


# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  italy_data_train$CPIinflationrate,       # Endogenous variable
  italy_data_train$Unemploymentrate,       # Endogenous variable
  italy_data_train$RealbroadEER,           # Endogenous variable
  italy_data_train$ShorttermIR,            # Endogenous variable
  italy_data_train$OilpriceGlobalWTI,      # Endogenous variable
  italy_data_train$logEPU,                 # Exogenous variable
  italy_data_train$GPRC,                   # Exogenous variable
  italy_data_train$USEMV,                  # Exogenous variable
  italy_data_train$USMPU                   # Exogenous variable
))

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                    # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 12)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 12-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_12m <- as.data.frame(multi_step_forecasts)
names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_12m
write.csv(forecast_df_12m, "forecast_12M_BigVAR_italy_new.csv", row.names = FALSE)


#========================================================
# BigVAR model for italy data (24M forecasts)
#========================================================
# Split the data into train and test sets
italy_data_train <- italy_data[1:327,]  # Training data
italy_data_test <- italy_data[328:351,] # Test data
str(italy_data_train)
str(italy_data_test)


# Convert the dataset into a matrix for BigVAR (endogenous variables only)
# Ensure the data is structured as a multivariate time series (T × k)
Y <- as.matrix(cbind(
  italy_data_train$CPIinflationrate,       # Endogenous variable
  italy_data_train$Unemploymentrate,       # Endogenous variable
  italy_data_train$RealbroadEER,           # Endogenous variable
  italy_data_train$ShorttermIR,            # Endogenous variable
  italy_data_train$OilpriceGlobalWTI,      # Endogenous variable
  italy_data_train$logEPU,                 # Exogenous variable
  italy_data_train$GPRC,                   # Exogenous variable
  italy_data_train$USEMV,                  # Exogenous variable
  italy_data_train$USMPU                   # Exogenous variable
))

str(Y)

# Define the parameters
p <- 4                                # Maximum lag order
# struct <- "Basic"                   # Basic VAR structure
# struct <- "MCP"                     # Minimax Concave Penalty (cf. Breheny and Huang))
struct <- "SCAD"                    # Smoothly Clipped Absolute Deviation (cf. Breheny and Huang)
gran <- c(5, 10)                      # Penalty grid
h <- 1                                # One-step forecast (we will manually do recursive)

# Construct the BigVAR model
var_model <- constructModel(Y, p, struct, gran, h = h, 
                            recursive = FALSE,  # We will handle recursion manually
                            cv = "Rolling",     # Rolling cross-validation
                            verbose = TRUE,     # Verbose output
                            IC = FALSE)         # Disable AIC/BIC benchmarks

# Perform cross-validation to fit the model
cv_model <- cv.BigVAR(var_model)

# Plot the cross-validation results
plot(cv_model)

# Function for manually performing recursive multi-step forecasts
recursive_forecast <- function(model, Y, h) {
  # Initialize matrix to store forecasts (12 steps for each variable)
  forecast_results <- matrix(nrow=h, ncol=ncol(Y))
  
  # Initialize current data with the initial dataset
  current_data <- Y
  
  for (i in 1:h) {
    # Generate a 1-step ahead forecast from the current data
    pred <- predict(model, h = 1)
    
    # Extract the forecast as a numeric vector
    pred <- as.numeric(pred)
    
    # Store the forecast result for this step
    forecast_results[i, ] <- pred
    
    # Append the forecast to the dataset (remove the first row and add the prediction)
    current_data <- rbind(current_data[-1, ], pred)
    
    # Rebuild the BigVAR model with the updated data
    model <- constructModel(current_data, p, struct, gran, h = 1, 
                            recursive = FALSE,  # Always do 1-step forecasts
                            cv = "Rolling", 
                            verbose = FALSE, 
                            IC = FALSE)
    
    # Perform cross-validation again with the updated data
    model <- cv.BigVAR(model)
  }
  
  return(forecast_results)
}

# Perform recursive 12-step forecasts
multi_step_forecasts <- recursive_forecast(cv_model, Y, h = 24)

# Check the structure of the forecast output
str(multi_step_forecasts)

# Display the 24-step ahead forecasts
multi_step_forecasts

# Optional: Convert the forecast results into a more readable format (e.g., as a data frame)
forecast_df_24m <- as.data.frame(multi_step_forecasts)
names(forecast_df_24m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER",
                            "ShorttermIR", "OilpriceGlobalWTI", "logEPU","GPRC","USEMV",
                            "USMPU")
# names(forecast_df_12m) <- c("CPIinflationrate", "Unemploymentrate", "RealbroadEER", 
#                             "ShorttermIR", "OilpriceGlobalWTI")
forecast_df_24m
write.csv(forecast_df_24m, "forecast_24M_BigVAR_italy_new.csv", row.names = FALSE)
###################### End of Code #######################





