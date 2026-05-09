#### VES Model: Canada: 12M and 24M ahead - forecasts ####
# Load required libraries
library(legion)
library(vars)
library(forecast)
library(nnet)
library(Metrics)
library(dplyr)
library(tsDyn)
library(tseries)

# Set working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/canada")
getwd()

# Read the dataset
ves.canada <- read.csv("all_mulvar_data_canada_v2.csv", header = TRUE)
tail(ves.canada)
# Convert Date into Datetime Value
ves.canada$Date <- as.Date(ves.canada$Date)
str(ves.canada)

# Creation of Train, test and validation dataset for 12M horizon
ves.canada.12M.train <- ves.canada[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.canada.12M.val <- ves.canada[328:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.canada.12M.test <- ves.canada[340:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.canada.12M.full.train <- ves.canada[1:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.canada.12M.test
# Creation of Train, test and validation dataset for 24M horizon
ves.canada.24M.train <- ves.canada[1:303, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.canada.24M.val <- ves.canada[304:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.canada.24M.test <- ves.canada[328:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.canada.24M.full.train <- ves.canada[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.canada.24M.test
# Check the size of the datasets
str(ves.canada.12M.train)
str(ves.canada.12M.val)
str(ves.canada.12M.test)
str(ves.canada.12M.full.train)

str(ves.canada.24M.train)
str(ves.canada.24M.val)
str(ves.canada.24M.test)
str(ves.canada.24M.full.train)

# Fit VES model for 12M forecast
ves.canada.12m <- ves(ts(ves.canada.12M.full.train),
                      model = "AAdN",
                      lags = c(frequency(ts(ves.canada.12M.full.train))),
                      phi = 'i',
                      persistence = 'i',
                      h = 12,
                      holdout = TRUE)

# Generate 12M forecast
forecast.ves.canada.12m <- forecast(ves.canada.12m, h = 12)
forecast.ves.canada.12m
ves.canada.12m

# Fit VES model for 24M forecast
ves.canada.24m <- ves(ts(ves.canada.24M.full.train),
                      model = "AAdN",
                      lags = c(frequency(ts(ves.canada.24M.full.train))),
                      phi = 'i',
                      persistence = 'i',
                      h = 24,
                      holdout = TRUE)

# Generate 24M forecast
forecast.ves.canada.24m <- forecast(ves.canada.24m, h = 24)
forecast.ves.canada.24m
################################# End of Code ###########################
#### VES Model: usa: 12M and 24M ahead - forecasts ####

# Set working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/usa")
getwd()

# Read the dataset
ves.usa <- read.csv("all_mulvar_data_usa_v2.csv", header = TRUE)
tail(ves.usa)
# Convert Date into Datetime Value
ves.usa$Date <- as.Date(ves.usa$Date)
str(ves.usa)

# Creation of Train, test and validation dataset for 12M horizon
ves.usa.12M.train <- ves.usa[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.usa.12M.val <- ves.usa[328:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.usa.12M.test <- ves.usa[340:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.usa.12M.full.train <- ves.usa[1:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]

# Creation of Train, test and validation dataset for 24M horizon
ves.usa.24M.train <- ves.usa[1:303, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.usa.24M.val <- ves.usa[304:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.usa.24M.test <- ves.usa[328:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.usa.24M.full.train <- ves.usa[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]


# Fit VES model for 12M forecast
ves.usa.12m <- ves(ts(ves.usa.12M.full.train),
                   model = "AAdN",
                   lags = c(frequency(ts(ves.usa.12M.full.train))),
                   phi = 'i',
                   persistence = 'i',
                   h = 12,
                   holdout = TRUE)

# Generate 12M forecast
forecast.ves.usa.12m <- forecast(ves.usa.12m, h = 12)
forecast.ves.usa.12m

# Fit VES model for 24M forecast
ves.usa.24m <- ves(ts(ves.usa.24M.full.train),
                   model = "AAdN",
                   lags = c(frequency(ts(ves.usa.24M.full.train))),
                   phi = 'i',
                   persistence = 'i',
                   h = 24,
                   holdout = TRUE)

# Generate 24M forecast
forecast.ves.usa.24m <- forecast(ves.usa.24m, h = 24)
forecast.ves.usa.24m
################### End of Code ###########################
#### VES Model: france: 12M and 24M ahead - forecasts ####

# Set working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/france")
getwd()

# Read the dataset
ves.france <- read.csv("all_mulvar_data_france_v2.csv", header = TRUE)
tail(ves.france)
# Convert Date into Datetime Value
ves.france$Date <- as.Date(ves.france$Date)
str(ves.france)

# Creation of Train, test and validation dataset for 12M horizon
ves.france.12M.train <- ves.france[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.france.12M.val <- ves.france[328:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.france.12M.test <- ves.france[340:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.france.12M.full.train <- ves.france[1:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]

# Creation of Train, test and validation dataset for 24M horizon
ves.france.24M.train <- ves.france[1:303, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.france.24M.val <- ves.france[304:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.france.24M.test <- ves.france[328:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.france.24M.full.train <- ves.france[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", 
                                                 "CPIinflationrate")]


# Fit VES model for 12M forecast
ves.france.12m <- ves(ts(ves.france.12M.full.train),
                      model = "AAdN",
                      lags = c(frequency(ts(ves.france.12M.full.train))),
                      phi = 'i',
                      persistence = 'i',
                      h = 12,
                      holdout = TRUE)

# Generate 12M forecast
forecast.ves.france.12m <- forecast(ves.france.12m, h = 12)
forecast.ves.france.12m

# Fit VES model for 24M forecast
ves.france.24m <- ves(ts(ves.france.24M.full.train),
                      model = "AAdN",
                      lags = c(frequency(ts(ves.france.24M.full.train))),
                      phi = 'i',
                      persistence = 'i',
                      h = 24,
                      holdout = TRUE)

# Generate 24M forecast
forecast.ves.france.24m <- forecast(ves.france.24m, h = 24)
forecast.ves.france.24m
#################### End of Code #######################
#### VES Model: germany: 12M and 24M ahead - forecasts ####

# Set working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/germany")
getwd()

# Read the dataset
ves.germany <- read.csv("all_mulvar_data_germany_v2.csv", header = TRUE)
tail(ves.germany)
# Convert Date into Datetime Value
ves.germany$Date <- as.Date(ves.germany$Date)
str(ves.germany)

# Creation of Train, test and validation dataset for 12M horizon
ves.germany.12M.train <- ves.germany[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.germany.12M.val <- ves.germany[328:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.germany.12M.test <- ves.germany[340:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.germany.12M.full.train <- ves.germany[1:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]

# Creation of Train, test and validation dataset for 24M horizon
ves.germany.24M.train <- ves.germany[1:303, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.germany.24M.val <- ves.germany[304:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.germany.24M.test <- ves.germany[328:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.germany.24M.full.train <- ves.germany[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]

# Fit VES model for 12M forecast
ves.germany.12m <- ves(ts(ves.germany.12M.full.train),
                       model = "AAdN",
                       lags = c(frequency(ts(ves.germany.12M.full.train))),
                       phi = 'i',
                       persistence = 'i',
                       h = 12,
                       holdout = TRUE)

# Generate 12M forecast
forecast.ves.germany.12m <- forecast(ves.germany.12m, h = 12)
forecast.ves.germany.12m

# Fit VES model for 24M forecast
ves.germany.24m <- ves(ts(ves.germany.24M.full.train),
                       model = "AAdN",
                       lags = c(frequency(ts(ves.germany.24M.full.train))),
                       phi = 'i',
                       persistence = 'i',
                       h = 24,
                       holdout = TRUE)

# Generate 24M forecast
forecast.ves.germany.24m <- forecast(ves.germany.24m, h = 24)
forecast.ves.germany.24m
######################## End of Code #######################
#### VES Model: japan: 12M and 24M ahead - forecasts ####
# Set working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/japan")
getwd()

# Read the dataset
ves.japan <- read.csv("all_mulvar_data_japan_v2.csv", header = TRUE)
tail(ves.japan)
# Convert Date into Datetime Value
ves.japan$Date <- as.Date(ves.japan$Date)
str(ves.japan)

# Creation of Train, test and validation dataset for 12M horizon
ves.japan.12M.train <- ves.japan[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.japan.12M.val <- ves.japan[328:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.japan.12M.test <- ves.japan[340:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.japan.12M.full.train <- ves.japan[1:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]

# Creation of Train, test and validation dataset for 24M horizon
ves.japan.24M.train <- ves.japan[1:303, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.japan.24M.val <- ves.japan[304:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.japan.24M.test <- ves.japan[328:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.japan.24M.full.train <- ves.japan[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]

# Fit VES model for 12M forecast
ves.japan.12m <- ves(ts(ves.japan.12M.full.train),
                     model = "AAdN",
                     lags = c(frequency(ts(ves.japan.12M.full.train))),
                     phi = 'i',
                     persistence = 'i',
                     h = 12,
                     holdout = TRUE)

# Generate 12M forecast
forecast.ves.japan.12m <- forecast(ves.japan.12m, h = 12)
forecast.ves.japan.12m

# Fit VES model for 24M forecast
ves.japan.24m <- ves(ts(ves.japan.24M.full.train),
                     model = "AAdN",
                     lags = c(frequency(ts(ves.japan.24M.full.train))),
                     phi = 'i',
                     persistence = 'i',
                     h = 24,
                     holdout = TRUE)

# Generate 24M forecast
forecast.ves.japan.24m <- forecast(ves.japan.24m, h = 24)
forecast.ves.japan.24m
######################## End of Code #########################
#### VES Model: uk: 12M and 24M ahead - forecasts ####
# Set working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/uk")
getwd()

# Read the dataset
ves.uk <- read.csv("all_mulvar_data_uk_v2.csv", header = TRUE)
tail(ves.uk)
# Convert Date into Datetime Value
ves.uk$Date <- as.Date(ves.uk$Date)
str(ves.uk)

# Creation of Train, test and validation dataset for 12M horizon
ves.uk.12M.train <- ves.uk[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.uk.12M.val <- ves.uk[328:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.uk.12M.test <- ves.uk[340:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.uk.12M.full.train <- ves.uk[1:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]

# Creation of Train, test and validation dataset for 24M horizon
ves.uk.24M.train <- ves.uk[1:303, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.uk.24M.val <- ves.uk[304:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.uk.24M.test <- ves.uk[328:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.uk.24M.full.train <- ves.uk[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]

# Fit VES model for 12M forecast
ves.uk.12m <- ves(ts(ves.uk.12M.full.train),
                  model = "AAdN",
                  lags = c(frequency(ts(ves.uk.12M.full.train))),
                  phi = 'i',
                  persistence = 'i',
                  h = 12,
                  holdout = TRUE)

# Generate 12M forecast
forecast.ves.uk.12m <- forecast(ves.uk.12m, h = 12)
forecast.ves.uk.12m

# Fit VES model for 24M forecast
ves.uk.24m <- ves(ts(ves.uk.24M.full.train),
                  model = "AAdN",
                  lags = c(frequency(ts(ves.uk.24M.full.train))),
                  phi = 'i',
                  persistence = 'i',
                  h = 24,
                  holdout = TRUE)

# Generate 24M forecast
forecast.ves.uk.24m <- forecast(ves.uk.24m, h = 24)
forecast.ves.uk.24m
####################### End of Code ######################
#### VES Model: italy: 12M and 24M ahead - forecasts ####
# Set working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/italy")
getwd()

# Read the dataset
ves.italy <- read.csv("all_mulvar_data_italy_v2.csv", header = TRUE)
tail(ves.italy)
# Convert Date into Datetime Value
ves.italy$Date <- as.Date(ves.italy$Date)
str(ves.italy)

# Creation of Train, test and validation dataset for 12M horizon
ves.italy.12M.train <- ves.italy[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.italy.12M.val <- ves.italy[328:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.italy.12M.test <- ves.italy[340:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.italy.12M.full.train <- ves.italy[1:339, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]

# Creation of Train, test and validation dataset for 24M horizon
ves.italy.24M.train <- ves.italy[1:303, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.italy.24M.val <- ves.italy[304:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.italy.24M.test <- ves.italy[328:351, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]
ves.italy.24M.full.train <- ves.italy[1:327, c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")]

# Fit VES model for 12M forecast
ves.italy.12m <- ves(ts(ves.italy.12M.full.train),
                     model = "AAdN",
                     lags = c(frequency(ts(ves.italy.12M.full.train))),
                     phi = 'i',
                     persistence = 'i',
                     h = 12,
                     holdout = TRUE)

# Generate 12M forecast
forecast.ves.italy.12m <- forecast(ves.italy.12m, h = 12)
forecast.ves.italy.12m

# Fit VES model for 24M forecast
ves.italy.24m <- ves(ts(ves.italy.24M.full.train),
                     model = "AAdN",
                     lags = c(frequency(ts(ves.italy.24M.full.train))),
                     phi = 'i',
                     persistence = 'i',
                     h = 24,
                     holdout = TRUE)

# Generate 24M forecast
forecast.ves.italy.24m <- forecast(ves.italy.24m, h = 24)
forecast.ves.italy.24m
#################### End of Code #######################
