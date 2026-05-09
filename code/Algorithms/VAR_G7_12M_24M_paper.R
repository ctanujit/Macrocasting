############## VAR Model: Canada: 12M and 24M ahead - forecasts ####################
######################### VAR Model: CANADA: 12M ahead - forecasts ####################
# link: https://cran.r-project.org/web/packages/tsDyn/tsDyn.pdf

# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/canada")
getwd()

# Load the relevant packages
library(vars)
library(forecast)
library(nnet)
library(Metrics)
library(dplyr)
library(tsDyn)

# Read the dataset
var.canada.12M <- read.csv("all_mulvar_data_canada_v2.csv", header = TRUE)
str(var.canada.12M)

# Convert Date into Datetime Value
var.canada.12M$Date <- as.Date(var.canada.12M$Date)
str(var.canada.12M)

# print a few examples
head(var.canada.12M)
tail(var.canada.12M)

# Creation of Train, test and validation dataset
# Creation of train and test data
var.canada.12M.train <- var.canada.12M[1:327,]
var.canada.12M.val <- var.canada.12M[328:339,]
var.canada.12M.test <- var.canada.12M[340:351,]
var.canada.12M.full.train <- var.canada.12M[1:339,]

# Check the size of the datasets
str(var.canada.12M.train)
str(var.canada.12M.val)
str(var.canada.12M.test)
str(var.canada.12M.full.train)

# Check for stationarity
library(tseries)

# Set of endogenous variables
kpss.test(var.canada.12M.full.train$Unemploymentrate, null="Trend") # Non - stationary
kpss.test(var.canada.12M.full.train$RealbroadEER, null="Trend") # Non - stationary
kpss.test(var.canada.12M.full.train$ShorttermIR, null="Trend") # Non - stationary
kpss.test(var.canada.12M.full.train$OilpriceGlobalWTI, null="Trend") # Non - stationary
kpss.test(var.canada.12M.full.train$CPIinflationrate, null="Trend") # Non - stationary
# Set of exogenous variables
kpss.test(var.canada.12M.full.train$logEPU, null="Trend") # Non - stationary
kpss.test(var.canada.12M.full.train$GPRC, null="Trend") # Non - stationary
kpss.test(var.canada.12M.full.train$USEMV, null="Trend") # Trend - stationary
kpss.test(var.canada.12M.full.train$USMPU, null="Trend") # Non - stationary

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.canada.12M.full.train.endog <- var.canada.12M.full.train[1:339,2:6]
str(var.canada.12M.full.train.endog)
VARselect(var.canada.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# Max Lag:
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      2      2 

# Check the lag - order using the difference stationary data
# 1st differenced data
diff.var.canada.12M.full.train.endog <- as.data.frame(diff(as.matrix(var.canada.12M.full.train.endog), 
                                                           lag = 1))
colnames(diff.var.canada.12M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                    'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.canada.12M.full.train.endog)

# Check for stationarity - differenced data
kpss.test(diff.var.canada.12M.full.train.endog$dUnemploymentrate, null="Trend") # difference stationary
kpss.test(diff.var.canada.12M.full.train.endog$dRealbroadEER, null="Trend") # difference stationary
kpss.test(diff.var.canada.12M.full.train.endog$dShorttermIR, null="Trend") # difference stationary
kpss.test(diff.var.canada.12M.full.train.endog$dOilpriceGlobalWTI, null="Trend") # difference stationary
kpss.test(diff.var.canada.12M.full.train.endog$dCPIinflationrate, null="Trend") # difference stationary

VARselect(diff.var.canada.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# Max Lag order:
#   AIC(n)  HQ(n)  SC(n) FPE(n) 
# 1      1      1      1 

# Comments: as per the difference stationary data the optimal lag value should be 1
# Final Decision: We can build the model using the lowest lag order:2 for first pass of the model
#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.canada.12M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.canada.12m.endog.exog <- lineVar(data = var.canada.12M.full.train[1:339,2:6],
                                                lag = 1,
                                                include = "const",
                                                model = "VAR",
                                                I = "diff",
                                                beta = NULL,
                                                exogen = var.canada.12M.full.train[1:339,7:10])
var.full.train.canada.12m.endog.exog


# Generate Forecast for the test horizon using the VAR Model
pred.var.canada.12m <- as.data.frame(predict(var.full.train.canada.12m.endog.exog, 
                                             n.ahead = 12,
                                             exoPred = var.canada.12M.full.train[328:339,7:10]))

pred.var.canada.12m.df <- as.data.frame(pred.var.canada.12m)
# Forecasts: 12M
View(pred.var.canada.12m.df)
######################### VAR Model: CANADA: 24M ahead - forecasts ####################
# Creation of Train, test and validation dataset
# Creation of train and test data
var.canada.24M.train <- var.canada.12M[1:303,]
var.canada.24M.val <- var.canada.12M[304:327,]
var.canada.24M.test <- var.canada.12M[328:351,]
var.canada.24M.full.train <- var.canada.12M[1:327,]

# Check the size of the datasets
str(var.canada.24M.train)
str(var.canada.24M.val)
str(var.canada.24M.test)
str(var.canada.24M.full.train)

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.canada.24M.full.train.endog <- var.canada.24M.full.train[1:327,2:6]
str(var.canada.24M.full.train.endog)
VARselect(var.canada.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      2      2 

# Check the lag - order using the difference stationary data
# 1st differenced data
diff.var.canada.24M.full.train.endog <- as.data.frame(diff(as.matrix(var.canada.24M.full.train.endog), 
                                                           lag = 1))
colnames(diff.var.canada.24M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                    'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.canada.12M.full.train.endog)

VARselect(diff.var.canada.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 1      1      1      1 

#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.canada.24M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.canada.24m.endog.exog <- lineVar(data = var.canada.24M.full.train[1:327,2:6],
                                                lag = 1,
                                                include = "const",
                                                model = "VAR",
                                                I = "diff",
                                                beta = NULL,
                                                exogen = var.canada.12M.full.train[1:327,7:10])
var.full.train.canada.24m.endog.exog


# Generate Forecast for the test horizon using the VAR Model
pred.var.canada.24m <- as.data.frame(predict(var.full.train.canada.24m.endog.exog, 
                                             n.ahead = 24,
                                             exoPred = var.canada.24M.full.train[304:327,7:10]))

pred.var.canada.24m.df <- as.data.frame(pred.var.canada.24m)
# Forecasts: 12M
View(pred.var.canada.24m.df)
############################## End of Code ########################################

############## VAR Model: USA: 12M and 24M ahead - forecasts ####################
######################### VAR Model: USA: 12M ahead - forecasts ####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/usa")
getwd()

# Load the relevant packages
library(vars)
library(forecast)
library(nnet)
library(Metrics)
library(dplyr)
library(tsDyn)

# Read the dataset
var.usa.12M <- read.csv("all_mulvar_data_usa_v2.csv", header = TRUE)
str(var.usa.12M)

# Convert Date into Datetime Value
var.usa.12M$Date <- as.Date(var.usa.12M$Date)
str(var.usa.12M)

# print a few examples
head(var.usa.12M)
tail(var.usa.12M)

# Creation of Train, test and validation dataset
# Creation of train and test data
var.usa.12M.train <- var.usa.12M[1:327,]
var.usa.12M.val <- var.usa.12M[328:339,]
var.usa.12M.test <- var.usa.12M[340:351,]
var.usa.12M.full.train <- var.usa.12M[1:339,]


# Check the size of the datasets
str(var.usa.12M.train)
str(var.usa.12M.val)
str(var.usa.12M.test)
str(var.usa.12M.full.train)

# Check for stationarity
library(tseries)

# Set of endogenous variables
kpss.test(var.usa.12M.full.train$Unemploymentrate, null="Trend") # Non - stationary
kpss.test(var.usa.12M.full.train$RealbroadEER, null="Trend") # Non - stationary
kpss.test(var.usa.12M.full.train$ShorttermIR, null="Trend") # Non - stationary
kpss.test(var.usa.12M.full.train$OilpriceGlobalWTI, null="Trend") # Non - stationary
kpss.test(var.usa.12M.full.train$CPIinflationrate, null="Trend") # Non - stationary
# Set of exogenous variables
kpss.test(var.usa.12M.full.train$logEPU, null="Trend") # Non - stationary
kpss.test(var.usa.12M.full.train$GPRC, null="Trend") # Non - stationary
kpss.test(var.usa.12M.full.train$USEMV, null="Trend") # Trend - stationary
kpss.test(var.usa.12M.full.train$USMPU, null="Trend") # Non - stationary

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.usa.12M.full.train.endog <- var.usa.12M.full.train[1:339,2:6]
str(var.usa.12M.full.train.endog)
VARselect(var.usa.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 3      2      2      3 

# Check the lag - order using the difference stationary data
# 1st differenced data
diff.var.usa.12M.full.train.endog <- as.data.frame(diff(as.matrix(var.usa.12M.full.train.endog), 
                                                        lag = 1))
colnames(diff.var.usa.12M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                 'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.usa.12M.full.train.endog)

# Check for stationarity - differenced data
kpss.test(diff.var.usa.12M.full.train.endog$dUnemploymentrate, null="Trend") # difference stationary
kpss.test(diff.var.usa.12M.full.train.endog$dRealbroadEER, null="Trend") # difference stationary
kpss.test(diff.var.usa.12M.full.train.endog$dShorttermIR, null="Trend") # difference stationary
kpss.test(diff.var.usa.12M.full.train.endog$dOilpriceGlobalWTI, null="Trend") # difference stationary
kpss.test(diff.var.usa.12M.full.train.endog$dCPIinflationrate, null="Trend") # difference stationary

VARselect(diff.var.usa.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      1      1      2 

# Comments: as per the difference stationary data the optimal lag value should be 2
# Final Decision: We can build the model using the lowest lag order:2 for first pass of the model

#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.usa.12M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.usa.12m.endog.exog <- lineVar(data = var.usa.12M.full.train[1:339,2:6],
                                             lag = 2,
                                             include = "const",
                                             model = "VAR",
                                             I = "diff",
                                             beta = NULL,
                                             exogen = var.usa.12M.full.train[1:339,7:10])
var.full.train.usa.12m.endog.exog

# Generate Forecast for the test horizon using the VAR Model
pred.var.usa.12m <- as.data.frame(predict(var.full.train.usa.12m.endog.exog, 
                                          n.ahead = 12,
                                          exoPred = var.usa.12M.full.train[328:339,7:10]))

pred.var.usa.12m.df <- as.data.frame(pred.var.usa.12m)
# Forecasts: 12M
View(pred.var.usa.12m.df)
######################### VAR Model: USA: 24M ahead - forecasts ####################
# Creation of Train, test and validation dataset
# Creation of train and test data
var.usa.24M.train <- var.usa.12M[1:303,]
var.usa.24M.val <- var.usa.12M[304:327,]
var.usa.24M.test <- var.usa.12M[328:351,]
var.usa.24M.full.train <- var.usa.12M[1:327,]

# Check the size of the datasets
str(var.usa.24M.train)
str(var.usa.24M.val)
str(var.usa.24M.test)
str(var.usa.24M.full.train)

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.usa.24M.full.train.endog <- var.usa.24M.full.train[1:327,2:6]
str(var.usa.24M.full.train.endog)
VARselect(var.usa.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 3      2      2      3 

# Check the lag - order using the difference stationary data
# 1st differenced data
diff.var.usa.24M.full.train.endog <- as.data.frame(diff(as.matrix(var.usa.24M.full.train.endog), 
                                                        lag = 1))
colnames(diff.var.usa.24M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                 'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.usa.12M.full.train.endog)

VARselect(diff.var.usa.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      1      1      2 

#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.usa.24M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.usa.24m.endog.exog <- lineVar(data = var.usa.24M.full.train[1:327,2:6],
                                             lag = 2,
                                             include = "const",
                                             model = "VAR",
                                             I = "diff",
                                             beta = NULL,
                                             exogen = var.usa.12M.full.train[1:327,7:10])
var.full.train.usa.24m.endog.exog


# Generate Forecast for the test horizon using the VAR Model
pred.var.usa.24m <- as.data.frame(predict(var.full.train.usa.24m.endog.exog, 
                                          n.ahead = 24,
                                          exoPred = var.usa.24M.full.train[304:327,7:10]))

pred.var.usa.24m.df <- as.data.frame(pred.var.usa.24m)
# Forecasts: 12M
View(pred.var.usa.24m.df)
########################### End of Code ##################################
######################### VAR Model: FRANCE: 12M ahead - forecasts ####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/france")
getwd()

# Load the relevant packages
library(vars)
library(forecast)
library(nnet)
library(Metrics)
library(dplyr)
library(tsDyn)

# Read the dataset
var.france.12M <- read.csv("all_mulvar_data_france_v2.csv", header = TRUE)
str(var.france.12M)

# Convert Date into Datetime Value
var.france.12M$Date <- as.Date(var.france.12M$Date)
str(var.france.12M)

# print a few examples
head(var.france.12M)
tail(var.france.12M)


# Creation of Train, test and validation dataset
# Creation of train and test data
var.france.12M.train <- var.france.12M[1:327,]
var.france.12M.val <- var.france.12M[328:339,]
var.france.12M.test <- var.france.12M[340:351,]
var.france.12M.full.train <- var.france.12M[1:339,]

# Check the size of the datasets
str(var.france.12M.train)
str(var.france.12M.val)
str(var.france.12M.test)
str(var.france.12M.full.train)

# Check for stationarity
library(tseries)

# Set of endogenous variables
kpss.test(var.france.12M.full.train$Unemploymentrate, null="Trend") # Non - stationary
kpss.test(var.france.12M.full.train$RealbroadEER, null="Trend") # Non - stationary
kpss.test(var.france.12M.full.train$ShorttermIR, null="Trend") # Non - stationary
kpss.test(var.france.12M.full.train$OilpriceGlobalWTI, null="Trend") # Non - stationary
kpss.test(var.france.12M.full.train$CPIinflationrate, null="Trend") # Non - stationary
# Set of exogenous variables
kpss.test(var.france.12M.full.train$logEPU, null="Trend") # Non - stationary
kpss.test(var.france.12M.full.train$GPRC, null="Trend") # Trend stationary
kpss.test(var.france.12M.full.train$USEMV, null="Trend") # Trend - stationary
kpss.test(var.france.12M.full.train$USMPU, null="Trend") # Non - stationary

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.france.12M.full.train.endog <- var.france.12M.full.train[1:339,2:6]
str(var.france.12M.full.train.endog)
VARselect(var.france.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      2      2 

# Check the lag - order using the difference stationary data
# 1st differenced data
diff.var.france.12M.full.train.endog <- as.data.frame(diff(as.matrix(var.france.12M.full.train.endog), 
                                                           lag = 1))
colnames(diff.var.france.12M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                    'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.france.12M.full.train.endog)

# Check for stationarity - differenced data
kpss.test(diff.var.france.12M.full.train.endog$dUnemploymentrate, null="Trend") # difference stationary
kpss.test(diff.var.france.12M.full.train.endog$dRealbroadEER, null="Trend") # difference stationary
kpss.test(diff.var.france.12M.full.train.endog$dShorttermIR, null="Trend") # difference stationary
kpss.test(diff.var.france.12M.full.train.endog$dOilpriceGlobalWTI, null="Trend") # difference stationary
kpss.test(diff.var.france.12M.full.train.endog$dCPIinflationrate, null="Trend") # difference stationary

VARselect(diff.var.france.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 6      1      1      6 

# Comments: as per the difference stationary data the optimal lag value should be 2
# Final Decision: We can build the model using the lowest lag order:2 for first pass of the model

#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.france.12M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.france.12m.endog.exog <- lineVar(data = var.france.12M.full.train[1:339,2:6],
                                                lag = 2,
                                                include = "const",
                                                model = "VAR",
                                                I = "diff",
                                                beta = NULL,
                                                exogen = var.france.12M.full.train[1:339,7:10])
var.full.train.france.12m.endog.exog

# Generate Forecast for the test horizon using the VAR Model
pred.var.france.12m <- as.data.frame(predict(var.full.train.france.12m.endog.exog, 
                                             n.ahead = 12,
                                             exoPred = var.france.12M.full.train[328:339,7:10]))

pred.var.france.12m.df <- as.data.frame(pred.var.france.12m)
# Forecasts: 12M
View(pred.var.france.12m.df)
######################## VAR Model: FRANCE: 24M ahead - forecasts ####################
# Creation of Train, test and validation dataset
# Creation of train and test data
var.france.24M.train <- var.france.12M[1:303,]
var.france.24M.val <- var.france.12M[304:327,]
var.france.24M.test <- var.france.12M[328:351,]
var.france.24M.full.train <- var.france.12M[1:327,]

# Check the size of the datasets
str(var.france.24M.train)
str(var.france.24M.val)
str(var.france.24M.test)
str(var.france.24M.full.train)

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.france.24M.full.train.endog <- var.france.24M.full.train[1:327,2:6]
str(var.france.24M.full.train.endog)
VARselect(var.france.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      2      2 


# Check the lag - order using the difference stationary data
# 1st differenced data
str(var.france.24M.full.train.endog)
diff.var.france.24M.full.train.endog <- as.data.frame(diff(as.matrix(var.france.24M.full.train.endog), 
                                                           lag = 1))
str(diff.var.france.24M.full.train.endog)
colnames(diff.var.france.24M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                    'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.france.24M.full.train.endog)

VARselect(diff.var.france.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 6      1      1      6 

#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.france.24M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.france.24m.endog.exog <- lineVar(data = var.france.24M.full.train[1:327,2:6],
                                                lag = 2,
                                                include = "const",
                                                model = "VAR",
                                                I = "diff",
                                                beta = NULL,
                                                exogen = var.france.12M.full.train[1:327,7:10])
var.full.train.france.24m.endog.exog


# Generate Forecast for the test horizon using the VAR Model
pred.var.france.24m <- as.data.frame(predict(var.full.train.france.24m.endog.exog, 
                                             n.ahead = 24,
                                             exoPred = var.france.24M.full.train[304:327,7:10]))

pred.var.france.24m.df <- as.data.frame(pred.var.france.24m)
# Forecasts: 12M
View(pred.var.france.24m.df)

##################### End of Code ############################################

######################### VAR Model: GERMANY: 12M ahead - forecasts ####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/germany")
getwd()

# Load the relevant packages
library(vars)
library(forecast)
library(nnet)
library(Metrics)
library(dplyr)
library(tsDyn)

# Read the dataset
var.germany.12M <- read.csv("all_mulvar_data_germany_v2.csv", header = TRUE)
str(var.germany.12M)

# Convert Date into Datetime Value
var.germany.12M$Date <- as.Date(var.germany.12M$Date)
str(var.germany.12M)

# print a few examples
head(var.germany.12M)
tail(var.germany.12M)

# Creation of Train, test and validation dataset
# Creation of train and test data
var.germany.12M.train <- var.germany.12M[1:327,]
var.germany.12M.val <- var.germany.12M[328:339,]
var.germany.12M.test <- var.germany.12M[340:351,]
var.germany.12M.full.train <- var.germany.12M[1:339,]

# Check the size of the datasets
str(var.germany.12M.train)
str(var.germany.12M.val)
str(var.germany.12M.test)
str(var.germany.12M.full.train)

# Check for stationarity
library(tseries)

# Set of endogenous variables
kpss.test(var.germany.12M.full.train$Unemploymentrate, null="Trend") # Non - stationary
kpss.test(var.germany.12M.full.train$RealbroadEER, null="Trend") # Non - stationary
kpss.test(var.germany.12M.full.train$ShorttermIR, null="Trend") # Non - stationary
kpss.test(var.germany.12M.full.train$OilpriceGlobalWTI, null="Trend") # Non - stationary
kpss.test(var.germany.12M.full.train$CPIinflationrate, null="Trend") # Non - stationary
# Set of exogenous variables
kpss.test(var.germany.12M.full.train$logEPU, null="Trend") # Non - stationary
kpss.test(var.germany.12M.full.train$GPRC, null="Trend") # Non stationary
kpss.test(var.germany.12M.full.train$USEMV, null="Trend") # Trend - stationary
kpss.test(var.germany.12M.full.train$USMPU, null="Trend") # Non - stationary


# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.germany.12M.full.train.endog <- var.germany.12M.full.train[1:339,2:6]
str(var.germany.12M.full.train.endog)
VARselect(var.germany.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      2      2

# Check the lag - order using the difference stationary data
# 1st differenced data
diff.var.germany.12M.full.train.endog <- as.data.frame(diff(as.matrix(var.germany.12M.full.train.endog), 
                                                            lag = 1))
colnames(diff.var.germany.12M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                     'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.germany.12M.full.train.endog)

# Check for stationarity - differenced data
kpss.test(diff.var.germany.12M.full.train.endog$dUnemploymentrate, null="Trend") # difference stationary
kpss.test(diff.var.germany.12M.full.train.endog$dRealbroadEER, null="Trend") # difference stationary
kpss.test(diff.var.germany.12M.full.train.endog$dShorttermIR, null="Trend") # difference stationary
kpss.test(diff.var.germany.12M.full.train.endog$dOilpriceGlobalWTI, null="Trend") # difference stationary
kpss.test(diff.var.germany.12M.full.train.endog$dCPIinflationrate, null="Trend") # difference stationary

VARselect(diff.var.germany.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 1      1      1      1 

# Comments: as per the difference stationary data the optimal lag value should be 2
# Final Decision: We can build the model using the lowest lag order:2 for first pass of the model

#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.germany.12M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.germany.12m.endog.exog <- lineVar(data = var.germany.12M.full.train[1:339,2:6],
                                                 lag = 2,
                                                 include = "const",
                                                 model = "VAR",
                                                 I = "diff",
                                                 beta = NULL,
                                                 exogen = var.germany.12M.full.train[1:339,7:10])
var.full.train.germany.12m.endog.exog

# Generate Forecast for the test horizon using the VAR Model
pred.var.germany.12m <- as.data.frame(predict(var.full.train.germany.12m.endog.exog, 
                                              n.ahead = 12,
                                              exoPred = var.germany.12M.full.train[328:339,7:10]))

pred.var.germany.12m.df <- as.data.frame(pred.var.germany.12m)
# Forecasts: 12M
View(pred.var.germany.12m.df)

######################## VAR Model: GERMANY: 24M ahead - forecasts ####################
# Creation of Train, test and validation dataset
# Creation of train and test data
var.germany.24M.train <- var.germany.12M[1:303,]
var.germany.24M.val <- var.germany.12M[304:327,]
var.germany.24M.test <- var.germany.12M[328:351,]
var.germany.24M.full.train <- var.germany.12M[1:327,]

# Check the size of the datasets
str(var.germany.24M.train)
str(var.germany.24M.val)
str(var.germany.24M.test)
str(var.germany.24M.full.train)

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.germany.24M.full.train.endog <- var.germany.24M.full.train[1:327,2:6]
str(var.germany.24M.full.train.endog)
VARselect(var.germany.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      2      2 

# Check the lag - order using the difference stationary data
# 1st differenced data
str(var.germany.24M.full.train.endog)
diff.var.germany.24M.full.train.endog <- as.data.frame(diff(as.matrix(var.germany.24M.full.train.endog), 
                                                            lag = 1))
str(diff.var.germany.24M.full.train.endog)
colnames(diff.var.germany.24M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                     'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.germany.24M.full.train.endog)

VARselect(diff.var.germany.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 1      1      1      1 

#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.germany.24M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.germany.24m.endog.exog <- lineVar(data = var.germany.24M.full.train[1:327,2:6],
                                                 lag = 2,
                                                 include = "const",
                                                 model = "VAR",
                                                 I = "diff",
                                                 beta = NULL,
                                                 exogen = var.germany.12M.full.train[1:327,7:10])
var.full.train.germany.24m.endog.exog


# Generate Forecast for the test horizon using the VAR Model
pred.var.germany.24m <- as.data.frame(predict(var.full.train.germany.24m.endog.exog, 
                                              n.ahead = 24,
                                              exoPred = var.germany.24M.full.train[304:327,7:10]))

pred.var.germany.24m.df <- as.data.frame(pred.var.germany.24m)
# Forecasts: 12M
View(pred.var.germany.24m.df)
###################### End of Code ##############################
######################### VAR Model: JAPAN: 12M ahead - forecasts ####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/japan")
getwd()

# Load the relevant packages
library(vars)
library(forecast)
library(nnet)
library(Metrics)
library(dplyr)
library(tsDyn)

# Read the dataset
var.japan.12M <- read.csv("all_mulvar_data_japan_v2.csv", header = TRUE)
str(var.japan.12M)

# Convert Date into Datetime Value
var.japan.12M$Date <- as.Date(var.japan.12M$Date)
str(var.japan.12M)

# print a few examples
head(var.japan.12M)
tail(var.japan.12M)


# Creation of Train, test and validation dataset
# Creation of train and test data
var.japan.12M.train <- var.japan.12M[1:327,]
var.japan.12M.val <- var.japan.12M[328:339,]
var.japan.12M.test <- var.japan.12M[340:351,]
var.japan.12M.full.train <- var.japan.12M[1:339,]

# Check the size of the datasets
str(var.japan.12M.train)
str(var.japan.12M.val)
str(var.japan.12M.test)
str(var.japan.12M.full.train)

# Check for stationarity
library(tseries)

# Set of endogenous variables
kpss.test(var.japan.12M.full.train$Unemploymentrate, null="Trend") # Non - stationary
kpss.test(var.japan.12M.full.train$RealbroadEER, null="Trend") # Non - stationary
kpss.test(var.japan.12M.full.train$ShorttermIR, null="Trend") # Non - stationary
kpss.test(var.japan.12M.full.train$OilpriceGlobalWTI, null="Trend") # Non - stationary
kpss.test(var.japan.12M.full.train$CPIinflationrate, null="Trend") # Non - stationary
# Set of exogenous variables
kpss.test(var.japan.12M.full.train$logEPU, null="Trend") # Non - stationary
kpss.test(var.japan.12M.full.train$GPRC, null="Trend") # Trend stationary
kpss.test(var.japan.12M.full.train$USEMV, null="Trend") # Trend - stationary
kpss.test(var.japan.12M.full.train$USMPU, null="Trend") # Non - stationary

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.japan.12M.full.train.endog <- var.japan.12M.full.train[1:339,2:6]
str(var.japan.12M.full.train.endog)
VARselect(var.japan.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      1      2 

# Check the lag - order using the difference stationary data
# 1st differenced data
diff.var.japan.12M.full.train.endog <- as.data.frame(diff(as.matrix(var.japan.12M.full.train.endog), 
                                                          lag = 1))
colnames(diff.var.japan.12M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                   'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.japan.12M.full.train.endog)

# Check for stationarity - differenced data
kpss.test(diff.var.japan.12M.full.train.endog$dUnemploymentrate, null="Trend") # difference stationary
kpss.test(diff.var.japan.12M.full.train.endog$dRealbroadEER, null="Trend") # difference stationary
kpss.test(diff.var.japan.12M.full.train.endog$dShorttermIR, null="Trend") # difference stationary
kpss.test(diff.var.japan.12M.full.train.endog$dOilpriceGlobalWTI, null="Trend") # difference stationary
kpss.test(diff.var.japan.12M.full.train.endog$dCPIinflationrate, null="Trend") # difference stationary

VARselect(diff.var.japan.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 6      1      1      6 

# Comments: as per the difference stationary data the optimal lag value should be 2
# Final Decision: We can build the model using the lowest lag order:2 for first pass of the model

#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.japan.12M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.japan.12m.endog.exog <- lineVar(data = var.japan.12M.full.train[1:339,2:6],
                                               lag = 2,
                                               include = "const",
                                               model = "VAR",
                                               I = "diff",
                                               beta = NULL,
                                               exogen = var.japan.12M.full.train[1:339,7:10])
var.full.train.japan.12m.endog.exog

# Generate Forecast for the test horizon using the VAR Model
pred.var.japan.12m <- as.data.frame(predict(var.full.train.japan.12m.endog.exog, 
                                            n.ahead = 12,
                                            exoPred = var.japan.12M.full.train[328:339,7:10]))

pred.var.japan.12m.df <- as.data.frame(pred.var.japan.12m)
# Forecasts: 12M
View(pred.var.japan.12m.df)
######################## VAR Model: JAPAN: 24M ahead - forecasts ####################
# Creation of Train, test and validation dataset
# Creation of train and test data
var.japan.24M.train <- var.japan.12M[1:303,]
var.japan.24M.val <- var.japan.12M[304:327,]
var.japan.24M.test <- var.japan.12M[328:351,]
var.japan.24M.full.train <- var.japan.12M[1:327,]

# Check the size of the datasets
str(var.japan.24M.train)
str(var.japan.24M.val)
str(var.japan.24M.test)
str(var.japan.24M.full.train)

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.japan.24M.full.train.endog <- var.japan.24M.full.train[1:327,2:6]
str(var.japan.24M.full.train.endog)
VARselect(var.japan.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      1      2 


# Check the lag - order using the difference stationary data
# 1st differenced data
str(var.japan.24M.full.train.endog)
diff.var.japan.24M.full.train.endog <- as.data.frame(diff(as.matrix(var.japan.24M.full.train.endog), 
                                                          lag = 1))
str(diff.var.japan.24M.full.train.endog)
colnames(diff.var.japan.24M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                   'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.japan.24M.full.train.endog)

VARselect(diff.var.japan.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 6      1      1      6 

#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.japan.24M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.japan.24m.endog.exog <- lineVar(data = var.japan.24M.full.train[1:327,2:6],
                                               lag = 2,
                                               include = "const",
                                               model = "VAR",
                                               I = "diff",
                                               beta = NULL,
                                               exogen = var.japan.12M.full.train[1:327,7:10])
var.full.train.japan.24m.endog.exog


# Generate Forecast for the test horizon using the VAR Model
pred.var.japan.24m <- as.data.frame(predict(var.full.train.japan.24m.endog.exog, 
                                            n.ahead = 24,
                                            exoPred = var.japan.24M.full.train[304:327,7:10]))

pred.var.japan.24m.df <- as.data.frame(pred.var.japan.24m)
# Forecasts: 12M
View(pred.var.japan.24m.df)
##################### End of Code ###########################
######################### VAR Model: UK: 12M ahead - forecasts ####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/uk")
getwd()

# Load the relevant packages
library(vars)
library(forecast)
library(nnet)
library(Metrics)
library(dplyr)
library(tsDyn)


# Read the dataset
var.UK.12M <- read.csv("all_mulvar_data_uk_v2.csv", header = TRUE)
str(var.UK.12M)

# Convert Date into Datetime Value
var.UK.12M$Date <- as.Date(var.UK.12M$Date)
str(var.UK.12M)

# print a few examples
head(var.UK.12M)
tail(var.UK.12M)


# Creation of Train, test and validation dataset
# Creation of train and test data
var.UK.12M.train <- var.UK.12M[1:327,]
var.UK.12M.val <- var.UK.12M[328:339,]
var.UK.12M.test <- var.UK.12M[340:351,]
var.UK.12M.full.train <- var.UK.12M[1:339,]

# Check the size of the datasets
str(var.UK.12M.train)
str(var.UK.12M.val)
str(var.UK.12M.test)
str(var.UK.12M.full.train)

# Check for stationarity
library(tseries)

# Set of endogenous variables
kpss.test(var.UK.12M.full.train$Unemploymentrate, null="Trend") # Non - stationary
kpss.test(var.UK.12M.full.train$RealbroadEER, null="Trend") # Non - stationary
kpss.test(var.UK.12M.full.train$ShorttermIR, null="Trend") # Non - stationary
kpss.test(var.UK.12M.full.train$OilpriceGlobalWTI, null="Trend") # Non - stationary
kpss.test(var.UK.12M.full.train$CPIinflationrate, null="Trend") # Non - stationary
# Set of exogenous variables
kpss.test(var.UK.12M.full.train$logEPU, null="Trend") # Non - stationary
kpss.test(var.UK.12M.full.train$GPRC, null="Trend") # Trend stationary
kpss.test(var.UK.12M.full.train$USEMV, null="Trend") # Trend - stationary
kpss.test(var.UK.12M.full.train$USMPU, null="Trend") # Non - stationary

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.UK.12M.full.train.endog <- var.UK.12M.full.train[1:339,2:6]
str(var.UK.12M.full.train.endog)
VARselect(var.UK.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 4      2      2      2 

# Check the lag - order using the difference stationary data
# 1st differenced data
diff.var.UK.12M.full.train.endog <- as.data.frame(diff(as.matrix(var.UK.12M.full.train.endog), 
                                                       lag = 1))
colnames(diff.var.UK.12M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.UK.12M.full.train.endog)

# Check for stationarity - differenced data
kpss.test(diff.var.UK.12M.full.train.endog$dUnemploymentrate, null="Trend") # difference stationary
kpss.test(diff.var.UK.12M.full.train.endog$dRealbroadEER, null="Trend") # difference stationary
kpss.test(diff.var.UK.12M.full.train.endog$dShorttermIR, null="Trend") # difference stationary
kpss.test(diff.var.UK.12M.full.train.endog$dOilpriceGlobalWTI, null="Trend") # difference stationary
kpss.test(diff.var.UK.12M.full.train.endog$dCPIinflationrate, null="Trend") # difference stationary

VARselect(diff.var.UK.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 4      1      1      3 

# Comments: as per the difference stationary data the optimal lag value should be 2
# Final Decision: We can build the model using the lowest lag order:2 for first pass of the model

#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.UK.12M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.UK.12m.endog.exog <- lineVar(data = var.UK.12M.full.train[1:339,2:6],
                                            lag = 2,
                                            include = "const",
                                            model = "VAR",
                                            I = "diff",
                                            beta = NULL,
                                            exogen = var.UK.12M.full.train[1:339,7:10])
var.full.train.UK.12m.endog.exog

# Generate Forecast for the test horizon using the VAR Model
pred.var.UK.12m <- as.data.frame(predict(var.full.train.UK.12m.endog.exog, 
                                         n.ahead = 12,
                                         exoPred = var.UK.12M.full.train[328:339,7:10]))

pred.var.UK.12m.df <- as.data.frame(pred.var.UK.12m)
# Forecasts: 12M
View(pred.var.UK.12m.df)
######################## VAR Model: UK: 24M ahead - forecasts ####################
# Creation of Train, test and validation dataset
# Creation of train and test data
var.UK.24M.train <- var.UK.12M[1:303,]
var.UK.24M.val <- var.UK.12M[304:327,]
var.UK.24M.test <- var.UK.12M[328:351,]
var.UK.24M.full.train <- var.UK.12M[1:327,]

# Check the size of the datasets
str(var.UK.24M.train)
str(var.UK.24M.val)
str(var.UK.24M.test)
str(var.UK.24M.full.train)

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.UK.24M.full.train.endog <- var.UK.24M.full.train[1:327,2:6]
str(var.UK.24M.full.train.endog)
VARselect(var.UK.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 5      2      2      5 


# Check the lag - order using the difference stationary data
# 1st differenced data
str(var.UK.24M.full.train.endog)
diff.var.UK.24M.full.train.endog <- as.data.frame(diff(as.matrix(var.UK.24M.full.train.endog), 
                                                       lag = 1))
str(diff.var.UK.24M.full.train.endog)
colnames(diff.var.UK.24M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.UK.24M.full.train.endog)

VARselect(diff.var.UK.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 4      1      1      4 

#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.UK.24M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.UK.24m.endog.exog <- lineVar(data = var.UK.24M.full.train[1:327,2:6],
                                            lag = 2,
                                            include = "const",
                                            model = "VAR",
                                            I = "diff",
                                            beta = NULL,
                                            exogen = var.UK.12M.full.train[1:327,7:10])
var.full.train.UK.24m.endog.exog


# Generate Forecast for the test horizon using the VAR Model
pred.var.UK.24m <- as.data.frame(predict(var.full.train.UK.24m.endog.exog, 
                                         n.ahead = 24,
                                         exoPred = var.UK.24M.full.train[304:327,7:10]))

pred.var.UK.24m.df <- as.data.frame(pred.var.UK.24m)
# Forecasts: 12M
View(pred.var.UK.24m.df)
#################### End of Code #######################
######################### VAR Model: ITALY: 12M ahead - forecasts ####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/italy")
getwd()

# Load the relevant packages
library(vars)
library(forecast)
library(nnet)
library(Metrics)
library(dplyr)
library(tsDyn)

# Read the dataset
var.italy.12M <- read.csv("all_mulvar_data_italy_v2.csv", header = TRUE)
str(var.italy.12M)

# Convert Date into Datetime Value
var.italy.12M$Date <- as.Date(var.italy.12M$Date)
str(var.italy.12M)

# print a few examples
head(var.italy.12M)
tail(var.italy.12M)


# Creation of Train, test and validation dataset
# Creation of train and test data
var.italy.12M.train <- var.italy.12M[1:327,]
var.italy.12M.val <- var.italy.12M[328:339,]
var.italy.12M.test <- var.italy.12M[340:351,]
var.italy.12M.full.train <- var.italy.12M[1:339,]

# Check the size of the datasets
str(var.italy.12M.train)
str(var.italy.12M.val)
str(var.italy.12M.test)
str(var.italy.12M.full.train)

# Check for stationarity
library(tseries)

# Set of endogenous variables
kpss.test(var.italy.12M.full.train$Unemploymentrate, null="Trend") # Non - stationary
kpss.test(var.italy.12M.full.train$RealbroadEER, null="Trend") # Non - stationary
kpss.test(var.italy.12M.full.train$ShorttermIR, null="Trend") # Non - stationary
kpss.test(var.italy.12M.full.train$OilpriceGlobalWTI, null="Trend") # Non - stationary
kpss.test(var.italy.12M.full.train$CPIinflationrate, null="Trend") # Non - stationary
# Set of exogenous variables
kpss.test(var.italy.12M.full.train$logEPU, null="Trend") # Non - stationary
kpss.test(var.italy.12M.full.train$GPRC, null="Trend") # Trend stationary
kpss.test(var.italy.12M.full.train$USEMV, null="Trend") # Trend - stationary
kpss.test(var.italy.12M.full.train$USMPU, null="Trend") # Non - stationary

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.italy.12M.full.train.endog <- var.italy.12M.full.train[1:339,2:6]
str(var.italy.12M.full.train.endog)
VARselect(var.italy.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 6      2      2      6 

# Check the lag - order using the difference stationary data
# 1st differenced data
diff.var.italy.12M.full.train.endog <- as.data.frame(diff(as.matrix(var.italy.12M.full.train.endog), 
                                                          lag = 1))
colnames(diff.var.italy.12M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                   'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.italy.12M.full.train.endog)

# Check for stationarity - differenced data
kpss.test(diff.var.italy.12M.full.train.endog$dUnemploymentrate, null="Trend") # difference stationary
kpss.test(diff.var.italy.12M.full.train.endog$dRealbroadEER, null="Trend") # difference stationary
kpss.test(diff.var.italy.12M.full.train.endog$dShorttermIR, null="Trend") # difference stationary
kpss.test(diff.var.italy.12M.full.train.endog$dOilpriceGlobalWTI, null="Trend") # difference stationary
kpss.test(diff.var.italy.12M.full.train.endog$dCPIinflationrate, null="Trend") # difference stationary

VARselect(diff.var.italy.12M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 6      1      1      6 

# Comments: as per the difference stationary data the optimal lag value should be 6
# Final Decision: We can build the model using the lowest lag order:2 for first pass of the model

#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.italy.12M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.italy.12m.endog.exog <- lineVar(data = var.italy.12M.full.train[1:339,2:6],
                                               lag = 6,
                                               include = "const",
                                               model = "VAR",
                                               I = "diff",
                                               beta = NULL,
                                               exogen = var.italy.12M.full.train[1:339,7:10])
var.full.train.italy.12m.endog.exog

# Generate Forecast for the test horizon using the VAR Model
pred.var.italy.12m <- as.data.frame(predict(var.full.train.italy.12m.endog.exog, 
                                            n.ahead = 12,
                                            exoPred = var.italy.12M.full.train[328:339,7:10]))

pred.var.italy.12m.df <- as.data.frame(pred.var.italy.12m)
# Forecasts: 12M
View(pred.var.italy.12m.df)
######################## VAR Model: ITALY: 24M ahead - forecasts ####################
# Creation of Train, test and validation dataset
# Creation of train and test data
var.italy.24M.train <- var.italy.12M[1:303,]
var.italy.24M.val <- var.italy.12M[304:327,]
var.italy.24M.test <- var.italy.12M[328:351,]
var.italy.24M.full.train <- var.italy.12M[1:327,]

# Check the size of the datasets
str(var.italy.24M.train)
str(var.italy.24M.val)
str(var.italy.24M.test)
str(var.italy.24M.full.train)

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
var.italy.24M.full.train.endog <- var.italy.24M.full.train[1:327,2:6]
str(var.italy.24M.full.train.endog)
VARselect(var.italy.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 4      2      2   4 


# Check the lag - order using the difference stationary data
# 1st differenced data
str(var.italy.24M.full.train.endog)
diff.var.italy.24M.full.train.endog <- as.data.frame(diff(as.matrix(var.italy.24M.full.train.endog), 
                                                          lag = 1))
str(diff.var.italy.24M.full.train.endog)
colnames(diff.var.italy.24M.full.train.endog) <- c('dUnemploymentrate', 'dRealbroadEER','dShorttermIR',
                                                   'dOilpriceGlobalWTI','dCPIinflationrate')
str(diff.var.italy.24M.full.train.endog)

VARselect(diff.var.italy.24M.full.train.endog, lag.max=6,type="const",season = 4)[["selection"]]
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 6      1      1      6 

#========================================================
# VAR model in difference using tsDyn
#========================================================
library(tsDyn)
# Check for the final dataset - training
str(var.italy.24M.full.train)

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.italy.24m.endog.exog <- lineVar(data = var.italy.24M.full.train[1:327,2:6],
                                               lag = 4,
                                               include = "const",
                                               model = "VAR",
                                               I = "diff",
                                               beta = NULL,
                                               exogen = var.italy.12M.full.train[1:327,7:10])
var.full.train.italy.24m.endog.exog


# Generate Forecast for the test horizon using the VAR Model
pred.var.italy.24m <- as.data.frame(predict(var.full.train.italy.24m.endog.exog, 
                                            n.ahead = 24,
                                            exoPred = var.italy.24M.full.train[304:327,7:10]))

pred.var.italy.24m.df <- as.data.frame(pred.var.italy.24m)
# Forecasts: 12M
View(pred.var.italy.24m.df)
####################### End of Code ###################################

