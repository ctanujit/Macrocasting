################################ Global Statistics ########################
# Important Libraries
library(nonlinearTseries)
library(tseries)
library(forecast)
library(Metrics)
library(ggplot2)
library(readr)
library(WaveletArima)
library(caret)
library(nnfor)
library(tsDyn)
library(fracdiff)
library(bsts)
library(forecastHybrid)
library(e1071)
library(tseriesChaos)
library(pracma)
library(Kendall)
library(GeneCycle)
library(fpp2)
library(seastests)
library(entropy)
library(zoo)
library(sandwich)
library(strucchange)
library(tseries)

#################### CANADA #######################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/canada")
getwd()

# Data
# cpi.df <- read.csv("all_mulvar_data_canada.csv",header=TRUE)
# str(cpi.df)

cpi.df <- read.csv("all_mulvar_data_canada_v2.csv",header=TRUE)
str(cpi.df)

# Calculate Coefficient of Variation and STDV
#calculate CoV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)
#calculate STDV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x))
# Check for the SD value
# df <- cpi.df[-1]
# sd_all_cols <- apply(df[, sapply(df, is.numeric)], 2, sd)
# print(sd_all_cols)

# entropy 
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
sapply(cpi.df[-1], function(x) min(x))
sapply(cpi.df[-1], function(x) max(x))
sapply(cpi.df[-1], function(x) mean(x))
sapply(cpi.df[-1], function(x) median(x))
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1))

#Test for Stationarity 
str(cpi.df)
kpss.test(cpi.df$Unemploymentrate) 
kpss.test(cpi.df$RealbroadEER) 
kpss.test(cpi.df$ShorttermIR) 
kpss.test(cpi.df$OilpriceGlobalWTI) 
kpss.test(cpi.df$CPIinflationrate) 
# kpss.test(cpi.df$EPU) 
kpss.test(cpi.df$logEPU) 
kpss.test(cpi.df$GPRC) 
kpss.test(cpi.df$USEMV) 
kpss.test(cpi.df$USMPU) 

# Check for Long-range dependence
hurstexp(cpi.df$Unemploymentrate) 
hurstexp(cpi.df$RealbroadEER) 
hurstexp(cpi.df$ShorttermIR) 
hurstexp(cpi.df$OilpriceGlobalWTI) 
hurstexp(cpi.df$CPIinflationrate) 
hurstexp(cpi.df$logEPU) 
hurstexp(cpi.df$GPRC) 
hurstexp(cpi.df$USEMV) 
hurstexp(cpi.df$USMPU) 

#Tests for non-linearity
nonlinearityTest(ts(cpi.df$Unemploymentrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$RealbroadEER), verbose = TRUE)
nonlinearityTest(ts(cpi.df$ShorttermIR), verbose = TRUE)
nonlinearityTest(ts(cpi.df$OilpriceGlobalWTI), verbose = TRUE)
nonlinearityTest(ts(cpi.df$CPIinflationrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$logEPU), verbose = TRUE)
nonlinearityTest(ts(cpi.df$GPRC), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USEMV), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USMPU), verbose = TRUE)

# Test for Seasonality
isSeasonal(ts(cpi.df$Unemploymentrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$RealbroadEER), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$ShorttermIR), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$OilpriceGlobalWTI), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$CPIinflationrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$logEPU), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$GPRC), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USEMV), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USMPU), test = "combined", freq = 12)

# Skewness
skewness(cpi.df$Unemploymentrate) 
skewness(cpi.df$RealbroadEER) 
skewness(cpi.df$ShorttermIR) 
skewness(cpi.df$OilpriceGlobalWTI) 
skewness(cpi.df$CPIinflationrate) 
skewness(cpi.df$logEPU) 
skewness(cpi.df$GPRC) 
skewness(cpi.df$USEMV) 
skewness(cpi.df$USMPU) 

# Kurtosis
kurtosis(cpi.df$Unemploymentrate) 
kurtosis(cpi.df$RealbroadEER) 
kurtosis(cpi.df$ShorttermIR) 
kurtosis(cpi.df$OilpriceGlobalWTI) 
kurtosis(cpi.df$CPIinflationrate) 
kurtosis(cpi.df$logEPU) 
kurtosis(cpi.df$GPRC) 
kurtosis(cpi.df$USEMV) 
kurtosis(cpi.df$USMPU) 

#################### USA #######################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/usa")
getwd()

# Data
cpi.df <- read.csv("all_mulvar_data_usa_v2.csv",header=TRUE)
str(cpi.df)

# Calculate Coefficient of Variation and STDV
#calculate CoV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)
#calculate STDV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x))

# entropy 
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
sapply(cpi.df[-1], function(x) min(x))
sapply(cpi.df[-1], function(x) max(x))
sapply(cpi.df[-1], function(x) mean(x))
sapply(cpi.df[-1], function(x) median(x))
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1))

#Test for Stationarity 
str(cpi.df)
kpss.test(cpi.df$Unemploymentrate) 
kpss.test(cpi.df$RealbroadEER) 
kpss.test(cpi.df$ShorttermIR) 
kpss.test(cpi.df$OilpriceGlobalWTI) 
kpss.test(cpi.df$CPIinflationrate) 
# kpss.test(cpi.df$EPU) 
kpss.test(cpi.df$logEPU) 
kpss.test(cpi.df$GPRC) 
kpss.test(cpi.df$USEMV) 
kpss.test(cpi.df$USMPU) 

# Check for Long-range dependence
hurstexp(cpi.df$Unemploymentrate) 
hurstexp(cpi.df$RealbroadEER) 
hurstexp(cpi.df$ShorttermIR) 
hurstexp(cpi.df$OilpriceGlobalWTI) 
hurstexp(cpi.df$CPIinflationrate) 
hurstexp(cpi.df$logEPU) 
hurstexp(cpi.df$GPRC) 
hurstexp(cpi.df$USEMV) 
hurstexp(cpi.df$USMPU) 

#Tests for non-linearity
nonlinearityTest(ts(cpi.df$Unemploymentrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$RealbroadEER), verbose = TRUE)
nonlinearityTest(ts(cpi.df$ShorttermIR), verbose = TRUE)
nonlinearityTest(ts(cpi.df$OilpriceGlobalWTI), verbose = TRUE)
nonlinearityTest(ts(cpi.df$CPIinflationrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$logEPU), verbose = TRUE)
nonlinearityTest(ts(cpi.df$GPRC), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USEMV), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USMPU), verbose = TRUE)

# Test for Seasonality
isSeasonal(ts(cpi.df$Unemploymentrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$RealbroadEER), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$ShorttermIR), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$OilpriceGlobalWTI), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$CPIinflationrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$logEPU), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$GPRC), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USEMV), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USMPU), test = "combined", freq = 12)

# Skewness
skewness(cpi.df$Unemploymentrate) 
skewness(cpi.df$RealbroadEER) 
skewness(cpi.df$ShorttermIR) 
skewness(cpi.df$OilpriceGlobalWTI) 
skewness(cpi.df$CPIinflationrate) 
skewness(cpi.df$logEPU) 
skewness(cpi.df$GPRC) 
skewness(cpi.df$USEMV) 
skewness(cpi.df$USMPU) 

# Kurtosis
kurtosis(cpi.df$Unemploymentrate) 
kurtosis(cpi.df$RealbroadEER) 
kurtosis(cpi.df$ShorttermIR) 
kurtosis(cpi.df$OilpriceGlobalWTI) 
kurtosis(cpi.df$CPIinflationrate) 
kurtosis(cpi.df$logEPU) 
kurtosis(cpi.df$GPRC) 
kurtosis(cpi.df$USEMV) 
kurtosis(cpi.df$USMPU) 

#################### Germany #######################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/germany")
getwd()

# Data
cpi.df <- read.csv("all_mulvar_data_germany_v2.csv",header=TRUE)
str(cpi.df)

# Calculate Coefficient of Variation and STDV
#calculate CoV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)
#calculate STDV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x))

# entropy 
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
sapply(cpi.df[-1], function(x) min(x))
sapply(cpi.df[-1], function(x) max(x))
sapply(cpi.df[-1], function(x) mean(x))
sapply(cpi.df[-1], function(x) median(x))
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1))

#Test for Stationarity 
str(cpi.df)
kpss.test(cpi.df$Unemploymentrate) 
kpss.test(cpi.df$RealbroadEER) 
kpss.test(cpi.df$ShorttermIR) 
kpss.test(cpi.df$OilpriceGlobalWTI) 
kpss.test(cpi.df$CPIinflationrate) 
kpss.test(cpi.df$logEPU) 
kpss.test(cpi.df$GPRC) 
kpss.test(cpi.df$USEMV) 
kpss.test(cpi.df$USMPU) 

# Check for Long-range dependence
hurstexp(cpi.df$Unemploymentrate) 
hurstexp(cpi.df$RealbroadEER) 
hurstexp(cpi.df$ShorttermIR) 
hurstexp(cpi.df$OilpriceGlobalWTI) 
hurstexp(cpi.df$CPIinflationrate) 
hurstexp(cpi.df$logEPU) 
hurstexp(cpi.df$GPRC) 
hurstexp(cpi.df$USEMV) 
hurstexp(cpi.df$USMPU) 

#Tests for non-linearity
nonlinearityTest(ts(cpi.df$Unemploymentrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$RealbroadEER), verbose = TRUE)
nonlinearityTest(ts(cpi.df$ShorttermIR), verbose = TRUE)
nonlinearityTest(ts(cpi.df$OilpriceGlobalWTI), verbose = TRUE)
nonlinearityTest(ts(cpi.df$CPIinflationrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$logEPU), verbose = TRUE)
nonlinearityTest(ts(cpi.df$GPRC), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USEMV), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USMPU), verbose = TRUE)

# Test for Seasonality
isSeasonal(ts(cpi.df$Unemploymentrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$RealbroadEER), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$ShorttermIR), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$OilpriceGlobalWTI), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$CPIinflationrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$logEPU), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$GPRC), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USEMV), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USMPU), test = "combined", freq = 12)

# Skewness
skewness(cpi.df$Unemploymentrate) 
skewness(cpi.df$RealbroadEER) 
skewness(cpi.df$ShorttermIR) 
skewness(cpi.df$OilpriceGlobalWTI) 
skewness(cpi.df$CPIinflationrate) 
skewness(cpi.df$logEPU) 
skewness(cpi.df$GPRC) 
skewness(cpi.df$USEMV) 
skewness(cpi.df$USMPU) 

# Kurtosis
kurtosis(cpi.df$Unemploymentrate) 
kurtosis(cpi.df$RealbroadEER) 
kurtosis(cpi.df$ShorttermIR) 
kurtosis(cpi.df$OilpriceGlobalWTI) 
kurtosis(cpi.df$CPIinflationrate) 
kurtosis(cpi.df$logEPU) 
kurtosis(cpi.df$GPRC) 
kurtosis(cpi.df$USEMV) 
kurtosis(cpi.df$USMPU) 

#################### France #######################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/france")
getwd()

# Data
cpi.df <- read.csv("all_mulvar_data_france_v2.csv",header=TRUE)
str(cpi.df)

# Calculate Coefficient of Variation and STDV
#calculate CoV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)
#calculate STDV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x))

# entropy 
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
sapply(cpi.df[-1], function(x) min(x))
sapply(cpi.df[-1], function(x) max(x))
sapply(cpi.df[-1], function(x) mean(x))
sapply(cpi.df[-1], function(x) median(x))
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1))

#Test for Stationarity 
str(cpi.df)
kpss.test(cpi.df$Unemploymentrate) 
kpss.test(cpi.df$RealbroadEER) 
kpss.test(cpi.df$ShorttermIR) 
kpss.test(cpi.df$OilpriceGlobalWTI) 
kpss.test(cpi.df$CPIinflationrate) 
# kpss.test(cpi.df$EPU) 
kpss.test(cpi.df$logEPU) 
kpss.test(cpi.df$GPRC) 
kpss.test(cpi.df$USEMV) 
kpss.test(cpi.df$USMPU)

# Check for Long-range dependence
hurstexp(cpi.df$Unemploymentrate) 
hurstexp(cpi.df$RealbroadEER) 
hurstexp(cpi.df$ShorttermIR) 
hurstexp(cpi.df$OilpriceGlobalWTI) 
hurstexp(cpi.df$CPIinflationrate) 
# hurstexp(cpi.df$EPU) 
hurstexp(cpi.df$logEPU) 
hurstexp(cpi.df$GPRC) 
hurstexp(cpi.df$USEMV) 
hurstexp(cpi.df$USMPU) 

#Tests for non-linearity
nonlinearityTest(ts(cpi.df$Unemploymentrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$RealbroadEER), verbose = TRUE)
nonlinearityTest(ts(cpi.df$ShorttermIR), verbose = TRUE)
nonlinearityTest(ts(cpi.df$OilpriceGlobalWTI), verbose = TRUE)
nonlinearityTest(ts(cpi.df$CPIinflationrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$logEPU), verbose = TRUE)
nonlinearityTest(ts(cpi.df$GPRC), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USEMV), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USMPU), verbose = TRUE)

# Test for Seasonality
isSeasonal(ts(cpi.df$Unemploymentrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$RealbroadEER), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$ShorttermIR), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$OilpriceGlobalWTI), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$CPIinflationrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$logEPU), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$GPRC), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USEMV), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USMPU), test = "combined", freq = 12)

# Skewness
skewness(cpi.df$Unemploymentrate) 
skewness(cpi.df$RealbroadEER) 
skewness(cpi.df$ShorttermIR) 
skewness(cpi.df$OilpriceGlobalWTI) 
skewness(cpi.df$CPIinflationrate) 
skewness(cpi.df$logEPU) 
skewness(cpi.df$GPRC) 
skewness(cpi.df$USEMV) 
skewness(cpi.df$USMPU) 

# Kurtosis
kurtosis(cpi.df$Unemploymentrate) 
kurtosis(cpi.df$RealbroadEER) 
kurtosis(cpi.df$ShorttermIR) 
kurtosis(cpi.df$OilpriceGlobalWTI) 
kurtosis(cpi.df$CPIinflationrate) 
kurtosis(cpi.df$logEPU) 
kurtosis(cpi.df$GPRC) 
kurtosis(cpi.df$USEMV) 
kurtosis(cpi.df$USMPU) 

#################### Japan #######################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/japan")
getwd()

# Data
cpi.df <- read.csv("all_mulvar_data_japan_v2.csv",header=TRUE)
str(cpi.df)

# Calculate Coefficient of Variation and STDV
#calculate CoV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)
#calculate STDV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x))

# entropy 
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
sapply(cpi.df[-1], function(x) min(x))
sapply(cpi.df[-1], function(x) max(x))
sapply(cpi.df[-1], function(x) mean(x))
sapply(cpi.df[-1], function(x) median(x))
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1))

#Test for Stationarity 
str(cpi.df)
kpss.test(cpi.df$Unemploymentrate) 
kpss.test(cpi.df$RealbroadEER) 
kpss.test(cpi.df$ShorttermIR) 
kpss.test(cpi.df$OilpriceGlobalWTI) 
kpss.test(cpi.df$CPIinflationrate) 
kpss.test(cpi.df$logEPU) 
kpss.test(cpi.df$GPRC) 
kpss.test(cpi.df$USEMV) 
kpss.test(cpi.df$USMPU)

# Check for Long-range dependence
hurstexp(cpi.df$Unemploymentrate) 
hurstexp(cpi.df$RealbroadEER) 
hurstexp(cpi.df$ShorttermIR) 
hurstexp(cpi.df$OilpriceGlobalWTI) 
hurstexp(cpi.df$CPIinflationrate) 
hurstexp(cpi.df$logEPU) 
hurstexp(cpi.df$GPRC) 
hurstexp(cpi.df$USEMV) 
hurstexp(cpi.df$USMPU) 

#Tests for non-linearity
nonlinearityTest(ts(cpi.df$Unemploymentrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$RealbroadEER), verbose = TRUE)
nonlinearityTest(ts(cpi.df$ShorttermIR), verbose = TRUE)
nonlinearityTest(ts(cpi.df$OilpriceGlobalWTI), verbose = TRUE)
nonlinearityTest(ts(cpi.df$CPIinflationrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$logEPU), verbose = TRUE)
nonlinearityTest(ts(cpi.df$GPRC), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USEMV), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USMPU), verbose = TRUE)

# Test for Seasonality
isSeasonal(ts(cpi.df$Unemploymentrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$RealbroadEER), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$ShorttermIR), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$OilpriceGlobalWTI), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$CPIinflationrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$logEPU), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$GPRC), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USEMV), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USMPU), test = "combined", freq = 12)

# Skewness
skewness(cpi.df$Unemploymentrate) 
skewness(cpi.df$RealbroadEER) 
skewness(cpi.df$ShorttermIR) 
skewness(cpi.df$OilpriceGlobalWTI) 
skewness(cpi.df$CPIinflationrate) 
skewness(cpi.df$logEPU) 
skewness(cpi.df$GPRC) 
skewness(cpi.df$USEMV) 
skewness(cpi.df$USMPU) 

# Kurtosis
kurtosis(cpi.df$Unemploymentrate) 
kurtosis(cpi.df$RealbroadEER) 
kurtosis(cpi.df$ShorttermIR) 
kurtosis(cpi.df$OilpriceGlobalWTI) 
kurtosis(cpi.df$CPIinflationrate) 
kurtosis(cpi.df$logEPU) 
kurtosis(cpi.df$GPRC) 
kurtosis(cpi.df$USEMV) 
kurtosis(cpi.df$USMPU) 

#################### UK #######################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/uk")
getwd()

# Data
cpi.df <- read.csv("all_mulvar_data_uk_v2.csv",header=TRUE)
str(cpi.df)

# Calculate Coefficient of Variation and STDV
#calculate CoV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)
#calculate STDV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x))
str(cpi.df[-1])

# entropy 
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
sapply(cpi.df[-1], function(x) min(x))
sapply(cpi.df[-1], function(x) max(x))
sapply(cpi.df[-1], function(x) mean(x))
sapply(cpi.df[-1], function(x) median(x))
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1))

#Test for Stationarity 
str(cpi.df)
kpss.test(cpi.df$Unemploymentrate) 
kpss.test(cpi.df$RealbroadEER) 
kpss.test(cpi.df$ShorttermIR) 
kpss.test(cpi.df$OilpriceGlobalWTI) 
kpss.test(cpi.df$CPIinflationrate) 
kpss.test(cpi.df$logEPU) 
kpss.test(cpi.df$GPRC) 
kpss.test(cpi.df$USEMV) 
kpss.test(cpi.df$USMPU) 

# Check for Long-range dependence
hurstexp(cpi.df$Unemploymentrate) 
hurstexp(cpi.df$RealbroadEER) 
hurstexp(cpi.df$ShorttermIR) 
hurstexp(cpi.df$OilpriceGlobalWTI) 
hurstexp(cpi.df$CPIinflationrate) 
hurstexp(cpi.df$logEPU) 
hurstexp(cpi.df$GPRC) 
hurstexp(cpi.df$USEMV) 
hurstexp(cpi.df$USMPU) 

#Tests for non-linearity
nonlinearityTest(ts(cpi.df$Unemploymentrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$RealbroadEER), verbose = TRUE)
nonlinearityTest(ts(cpi.df$ShorttermIR), verbose = TRUE)
nonlinearityTest(ts(cpi.df$OilpriceGlobalWTI), verbose = TRUE)
nonlinearityTest(ts(cpi.df$CPIinflationrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$logEPU), verbose = TRUE)
nonlinearityTest(ts(cpi.df$GPRC), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USEMV), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USMPU), verbose = TRUE)

# Test for Seasonality
isSeasonal(ts(cpi.df$Unemploymentrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$RealbroadEER), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$ShorttermIR), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$OilpriceGlobalWTI), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$CPIinflationrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$logEPU), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$GPRC), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USEMV), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USMPU), test = "combined", freq = 12)

# Skewness
skewness(cpi.df$Unemploymentrate) 
skewness(cpi.df$RealbroadEER) 
skewness(cpi.df$ShorttermIR) 
skewness(cpi.df$OilpriceGlobalWTI) 
skewness(cpi.df$CPIinflationrate) 
skewness(cpi.df$logEPU) 
skewness(cpi.df$GPRC) 
skewness(cpi.df$USEMV) 
skewness(cpi.df$USMPU) 

# Kurtosis
kurtosis(cpi.df$Unemploymentrate) 
kurtosis(cpi.df$RealbroadEER) 
kurtosis(cpi.df$ShorttermIR) 
kurtosis(cpi.df$OilpriceGlobalWTI) 
kurtosis(cpi.df$CPIinflationrate) 
kurtosis(cpi.df$logEPU) 
kurtosis(cpi.df$GPRC) 
kurtosis(cpi.df$USEMV) 
kurtosis(cpi.df$USMPU) 
#################### Italy #######################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/italy")
getwd()

# Data
cpi.df <- read.csv("all_mulvar_data_italy_v2.csv",header=TRUE)
str(cpi.df)

# Calculate Coefficient of Variation and STDV
#calculate CoV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)
#calculate STDV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x))

# entropy 
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
sapply(cpi.df[-1], function(x) min(x))
sapply(cpi.df[-1], function(x) max(x))
sapply(cpi.df[-1], function(x) mean(x))
sapply(cpi.df[-1], function(x) median(x))
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1))

#Test for Stationarity 
str(cpi.df)
kpss.test(cpi.df$Unemploymentrate) 
kpss.test(cpi.df$RealbroadEER) 
kpss.test(cpi.df$ShorttermIR) 
kpss.test(cpi.df$OilpriceGlobalWTI) 
kpss.test(cpi.df$CPIinflationrate) 
kpss.test(cpi.df$logEPU) 
kpss.test(cpi.df$GPRC) 
kpss.test(cpi.df$USEMV) 
kpss.test(cpi.df$USMPU) 

# Check for Long-range dependence
hurstexp(cpi.df$Unemploymentrate) 
hurstexp(cpi.df$RealbroadEER) 
hurstexp(cpi.df$ShorttermIR) 
hurstexp(cpi.df$OilpriceGlobalWTI) 
hurstexp(cpi.df$CPIinflationrate) 
hurstexp(cpi.df$logEPU) 
hurstexp(cpi.df$GPRC) 
hurstexp(cpi.df$USEMV) 
hurstexp(cpi.df$USMPU) 

#Tests for non-linearity
nonlinearityTest(ts(cpi.df$Unemploymentrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$RealbroadEER), verbose = TRUE)
nonlinearityTest(ts(cpi.df$ShorttermIR), verbose = TRUE)
nonlinearityTest(ts(cpi.df$OilpriceGlobalWTI), verbose = TRUE)
nonlinearityTest(ts(cpi.df$CPIinflationrate), verbose = TRUE)
nonlinearityTest(ts(cpi.df$logEPU), verbose = TRUE)
nonlinearityTest(ts(cpi.df$GPRC), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USEMV), verbose = TRUE)
nonlinearityTest(ts(cpi.df$USMPU), verbose = TRUE)

# Test for Seasonality
isSeasonal(ts(cpi.df$Unemploymentrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$RealbroadEER), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$ShorttermIR), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$OilpriceGlobalWTI), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$CPIinflationrate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$logEPU), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$GPRC), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USEMV), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$USMPU), test = "combined", freq = 12)

# Skewness
skewness(cpi.df$Unemploymentrate) 
skewness(cpi.df$RealbroadEER) 
skewness(cpi.df$ShorttermIR) 
skewness(cpi.df$OilpriceGlobalWTI) 
skewness(cpi.df$CPIinflationrate) 
skewness(cpi.df$logEPU) 
skewness(cpi.df$GPRC) 
skewness(cpi.df$USEMV) 
skewness(cpi.df$USMPU) 

# Kurtosis
kurtosis(cpi.df$Unemploymentrate) 
kurtosis(cpi.df$RealbroadEER) 
kurtosis(cpi.df$ShorttermIR) 
kurtosis(cpi.df$OilpriceGlobalWTI) 
kurtosis(cpi.df$CPIinflationrate) 
kurtosis(cpi.df$logEPU) 
kurtosis(cpi.df$GPRC) 
kurtosis(cpi.df$USEMV) 
kurtosis(cpi.df$USMPU) 
########################## End of Code #######################
########################## End of Code #####################