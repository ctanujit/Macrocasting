############## TVAR Model: Canada: 12M and 24M ahead - forecasts ####################
# Load required libraries
library(vars)
library(forecast)
library(nnet)
library(Metrics)
library(dplyr)
library(tsDyn)
library(urca)
library(tseries)

# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/canada")
getwd()

# Read the dataset
var.canada <- read.csv("all_mulvar_data_canada_v2.csv", header = TRUE)
str(var.canada)
# Convert Date into Datetime Value
var.canada$Date <- as.Date(var.canada$Date)
str(var.canada)

# Create train, validation, and test datasets for 12M forecast
var.canada.12M.train <- var.canada[1:327,]
var.canada.12M.val <- var.canada[328:339,]
var.canada.12M.test <- var.canada[340:351,]
var.canada.12M.full.train <- var.canada[1:339,]

# Create train, validation, and test datasets for 24M forecast
var.canada.24M.train <- var.canada[1:303,]
var.canada.24M.val <- var.canada[304:327,]
var.canada.24M.test <- var.canada[328:351,]
var.canada.24M.full.train <- var.canada[1:327,]

# Check for stationarity
check_stationarity <- function(data) {
  variables <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
  for (var in variables) {
    print(paste("KPSS test for", var))
    print(kpss.test(data[[var]], null="Trend"))
  }
}

check_stationarity(var.canada.12M.full.train)

# Function to select optimal lag
select_lag <- function(data, max_lag=6) {
  VARselect(data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")], 
            lag.max=max_lag, type="const", season=4)[["selection"]]
}

# Select lag for 12M and 24M models
lag_12M <- select_lag(var.canada.12M.full.train)
lag_12M

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      2      2 

lag_24M <- select_lag(var.canada.24M.full.train)
lag_24M

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      2      2

# Function to fit TVAR model
fit_tvar <- function(data, lag) {
  TVAR(data=data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")],
       lag=lag,
       include="const",
       nthresh=2,
       thDelay=1,
       trim=0.1,
       mTh=1,
       plot=FALSE)
}

# Fit TVAR models
tvar_12M <- fit_tvar(var.canada.12M.full.train, lag_12M[1])
# Best unique threshold 6.5 
# Second best: 5.8 (conditionnal on th= 6.5 and Delay= 1 ) 	 SSR/AIC: 7451.783
# Second best: 6.5 (conditionnal on th= 5.8 and Delay= 1 ) 	 SSR/AIC: 7451.783
# 
# Second step best thresholds 5.8 6.4 		 SSR: 7448.938 
tvar_24M <- fit_tvar(var.canada.24M.full.train, lag_24M[1])
# Best unique threshold 6.5 
# Second best: 5.9 (conditionnal on th= 6.5 and Delay= 1 ) 	 SSR/AIC: 6783.276
# Second best: 6.5 (conditionnal on th= 5.9 and Delay= 1 ) 	 SSR/AIC: 6783.276
# 
# Second step best thresholds 5.9 6.4 		 SSR: 6778.508 

# Function to generate forecasts
generate_forecasts <- function(model, n_ahead) {
  as.data.frame(predict(model, n.ahead=n_ahead))
}

# Generate 12M and 24M forecasts
forecast_12M <- generate_forecasts(tvar_12M, 12)
forecast_12M

# > forecast_12M
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 340         5.351794     97.27669    4.716886          66.14166        3.5932584
# 341         5.257151     96.32914    4.557942          57.76847        2.7960688
# 342         5.231710     95.50327    4.369202          49.57238        1.9994779
# 343         5.259793     94.89518    4.157428          41.93966        1.2227232
# 344         5.308167     94.49820    3.919606          35.07022        0.4764396
# 345         5.364688     94.30741    3.656244          29.11402       -0.2259292
# 346         5.425909     94.31288    3.370816          24.10182       -0.8743707
# 347         5.492305     94.48946    3.067180          19.98184       -1.4616730
# 348         5.565335     94.80759    2.748380          16.68525       -1.9823293
# 349         5.645214     95.24167    2.416907          14.16600       -2.4316605
# 350         5.730564     95.77247    2.075448          12.40947       -2.8052206
# 351         5.819099     96.38590    1.727397          11.42062       -3.0987659

forecast_24M <- generate_forecasts(tvar_24M, 24)
forecast_24M

# > forecast_24M
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 328         5.956825     106.0281    1.139506         115.40178         6.982974
# 329         6.449210     103.7394    1.176048          95.20464         4.952186
# 330         6.450159     104.3667    1.197653          90.47076         4.369509
# 331         6.105717     105.1025    1.224797          88.57472         4.157267
# 332         6.553793     103.5447    1.155138          73.50467         2.999196
# 333         6.798057     103.7074    1.114886          70.56090         2.660950
# 334         6.723976     104.0531    1.102640          70.04977         2.590934
# 335         6.588965     104.3727    1.105078          69.88860         2.596151
# 336         6.469372     104.6494    1.115027          69.75870         2.613807
# 337         6.376816     104.8971    1.128715          69.63847         2.628828
# 338         6.343172     104.3760    1.126591          64.20367         2.293997
# 339         6.409059     103.6458    1.075780          55.91642         1.899885
# 340         6.743577     103.5954    1.044246          54.88568         1.776272
# 341         6.879096     103.6982    1.033729          55.52072         1.784225
# 342         6.920219     103.8457    1.035568          56.39717         1.839541
# 343         6.923300     104.0088    1.044463          57.24698         1.904855
# 344         6.908931     104.1829    1.057375          58.04010         1.968429
# 345         6.885333     104.3664    1.072535          58.78098         2.027024
# 346         6.856837     104.5570    1.088892          59.47540         2.079840
# 347         6.826185     104.7526    1.105810          60.12779         2.126822
# 348         6.795196     104.9514    1.122900          60.74184         2.168202
# 349         6.765056     105.1517    1.139923          61.32078         2.204344
# 350         6.736513     105.3525    1.156729          61.86755         2.235673
# 351         6.710006     105.5528    1.173225          62.38479         2.262634

################### End of Code ##############################

############## TVAR Model: USA: 12M and 24M ahead - forecasts ####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/usa")
getwd()

# Read the dataset
var.usa <- read.csv("all_mulvar_data_usa_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.usa$Date <- as.Date(var.usa$Date)
str(var.usa)

# Create train, validation, and test datasets for 12M forecast
var.usa.12M.train <- var.usa[1:327,]
var.usa.12M.val <- var.usa[328:339,]
var.usa.12M.test <- var.usa[340:351,]
var.usa.12M.full.train <- var.usa[1:339,]

# Create train, validation, and test datasets for 24M forecast
var.usa.24M.train <- var.usa[1:303,]
var.usa.24M.val <- var.usa[304:327,]
var.usa.24M.test <- var.usa[328:351,]
var.usa.24M.full.train <- var.usa[1:327,]

# Check for stationarity
check_stationarity <- function(data) {
  variables <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
  for (var in variables) {
    print(paste("KPSS test for", var))
    print(kpss.test(data[[var]], null="Trend"))
  }
}

check_stationarity(var.usa.12M.full.train)

# Function to select optimal lag
select_lag <- function(data, max_lag=6) {
  VARselect(data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")], 
            lag.max=max_lag, type="const", season=4)[["selection"]]
}

# Select lag for 12M and 24M models
lag_12M <- select_lag(var.usa.12M.full.train)
lag_12M
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 3      2      2      3 
lag_24M <- select_lag(var.usa.24M.full.train)
lag_24M
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 3      2      2      3 

# Function to fit TVAR model
fit_tvar <- function(data, lag) {
  TVAR(data=data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")],
       lag=lag,
       include="const",
       nthresh=2,
       thDelay=1,
       trim=0.1,
       mTh=1,
       plot=FALSE)
}

# Fit TVAR models
tvar_12M <- fit_tvar(var.usa.12M.full.train, lag_12M[1])
# Best unique threshold 5.8 
# Second best: 4 (conditionnal on th= 5.8 and Delay= 1 ) 	 SSR/AIC: 5898.005
# Second best: 5.8 (conditionnal on th= 4 and Delay= 1 ) 	 SSR/AIC: 5898.005
# 
# Second step best thresholds 4 5.8 		 SSR: 5898.005
tvar_24M <- fit_tvar(var.usa.24M.full.train, lag_24M[1])
# Best unique threshold 5.8 
# Second best: 4 (conditionnal on th= 5.8 and Delay= 1 ) 	 SSR/AIC: 5546.42
# Second best: 5.8 (conditionnal on th= 4 and Delay= 1 ) 	 SSR/AIC: 5546.42
# 
# Second step best thresholds 5.3 6 		 SSR: 5372.434 

# Function to generate forecasts
generate_forecasts <- function(model, n_ahead) {
  as.data.frame(predict(model, n.ahead=n_ahead))
}

# Generate 12M and 24M forecasts
forecast_12M <- generate_forecasts(tvar_12M, 12)
forecast_12M
# > forecast_12M
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 340         3.500165     107.0183    5.227365          75.00038         4.568266
# 341         3.186164     106.1459    5.437892          73.95963         4.327333
# 342         2.918915     106.0945    5.452830          68.56247         4.038316
# 343         3.110190     106.4210    5.451623          67.61481         3.994445
# 344         3.457526     106.6139    5.429622          65.40735         3.942169
# 345         3.607173     107.1300    5.320378          57.24424         3.556669
# 346         3.655378     107.9212    5.217714          49.90809         3.070363
# 347         3.601377     107.9863    5.173620          45.90092         2.666187
# 348         3.402812     107.0482    5.088708          41.81621         2.276897
# 349         3.273478     105.5048    4.917922          39.05435         1.987299
# 350         3.389856     103.6920    4.688257          39.30080         1.876785
# 351         3.645749     102.0359    4.403507          39.48644         1.780916
forecast_24M <- generate_forecasts(tvar_24M, 24)
forecast_24M
# > forecast_24M
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 328         2.700665     103.3336    1.103212          116.5705         9.200169
# 329         1.917211     104.3674    1.418290          121.7230         9.686322
# 330         1.952242     105.3849    1.646578          125.3770        10.038864
# 331         2.430364     106.2344    1.798266          128.7754        10.328698
# 332         2.841835     106.9379    1.903142          132.9457        10.572669
# 333         2.953103     107.5028    1.985944          138.1220        10.801575
# 334         2.806513     107.9141    2.055613          143.9203        11.055663
# 335         2.556813     108.2067    2.110981          149.8061        11.341588
# 336         2.338584     108.4596    2.148831          155.4829        11.636077
# 337         2.211916     108.7300    2.167806          160.9719        11.918834
# 338         2.165352     109.0217    2.168995          166.4541        12.187563
# 339         2.150587     109.3068    2.155119          172.0941        12.450097
# 340         2.123026     109.5589    2.129205          177.9632        12.713838
# 341         2.064754     109.7667    2.093449          184.0518        12.982568
# 342         1.982964     109.9319    2.048856          190.3155        13.257391
# 343         1.894769     110.0620    1.995661          196.7174        13.537840
# 344         1.813348     110.1643    1.933966          203.2488        13.822840
# 345         1.742685     110.2425    1.864131          209.9271        14.111788
# 346         1.679621     110.2961    1.786799          216.7811        14.405139
# 347         1.618656     110.3222    1.702740          223.8361        14.704113
# 348         1.555783     110.3182    1.612676          231.1080        15.010026
# 349         1.489859     110.2827    1.517192          238.6051        15.323855
# 350         1.421891     110.2157    1.416737          246.3337        15.646223
# 351         1.353488     110.1177    1.311681          254.3021        15.977583

################### End of Code ##############################

############## TVAR Model: France: 12M and 24M ahead - forecasts ####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/france")
getwd()

# Read the dataset
var.france <- read.csv("all_mulvar_data_france_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.france$Date <- as.Date(var.france$Date)
str(var.france)

# Create train, validation, and test datasets for 12M forecast
var.france.12M.train <- var.france[1:327,]
var.france.12M.val <- var.france[328:339,]
var.france.12M.test <- var.france[340:351,]
var.france.12M.full.train <- var.france[1:339,]

# Create train, validation, and test datasets for 24M forecast
var.france.24M.train <- var.france[1:303,]
var.france.24M.val <- var.france[304:327,]
var.france.24M.test <- var.france[328:351,]
var.france.24M.full.train <- var.france[1:327,]

# Check for stationarity
check_stationarity <- function(data) {
  variables <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
  for (var in variables) {
    print(paste("KPSS test for", var))
    print(kpss.test(data[[var]], null="Trend"))
  }
}

check_stationarity(var.france.12M.full.train)

# Function to select optimal lag
select_lag <- function(data, max_lag=6) {
  VARselect(data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")], 
            lag.max=max_lag, type="const", season=4)[["selection"]]
}

# Select lag for 12M and 24M models
lag_12M <- select_lag(var.france.12M.full.train)
lag_12M
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      2      2 
lag_24M <- select_lag(var.france.24M.full.train)
lag_24M
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      2      2 

# Function to fit TVAR model
fit_tvar <- function(data, lag) {
  TVAR(data=data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")],
       lag=lag,
       include="const",
       nthresh=2,
       thDelay=1,
       trim=0.1,
       mTh=1,
       plot=FALSE)
}

# Fit TVAR models
tvar_12M <- fit_tvar(var.france.12M.full.train, lag_12M[1])
tvar_24M <- fit_tvar(var.france.24M.full.train, lag_24M[1])

# Function to generate forecasts
generate_forecasts <- function(model, n_ahead) {
  as.data.frame(predict(model, n.ahead=n_ahead))
}

# Generate 12M and 24M forecasts
forecast_12M <- generate_forecasts(tvar_12M, 12)
forecast_24M <- generate_forecasts(tvar_24M, 24)
forecast_12M
forecast_24M
# > forecast_12M
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 340         6.957435     96.88634    3.345126          70.15037         5.636931
# 341         7.067868     97.79747    3.681719          67.50057         5.454136
# 342         7.210535     98.78420    4.023602          63.68707         5.295324
# 343         7.305492     99.79352    4.346664          59.92911         5.107114
# 344         7.333686    100.84626    4.662174          57.02460         4.919903
# 345         7.320295    101.95759    4.970585          55.29531         4.732387
# 346         7.301505    103.12339    5.274549          54.48234         4.545758
# 347         7.301242    104.32185    5.572116          54.14499         4.356145
# 348         7.323986    105.52689    5.859486          53.93100         4.161144
# 349         7.360739    106.71799    6.132438          53.70137         3.959927
# 350         7.399384    107.88371    6.387927          53.48977         3.753455
# 351         7.432254    109.01953    6.624366          53.39518         3.543446
# forecast_24M
# > forecast_24M
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 328         7.353838     95.59705  -0.4049224          119.0825         5.062997
# 329         7.268851     94.91538  -0.3129578          125.6871         5.572254
# 330         7.093207     94.12843  -0.2578663          129.3592         5.952742
# 331         6.848651     93.36235  -0.2428973          130.3555         6.186424
# 332         6.586269     92.64552  -0.2646083          129.3186         6.302750
# 333         6.365851     91.97149  -0.3221960          127.0793         6.350413
# 334         6.233217     91.33524  -0.4164649          124.4249         6.385001
# 335         6.203204     90.73335  -0.5466497          121.9930         6.454432
# 336         6.257280     90.16191  -0.7079447          120.1986         6.586914
# 337         6.354049     89.61458  -0.8910012          119.2171         6.787427
# 338         6.446193     89.08087  -1.0835742          119.0310         7.042389
# 339         6.496565     88.54630  -1.2734448          119.5131         7.329199
# 340         6.487889     87.99442  -1.4513225          120.5126         7.626444
# 341         6.423854     87.40985  -1.6126449          121.9172         7.921156
# 342         6.322919     86.78095  -1.7577798          123.6780         8.211270
# 343         6.208461     86.10128  -1.8907956          125.7992         8.503518
# 344         6.099461     85.36953  -2.0174432          128.3071         8.808530
# 345         6.004876     84.58806  -2.1431493          131.2155         9.135550
# 346         5.922861     83.76090  -2.2716628          134.5024         9.488782
# 347         5.844085     82.89182  -2.4046426          138.1055         9.866362
# 348         5.757044     81.98312  -2.5420923          141.9356        10.261809
# 349         5.653046     81.03529  -2.6832765          145.8987        10.666925
# 350         5.529119     80.04751  -2.8276685          149.9183        11.074813
# 351         5.388228     79.01846  -2.9755686          153.9497        11.481902
################### End of Code ##############################

############## TVAR Model: Germany: 12M and 24M ahead - forecasts ####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/germany")
getwd()

# Read the dataset
var.germany <- read.csv("all_mulvar_data_germany_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.germany$Date <- as.Date(var.germany$Date)
str(var.germany)

# Create train, validation, and test datasets for 12M forecast
var.germany.12M.train <- var.germany[1:327,]
var.germany.12M.val <- var.germany[328:339,]
var.germany.12M.test <- var.germany[340:351,]
var.germany.12M.full.train <- var.germany[1:339,]

# Create train, validation, and test datasets for 24M forecast
var.germany.24M.train <- var.germany[1:303,]
var.germany.24M.val <- var.germany[304:327,]
var.germany.24M.test <- var.germany[328:351,]
var.germany.24M.full.train <- var.germany[1:327,]

# Check for stationarity
check_stationarity <- function(data) {
  variables <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
  for (var in variables) {
    print(paste("KPSS test for", var))
    print(kpss.test(data[[var]], null="Trend"))
  }
}

check_stationarity(var.germany.12M.full.train)

# Function to select optimal lag
select_lag <- function(data, max_lag=6) {
  VARselect(data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")], 
            lag.max=max_lag, type="const", season=4)[["selection"]]
}

# Select lag for 12M and 24M models
lag_12M <- select_lag(var.germany.12M.full.train)
lag_12M
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      2      2 
lag_24M <- select_lag(var.germany.24M.full.train)
lag_24M
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      2      2 

# Function to fit TVAR model
fit_tvar <- function(data, lag) {
  TVAR(data=data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")],
       lag=lag,
       include="const",
       nthresh=2,
       thDelay=1,
       trim=0.1,
       mTh=1,
       plot=FALSE)
}

# Fit TVAR models
tvar_12M <- fit_tvar(var.germany.12M.full.train, lag_12M[1])
# Best unique threshold 7.4 
# Second best: 4.3 (conditionnal on th= 7.4 and Delay= 1 ) 	 SSR/AIC: 6664.395
# Second best: 7.4 (conditionnal on th= 4.3 and Delay= 1 ) 	 SSR/AIC: 6664.395
# 
# Second step best thresholds 4.3 7.4 		 SSR: 6664.395 
tvar_24M <- fit_tvar(var.germany.24M.full.train, lag_24M[1])
# Best unique threshold 7.4 
# Second best: 4.3 (conditionnal on th= 7.4 and Delay= 1 ) 	 SSR/AIC: 5955.894
# Second best: 7.4 (conditionnal on th= 4.3 and Delay= 1 ) 	 SSR/AIC: 5955.894
# 
# Second step best thresholds 4.3 7.4 		 SSR: 5955.894 

# Function to generate forecasts
generate_forecasts <- function(model, n_ahead) {
  as.data.frame(predict(model, n.ahead=n_ahead))
}

# Generate 12M and 24M forecasts
forecast_12M <- generate_forecasts(tvar_12M, 12)
forecast_24M <- generate_forecasts(tvar_24M, 24)
forecast_12M
# > forecast_12M
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 340         2.927361     99.71461    3.160496          68.20096        6.8830214
# 341         2.958789     99.83617    3.381728          59.18658        6.0713414
# 342         2.988914     99.97160    3.573589          48.35350        5.0876190
# 343         3.040725    100.15766    3.730354          36.49396        3.9311428
# 344         3.114523    100.41530    3.845270          23.92949        2.6127232
# 345         3.210275    100.73706    3.911421          10.81507        1.1394775
# 346         3.325883    101.11012    3.922083          -2.73935       -0.4789186
# 347         3.459297    101.52024    3.870880         -16.61920       -2.2307747
# 348         3.608535    101.95436    3.751924         -30.68873       -4.1016808
# 349         3.771742    102.40089    3.559929         -44.78614       -6.0742843
# 350         3.947083    102.84937    3.290322         -58.72347       -8.1280827
# 351         4.132650    103.29014    2.939362         -72.28816      -10.2393361
forecast_24M
# > forecast_24M
# Unemploymentrate RealbroadEER   ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 328         2.991937     98.02274 -0.4764472983         113.12810         6.499750
# 329         2.855228     97.76322 -0.4379701335         112.55366         7.025181
# 330         2.743134     97.39890 -0.3910807777         111.08685         7.381123
# 331         2.650117     97.00035 -0.3474372066         110.20072         7.593483
# 332         2.584807     96.59927 -0.3095318125         109.64995         7.678497
# 333         2.540018     96.23106 -0.2752842040         108.91178         7.675312
# 334         2.511794     95.90456 -0.2423487805         107.72217         7.610251
# 335         2.497786     95.61698 -0.2098912606         106.05746         7.495367
# 336         2.497052     95.36311 -0.1781908682         103.97405         7.333567
# 337         2.508838     95.14055 -0.1478468111         101.51741         7.125491
# 338         2.532278     94.94963 -0.1193433194          98.71042         6.872630
# 339         2.566414     94.79189 -0.0929905792          95.57103         6.577760
# 340         2.610293     94.66891 -0.0690062898          92.12541         6.244413
# 341         2.662999     94.58185 -0.0475821484          88.40949         5.876440
# 342         2.723625     94.53142 -0.0289008379          84.46512         5.477880
# 343         2.791250     94.51789 -0.0131264572          80.33628         5.052972
# 344         2.864923     94.54118 -0.0003927934          76.06734         4.606165
# 345         2.943668     94.60082  0.0092017297          71.70256         4.142086
# 346         3.026490     94.69598  0.0155939693          67.28616         3.665483
# 347         3.112389     94.82543  0.0187544615          62.86211         3.181162
# 348         3.200366     94.98763  0.0186861811          58.47384         2.693938
# 349         3.289431     95.18070  0.0154238099          54.16380         2.208582
# 350         3.378611     95.40248  0.0090330561          49.97308         1.729786
# 351         3.466956     95.65055 -0.0003903158          45.94104         1.262111
################### End of Code ##############################

############## TVAR Model: Japan: 12M and 24M ahead - forecasts ####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/japan")
getwd()

# Read the dataset
var.japan <- read.csv("all_mulvar_data_japan_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.japan$Date <- as.Date(var.japan$Date)
str(var.japan)

# Create train, validation, and test datasets for 12M forecast
var.japan.12M.train <- var.japan[1:327,]
var.japan.12M.val <- var.japan[328:339,]
var.japan.12M.test <- var.japan[340:351,]
var.japan.12M.full.train <- var.japan[1:339,]

# Create train, validation, and test datasets for 24M forecast
var.japan.24M.train <- var.japan[1:303,]
var.japan.24M.val <- var.japan[304:327,]
var.japan.24M.test <- var.japan[328:351,]
var.japan.24M.full.train <- var.japan[1:327,]

# Check for stationarity
check_stationarity <- function(data) {
  variables <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
  for (var in variables) {
    print(paste("KPSS test for", var))
    print(kpss.test(data[[var]], null="Trend"))
  }
}

check_stationarity(var.japan.12M.full.train)

# Function to select optimal lag
select_lag <- function(data, max_lag=6) {
  VARselect(data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")], 
            lag.max=max_lag, type="const", season=4)[["selection"]]
}

# Select lag for 12M and 24M models
lag_12M <- select_lag(var.japan.12M.full.train)
lag_12M
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      1      2 
lag_24M <- select_lag(var.japan.24M.full.train)
lag_24M
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      1      2 
# Function to fit TVAR model
fit_tvar <- function(data, lag) {
  TVAR(data=data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")],
       lag=lag,
       include="const",
       nthresh=2,
       thDelay=1,
       trim=0.1,
       mTh=1,
       plot=FALSE)
}

# Fit TVAR models
tvar_12M <- fit_tvar(var.japan.12M.full.train, lag_12M[1])
# Best unique threshold 4.5 
# Second best: 3.5 (conditionnal on th= 4.5 and Delay= 1 ) 	 SSR/AIC: 9277.661
# Second best: 4.1 (conditionnal on th= 3.5 and Delay= 1 ) 	 SSR/AIC: 9109.352

# Second step best thresholds 3.6 4.1 		 SSR: 9071.611 
tvar_24M <- fit_tvar(var.japan.24M.full.train, lag_24M[1])
# Best unique threshold 4.1 
# Second best: 3.6 (conditionnal on th= 4.1 and Delay= 1 ) 	 SSR/AIC: 8490.176
# Second best: 4.1 (conditionnal on th= 3.6 and Delay= 1 ) 	 SSR/AIC: 8490.176
# 
# Second step best thresholds 3.6 4.1 		 SSR: 8490.176

# Function to generate forecasts
generate_forecasts <- function(model, n_ahead) {
  as.data.frame(predict(model, n.ahead=n_ahead))
}

# Generate 12M and 24M forecasts
forecast_12M <- generate_forecasts(tvar_12M, 12)
forecast_24M <- generate_forecasts(tvar_24M, 24)
forecast_12M
forecast_24M

# > forecast_12M
# Unemploymentrate RealbroadEER  ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 340         2.767080     78.46848 -0.039771204          71.21984         3.064112
# 341         2.791657     80.00518 -0.039500847          68.54394         2.882696
# 342         2.802974     81.55065 -0.040447292          66.19421         2.725232
# 343         2.814369     83.03524 -0.038606623          64.10265         2.572567
# 344         2.823919     84.42889 -0.035838367          62.24867         2.431370
# 345         2.832398     85.73663 -0.032202849          60.60854         2.298494
# 346         2.839837     86.96518 -0.028108398          59.16176         2.174002
# 347         2.846377     88.12076 -0.023751389          57.89113         2.057339
# 348         2.852100     89.20738 -0.019294763          56.78083         1.948259
# 349         2.857095     90.22759 -0.014847238          55.81614         1.846496
# 350         2.861439     91.18303 -0.010487235          54.98315         1.751801
# 351         2.865207     92.07498 -0.006269515          54.26882         1.663913
# > forecast_24M
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 328         2.543636     83.03172 -0.05208324         112.83141        1.4112960
# 329         2.550940     82.03497 -0.07484742         112.31050        1.4432356
# 330         2.560618     80.88454 -0.09723129         110.00886        1.4214694
# 331         2.576146     79.75758 -0.11522956         107.12025        1.3643756
# 332         2.591000     78.76549 -0.13031226         104.18616        1.2967604
# 333         2.604472     77.96006 -0.14253593         101.42949        1.2258184
# 334         2.615770     77.34719 -0.15240273          98.93565        1.1568082
# 335         2.624901     76.91072 -0.16024097          96.72271        1.0918115
# 336         2.632005     76.62540 -0.16638007          94.77851        1.0318538
# 337         2.637338     76.46452 -0.17109908          93.07817        0.9771923
# 338         2.641164     76.40334 -0.17464403          91.59302        0.9277356
# 339         2.643736     76.42040 -0.17722573          90.29480        0.8832010
# 340         2.645280     76.49775 -0.17902389          89.15755        0.8432248
# 341         2.645991     76.62068 -0.18018996          88.15827        0.8074165
# 342         2.646033     76.77734 -0.18085053          87.27697        0.7753888
# 343         2.645546     76.95822 -0.18111062          86.49647        0.7467729
# 344         2.644645     77.15579 -0.18105674          85.80210        0.7212252
# 345         2.643423     77.36411 -0.18075971          85.18138        0.6984301
# 346         2.641961     77.57854 -0.18027716          84.62372        0.6780996
# 347         2.640321     77.79547 -0.17965572          84.12012        0.6599729
# 348         2.638557     78.01215 -0.17893286          83.66296        0.6438142
# 349         2.636711     78.22650 -0.17813851          83.24575        0.6294113
# 350         2.634818     78.43696 -0.17729637          82.86300        0.6165732
# 351         2.632905     78.64242 -0.17642509          82.51006        0.6051288

################### End of Code ##############################

############## TVAR Model: UK: 12M and 24M ahead - forecasts ####################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/uk")
getwd()

# Read the dataset
var.uk <- read.csv("all_mulvar_data_uk_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.uk$Date <- as.Date(var.uk$Date)
str(var.uk)

# Create train, validation, and test datasets for 12M forecast
var.uk.12M.train <- var.uk[1:327,]
var.uk.12M.val <- var.uk[328:339,]
var.uk.12M.test <- var.uk[340:351,]
var.uk.12M.full.train <- var.uk[1:339,]

# Create train, validation, and test datasets for 24M forecast
var.uk.24M.train <- var.uk[1:303,]
var.uk.24M.val <- var.uk[304:327,]
var.uk.24M.test <- var.uk[328:351,]
var.uk.24M.full.train <- var.uk[1:327,]

# Check for stationarity
check_stationarity <- function(data) {
  variables <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
  for (var in variables) {
    print(paste("KPSS test for", var))
    print(kpss.test(data[[var]], null="Trend"))
  }
}

check_stationarity(var.uk.12M.full.train)

# Function to select optimal lag
select_lag <- function(data, max_lag=6) {
  VARselect(data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")], 
            lag.max=max_lag, type="const", season=4)[["selection"]]
}

# Select lag for 12M and 24M models
lag_12M <- select_lag(var.uk.12M.full.train)
lag_12M
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 4      2      2      2 
lag_24M <- select_lag(var.uk.24M.full.train)
lag_24M
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 5      2      2      5

# Function to fit TVAR model
fit_tvar <- function(data, lag) {
  TVAR(data=data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")],
       lag=lag,
       include="const",
       nthresh=2,
       thDelay=1,
       trim=0.1,
       mTh=1,
       plot=FALSE)
}

# Fit TVAR models
tvar_12M <- fit_tvar(var.uk.12M.full.train, lag_12M[1])
# Best unique threshold 5.6 
# Second best: 4.1 (conditionnal on th= 5.6 and Delay= 1 ) 	 SSR/AIC: 5754.329
# Second best: 5.6 (conditionnal on th= 4.1 and Delay= 1 ) 	 SSR/AIC: 5754.329
# 
# Second step best thresholds 4 5.6 		 SSR: 5696.159 
tvar_24M <- fit_tvar(var.uk.24M.full.train, lag_24M[1])
# Best unique threshold 5.6 
# Second best: 4.2 (conditionnal on th= 5.6 and Delay= 1 ) 	 SSR/AIC: 4703.163
# Second best: 5.6 (conditionnal on th= 4.2 and Delay= 1 ) 	 SSR/AIC: 4703.163
# 
# Second step best thresholds 4.2 5.6 		 SSR: 4703.163 

# Function to generate forecasts
generate_forecasts <- function(model, n_ahead) {
  as.data.frame(predict(model, n.ahead=n_ahead))
}

# Generate 12M and 24M forecasts
forecast_12M <- generate_forecasts(tvar_12M, 12)
forecast_24M <- generate_forecasts(tvar_24M, 24)
forecast_12M
forecast_24M

# > forecast_12M
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 340         3.588243    101.54542    4.284335        61.8000351        8.1308660
# 341         3.763874     99.87091    4.298768        49.7717052        7.3614050
# 342         3.987835     98.15443    4.305966        44.4140293        6.8728876
# 343         4.080417     97.39143    4.271125        40.9743422        6.2103376
# 344         4.030531     99.95742    4.424988        45.3687175        5.7281971
# 345         3.970350    102.39207    4.608765        50.3374071        5.4403246
# 346         3.804163     98.67525    4.375649        55.8032293        4.5547945
# 347         3.623169    100.06165    4.090340        35.8743074        4.1687565
# 348         3.723129     96.65163    3.585138        13.0079260        2.5262748
# 349         4.060842     92.47816    3.314383        -8.7786762        0.8059216
# 350         4.256520     93.31611    3.132003        -7.7745541        0.4586015
# 351         4.391363     95.62288    3.021225         0.3909533        0.1484019
# > forecast_24M
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 328         3.722746    101.81098    1.102816        123.898832        6.1888491
# 329         3.832901     98.36533    1.468392        119.641199        5.7558900
# 330         3.600707     96.70927    2.187958        117.559540        5.7413946
# 331         3.345494     93.35923    2.520838        123.181430        5.1013566
# 332         3.129924     89.89781    2.751264        119.481117        4.0887402
# 333         3.135533     93.98361    3.285695        134.291399        3.1507107
# 334         3.307590     92.50987    3.566074        134.338470        1.8231271
# 335         2.507362     90.35898    4.328323        116.262834        1.8574139
# 336         2.121957     92.85672    4.619146        102.542465        0.4733912
# 337         2.443587     82.14580    3.897648         30.991706       -2.8852948
# 338         1.992205     87.61447    4.745344         -5.438956       -4.5128621
# 339         2.363559     95.07281    4.645607         20.912838       -7.2772493
# 340         2.208551     79.62325    3.636234        -24.824843       -7.8269331
# 341         1.487308     93.12780    3.970918        -10.749155       -6.1548514
# 342         3.918456     84.68355    1.256182        -61.159931      -12.1846831
# 343         3.289769     65.90094    1.110187       -215.702965      -12.6779624
# 344         2.243620    111.02158    2.756977       -115.258000      -11.3325086
# 345         4.968475     78.57422   -2.848931       -164.477669      -17.5612360
# 346         6.895737     83.89683   -5.737632       -177.810131      -18.8733690
# 347         6.933558     91.72635   -6.489304       -130.422759      -18.6265511
# 348         5.525750     93.55615   -5.438885       -110.284081      -16.0877085
# 349         6.329920    111.63469   -5.436429        -89.624738      -14.3816843
# 350         6.058606    106.89904   -5.264652        -65.932658      -13.2637388
# 351         5.318681    106.01551   -4.514884        -18.307524      -11.3212643

################### End of Code ##############################

############## TVAR Model: Italy: 12M and 24M ahead - forecasts ####################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/italy")
getwd()

# Read the dataset
var.italy <- read.csv("all_mulvar_data_italy_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.italy$Date <- as.Date(var.italy$Date)
str(var.italy)

# Create train, validation, and test datasets for 12M forecast
var.italy.12M.train <- var.italy[1:327,]
var.italy.12M.val <- var.italy[328:339,]
var.italy.12M.test <- var.italy[340:351,]
var.italy.12M.full.train <- var.italy[1:339,]

# Create train, validation, and test datasets for 24M forecast
var.italy.24M.train <- var.italy[1:303,]
var.italy.24M.val <- var.italy[304:327,]
var.italy.24M.test <- var.italy[328:351,]
var.italy.24M.full.train <- var.italy[1:327,]

# Check for stationarity
check_stationarity <- function(data) {
  variables <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
  for (var in variables) {
    print(paste("KPSS test for", var))
    print(kpss.test(data[[var]], null="Trend"))
  }
}

check_stationarity(var.italy.12M.full.train)

# Function to select optimal lag
select_lag <- function(data, max_lag=6) {
  VARselect(data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")], 
            lag.max=max_lag, type="const", season=4)[["selection"]]
}

# Select lag for 12M and 24M models
lag_12M <- select_lag(var.italy.12M.full.train)
lag_12M
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 6      2      2      6 
lag_24M <- select_lag(var.italy.24M.full.train)
lag_24M
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 4      2      2      4 

# Function to fit TVAR model
fit_tvar <- function(data, lag) {
  TVAR(data=data[,c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")],
       lag=lag,
       include="const",
       nthresh=2,
       thDelay=1,
       trim=0.1,
       mTh=1,
       plot=FALSE)
}

# Fit TVAR models
tvar_12M <- fit_tvar(var.italy.12M.full.train, lag_12M[1])
# Best unique threshold 7.4 
# Second best: 9.1 (conditionnal on th= 7.4 and Delay= 1 ) 	 SSR/AIC: 4868.304
# Second best: 7.4 (conditionnal on th= 9.1 and Delay= 1 ) 	 SSR/AIC: 4868.304
# 
# Second step best thresholds 7 8.1 		 SSR: 4728.534 
tvar_24M <- fit_tvar(var.italy.24M.full.train, lag_24M[1])
# Best unique threshold 7.3 
# Second best: 9.1 (conditionnal on th= 7.3 and Delay= 1 ) 	 SSR/AIC: 5059.51
# Second best: 7.3 (conditionnal on th= 9.1 and Delay= 1 ) 	 SSR/AIC: 5059.51
# 
# Second step best thresholds 7.3 9.1 		 SSR: 5059.51 
# Function to generate forecasts
generate_forecasts <- function(model, n_ahead) {
  as.data.frame(predict(model, n.ahead=n_ahead))
}

# Generate 12M and 24M forecasts
forecast_12M <- generate_forecasts(tvar_12M, 12)
forecast_24M <- generate_forecasts(tvar_24M, 24)
forecast_12M
forecast_24M

# > forecast_12M
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 340         7.641744     101.0031    3.112711          74.71335       6.49875827
# 341         9.180091     100.7165    1.901449          35.33376       5.47806592
# 342         8.911492     103.3723    1.524088          27.22373       4.29880449
# 343         8.393884     106.4231    1.613516          31.32214       3.43262340
# 344        10.173425     108.1768    1.801272          42.61452       2.77985440
# 345        10.982983     108.2660    1.725251          45.78169       1.83713967
# 346        10.227541     108.1305    1.670734          44.39470       1.19800604
# 347         9.827744     109.3645    1.782490          44.86772       0.63967497
# 348        10.366712     110.3287    1.902173          48.49626       0.27231255
# 349        10.443447     110.6544    1.922874          52.62397      -0.06273413
# 350        10.178013     110.9873    1.908626          54.90247      -0.29363272
# 351        10.312522     111.2941    1.956286          55.79569      -0.49593291
# > forecast_24M
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# 328         8.717732     98.76339  -0.5091322          118.7058         7.365139
# 329         8.778680     99.03774  -0.5134918          125.3823         8.013341
# 330         8.064624     99.34914  -0.5271262          131.3497         8.823990
# 331         7.532479     99.83512  -0.5497563          138.0452         9.582065
# 332         6.987685    100.27323  -0.5897835          144.1442        10.355982
# 333        14.553546    103.13881  -1.3419671          159.5173         9.208290
# 334        14.014721    104.58091  -1.6959551          164.5532         9.117518
# 335        12.560661    104.66896  -1.6099392          158.7538         8.260063
# 336        11.460880    107.11657  -1.6384018          163.4453         7.681203
# 337        12.317625    108.43445  -1.6914154          171.0335         7.064438
# 338        12.654684    108.28797  -1.7521001          178.8446         6.428498
# 339        12.275625    108.35723  -1.8344695          183.7049         5.878117
# 340        12.098395    108.98576  -1.8961959          187.8881         5.370900
# 341        12.279398    109.48103  -1.9409576          191.1638         4.863642
# 342        12.518199    109.56763  -2.0042571          192.9229         4.360028
# 343        12.627415    109.54389  -2.0865194          193.4613         3.882897
# 344        12.752866    109.51926  -2.1674643          193.3212         3.442159
# 345        12.967893    109.42913  -2.2444667          192.6271         3.028563
# 346        13.195821    109.24167  -2.3250380          191.2745         2.638016
# 347        13.389014    109.00215  -2.4064787          189.2979         2.271810
# 348        13.566904    108.74203  -2.4828836          186.8836         1.931429
# 349        13.746661    108.45698  -2.5538877          184.1535         1.614727
# 350        13.915519    108.14199  -2.6208844          181.1566         1.320178
# 351        14.060902    107.80724  -2.6829677          177.9366         1.047656
################### End of Code ##############################

