############# Outlier Tests ###################
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

############################# Country: CANADA ###################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/canada")
getwd()

# Read the dataset
var.canada <- read.csv("all_mulvar_data_canada_v2.csv", header = TRUE)
str(var.canada)

# Convert Unemploymentrate into a time series object
Unemploymentrate_canada <- ts(var.canada$Unemploymentrate, 
                              start = 1995, 
                              end = 2024, 
                              frequency = 12)
length(Unemploymentrate_canada)


# Convert RealbroadEER into a time series object
RealbroadEER_canada <- ts(var.canada$RealbroadEER, 
                          start = 1995, 
                          end = 2024, 
                          frequency = 12)
# RealbroadEER_canada
length(RealbroadEER_canada)

# Convert ShorttermIR into a time series object
ShorttermIR_canada <- ts(var.canada$ShorttermIR, 
                         start = 1995, 
                         end = 2024, 
                         frequency = 12)
# ShorttermIR_canada
length(ShorttermIR_canada)

# Convert OilpriceGlobalWTI into a time series object
OilpriceGlobalWTI_canada <- ts(var.canada$OilpriceGlobalWTI, 
                               start = 1995, 
                               end = 2024, 
                               frequency = 12)
# OilpriceGlobalWTI_canada
length(OilpriceGlobalWTI_canada)

# Convert CPIinflationrate into a time series object
CPIinflationrate_canada <- ts(var.canada$CPIinflationrate, 
                              start = 1995, 
                              end = 2024, 
                              frequency = 12)
# CPIinflationrate
length(CPIinflationrate_canada)

# Convert logEPU into a time series object
logEPU_canada <- ts(var.canada$logEPU, 
                    start = 1995, 
                    end = 2024, 
                    frequency = 12)
# logEPU_canada
length(logEPU_canada)

# Convert GPRC into a time series object
GPRC_canada <- ts(var.canada$GPRC, 
                  start = 1995, 
                  end = 2024, 
                  frequency = 12)
# GPRC_canada
length(GPRC_canada)

# Convert USEMV into a time series object
USEMV_canada <- ts(var.canada$USEMV, 
                   start = 1995, 
                   end = 2024, 
                   frequency = 12)
# USEMV_canada
length(USEMV_canada)

# Convert USMPU into a time series object
USMPU_canada <- ts(var.canada$USMPU, 
                   start = 1995, 
                   end = 2024, 
                   frequency = 12)
# USMPU_canada
length(USMPU_canada)

time <- c(1:349)
library(car)

# Unemploymentrate
test_out_Unemploymentrate <- outlierTest(lm(Unemploymentrate_canada~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_Unemploymentrate)

# RealbroadEER
test_out_RealbroadEER <- outlierTest(lm(RealbroadEER_canada~time, 
                                        cutoff=0.05, 
                                        n.max=10, 
                                        order=TRUE, 
                                        labels=names(rstudent)))
print(test_out_RealbroadEER)

# ShorttermIR
test_out_ShorttermIR <- outlierTest(lm(ShorttermIR_canada~time, 
                                       cutoff=0.05, 
                                       n.max=10, 
                                       order=TRUE, 
                                       labels=names(rstudent)))
print(test_out_ShorttermIR)

# OilpriceGlobalWTI
test_out_OilpriceGlobalWTI <- outlierTest(lm(OilpriceGlobalWTI_canada~time, 
                                             cutoff=0.05, 
                                             n.max=10, 
                                             order=TRUE, 
                                             labels=names(rstudent)))
print(test_out_OilpriceGlobalWTI)

# CPIinflationrate
test_out_CPIinflationrate <- outlierTest(lm(CPIinflationrate_canada~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_CPIinflationrate)

# logEPU
test_out_logEPU <- outlierTest(lm(logEPU_canada~time, 
                                  cutoff=0.05, 
                                  n.max=10, 
                                  order=TRUE, 
                                  labels=names(rstudent)))
print(test_out_logEPU)

# GPRC
test_out_GPRC <- outlierTest(lm(GPRC_canada~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_GPRC)

# USEMV
test_out_USEMV <- outlierTest(lm(USEMV_canada~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USEMV)

# USMPU
test_out_USMPU <- outlierTest(lm(USMPU_canada~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USMPU)

# ============================================================

############################# Country: USA ###################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/usa")
getwd()

# Read the dataset
var.usa <- read.csv("all_mulvar_data_usa_v2.csv", header = TRUE)
str(var.usa)

# Convert Unemploymentrate into a time series object
Unemploymentrate_usa <- ts(var.usa$Unemploymentrate, 
                           start = 1995, 
                           end = 2024, 
                           frequency = 12)
length(Unemploymentrate_usa)


# Convert RealbroadEER into a time series object
RealbroadEER_usa <- ts(var.usa$RealbroadEER, 
                       start = 1995, 
                       end = 2024, 
                       frequency = 12)
# RealbroadEER_usa
length(RealbroadEER_usa)

# Convert ShorttermIR into a time series object
ShorttermIR_usa <- ts(var.usa$ShorttermIR, 
                      start = 1995, 
                      end = 2024, 
                      frequency = 12)
# ShorttermIR_usa
length(ShorttermIR_usa)

# Convert OilpriceGlobalWTI into a time series object
OilpriceGlobalWTI_usa <- ts(var.usa$OilpriceGlobalWTI, 
                            start = 1995, 
                            end = 2024, 
                            frequency = 12)
# OilpriceGlobalWTI_usa
length(OilpriceGlobalWTI_usa)

# Convert CPIinflationrate into a time series object
CPIinflationrate_usa <- ts(var.usa$CPIinflationrate, 
                           start = 1995, 
                           end = 2024, 
                           frequency = 12)
# CPIinflationrate
length(CPIinflationrate_usa)

# Convert logEPU into a time series object
logEPU_usa <- ts(var.usa$logEPU, 
                 start = 1995, 
                 end = 2024, 
                 frequency = 12)
# logEPU_usa
length(logEPU_usa)

# Convert GPRC into a time series object
GPRC_usa <- ts(var.usa$GPRC, 
               start = 1995, 
               end = 2024, 
               frequency = 12)
# GPRC_usa
length(GPRC_usa)

# Convert USEMV into a time series object
USEMV_usa <- ts(var.usa$USEMV, 
                start = 1995, 
                end = 2024, 
                frequency = 12)
# USEMV_usa
length(USEMV_usa)

# Convert USMPU into a time series object
USMPU_usa <- ts(var.usa$USMPU, 
                start = 1995, 
                end = 2024, 
                frequency = 12)
# USMPU_usa
length(USMPU_usa)

time <- c(1:349)

library(car)

# Unemploymentrate
test_out_Unemploymentrate <- outlierTest(lm(Unemploymentrate_usa~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_Unemploymentrate)

# RealbroadEER
test_out_RealbroadEER <- outlierTest(lm(RealbroadEER_usa~time, 
                                        cutoff=0.05, 
                                        n.max=10, 
                                        order=TRUE, 
                                        labels=names(rstudent)))
print(test_out_RealbroadEER)

# ShorttermIR
test_out_ShorttermIR <- outlierTest(lm(ShorttermIR_usa~time, 
                                       cutoff=0.05, 
                                       n.max=10, 
                                       order=TRUE, 
                                       labels=names(rstudent)))
print(test_out_ShorttermIR)

# OilpriceGlobalWTI
test_out_OilpriceGlobalWTI <- outlierTest(lm(OilpriceGlobalWTI_usa~time, 
                                             cutoff=0.05, 
                                             n.max=10, 
                                             order=TRUE, 
                                             labels=names(rstudent)))
print(test_out_OilpriceGlobalWTI)

# CPIinflationrate
test_out_CPIinflationrate <- outlierTest(lm(CPIinflationrate_usa~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_CPIinflationrate)

# logEPU
test_out_logEPU <- outlierTest(lm(logEPU_usa~time, 
                                  cutoff=0.05, 
                                  n.max=10, 
                                  order=TRUE, 
                                  labels=names(rstudent)))
print(test_out_logEPU)

# GPRC
test_out_GPRC <- outlierTest(lm(GPRC_usa~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_GPRC)

# USEMV
test_out_USEMV <- outlierTest(lm(USEMV_usa~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USEMV)

# USMPU
test_out_USEMV <- outlierTest(lm(USMPU_usa~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USMPU)

# ============================================================

############################# Country: GERMANY ###################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/germany")
getwd()

# Read the dataset
var.germany <- read.csv("all_mulvar_data_germany_v2.csv", header = TRUE)
str(var.germany)

# Convert Unemploymentrate into a time series object
Unemploymentrate_germany <- ts(var.germany$Unemploymentrate, 
                               start = 1995, 
                               end = 2024, 
                               frequency = 12)
length(Unemploymentrate_germany)


# Convert RealbroadEER into a time series object
RealbroadEER_germany <- ts(var.germany$RealbroadEER, 
                           start = 1995, 
                           end = 2024, 
                           frequency = 12)
# RealbroadEER_germany
length(RealbroadEER_germany)

# Convert ShorttermIR into a time series object
ShorttermIR_germany <- ts(var.germany$ShorttermIR, 
                          start = 1995, 
                          end = 2024, 
                          frequency = 12)
# ShorttermIR_germany
length(ShorttermIR_germany)

# Convert OilpriceGlobalWTI into a time series object
OilpriceGlobalWTI_germany <- ts(var.germany$OilpriceGlobalWTI, 
                                start = 1995, 
                                end = 2024, 
                                frequency = 12)
# OilpriceGlobalWTI_germany
length(OilpriceGlobalWTI_germany)

# Convert CPIinflationrate into a time series object
CPIinflationrate_germany <- ts(var.germany$CPIinflationrate, 
                               start = 1995, 
                               end = 2024, 
                               frequency = 12)
# CPIinflationrate
length(CPIinflationrate_germany)

# Convert logEPU into a time series object
logEPU_germany <- ts(var.germany$logEPU, 
                     start = 1995, 
                     end = 2024, 
                     frequency = 12)
# logEPU_germany
length(logEPU_germany)

# Convert GPRC into a time series object
GPRC_germany <- ts(var.germany$GPRC, 
                   start = 1995, 
                   end = 2024, 
                   frequency = 12)
# GPRC_germany
length(GPRC_germany)

# Convert USEMV into a time series object
USEMV_germany <- ts(var.germany$USEMV, 
                    start = 1995, 
                    end = 2024, 
                    frequency = 12)
# USEMV_germany
length(USEMV_germany)

# Convert USMPU into a time series object
USMPU_germany <- ts(var.germany$USMPU, 
                    start = 1995, 
                    end = 2024, 
                    frequency = 12)
# USMPU_germany
length(USMPU_germany)

time <- c(1:349)

library(car)

# Unemploymentrate
test_out_Unemploymentrate <- outlierTest(lm(Unemploymentrate_germany~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_Unemploymentrate)

# RealbroadEER
test_out_RealbroadEER <- outlierTest(lm(RealbroadEER_germany~time, 
                                        cutoff=0.05, 
                                        n.max=10, 
                                        order=TRUE, 
                                        labels=names(rstudent)))
print(test_out_RealbroadEER)

# ShorttermIR
test_out_ShorttermIR <- outlierTest(lm(ShorttermIR_germany~time, 
                                       cutoff=0.05, 
                                       n.max=10, 
                                       order=TRUE, 
                                       labels=names(rstudent)))
print(test_out_ShorttermIR)

# OilpriceGlobalWTI
test_out_OilpriceGlobalWTI <- outlierTest(lm(OilpriceGlobalWTI_germany~time, 
                                             cutoff=0.05, 
                                             n.max=10, 
                                             order=TRUE, 
                                             labels=names(rstudent)))
print(test_out_OilpriceGlobalWTI)

# CPIinflationrate
test_out_CPIinflationrate <- outlierTest(lm(CPIinflationrate_germany~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_CPIinflationrate)

# logEPU
test_out_logEPU <- outlierTest(lm(logEPU_germany~time, 
                                  cutoff=0.05, 
                                  n.max=10, 
                                  order=TRUE, 
                                  labels=names(rstudent)))
print(test_out_logEPU)

# GPRC
test_out_GPRC <- outlierTest(lm(GPRC_germany~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_GPRC)

# USEMV
test_out_USEMV <- outlierTest(lm(USEMV_germany~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USEMV)

# USMPU
test_out_USEMV <- outlierTest(lm(USMPU_germany~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USMPU)

# ============================================================
############################# Country: FRANCE ###################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/france")
getwd()

# Read the dataset
var.france <- read.csv("all_mulvar_data_france_v2.csv", header = TRUE)
str(var.france)

# Convert Unemploymentrate into a time series object
Unemploymentrate_france <- ts(var.france$Unemploymentrate, 
                              start = 1995, 
                              end = 2024, 
                              frequency = 12)
length(Unemploymentrate_france)


# Convert RealbroadEER into a time series object
RealbroadEER_france <- ts(var.france$RealbroadEER, 
                          start = 1995, 
                          end = 2024, 
                          frequency = 12)
# RealbroadEER_france
length(RealbroadEER_france)

# Convert ShorttermIR into a time series object
ShorttermIR_france <- ts(var.france$ShorttermIR, 
                         start = 1995, 
                         end = 2024, 
                         frequency = 12)
# ShorttermIR_france
length(ShorttermIR_france)

# Convert OilpriceGlobalWTI into a time series object
OilpriceGlobalWTI_france <- ts(var.france$OilpriceGlobalWTI, 
                               start = 1995, 
                               end = 2024, 
                               frequency = 12)
# OilpriceGlobalWTI_france
length(OilpriceGlobalWTI_france)

# Convert CPIinflationrate into a time series object
CPIinflationrate_france <- ts(var.france$CPIinflationrate, 
                              start = 1995, 
                              end = 2024, 
                              frequency = 12)
# CPIinflationrate
length(CPIinflationrate_france)

# Convert logEPU into a time series object
logEPU_france <- ts(var.france$logEPU, 
                    start = 1995, 
                    end = 2024, 
                    frequency = 12)
# logEPU_france
length(logEPU_france)

# Convert GPRC into a time series object
GPRC_france <- ts(var.france$GPRC, 
                  start = 1995, 
                  end = 2024, 
                  frequency = 12)
# GPRC_france
length(GPRC_france)

# Convert USEMV into a time series object
USEMV_france <- ts(var.france$USEMV, 
                   start = 1995, 
                   end = 2024, 
                   frequency = 12)
# USEMV_france
length(USEMV_france)

# Convert USMPU into a time series object
USMPU_france <- ts(var.france$USMPU, 
                   start = 1995, 
                   end = 2024, 
                   frequency = 12)
# USMPU_france
length(USMPU_france)

time <- c(1:349)

library(car)

# Unemploymentrate
test_out_Unemploymentrate <- outlierTest(lm(Unemploymentrate_france~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_Unemploymentrate)

# RealbroadEER
test_out_RealbroadEER <- outlierTest(lm(RealbroadEER_france~time, 
                                        cutoff=0.05, 
                                        n.max=10, 
                                        order=TRUE, 
                                        labels=names(rstudent)))
print(test_out_RealbroadEER)

# ShorttermIR
test_out_ShorttermIR <- outlierTest(lm(ShorttermIR_france~time, 
                                       cutoff=0.05, 
                                       n.max=10, 
                                       order=TRUE, 
                                       labels=names(rstudent)))
print(test_out_ShorttermIR)

# OilpriceGlobalWTI
test_out_OilpriceGlobalWTI <- outlierTest(lm(OilpriceGlobalWTI_france~time, 
                                             cutoff=0.05, 
                                             n.max=10, 
                                             order=TRUE, 
                                             labels=names(rstudent)))
print(test_out_OilpriceGlobalWTI)

# CPIinflationrate
test_out_CPIinflationrate <- outlierTest(lm(CPIinflationrate_france~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_CPIinflationrate)

# logEPU
test_out_logEPU <- outlierTest(lm(logEPU_france~time, 
                                  cutoff=0.05, 
                                  n.max=10, 
                                  order=TRUE, 
                                  labels=names(rstudent)))
print(test_out_logEPU)

# GPRC
test_out_GPRC <- outlierTest(lm(GPRC_france~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_GPRC)

# USEMV
test_out_USEMV <- outlierTest(lm(USEMV_france~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USEMV)

# USMPU
test_out_USEMV <- outlierTest(lm(USMPU_france~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USMPU)

# ============================================================

############################# Country: JAPAN ###################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/japan")
getwd()

# Read the dataset
var.japan <- read.csv("all_mulvar_data_japan_v2.csv", header = TRUE)
str(var.japan)

# Convert Unemploymentrate into a time series object
Unemploymentrate_japan <- ts(var.japan$Unemploymentrate, 
                             start = 1995, 
                             end = 2024, 
                             frequency = 12)
length(Unemploymentrate_japan)


# Convert RealbroadEER into a time series object
RealbroadEER_japan <- ts(var.japan$RealbroadEER, 
                         start = 1995, 
                         end = 2024, 
                         frequency = 12)
# RealbroadEER_japan
length(RealbroadEER_japan)

# Convert ShorttermIR into a time series object
ShorttermIR_japan <- ts(var.japan$ShorttermIR, 
                        start = 1995, 
                        end = 2024, 
                        frequency = 12)
# ShorttermIR_japan
length(ShorttermIR_japan)

# Convert OilpriceGlobalWTI into a time series object
OilpriceGlobalWTI_japan <- ts(var.japan$OilpriceGlobalWTI, 
                              start = 1995, 
                              end = 2024, 
                              frequency = 12)
# OilpriceGlobalWTI_japan
length(OilpriceGlobalWTI_japan)

# Convert CPIinflationrate into a time series object
CPIinflationrate_japan <- ts(var.japan$CPIinflationrate, 
                             start = 1995, 
                             end = 2024, 
                             frequency = 12)
# CPIinflationrate
length(CPIinflationrate_japan)

# Convert logEPU into a time series object
logEPU_japan <- ts(var.japan$logEPU, 
                   start = 1995, 
                   end = 2024, 
                   frequency = 12)
# logEPU_japan
length(logEPU_japan)

# Convert GPRC into a time series object
GPRC_japan <- ts(var.japan$GPRC, 
                 start = 1995, 
                 end = 2024, 
                 frequency = 12)
# GPRC_japan
length(GPRC_japan)

# Convert USEMV into a time series object
USEMV_japan <- ts(var.japan$USEMV, 
                  start = 1995, 
                  end = 2024, 
                  frequency = 12)
# USEMV_japan
length(USEMV_japan)

# Convert USMPU into a time series object
USMPU_japan <- ts(var.japan$USMPU, 
                  start = 1995, 
                  end = 2024, 
                  frequency = 12)
# USMPU_japan
length(USMPU_japan)

time <- c(1:349)

library(car)

# Unemploymentrate
test_out_Unemploymentrate <- outlierTest(lm(Unemploymentrate_japan~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_Unemploymentrate)

# RealbroadEER
test_out_RealbroadEER <- outlierTest(lm(RealbroadEER_japan~time, 
                                        cutoff=0.05, 
                                        n.max=10, 
                                        order=TRUE, 
                                        labels=names(rstudent)))
print(test_out_RealbroadEER)

# ShorttermIR
test_out_ShorttermIR <- outlierTest(lm(ShorttermIR_japan~time, 
                                       cutoff=0.05, 
                                       n.max=10, 
                                       order=TRUE, 
                                       labels=names(rstudent)))
print(test_out_ShorttermIR)

# OilpriceGlobalWTI
test_out_OilpriceGlobalWTI <- outlierTest(lm(OilpriceGlobalWTI_japan~time, 
                                             cutoff=0.05, 
                                             n.max=10, 
                                             order=TRUE, 
                                             labels=names(rstudent)))
print(test_out_OilpriceGlobalWTI)

# CPIinflationrate
test_out_CPIinflationrate <- outlierTest(lm(CPIinflationrate_japan~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_CPIinflationrate)

# logEPU
test_out_logEPU <- outlierTest(lm(logEPU_japan~time, 
                                  cutoff=0.05, 
                                  n.max=10, 
                                  order=TRUE, 
                                  labels=names(rstudent)))
print(test_out_logEPU)

# GPRC
test_out_GPRC <- outlierTest(lm(GPRC_japan~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_GPRC)

# USEMV
test_out_USEMV <- outlierTest(lm(USEMV_japan~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USEMV)

# USMPU
test_out_USEMV <- outlierTest(lm(USMPU_japan~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USMPU)
# ============================================================

############################# Country: UK ###################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/uk")
getwd()

# Read the dataset
var.uk <- read.csv("all_mulvar_data_uk_v2.csv", header = TRUE)
str(var.uk)

# Convert Unemploymentrate into a time series object
Unemploymentrate_uk <- ts(var.uk$Unemploymentrate, 
                          start = 1995, 
                          end = 2024, 
                          frequency = 12)
length(Unemploymentrate_uk)


# Convert RealbroadEER into a time series object
RealbroadEER_uk <- ts(var.uk$RealbroadEER, 
                      start = 1995, 
                      end = 2024, 
                      frequency = 12)
# RealbroadEER_uk
length(RealbroadEER_uk)

# Convert ShorttermIR into a time series object
ShorttermIR_uk <- ts(var.uk$ShorttermIR, 
                     start = 1995, 
                     end = 2024, 
                     frequency = 12)
# ShorttermIR_uk
length(ShorttermIR_uk)

# Convert OilpriceGlobalWTI into a time series object
OilpriceGlobalWTI_uk <- ts(var.uk$OilpriceGlobalWTI, 
                           start = 1995, 
                           end = 2024, 
                           frequency = 12)
# OilpriceGlobalWTI_uk
length(OilpriceGlobalWTI_uk)

# Convert CPIinflationrate into a time series object
CPIinflationrate_uk <- ts(var.uk$CPIinflationrate, 
                          start = 1995, 
                          end = 2024, 
                          frequency = 12)
# CPIinflationrate
length(CPIinflationrate_uk)

# Convert logEPU into a time series object
logEPU_uk <- ts(var.uk$logEPU, 
                start = 1995, 
                end = 2024, 
                frequency = 12)
# logEPU_uk
length(logEPU_uk)

# Convert GPRC into a time series object
GPRC_uk <- ts(var.uk$GPRC, 
              start = 1995, 
              end = 2024, 
              frequency = 12)
# GPRC_uk
length(GPRC_uk)

# Convert USEMV into a time series object
USEMV_uk <- ts(var.uk$USEMV, 
               start = 1995, 
               end = 2024, 
               frequency = 12)
# USEMV_uk
length(USEMV_uk)

# Convert USMPU into a time series object
USMPU_uk <- ts(var.uk$USMPU, 
               start = 1995, 
               end = 2024, 
               frequency = 12)
# USMPU_uk
length(USMPU_uk)

time <- c(1:349)

library(car)

# Unemploymentrate
test_out_Unemploymentrate <- outlierTest(lm(Unemploymentrate_uk~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_Unemploymentrate)

# RealbroadEER
test_out_RealbroadEER <- outlierTest(lm(RealbroadEER_uk~time, 
                                        cutoff=0.05, 
                                        n.max=10, 
                                        order=TRUE, 
                                        labels=names(rstudent)))
print(test_out_RealbroadEER)

# ShorttermIR
test_out_ShorttermIR <- outlierTest(lm(ShorttermIR_uk~time, 
                                       cutoff=0.05, 
                                       n.max=10, 
                                       order=TRUE, 
                                       labels=names(rstudent)))
print(test_out_ShorttermIR)

# OilpriceGlobalWTI
test_out_OilpriceGlobalWTI <- outlierTest(lm(OilpriceGlobalWTI_uk~time, 
                                             cutoff=0.05, 
                                             n.max=10, 
                                             order=TRUE, 
                                             labels=names(rstudent)))
print(test_out_OilpriceGlobalWTI)

# CPIinflationrate
test_out_CPIinflationrate <- outlierTest(lm(CPIinflationrate_uk~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_CPIinflationrate)

# logEPU
test_out_logEPU <- outlierTest(lm(logEPU_uk~time, 
                                  cutoff=0.05, 
                                  n.max=10, 
                                  order=TRUE, 
                                  labels=names(rstudent)))
print(test_out_logEPU)

# GPRC
test_out_GPRC <- outlierTest(lm(GPRC_uk~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_GPRC)

# USEMV
test_out_USEMV <- outlierTest(lm(USEMV_uk~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USEMV)

# USMPU
test_out_USEMV <- outlierTest(lm(USMPU_uk~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USMPU)
# ============================================================

############################# Country: ITALY ###################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/italy")
getwd()

# Read the dataset
var.italy <- read.csv("all_mulvar_data_italy_v2.csv", header = TRUE)
str(var.italy)

# Convert Unemploymentrate into a time series object
Unemploymentrate_italy <- ts(var.italy$Unemploymentrate, 
                             start = 1995, 
                             end = 2024, 
                             frequency = 12)
length(Unemploymentrate_italy)


# Convert RealbroadEER into a time series object
RealbroadEER_italy <- ts(var.italy$RealbroadEER, 
                         start = 1995, 
                         end = 2024, 
                         frequency = 12)
# RealbroadEER_italy
length(RealbroadEER_italy)

# Convert ShorttermIR into a time series object
ShorttermIR_italy <- ts(var.italy$ShorttermIR, 
                        start = 1995, 
                        end = 2024, 
                        frequency = 12)
# ShorttermIR_italy
length(ShorttermIR_italy)

# Convert OilpriceGlobalWTI into a time series object
OilpriceGlobalWTI_italy <- ts(var.italy$OilpriceGlobalWTI, 
                              start = 1995, 
                              end = 2024, 
                              frequency = 12)
# OilpriceGlobalWTI_italy
length(OilpriceGlobalWTI_italy)

# Convert CPIinflationrate into a time series object
CPIinflationrate_italy <- ts(var.italy$CPIinflationrate, 
                             start = 1995, 
                             end = 2024, 
                             frequency = 12)
# CPIinflationrate
length(CPIinflationrate_italy)

# Convert logEPU into a time series object
logEPU_italy <- ts(var.italy$logEPU, 
                   start = 1995, 
                   end = 2024, 
                   frequency = 12)
# logEPU_italy
length(logEPU_italy)

# Convert GPRC into a time series object
GPRC_italy <- ts(var.italy$GPRC, 
                 start = 1995, 
                 end = 2024, 
                 frequency = 12)
# GPRC_italy
length(GPRC_italy)

# Convert USEMV into a time series object
USEMV_italy <- ts(var.italy$USEMV, 
                  start = 1995, 
                  end = 2024, 
                  frequency = 12)
# USEMV_italy
length(USEMV_italy)

# Convert USMPU into a time series object
USMPU_italy <- ts(var.italy$USMPU, 
                  start = 1995, 
                  end = 2024, 
                  frequency = 12)
# USMPU_italy
length(USMPU_italy)

time <- c(1:349)

library(car)

# Unemploymentrate
test_out_Unemploymentrate <- outlierTest(lm(Unemploymentrate_italy~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_Unemploymentrate)

# RealbroadEER
test_out_RealbroadEER <- outlierTest(lm(RealbroadEER_italy~time, 
                                        cutoff=0.05, 
                                        n.max=10, 
                                        order=TRUE, 
                                        labels=names(rstudent)))
print(test_out_RealbroadEER)

# ShorttermIR
test_out_ShorttermIR <- outlierTest(lm(ShorttermIR_italy~time, 
                                       cutoff=0.05, 
                                       n.max=10, 
                                       order=TRUE, 
                                       labels=names(rstudent)))
print(test_out_ShorttermIR)

# OilpriceGlobalWTI
test_out_OilpriceGlobalWTI <- outlierTest(lm(OilpriceGlobalWTI_italy~time, 
                                             cutoff=0.05, 
                                             n.max=10, 
                                             order=TRUE, 
                                             labels=names(rstudent)))
print(test_out_OilpriceGlobalWTI)

# CPIinflationrate
test_out_CPIinflationrate <- outlierTest(lm(CPIinflationrate_italy~time, 
                                            cutoff=0.05, 
                                            n.max=10, 
                                            order=TRUE, 
                                            labels=names(rstudent)))
print(test_out_CPIinflationrate)

# logEPU
test_out_logEPU <- outlierTest(lm(logEPU_italy~time, 
                                  cutoff=0.05, 
                                  n.max=10, 
                                  order=TRUE, 
                                  labels=names(rstudent)))
print(test_out_logEPU)

# GPRC
test_out_GPRC <- outlierTest(lm(GPRC_italy~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_GPRC)

# USEMV
test_out_USEMV <- outlierTest(lm(USEMV_italy~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USEMV)

# USMPU
test_out_USEMV <- outlierTest(lm(USMPU_italy~time, 
                                 cutoff=0.05, 
                                 n.max=10, 
                                 order=TRUE, 
                                 labels=names(rstudent)))
print(test_out_USMPU)
# ============================================================
########################## End of Code ##########################

