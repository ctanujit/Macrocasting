######################### ACF and PACF Plots: G7 countries ####################
# =========================
# Libraries
# =========================
library(ggplot2)
library(forecast)
library(gridExtra)
library(grid)
library(readr)
library(dplyr)
library(stringr)

# =========================
# Parameters and Setup
# =========================
base_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset"
output_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Data_Analysis/output"
# setwd(base_dir)
# getwd()
# setwd(output_dir)
# getwd()


country_dirs <- c("CANADA", "USA", "FRANCE", "GERMANY", "JAPAN", "UK", "ITALY")
country_names <- c("Canada", "US", "France", "Germany", "Japan", "UK", "Italy")
country_files <- tolower(country_dirs)

var_list <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
var_titles <- c("Unemployment Rate", "REER", "SIR", "Oil Price (WTI)", "CPI Inflation")

training_start <- as.Date("1995-01-01")
training_end <- as.Date("2022-03-01")

# =========================
# Helper function to load and filter data
# =========================
load_country_data <- function(country_dir, country_file) {
  setwd(file.path(base_dir, country_dir))
  data_ts <- read.csv(paste0("all_mulvar_data_", country_file, "_v2.csv"), header = TRUE)
  data_ts$Date <- as.Date(data_ts$Date)
  data_ts_train <- data_ts %>%
    filter(Date >= training_start & Date <= training_end)
  return(data_ts_train)
}

make_acf_pacf_plot <- function(ts_data, var, country, var_title, plot_type = "acf", show_xlab = FALSE, show_ylab = FALSE) {
  y <- ts_data[[var]]
  plot_title <- paste0(var_title, " (", country, ")")
  if (plot_type == "acf") {
    p <- ggAcf(y, lag.max = 36, size = 0.6) +
      ggtitle(plot_title) +
      theme_minimal(base_size = 8) +
      theme(
        panel.grid = element_blank(),
        # plot.title = element_text(hjust = 0.5, size = 4, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 6, face = "bold"),
        axis.title.y = element_text(size = 7, face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"),
        axis.text.y = element_text(size = 6),      # y-axis values always shown
        axis.text.x = element_text(size = 6),      # x-axis values controlled below
        axis.ticks.y = element_line(),             # y-axis ticks always shown
        axis.ticks.x = element_line()              # x-axis ticks controlled below
      )
  } else {
    p <- ggPacf(y, lag.max = 36, size = 0.6) +
      ggtitle(plot_title) +
      theme_minimal(base_size = 8) +
      theme(
        panel.grid = element_blank(),
        # plot.title = element_text(hjust = 0.5, size = 4, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 6, face = "bold"),
        axis.title.y = element_text(size = 7, face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"),
        axis.text.y = element_text(size = 6),      # y-axis values always shown
        axis.text.x = element_text(size = 6),      # x-axis values controlled below
        axis.ticks.y = element_line(),             # y-axis ticks always shown
        axis.ticks.x = element_line()              # x-axis ticks controlled below
      )
  }
  # Only show x-axis label and values for last row
  if (show_xlab) {
    p <- p + xlab("lag")
  } else {
    p <- p + xlab(NULL) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  # Only show y-axis label for first column, but always show y-axis values/ticks
  if (show_ylab) {
    p <- p + ylab(ifelse(plot_type == "acf", "ACF", "PACF"))
  } else {
    p <- p + ylab(NULL)
  }
  return(p)
}

# Generate all plots
acf_plots <- list()
pacf_plots <- list()
n_countries <- length(country_dirs)
n_vars <- length(var_list)

for (i in seq_along(country_dirs)) {
  country_dir <- country_dirs[i]
  country_file <- country_files[i]
  country_name <- country_names[i]
  data_ts_train <- load_country_data(country_dir, country_file)
  
  for (j in seq_along(var_list)) {
    var <- var_list[j]
    var_title <- var_titles[j]
    show_xlab <- (i == n_countries)
    show_ylab <- (j == 1)
    acf_plots[[length(acf_plots) + 1]] <- make_acf_pacf_plot(data_ts_train, var, country_name, var_title, "acf", show_xlab, show_ylab)
    pacf_plots[[length(pacf_plots) + 1]] <- make_acf_pacf_plot(data_ts_train, var, country_name, var_title, "pacf", show_xlab, show_ylab)
  }
}

# Save ACF grid as PNG (no global label)
png(file = file.path(output_dir, "G7_ACF_Stacked_revised_paper.png"), width = 2400, height = 1800, res = 300)
grid.arrange(grobs = acf_plots, nrow = 7, ncol = 5)
dev.off()

# Save PACF grid as PNG (no global label)
# png(file = file.path(output_dir, "G7_PACF_Stacked_revised.png"), width = 2400, height = 1800, res = 300)
# grid.arrange(grobs = pacf_plots, nrow = 7, ncol = 5)
# dev.off()