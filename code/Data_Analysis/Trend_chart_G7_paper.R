################################ Stacked Trend Plot -G7 ############################
library(ggplot2)
library(gridExtra)
library(grid)
library(readr)
library(dplyr)
library(stringr)

# Parameters and Setup
base_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset"
output_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Data_Analysis/output"

country_dirs <- c("CANADA", "USA", "FRANCE", "GERMANY", "JAPAN", "UK", "ITALY")
country_names <- c("Canada", "US", "France", "Germany", "Japan", "UK", "Italy")
country_files <- tolower(country_dirs)

var_list <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
var_titles <- c("Unemployment Rate", "REER", "SIR", "Oil Price (WTI)", "CPI Inflation")

training_start <- as.Date("1995-01-01")
training_end <- as.Date("2022-03-01")

# Helper function to load and filter data
load_country_data <- function(country_dir, country_file) {
  setwd(file.path(base_dir, country_dir))
  data_ts <- read.csv(paste0("all_mulvar_data_", country_file, "_v2.csv"), header = TRUE)
  data_ts$Date <- as.Date(data_ts$Date)
  data_ts_train <- data_ts %>%
    filter(Date >= training_start & Date <= training_end)
  return(data_ts_train)
}

# Helper function to create trend plots (no axes bars)
make_trend_plot <- function(ts_data, var, country, var_title) {
  plot_title <- paste0(var_title, " (", country, ")")
  p <- ggplot(ts_data, aes(x = Date, y = .data[[var]])) +
    geom_line(size = 0.6, color = "#1f77b4") +
    ggtitle(plot_title) +
    theme_minimal(base_size = 8) +
    theme(
      panel.grid = element_blank(),
      # plot.title = element_text(hjust = 0.5, size = 4, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 6, face = "bold"),
      axis.title.y = element_blank(), # No y-axis label
      axis.title.x = element_blank(), # No x-axis label
      axis.text.y = element_text(size = 6),
      axis.text.x = element_text(size = 6),
      axis.ticks.y = element_line(),
      axis.ticks.x = element_line(),
      panel.border = element_blank()
      # axis.line removed: no axes bars
    )
  return(p)
}

# Generate all trend plots
trend_plots <- list()
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
    trend_plots[[length(trend_plots) + 1]] <- make_trend_plot(data_ts_train, var, country_name, var_title)
  }
}

# Save stacked trend chart as PNG
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

png(file = file.path(output_dir, "G7_Trend_Stacked_revised_paper.png"), width = 2400, height = 2400, res = 300)
grid.arrange(
  arrangeGrob(grobs = trend_plots, nrow = 7, ncol = 5),
  nrow = 2,
  heights = c(35, 1) # More height for plots, adjust as needed
)
dev.off()
#################### End of Code ###############
