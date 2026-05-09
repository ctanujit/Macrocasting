############### Check for Structural Breaks: OLS-CUSUM test : G7 countries ######################
# Comment: Please convert the Date Column in the input dataset to 1995-01-01 format
# ========================= Final Stacked Chart Generation ======================
# install.packages('gtable')
# install.packages(c("patchwork", "cowplot"))

library(strucchange)
library(ggplot2)
library(zoo)
library(cowplot)

# List of G7 countries and their data file names
g7_info <- list(
  Canada  = list(path = "CANADA",  file = "all_mulvar_data_canada_v2.csv"),
  US     = list(path = "USA",     file = "all_mulvar_data_usa_v2.csv"),
  France  = list(path = "FRANCE",  file = "all_mulvar_data_france_v2.csv"),
  Germany = list(path = "GERMANY", file = "all_mulvar_data_germany_v2.csv"),
  Japan   = list(path = "JAPAN",   file = "all_mulvar_data_japan_v2.csv"),
  UK      = list(path = "UK",      file = "all_mulvar_data_uk_v2.csv"),
  Italy   = list(path = "ITALY",   file = "all_mulvar_data_italy_v2.csv")
)

main_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset"
out_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Data_Analysis/output"
# out_dir <- file.path(main_dir, "CUSUM_test_SB_Overleaf_chart")
out_dir <- file.path(out_dir, "CUSUM_test_SB_Overleaf_chart")

var_names <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
# pretty_labels <- c("Unemployment Rate", "Real broad EER", "Short-term IR", "Oil Price (WTI)", "CPI Inflation")
pretty_labels <- c("Unemployment Rate", "REER", "SIR", "Oil Price (WTI)", "CPI Inflation")

# Custom plot function: no axis labels, no legend, small font
create_cusum_plot <- function(ts_var, time_index, bp, var_name, country_name) {
  cusum <- efp(ts_var ~ breakfactor(bp), type = "OLS-CUSUM")
  process <- as.numeric(cusum$process)
  n <- length(process)
  bound <- as.numeric(strucchange:::boundary.efp(cusum, alpha = 0.05, alt.boundary = FALSE, functional = "max"))
  if (is.ts(ts_var)) {
    time_seq <- as.yearmon(time(ts_var))
    date_seq <- as.Date(time_seq, frac = 0)
    date_seq <- date_seq[1:n]
  } else {
    date_seq <- 1:n
  }
  process_data <- data.frame(
    Time = date_seq,
    Score = process,
    UpperBound = bound,
    LowerBound = -bound
  )
  p <- ggplot(process_data, aes(x = Time)) +
    geom_line(aes(y = Score), color = "blue", size = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(aes(y = UpperBound), linetype = "dashed", color = "red", size = 0.6) +
    geom_line(aes(y = LowerBound), linetype = "dashed", color = "red", size = 0.6) +
    ggtitle(bquote(bold(.(paste0(var_name, " (", country_name, ")"))))) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(size = 7, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = margin(2, 2, 2, 2),
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 6),
      legend.position = "none"
    )
  return(p)
}

# Collect all plots in a list: 7 rows × 5 columns = 35 plots
all_plots <- list()
country_names <- names(g7_info)
for (row in seq_along(country_names)) {
  country <- country_names[row]
  setwd(file.path(main_dir, g7_info[[country]]$path))
  var_data <- read.csv(g7_info[[country]]$file, header = TRUE)
  var_data$Date <- as.Date(var_data$Date)
  var_data$date_n <- as.numeric(var_data$Date)
  var_data <- var_data[1:327,]
  time <- c(1:325)
  ts_list <- lapply(var_names, function(v) ts(var_data[[v]], start = 1995, end = 2022, frequency = 12))
  bp_list <- lapply(ts_list, function(tsv) breakpoints(tsv ~ time, h = 12))
  for (col in 1:5) {
    all_plots[[length(all_plots) + 1]] <- create_cusum_plot(
      ts_list[[col]], time, bp_list[[col]],
      pretty_labels[col], country
    )
  }
}

# Arrange all plots in a 7x5 grid with cowplot
main_grid <- cowplot::plot_grid(
  plotlist = all_plots,
  nrow = 7, ncol = 5,
  align = "hv",
  axis = "tblr",
  rel_heights = rep(1, 7),
  rel_widths = rep(1, 5)
)

# Add global axis labels (smaller font, x label moved up, overall plot slightly smaller)
final_plot <- cowplot::ggdraw() +
  cowplot::draw_plot(main_grid, 0, 0, 1, 1) 
   # +
  # cowplot::draw_label(
  #   "Empirical Fluctuation Process Score (OLS-residuals)",
  #   x = 0.008, y = 0.5, angle = 90, vjust = 0.5, hjust = 0.5, fontface = "bold", size = 7
  # ) +
  # cowplot::draw_label(
  #   "Time horizon (Training)",
  #   x = 0.5, y = 0.03, angle = 0, vjust = 0.5, hjust = 0.5, fontface = "bold", size = 7
  # )

# Save as PNG and JPG (Overleaf-friendly, slightly smaller)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
png_file <- file.path(out_dir, "G7_OLS_CUSUM_grid_paper.png")
# jpg_file <- file.path(out_dir, "G7_OLS_CUSUM_grid_revised.jpg")
ggsave(png_file, final_plot, width = 11.5, height = 13.5, dpi = 300)
# ggsave(jpg_file, final_plot, width = 11.5, height = 13.5, dpi = 300)

# Show the plot in RStudio/interactive
print(final_plot)
####################### End of Code ###############################
