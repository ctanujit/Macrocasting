####################### PPI - Charts for the G7 countries ###################
####################### For 24M forward forecast horizons ##########################
# =============================================================================
# G7 Countries Macroeconomic Variables Forecast Visualization
# Academic Quality Charts for 24-Month Forecast Horizon
# Predictive Credible Intervals (PCI)
# =============================================================================
# Set the working directory
# setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/output/Probabilistic_Prediction_intervals_input_output')
# getwd()

setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Algorithms/SZBVARx/PPI_G7_input_data')
getwd()

# install.packages('tidyr')

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(scales)
library(readr)
library(lubridate)
library(cowplot)
library(ggtext)
library(extrafont)

# Import Arial font if available
if(require(extrafont)) {
  loadfonts(quiet = TRUE)
}

# Set high-quality graphics parameters
options(scipen = 999)  # Disable scientific notation

# Define color scheme and styling parameters - Enhanced for publication
colors <- list(
  ground_truth = "#D32F2F",      # Deeper red for ground truth (better visibility)
  szbvar = "#1976D2",            # Professional blue for SZBVAR
  var = "#388E3C",               # Professional green for VAR
  catboost = "#7B1FA2",          # Professional purple for CatBoost
  interval = "#BDBDBD",          # Medium grey for prediction intervals (better visibility)
  background = "#FFFFFF",        # White background
  axis = "#212121"               # Dark grey for axes (softer than pure black)
)

# Define line types and sizes - Optimized for publication
line_specs <- list(
  ground_truth_size = 0.6,       # Slightly thicker for better visibility
  model_line_size = 0.65,        # Slightly thicker for better visibility
  interval_alpha = 0.35          # Optimized transparency
)

# Define G7 countries and variables
g7_countries <- c("Canada", "USA", "France", "Germany", "Japan", "UK", "Italy")

# Country display names for plot titles
country_display_names <- c(
  "Canada" = "Canada", 
  "USA" = "US", 
  "France" = "France", 
  "Germany" = "Germany", 
  "Japan" = "Japan", 
  "UK" = "UK", 
  "Italy" = "Italy"
)

variables <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", 
               "OilpriceGlobalWTI", "CPIinflationrate")

# Variable labels for display
variable_labels <- c(
  "Unemploymentrate" = "Unemployment Rate",
  "RealbroadEER" = "REER", 
  "ShorttermIR" = "SIR",
  "OilpriceGlobalWTI" = "Oil Price (WTI)",
  "CPIinflationrate" = "CPI Inflation"
)

# Function to read and process data for a country
read_country_data <- function(country) {
  # File naming pattern for PPI - 24M horizon
  file_patterns <- list(
    "Canada" = "canada_ppi_results_baselines_24M.csv",
    "USA" = "usa_ppi_results_baselines_24M.csv",
    "France" = "france_ppi_results_baselines_24M.csv",
    "Germany" = "germany_ppi_results_baselines_24M.csv",
    "Japan" = "japan_ppi_results_baselines_24M.csv",
    "UK" = "uk_ppi_results_basellines_24M.csv",
    "Italy" = "italy_ppi_results_baselines_24M.csv"
  )
  
  filename <- file_patterns[[country]]
  
  if (!file.exists(filename)) {
    warning(paste("File not found for", country, ":", filename))
    return(NULL)
  }
  
  # Read data
  data <- read_csv(filename, show_col_types = FALSE)
  
  # Convert date and add month sequence
  data$Date <- mdy(data$Date)
  data <- data %>%
    arrange(Variable, Date) %>%
    group_by(Variable) %>%
    mutate(Month = row_number()) %>%
    ungroup()
  
  # Add country identifier
  data$Country <- country
  
  return(data)
}

# Function to create individual plot for variable-country combination
create_individual_plot <- function(data, country, variable, show_x_axis = FALSE, show_y_axis = FALSE) {
  
  # Filter data for specific variable
  plot_data <- data %>%
    filter(Variable == variable) %>%
    arrange(Month)
  
  if (nrow(plot_data) == 0) {
    return(ggplot() + theme_void())
  }
  
  # Calculate ranges for different components
  forecast_values <- c(plot_data$`Ground Truth`, plot_data$SZBVAR, 
                       plot_data$VAR, plot_data$CatBoost)
  forecast_values <- forecast_values[!is.na(forecast_values)]
  forecast_range <- range(forecast_values, na.rm = TRUE)
  forecast_span <- diff(forecast_range)
  
  ppi_range <- range(c(plot_data$Lower, plot_data$Upper), na.rm = TRUE)
  ppi_span <- diff(ppi_range)
  
  # Determine if this combination needs tighter scaling
  # Enhanced criteria based on specific problematic series identified
  needs_tight_scale <- FALSE
  
  # Original criteria
  if (ppi_span > 3 * forecast_span && forecast_span < 5) {
    if ((variable == "Unemploymentrate" && country %in% c("Japan", "France", "Canada")) ||
        (variable == "ShorttermIR" && country %in% c("Japan", "Canada"))) {
      needs_tight_scale <- TRUE
    }
  }
  
  # Additional specific problematic series
  if ((variable == "CPIinflationrate" && country == "Japan") ||
      (variable == "ShorttermIR" && country == "France") ||
      (variable == "ShorttermIR" && country == "Germany") ||
      (variable == "ShorttermIR" && country == "Canada") ||
      (variable == "CPIinflationrate" && country == "Canada")) {
    
    # Check if forecast lines are very close together (span < 2)
    if (forecast_span < 2) {
      needs_tight_scale <- TRUE
    }
  }
  
  # Calculate y-axis limits
  if (needs_tight_scale) {
    # Use forecast-focused range but include some PPI context
    padding_factor <- max(0.3, 0.5 * forecast_span)  # Ensure minimum visibility
    y_limits <- c(forecast_range[1] - padding_factor, 
                  forecast_range[2] + padding_factor)
    
    # Clip PPI to visible range for plotting
    plot_data <- plot_data %>%
      mutate(
        Lower_clipped = pmax(Lower, y_limits[1]),
        Upper_clipped = pmin(Upper, y_limits[2])
      )
  } else {
    # Use full range including PPI
    y_range <- range(c(plot_data$Lower, plot_data$Upper, forecast_values), na.rm = TRUE)
    y_span <- diff(y_range)
    y_limits <- c(y_range[1] - 0.05 * y_span, y_range[2] + 0.05 * y_span)
    
    plot_data <- plot_data %>%
      mutate(
        Lower_clipped = Lower,
        Upper_clipped = Upper
      )
  }
  
  # Create base plot with PCI (always shown, but may be clipped)
  p <- ggplot(plot_data, aes(x = Month)) +
    
    # Add prediction interval (grey shaded region) - always included
    geom_ribbon(aes(ymin = Lower_clipped, ymax = Upper_clipped), 
                fill = colors$interval, 
                alpha = line_specs$interval_alpha) +
    
    # Add model prediction lines
    geom_line(aes(y = SZBVAR), 
              color = colors$szbvar, 
              linewidth = line_specs$model_line_size) +
    
    geom_line(aes(y = VAR), 
              color = colors$var, 
              linewidth = line_specs$model_line_size) +
    
    geom_line(aes(y = CatBoost), 
              color = colors$catboost, 
              linewidth = line_specs$model_line_size) +
    
    # Add ground truth (red dotted line)
    geom_line(aes(y = `Ground Truth`), 
              color = colors$ground_truth, 
              linetype = "dotted", 
              linewidth = line_specs$ground_truth_size) +
    
    # Styling - Enhanced for publication
    theme_minimal() +
    theme(
      text = element_text(family = "Arial"),
      panel.background = element_rect(fill = colors$background, color = NA),
      plot.background = element_rect(fill = colors$background, color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = colors$axis, linewidth = 0.6),
      axis.text = element_text(color = colors$axis, size = 9, family = "Arial"),
      axis.title = element_text(color = colors$axis, size = 10, face = "bold", family = "Arial"),
      plot.title = element_text(color = colors$axis, size = 11, face = "bold", hjust = 0.5, family = "Arial"),
      axis.ticks = element_line(color = colors$axis, linewidth = 0.4),
      panel.border = element_rect(color = colors$axis, fill = NA, linewidth = 0.7),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    
    # Labels and scales
    scale_x_continuous(
      breaks = seq(2, 24, by = 2),
      labels = seq(2, 24, by = 2),
      limits = c(1, 24),
      expand = c(0.01, 0.01)
    )
  
  # Determine appropriate number of decimal places based on data range
  y_span_display <- diff(y_limits)
  if (y_span_display < 5) {
    accuracy_val <- 0.1
  } else if (y_span_display < 20) {
    accuracy_val <- 0.5
  } else if (y_span_display < 50) {
    accuracy_val <- 1
  } else {
    accuracy_val <- 5
  }
  
  p <- p + scale_y_continuous(
    labels = number_format(accuracy = accuracy_val),
    limits = y_limits,
    expand = expansion(mult = c(0.02, 0.02))
  )
  
  # Add title with variable and country
  country_display <- country_display_names[[country]]
  p <- p + ggtitle(paste0(variable_labels[[variable]], " (", country_display, ")"))
  
  # Add x-axis label only for bottom row
  if (show_x_axis) {
    p <- p + xlab("Months")
  } else {
    p <- p + 
      xlab("") +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  }
  
  # Add y-axis label "Value" only for leftmost column
  if (show_y_axis) {
    p <- p + ylab("Value")
  } else {
    p <- p + theme(axis.title.y = element_blank())
  }
  
  return(p)
}

# Function to create an enhanced legend for PCI
create_simplified_legend <- function() {
  legend_grob <- grobTree(
    # Red dotted line for Ground Truth
    linesGrob(x = unit(c(0.02, 0.08), "npc"), 
              y = unit(0.5, "npc"), 
              gp = gpar(col = colors$ground_truth, lty = "dotted", lwd = 1.5)),
    textGrob("Ground Truth", x = unit(0.10, "npc"), y = unit(0.5, "npc"), 
             just = "left", gp = gpar(fontsize = 11, fontface = "bold", fontfamily = "Arial")),
    
    # Blue line for SZBVAR
    linesGrob(x = unit(c(0.23, 0.29), "npc"), 
              y = unit(0.5, "npc"), 
              gp = gpar(col = colors$szbvar, lwd = 1.5)),
    textGrob("SZBVARx", x = unit(0.31, "npc"), y = unit(0.5, "npc"), 
             just = "left", gp = gpar(fontsize = 11, fontface = "bold", fontfamily = "Arial")),
    
    # Green line for VAR
    linesGrob(x = unit(c(0.43, 0.49), "npc"), 
              y = unit(0.5, "npc"), 
              gp = gpar(col = colors$var, lwd = 1.5)),
    textGrob("VARx", x = unit(0.51, "npc"), y = unit(0.5, "npc"), 
             just = "left", gp = gpar(fontsize = 11, fontface = "bold", fontfamily = "Arial")),
    
    # Purple line for CatBoost
    linesGrob(x = unit(c(0.60, 0.66), "npc"), 
              y = unit(0.5, "npc"), 
              gp = gpar(col = colors$catboost, lwd = 1.5)),
    textGrob("CatBoostx", x = unit(0.68, "npc"), y = unit(0.5, "npc"), 
             just = "left", gp = gpar(fontsize = 11, fontface = "bold", fontfamily = "Arial")),
    
    # Second row - Predictive Credible Interval
    rectGrob(x = unit(0.30, "npc"), y = unit(0.2, "npc"), 
             width = unit(0.06, "npc"), height = unit(0.15, "npc"),
             gp = gpar(fill = colors$interval, alpha = line_specs$interval_alpha)),
    textGrob("Predictive Credible Interval", x = unit(0.38, "npc"), y = unit(0.2, "npc"), 
             just = "left", gp = gpar(fontsize = 11, fontface = "bold", fontfamily = "Arial"))
  )
  
  return(legend_grob)
}

# Main function to generate consolidated charts
generate_consolidated_charts <- function(output_file = "G7_Macroeconomic_Forecasts_PCI_Charts_24M.png") {
  
  # Read data for all G7 countries
  all_data <- list()
  
  for (country in g7_countries) {
    cat("Reading data for", country, "...\n")
    country_data <- read_country_data(country)
    if (!is.null(country_data)) {
      all_data[[country]] <- country_data
    }
  }
  
  if (length(all_data) == 0) {
    stop("No data files found!")
  }
  
  cat("Creating individual plots...\n")
  
  # Create matrix of plots
  plot_list <- list()
  plot_index <- 1
  
  # Loop through countries first (rows)
  for (i in seq_along(g7_countries)) {
    country <- g7_countries[i]
    
    # Then loop through variables (columns)
    for (j in seq_along(variables)) {
      variable <- variables[j]
      
      if (country %in% names(all_data)) {
        show_x_axis <- (i == length(g7_countries))
        show_y_axis <- (j == 1)
        
        plot_list[[plot_index]] <- create_individual_plot(
          all_data[[country]], 
          country, 
          variable, 
          show_x_axis,
          show_y_axis
        )
      } else {
        country_display <- country_display_names[[country]]
        plot_list[[plot_index]] <- ggplot() + 
          theme_void() + 
          ggtitle(paste0(variable_labels[[variable]], " (", country_display, ")"))
      }
      
      plot_index <- plot_index + 1
    }
  }
  
  cat("Arranging plots in grid...\n")
  
  n_countries <- length(g7_countries)
  n_variables <- length(variables)
  
  # Create the main grid
  main_grid <- do.call(grid.arrange, c(
    plot_list, 
    list(
      ncol = n_variables,
      nrow = n_countries
    )
  ))
  
  # Create legend
  cat("Creating legend...\n")
  legend <- create_simplified_legend()
  
  # Combine main grid with legend
  cat("Combining plots with legend...\n")
  final_plot <- grid.arrange(
    main_grid,
    legend,
    ncol = 1,
    heights = c(0.96, 0.04)
  )
  
  # Save high-quality plot
  cat("Saving plot to", output_file, "...\n")
  
  ggsave(
    filename = output_file,
    plot = final_plot,
    width = 16,
    height = 18,
    dpi = 300,
    units = "in",
    bg = "white"
  )
  
  cat("Plot saved successfully!\n")
  cat("File:", output_file, "\n")
  cat("Dimensions: 16 x 18 inches at 300 DPI\n")
  
  return(final_plot)
}

# Execute the main function
cat("=== G7 Macroeconomic Forecast Visualization - 24M Horizon (PCI) ===\n")
cat("Starting chart generation...\n\n")

# Generate the consolidated charts
final_plot <- generate_consolidated_charts("G7_Macroeconomic_Forecasts_PCI_Charts_24M.png")

cat("\n=== Chart Generation Complete ===\n")
cat("Charts follow academic publication standards with:\n")
cat("- Enhanced color scheme for better visibility\n")
cat("- Professional line weights optimized for publication\n")
cat("- Adaptive y-axis scaling to highlight forecast differences\n")
cat("- PCI always visible (clipped when necessary for scale)\n")
cat("- Automatic handling of negative values in Lower bounds\n")
cat("- Predictive Credible Interval legend\n")
cat("- High-resolution output (300 DPI PNG)\n")

cat("\n=== 24M Chart Generation Complete ===\n")

####################### For 12M forward forecast horizons ##########################
# =============================================================================
# G7 Countries Macroeconomic Variables Forecast Visualization - 12M Horizon
# Predictive Credible Intervals (PCI)
# =============================================================================
# Set the working directory
# setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/output/Probabilistic_Prediction_intervals_input_output')
# getwd()

setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Algorithms/SZBVARx/PPI_G7_input_data')
getwd()

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(scales)
library(readr)
library(lubridate)
library(cowplot)
library(ggtext)
library(extrafont)

# Import Arial font if available
if(require(extrafont)) {
  loadfonts(quiet = TRUE)
}

# Set high-quality graphics parameters
options(scipen = 999)

# Define color scheme and styling parameters - Enhanced for publication
colors <- list(
  ground_truth = "#D32F2F",      # Deeper red for ground truth
  szbvar = "#1976D2",            # Professional blue for SZBVAR
  var = "#388E3C",               # Professional green for VAR
  catboost = "#7B1FA2",          # Professional purple for CatBoost
  interval = "#BDBDBD",          # Medium grey for prediction intervals
  background = "#FFFFFF",        # White background
  axis = "#212121"               # Dark grey for axes
)

# Define line types and sizes - Optimized for publication
line_specs <- list(
  ground_truth_size = 0.6,
  model_line_size = 0.65,
  interval_alpha = 0.35
)

# Define G7 countries and variables
g7_countries <- c("Canada", "USA", "France", "Germany", "Japan", "UK", "Italy")

country_display_names <- c(
  "Canada" = "Canada", 
  "USA" = "US", 
  "France" = "France", 
  "Germany" = "Germany", 
  "Japan" = "Japan", 
  "UK" = "UK", 
  "Italy" = "Italy"
)

variables <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", 
               "OilpriceGlobalWTI", "CPIinflationrate")

variable_labels <- c(
  "Unemploymentrate" = "Unemployment Rate",
  "RealbroadEER" = "REER", 
  "ShorttermIR" = "SIR",
  "OilpriceGlobalWTI" = "Oil Price (WTI)",
  "CPIinflationrate" = "CPI Inflation"
)

# Function to read and process data for a country
read_country_data <- function(country) {
  # File naming pattern for PPI - 12M horizon
  file_patterns <- list(
    "Canada" = "canada_ppi_results_baselines_12M.csv",
    "USA" = "usa_ppi_results_baselines_12M.csv",
    "France" = "france_ppi_results_baselines_12M.csv",
    "Germany" = "germany_ppi_results_baselines_12M.csv",
    "Japan" = "japan_ppi_results_baselines_12M.csv",
    "UK" = "uk_ppi_results_baselines_12M.csv",
    "Italy" = "italy_ppi_results_baselines_12M.csv"
  )
  
  filename <- file_patterns[[country]]
  
  if (!file.exists(filename)) {
    warning(paste("File not found for", country, ":", filename))
    return(NULL)
  }
  
  # Read data
  data <- read_csv(filename, show_col_types = FALSE)
  
  # Convert date and add month sequence
  data$Date <- mdy(data$Date)
  data <- data %>%
    arrange(Variable, Date) %>%
    group_by(Variable) %>%
    mutate(Month = row_number()) %>%
    ungroup()
  
  # Add country identifier
  data$Country <- country
  
  return(data)
}

# Function to create individual plot for variable-country combination
create_individual_plot <- function(data, country, variable, show_x_axis = FALSE, show_y_axis = FALSE) {
  
  # Filter data for specific variable
  plot_data <- data %>%
    filter(Variable == variable) %>%
    arrange(Month)
  
  if (nrow(plot_data) == 0) {
    return(ggplot() + theme_void())
  }
  
  # Calculate ranges for different components
  forecast_values <- c(plot_data$`Ground Truth`, plot_data$SZBVAR, 
                       plot_data$VAR, plot_data$CatBoost)
  forecast_values <- forecast_values[!is.na(forecast_values)]
  forecast_range <- range(forecast_values, na.rm = TRUE)
  forecast_span <- diff(forecast_range)
  
  ppi_range <- range(c(plot_data$Lower, plot_data$Upper), na.rm = TRUE)
  ppi_span <- diff(ppi_range)
  
  # Determine if this combination needs tighter scaling
  # Enhanced criteria based on specific problematic series identified
  needs_tight_scale <- FALSE
  
  # Original criteria
  if (ppi_span > 3 * forecast_span && forecast_span < 5) {
    if ((variable == "Unemploymentrate" && country %in% c("Japan", "France", "Canada")) ||
        (variable == "ShorttermIR" && country %in% c("Japan", "Canada"))) {
      needs_tight_scale <- TRUE
    }
  }
  
  # Additional specific problematic series
  if ((variable == "CPIinflationrate" && country == "Japan") ||
      (variable == "ShorttermIR" && country == "France") ||
      (variable == "ShorttermIR" && country == "Germany") ||
      (variable == "ShorttermIR" && country == "Canada") ||
      (variable == "CPIinflationrate" && country == "Canada")) {
    
    # Check if forecast lines are very close together (span < 2)
    if (forecast_span < 2) {
      needs_tight_scale <- TRUE
    }
  }
  
  # Calculate y-axis limits
  if (needs_tight_scale) {
    # Use forecast-focused range but include some PCI context
    padding_factor <- max(0.3, 0.5 * forecast_span)  # Ensure minimum visibility
    y_limits <- c(forecast_range[1] - padding_factor, 
                  forecast_range[2] + padding_factor)
    
    # Clip PCI to visible range for plotting
    plot_data <- plot_data %>%
      mutate(
        Lower_clipped = pmax(Lower, y_limits[1]),
        Upper_clipped = pmin(Upper, y_limits[2])
      )
  } else {
    # Use full range including PCI
    y_range <- range(c(plot_data$Lower, plot_data$Upper, forecast_values), na.rm = TRUE)
    y_span <- diff(y_range)
    y_limits <- c(y_range[1] - 0.05 * y_span, y_range[2] + 0.05 * y_span)
    
    plot_data <- plot_data %>%
      mutate(
        Lower_clipped = Lower,
        Upper_clipped = Upper
      )
  }
  
  # Create base plot with PCI (always shown, but may be clipped)
  p <- ggplot(plot_data, aes(x = Month)) +
    
    # Add prediction interval (grey shaded region) - always included
    geom_ribbon(aes(ymin = Lower_clipped, ymax = Upper_clipped), 
                fill = colors$interval, 
                alpha = line_specs$interval_alpha) +
    
    # Add model prediction lines
    geom_line(aes(y = SZBVAR), 
              color = colors$szbvar, 
              linewidth = line_specs$model_line_size) +
    
    geom_line(aes(y = VAR), 
              color = colors$var, 
              linewidth = line_specs$model_line_size) +
    
    geom_line(aes(y = CatBoost), 
              color = colors$catboost, 
              linewidth = line_specs$model_line_size) +
    
    # Add ground truth (red dotted line)
    geom_line(aes(y = `Ground Truth`), 
              color = colors$ground_truth, 
              linetype = "dotted", 
              linewidth = line_specs$ground_truth_size) +
    
    # Styling - Enhanced for publication
    theme_minimal() +
    theme(
      text = element_text(family = "Arial"),
      panel.background = element_rect(fill = colors$background, color = NA),
      plot.background = element_rect(fill = colors$background, color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = colors$axis, linewidth = 0.6),
      axis.text = element_text(color = colors$axis, size = 9, family = "Arial"),
      axis.title = element_text(color = colors$axis, size = 10, face = "bold", family = "Arial"),
      plot.title = element_text(color = colors$axis, size = 11, face = "bold", hjust = 0.5, family = "Arial"),
      axis.ticks = element_line(color = colors$axis, linewidth = 0.4),
      panel.border = element_rect(color = colors$axis, fill = NA, linewidth = 0.7),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    
    # Labels and scales - 12M horizon
    scale_x_continuous(
      breaks = seq(2, 12, by = 2),
      labels = seq(2, 12, by = 2),
      limits = c(1, 12),
      expand = c(0.01, 0.01)
    )
  
  # Determine appropriate number of decimal places
  y_span_display <- diff(y_limits)
  if (y_span_display < 5) {
    accuracy_val <- 0.1
  } else if (y_span_display < 20) {
    accuracy_val <- 0.5
  } else if (y_span_display < 50) {
    accuracy_val <- 1
  } else {
    accuracy_val <- 5
  }
  
  p <- p + scale_y_continuous(
    labels = number_format(accuracy = accuracy_val),
    limits = y_limits,
    expand = expansion(mult = c(0.02, 0.02))
  )
  
  # Add title with variable and country
  country_display <- country_display_names[[country]]
  p <- p + ggtitle(paste0(variable_labels[[variable]], " (", country_display, ")"))
  
  # Add x-axis label only for bottom row
  if (show_x_axis) {
    p <- p + xlab("Months")
  } else {
    p <- p + 
      xlab("") +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  }
  
  # Add y-axis label "Value" only for leftmost column
  if (show_y_axis) {
    p <- p + ylab("Value")
  } else {
    p <- p + theme(axis.title.y = element_blank())
  }
  
  return(p)
}

# Function to create an enhanced legend for PCI
create_simplified_legend <- function() {
  legend_grob <- grobTree(
    # Red dotted line for Ground Truth
    linesGrob(x = unit(c(0.02, 0.08), "npc"), 
              y = unit(0.5, "npc"), 
              gp = gpar(col = colors$ground_truth, lty = "dotted", lwd = 1.5)),
    textGrob("Ground Truth", x = unit(0.10, "npc"), y = unit(0.5, "npc"), 
             just = "left", gp = gpar(fontsize = 11, fontface = "bold", fontfamily = "Arial")),
    
    # Blue line for SZBVAR
    linesGrob(x = unit(c(0.23, 0.29), "npc"), 
              y = unit(0.5, "npc"), 
              gp = gpar(col = colors$szbvar, lwd = 1.5)),
    textGrob("SZBVARx", x = unit(0.31, "npc"), y = unit(0.5, "npc"), 
             just = "left", gp = gpar(fontsize = 11, fontface = "bold", fontfamily = "Arial")),
    
    # Green line for VAR
    linesGrob(x = unit(c(0.43, 0.49), "npc"), 
              y = unit(0.5, "npc"), 
              gp = gpar(col = colors$var, lwd = 1.5)),
    textGrob("VARx", x = unit(0.51, "npc"), y = unit(0.5, "npc"), 
             just = "left", gp = gpar(fontsize = 11, fontface = "bold", fontfamily = "Arial")),
    
    # Purple line for CatBoost
    linesGrob(x = unit(c(0.60, 0.66), "npc"), 
              y = unit(0.5, "npc"), 
              gp = gpar(col = colors$catboost, lwd = 1.5)),
    textGrob("CatBoostx", x = unit(0.68, "npc"), y = unit(0.5, "npc"), 
             just = "left", gp = gpar(fontsize = 11, fontface = "bold", fontfamily = "Arial")),
    
    # Second row - Predictive Credible Interval
    rectGrob(x = unit(0.30, "npc"), y = unit(0.2, "npc"), 
             width = unit(0.06, "npc"), height = unit(0.15, "npc"),
             gp = gpar(fill = colors$interval, alpha = line_specs$interval_alpha)),
    textGrob("Predictive Credible Interval", x = unit(0.38, "npc"), y = unit(0.2, "npc"), 
             just = "left", gp = gpar(fontsize = 11, fontface = "bold", fontfamily = "Arial"))
  )
  
  return(legend_grob)
}

# Main function to generate consolidated charts
generate_consolidated_charts <- function(output_file = "G7_Macroeconomic_Forecasts_PCI_Charts_12M.png") {
  
  # Read data for all G7 countries
  all_data <- list()
  
  for (country in g7_countries) {
    cat("Reading data for", country, "...\n")
    country_data <- read_country_data(country)
    if (!is.null(country_data)) {
      all_data[[country]] <- country_data
    }
  }
  
  if (length(all_data) == 0) {
    stop("No data files found!")
  }
  
  cat("Creating individual plots...\n")
  
  # Create matrix of plots
  plot_list <- list()
  plot_index <- 1
  
  for (i in seq_along(g7_countries)) {
    country <- g7_countries[i]
    
    for (j in seq_along(variables)) {
      variable <- variables[j]
      
      if (country %in% names(all_data)) {
        show_x_axis <- (i == length(g7_countries))
        show_y_axis <- (j == 1)
        
        plot_list[[plot_index]] <- create_individual_plot(
          all_data[[country]], 
          country, 
          variable, 
          show_x_axis,
          show_y_axis
        )
      } else {
        country_display <- country_display_names[[country]]
        plot_list[[plot_index]] <- ggplot() + 
          theme_void() + 
          ggtitle(paste0(variable_labels[[variable]], " (", country_display, ")"))
      }
      
      plot_index <- plot_index + 1
    }
  }
  
  cat("Arranging plots in grid...\n")
  
  n_countries <- length(g7_countries)
  n_variables <- length(variables)
  
  # Create the main grid
  main_grid <- do.call(grid.arrange, c(
    plot_list, 
    list(
      ncol = n_variables,
      nrow = n_countries
    )
  ))
  
  # Create legend
  cat("Creating legend...\n")
  legend <- create_simplified_legend()
  
  # Combine main grid with legend
  cat("Combining plots with legend...\n")
  final_plot <- grid.arrange(
    main_grid,
    legend,
    ncol = 1,
    heights = c(0.96, 0.04)
  )
  
  # Save high-quality plot
  cat("Saving plot to", output_file, "...\n")
  
  ggsave(
    filename = output_file,
    plot = final_plot,
    width = 16,
    height = 18,
    dpi = 300,
    units = "in",
    bg = "white"
  )
  
  cat("Plot saved successfully!\n")
  cat("File:", output_file, "\n")
  cat("Dimensions: 16 x 18 inches at 300 DPI\n")
  
  return(final_plot)
}

# Execute the main function
cat("=== G7 Macroeconomic Forecast Visualization - 12M Horizon (PCI) ===\n")
cat("Starting chart generation...\n\n")

# Generate the consolidated charts
final_plot <- generate_consolidated_charts("G7_Macroeconomic_Forecasts_PCI_Charts_12M.png")

cat("\n=== Chart Generation Complete ===\n")
cat("Charts follow academic publication standards with:\n")
cat("- Enhanced color scheme for better visibility\n")
cat("- Professional line weights optimized for publication\n")
cat("- Adaptive y-axis scaling to highlight forecast differences\n")
cat("- PCI always visible (clipped when necessary for scale)\n")
cat("- Automatic handling of negative values in Lower bounds\n")
cat("- Predictive Credible Interval legend\n")
cat("- High-resolution output (300 DPI PNG)\n")

cat("\n=== All Chart Generation Complete ===\n")