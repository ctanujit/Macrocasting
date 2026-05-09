################### Non - Linear IRFs using Local Projections : G7 Countries ############## 
# =========================
# Setup
# =========================
required_packages <- c("lpirfs", "ggpubr", "gridExtra", "magick", "Cairo")
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) install.packages(pkg)
}

library(lpirfs)
library(ggpubr)
library(gridExtra)
library(magick)
library(Cairo)

######################## Code Module ###########################
# =========================
# Setup
# =========================
required_packages <- c("lpirfs", "ggpubr", "gridExtra", "magick", "Cairo")
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) install.packages(pkg)
}

library(lpirfs)
library(ggpubr)
library(gridExtra)
library(magick)
library(Cairo)

############################## Country: Canada #####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/canada")
getwd()

endog_data <- read.csv("all_mulvar_data_canada_v2.csv", header = TRUE)
endog_data$Date <- as.Date(endog_data$Date)
str(endog_data)
head(endog_data)
tail(endog_data)

endog_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")
switching_variable <- endog_data$ShorttermIR

endog_data_canada <- endog_data[, endog_vars]
exog_data <- endog_data[, exog_vars]

# =========================
# Variable Name Mapping
# =========================
var_name_mapping <- list(
  "Unemploymentrate" = "Unemployment Rate",
  "RealbroadEER" = "REER",
  "ShorttermIR" = "SIR",
  "OilpriceGlobalWTI" = "Oil Price (WTI)",
  "CPIinflationrate" = "CPI Inflation",
  "logEPU" = "EPU",
  "GPRC" = "GPR",
  "USEMV" = "USEMV",
  "USMPU" = "USMPU"
)

# Helper function to get display name
get_display_name <- function(var_name) {
  return(var_name_mapping[[var_name]])
}

# =========================
# Helper Function to Generate IRFs
# =========================
generate_irf_plots <- function(shock_name) {
  shock_data <- as.data.frame(endog_data[[shock_name]])
  results <- lp_nl_iv(endog_data_canada,
                      lags_endog_nl     = 6,
                      shock             = shock_data,
                      exog_data         = exog_data,
                      lags_exog         = 4,
                      trend             = 0,
                      confint           = 1.96,
                      hor               = 24,
                      switching         = switching_variable,
                      use_logistic      = TRUE,
                      lag_switching     = TRUE,
                      use_hp            = FALSE,
                      gamma             = 3)
  
  plot_objs <- plot_nl(results)
  return(list(s1 = plot_objs$gg_s1, s2 = plot_objs$gg_s2))
}

# =========================
# Generate and Store IRFs for All Shocks
# =========================
plot_grid_s1 <- list()
plot_grid_s2 <- list()

for (shock in exog_vars) {
  plots <- generate_irf_plots(shock)
  
  for (i in seq_along(endog_vars)) {
    endog <- endog_vars[i]
    
    # Determine if this plot is in the first column (needs y-axis label)
    is_first_col <- shock == exog_vars[1]
    
    # Determine if this plot is in the last row (needs x-axis label)
    is_last_row <- i == length(endog_vars)
    
    # Create a more readable title with proper variable names
    endog_display <- get_display_name(endog)
    shock_display <- get_display_name(shock)
    plot_title <- paste0("Resp. of ", endog_display, " to a shock of ", shock_display)
    
    # Get the plot for this endogenous variable
    p1 <- plots$s1[[i]]
    p2 <- plots$s2[[i]]
    
    # Add appropriate axis labels based on position with improved formatting
    if (is_first_col) {
      p1 <- p1 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
      p2 <- p2 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + ylab("")
      p2 <- p2 + ylab("")
    }
    
    if (is_last_row) {
      p1 <- p1 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
      p2 <- p2 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + xlab("")
      p2 <- p2 + xlab("")
    }
    
    # Add improved title formatting with proper variable names
    p1 <- p1 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    p2 <- p2 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    # Store the modified plots
    plot_grid_s1[[paste0(endog, "_", shock)]] <- p1
    plot_grid_s2[[paste0(endog, "_", shock)]] <- p2
  }
}

# =========================
# Save IRF Grid Function
# =========================
plot_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Causality_Analysis/Output/IRF_Grids"
if (!dir.exists(plot_dir)) dir.create(plot_dir)

save_irf_grid <- function(plot_list, filename_png) {
  # Save individual plots first
  for (name in names(plot_list)) {
    p <- plot_list[[name]]
    file_path <- file.path(plot_dir, paste0(name, ".png"))
    ggsave(file_path, plot = p, width = 6, height = 5, dpi = 150)
  }
  
  # Create a list to hold plots in the correct order for the grid
  grid_plots <- list()
  
  # Fill the list with plots in the correct order
  for (i in seq_along(endog_vars)) {
    for (j in seq_along(exog_vars)) {
      endog <- endog_vars[i]
      shock <- exog_vars[j]
      plot_name <- paste0(endog, "_", shock)
      grid_plots <- c(grid_plots, list(plot_list[[plot_name]]))
    }
  }
  
  # Arrange plots in a grid
  arranged_plots <- do.call(arrangeGrob, c(grid_plots, list(ncol = length(exog_vars))))
  
  # Save as high-quality PNG
  ggsave(filename_png, arranged_plots, width = 16, height = 20, dpi = 300)
  
  # Convert to JPG as well
  jpg_filename <- sub(".png$", ".jpg", filename_png)
  ggsave(jpg_filename, arranged_plots, width = 16, height = 20, dpi = 300)
}

# =========================
# Save Both Regime Grids
# =========================
# Checks
save_irf_grid(plot_grid_s1, "IRF_Grid_Regime1_HighIR_CANADA_revised.png")
save_irf_grid(plot_grid_s2, "IRF_Grid_Regime2_LowIR_CANADA_revised.png")

cat("\n=== IRF Generation Complete ===\n")
cat("Plot titles updated with proper variable names:\n")
cat("  - Unemployment Rate (was: Unemploymentrate)\n")
cat("  - REER (was: RealbroadEER)\n")
cat("  - SIR (was: ShorttermIR)\n")
cat("  - Oil Price (WTI) (was: OilpriceGlobalWTI)\n")
cat("  - CPI Inflation (was: CPIinflationrate)\n")
cat("  - EPU (was: logEPU)\n")
cat("  - GPRC, USEMV, USMPU (unchanged)\n")
cat("\nFiles saved in:", plot_dir, "\n")

############################## Country: USA #####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/usa")
getwd()

endog_data <- read.csv("all_mulvar_data_usa_v2.csv", header = TRUE)
endog_data$Date <- as.Date(endog_data$Date)
str(endog_data)
head(endog_data)
tail(endog_data)

endog_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")
switching_variable <- endog_data$ShorttermIR

endog_data_usa <- endog_data[, endog_vars]
exog_data <- endog_data[, exog_vars]

# =========================
# Variable Name Mapping
# =========================
var_name_mapping <- list(
  "Unemploymentrate" = "Unemployment Rate",
  "RealbroadEER" = "REER",
  "ShorttermIR" = "SIR",
  "OilpriceGlobalWTI" = "Oil Price (WTI)",
  "CPIinflationrate" = "CPI Inflation",
  "logEPU" = "EPU",
  "GPRC" = "GPR",
  "USEMV" = "USEMV",
  "USMPU" = "USMPU"
)

# Helper function to get display name
get_display_name <- function(var_name) {
  return(var_name_mapping[[var_name]])
}

# =========================
# Helper Function to Generate IRFs
# =========================
generate_irf_plots <- function(shock_name) {
  shock_data <- as.data.frame(endog_data[[shock_name]])
  results <- lp_nl_iv(endog_data_usa,
                      lags_endog_nl     = 6,
                      shock             = shock_data,
                      exog_data         = exog_data,
                      lags_exog         = 4,
                      trend             = 0,
                      confint           = 1.96,
                      hor               = 24,
                      switching         = switching_variable,
                      use_logistic      = TRUE,
                      lag_switching     = TRUE,
                      use_hp            = FALSE,
                      gamma             = 3)
  
  plot_objs <- plot_nl(results)
  return(list(s1 = plot_objs$gg_s1, s2 = plot_objs$gg_s2))
}

# =========================
# Generate and Store IRFs for All Shocks
# =========================
plot_grid_s1 <- list()
plot_grid_s2 <- list()

for (shock in exog_vars) {
  plots <- generate_irf_plots(shock)
  
  for (i in seq_along(endog_vars)) {
    endog <- endog_vars[i]
    
    # Determine if this plot is in the first column (needs y-axis label)
    is_first_col <- shock == exog_vars[1]
    
    # Determine if this plot is in the last row (needs x-axis label)
    is_last_row <- i == length(endog_vars)
    
    # Create a more readable title with proper variable names
    endog_display <- get_display_name(endog)
    shock_display <- get_display_name(shock)
    plot_title <- paste0("Resp. of ", endog_display, " to a shock of ", shock_display)
    
    # Get the plot for this endogenous variable
    p1 <- plots$s1[[i]]
    p2 <- plots$s2[[i]]
    
    # Add appropriate axis labels based on position with improved formatting
    if (is_first_col) {
      p1 <- p1 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
      p2 <- p2 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + ylab("")
      p2 <- p2 + ylab("")
    }
    
    if (is_last_row) {
      p1 <- p1 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
      p2 <- p2 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + xlab("")
      p2 <- p2 + xlab("")
    }
    
    # Add improved title formatting with proper variable names
    p1 <- p1 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    p2 <- p2 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    # Store the modified plots
    plot_grid_s1[[paste0(endog, "_", shock)]] <- p1
    plot_grid_s2[[paste0(endog, "_", shock)]] <- p2
  }
}

# =========================
# Save IRF Grid Function
# =========================
plot_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Causality_Analysis/Output/IRF_Grids"
if (!dir.exists(plot_dir)) dir.create(plot_dir)

save_irf_grid <- function(plot_list, filename_png) {
  # Save individual plots first
  for (name in names(plot_list)) {
    p <- plot_list[[name]]
    file_path <- file.path(plot_dir, paste0(name, ".png"))
    ggsave(file_path, plot = p, width = 6, height = 5, dpi = 150)
  }
  
  # Create a list to hold plots in the correct order for the grid
  grid_plots <- list()
  
  # Fill the list with plots in the correct order
  for (i in seq_along(endog_vars)) {
    for (j in seq_along(exog_vars)) {
      endog <- endog_vars[i]
      shock <- exog_vars[j]
      plot_name <- paste0(endog, "_", shock)
      grid_plots <- c(grid_plots, list(plot_list[[plot_name]]))
    }
  }
  
  # Arrange plots in a grid
  arranged_plots <- do.call(arrangeGrob, c(grid_plots, list(ncol = length(exog_vars))))
  
  # Save as high-quality PNG
  ggsave(filename_png, arranged_plots, width = 16, height = 20, dpi = 300)
  
  # Convert to JPG as well
  jpg_filename <- sub(".png$", ".jpg", filename_png)
  ggsave(jpg_filename, arranged_plots, width = 16, height = 20, dpi = 300)
}

# =========================
# Save Both Regime Grids
# =========================
save_irf_grid(plot_grid_s1, "IRF_Grid_Regime1_HighIR_USA_revised.png")
save_irf_grid(plot_grid_s2, "IRF_Grid_Regime2_LowIR_USA_revised.png")

cat("\n=== IRF Generation Complete for USA ===\n")
cat("Plot titles updated with proper variable names:\n")
cat("  - Unemployment Rate (was: Unemploymentrate)\n")
cat("  - REER (was: RealbroadEER)\n")
cat("  - SIR (was: ShorttermIR)\n")
cat("  - Oil Price (WTI) (was: OilpriceGlobalWTI)\n")
cat("  - CPI Inflation (was: CPIinflationrate)\n")
cat("  - EPU (was: logEPU)\n")
cat("  - GPRC, USEMV, USMPU (unchanged)\n")
cat("\nFiles saved in:", plot_dir, "\n")

############################## Country: FRANCE #####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/france")
getwd()

endog_data <- read.csv("all_mulvar_data_france_v2.csv", header = TRUE)
endog_data$Date <- as.Date(endog_data$Date)
str(endog_data)
head(endog_data)
tail(endog_data)

endog_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")
switching_variable <- endog_data$ShorttermIR

endog_data_france <- endog_data[, endog_vars]
exog_data <- endog_data[, exog_vars]

# =========================
# Variable Name Mapping
# =========================
var_name_mapping <- list(
  "Unemploymentrate" = "Unemployment Rate",
  "RealbroadEER" = "REER",
  "ShorttermIR" = "SIR",
  "OilpriceGlobalWTI" = "Oil Price (WTI)",
  "CPIinflationrate" = "CPI Inflation",
  "logEPU" = "EPU",
  "GPRC" = "GPR",
  "USEMV" = "USEMV",
  "USMPU" = "USMPU"
)

# Helper function to get display name
get_display_name <- function(var_name) {
  return(var_name_mapping[[var_name]])
}

# =========================
# Helper Function to Generate IRFs
# =========================
generate_irf_plots <- function(shock_name) {
  shock_data <- as.data.frame(endog_data[[shock_name]])
  results <- lp_nl_iv(endog_data_france,
                      lags_endog_nl     = 6,
                      shock             = shock_data,
                      exog_data         = exog_data,
                      lags_exog         = 4,
                      trend             = 0,
                      confint           = 1.96,
                      hor               = 24,
                      switching         = switching_variable,
                      use_logistic      = TRUE,
                      lag_switching     = TRUE,
                      use_hp            = FALSE,
                      gamma             = 3)
  
  plot_objs <- plot_nl(results)
  return(list(s1 = plot_objs$gg_s1, s2 = plot_objs$gg_s2))
}

# =========================
# Generate and Store IRFs for All Shocks
# =========================
plot_grid_s1 <- list()
plot_grid_s2 <- list()

for (shock in exog_vars) {
  plots <- generate_irf_plots(shock)
  
  for (i in seq_along(endog_vars)) {
    endog <- endog_vars[i]
    
    # Determine if this plot is in the first column (needs y-axis label)
    is_first_col <- shock == exog_vars[1]
    
    # Determine if this plot is in the last row (needs x-axis label)
    is_last_row <- i == length(endog_vars)
    
    # Create a more readable title with proper variable names
    endog_display <- get_display_name(endog)
    shock_display <- get_display_name(shock)
    plot_title <- paste0("Resp. of ", endog_display, " to a shock of ", shock_display)
    
    # Get the plot for this endogenous variable
    p1 <- plots$s1[[i]]
    p2 <- plots$s2[[i]]
    
    # Add appropriate axis labels based on position with improved formatting
    if (is_first_col) {
      p1 <- p1 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
      p2 <- p2 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + ylab("")
      p2 <- p2 + ylab("")
    }
    
    if (is_last_row) {
      p1 <- p1 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
      p2 <- p2 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + xlab("")
      p2 <- p2 + xlab("")
    }
    
    # Add improved title formatting with proper variable names
    p1 <- p1 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    p2 <- p2 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    # Store the modified plots
    plot_grid_s1[[paste0(endog, "_", shock)]] <- p1
    plot_grid_s2[[paste0(endog, "_", shock)]] <- p2
  }
}

# =========================
# Save IRF Grid Function
# =========================
plot_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Causality_Analysis/Output/IRF_Grids"
if (!dir.exists(plot_dir)) dir.create(plot_dir)

save_irf_grid <- function(plot_list, filename_png) {
  # Save individual plots first
  for (name in names(plot_list)) {
    p <- plot_list[[name]]
    file_path <- file.path(plot_dir, paste0(name, ".png"))
    ggsave(file_path, plot = p, width = 6, height = 5, dpi = 150)
  }
  
  # Create a list to hold plots in the correct order for the grid
  grid_plots <- list()
  
  # Fill the list with plots in the correct order
  for (i in seq_along(endog_vars)) {
    for (j in seq_along(exog_vars)) {
      endog <- endog_vars[i]
      shock <- exog_vars[j]
      plot_name <- paste0(endog, "_", shock)
      grid_plots <- c(grid_plots, list(plot_list[[plot_name]]))
    }
  }
  
  # Arrange plots in a grid
  arranged_plots <- do.call(arrangeGrob, c(grid_plots, list(ncol = length(exog_vars))))
  
  # Save as high-quality PNG
  ggsave(filename_png, arranged_plots, width = 16, height = 20, dpi = 300)
  
  # Convert to JPG as well
  jpg_filename <- sub(".png$", ".jpg", filename_png)
  ggsave(jpg_filename, arranged_plots, width = 16, height = 20, dpi = 300)
}

# =========================
# Save Both Regime Grids
# =========================
save_irf_grid(plot_grid_s1, "IRF_Grid_Regime1_HighIR_FRANCE_revised.png")
save_irf_grid(plot_grid_s2, "IRF_Grid_Regime2_LowIR_FRANCE_revised.png")

cat("\n=== IRF Generation Complete for FRANCE ===\n")
cat("Plot titles updated with proper variable names:\n")
cat("  - Unemployment Rate (was: Unemploymentrate)\n")
cat("  - REER (was: RealbroadEER)\n")
cat("  - SIR (was: ShorttermIR)\n")
cat("  - Oil Price (WTI) (was: OilpriceGlobalWTI)\n")
cat("  - CPI Inflation (was: CPIinflationrate)\n")
cat("  - EPU (was: logEPU)\n")
cat("  - GPRC, USEMV, USMPU (unchanged)\n")
cat("\nFiles saved in:", plot_dir, "\n")

############################## Country: GERMANY #####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/germany")
getwd()

endog_data <- read.csv("all_mulvar_data_germany_v2.csv", header = TRUE)
endog_data$Date <- as.Date(endog_data$Date)
str(endog_data)
head(endog_data)
tail(endog_data)

endog_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")
switching_variable <- endog_data$ShorttermIR

endog_data_germany <- endog_data[, endog_vars]
exog_data <- endog_data[, exog_vars]

# =========================
# Variable Name Mapping
# =========================
var_name_mapping <- list(
  "Unemploymentrate" = "Unemployment Rate",
  "RealbroadEER" = "REER",
  "ShorttermIR" = "SIR",
  "OilpriceGlobalWTI" = "Oil Price (WTI)",
  "CPIinflationrate" = "CPI Inflation",
  "logEPU" = "EPU",
  "GPRC" = "GPR",
  "USEMV" = "USEMV",
  "USMPU" = "USMPU"
)

# Helper function to get display name
get_display_name <- function(var_name) {
  return(var_name_mapping[[var_name]])
}

# =========================
# Helper Function to Generate IRFs
# =========================
generate_irf_plots <- function(shock_name) {
  shock_data <- as.data.frame(endog_data[[shock_name]])
  results <- lp_nl_iv(endog_data_germany,
                      lags_endog_nl     = 6,
                      shock             = shock_data,
                      exog_data         = exog_data,
                      lags_exog         = 4,
                      trend             = 0,
                      confint           = 1.96,
                      hor               = 24,
                      switching         = switching_variable,
                      use_logistic      = TRUE,
                      lag_switching     = TRUE,
                      use_hp            = FALSE,
                      gamma             = 3)
  
  plot_objs <- plot_nl(results)
  return(list(s1 = plot_objs$gg_s1, s2 = plot_objs$gg_s2))
}

# =========================
# Generate and Store IRFs for All Shocks
# =========================
plot_grid_s1 <- list()
plot_grid_s2 <- list()

for (shock in exog_vars) {
  plots <- generate_irf_plots(shock)
  
  for (i in seq_along(endog_vars)) {
    endog <- endog_vars[i]
    
    # Determine if this plot is in the first column (needs y-axis label)
    is_first_col <- shock == exog_vars[1]
    
    # Determine if this plot is in the last row (needs x-axis label)
    is_last_row <- i == length(endog_vars)
    
    # Create a more readable title with proper variable names
    endog_display <- get_display_name(endog)
    shock_display <- get_display_name(shock)
    plot_title <- paste0("Resp. of ", endog_display, " to a shock of ", shock_display)
    
    # Get the plot for this endogenous variable
    p1 <- plots$s1[[i]]
    p2 <- plots$s2[[i]]
    
    # Add appropriate axis labels based on position with improved formatting
    if (is_first_col) {
      p1 <- p1 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
      p2 <- p2 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + ylab("")
      p2 <- p2 + ylab("")
    }
    
    if (is_last_row) {
      p1 <- p1 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
      p2 <- p2 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + xlab("")
      p2 <- p2 + xlab("")
    }
    
    # Add improved title formatting with proper variable names
    p1 <- p1 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    p2 <- p2 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    # Store the modified plots
    plot_grid_s1[[paste0(endog, "_", shock)]] <- p1
    plot_grid_s2[[paste0(endog, "_", shock)]] <- p2
  }
}

# =========================
# Save IRF Grid Function
# =========================
plot_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Causality_Analysis/Output/IRF_Grids"
if (!dir.exists(plot_dir)) dir.create(plot_dir)

save_irf_grid <- function(plot_list, filename_png) {
  # Save individual plots first
  for (name in names(plot_list)) {
    p <- plot_list[[name]]
    file_path <- file.path(plot_dir, paste0(name, ".png"))
    ggsave(file_path, plot = p, width = 6, height = 5, dpi = 150)
  }
  
  # Create a list to hold plots in the correct order for the grid
  grid_plots <- list()
  
  # Fill the list with plots in the correct order
  for (i in seq_along(endog_vars)) {
    for (j in seq_along(exog_vars)) {
      endog <- endog_vars[i]
      shock <- exog_vars[j]
      plot_name <- paste0(endog, "_", shock)
      grid_plots <- c(grid_plots, list(plot_list[[plot_name]]))
    }
  }
  
  # Arrange plots in a grid
  arranged_plots <- do.call(arrangeGrob, c(grid_plots, list(ncol = length(exog_vars))))
  
  # Save as high-quality PNG
  ggsave(filename_png, arranged_plots, width = 16, height = 20, dpi = 300)
  
  # Convert to JPG as well
  jpg_filename <- sub(".png$", ".jpg", filename_png)
  ggsave(jpg_filename, arranged_plots, width = 16, height = 20, dpi = 300)
}

# =========================
# Save Both Regime Grids
# =========================
save_irf_grid(plot_grid_s1, "IRF_Grid_Regime1_HighIR_GERMANY_revised.png")
save_irf_grid(plot_grid_s2, "IRF_Grid_Regime2_LowIR_GERMANY_revised.png")

cat("\n=== IRF Generation Complete for GERMANY ===\n")
cat("Plot titles updated with proper variable names:\n")
cat("  - Unemployment Rate (was: Unemploymentrate)\n")
cat("  - REER (was: RealbroadEER)\n")
cat("  - SIR (was: ShorttermIR)\n")
cat("  - Oil Price (WTI) (was: OilpriceGlobalWTI)\n")
cat("  - CPI Inflation (was: CPIinflationrate)\n")
cat("  - EPU (was: logEPU)\n")
cat("  - GPRC, USEMV, USMPU (unchanged)\n")
cat("\nFiles saved in:", plot_dir, "\n")

############################## Country: JAPAN #####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/japan")
getwd()

endog_data <- read.csv("all_mulvar_data_japan_v2.csv", header = TRUE)
endog_data$Date <- as.Date(endog_data$Date)
str(endog_data)
head(endog_data)
tail(endog_data)

endog_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")
switching_variable <- endog_data$ShorttermIR

endog_data_japan <- endog_data[, endog_vars]
exog_data <- endog_data[, exog_vars]

# =========================
# Variable Name Mapping
# =========================
var_name_mapping <- list(
  "Unemploymentrate" = "Unemployment Rate",
  "RealbroadEER" = "REER",
  "ShorttermIR" = "SIR",
  "OilpriceGlobalWTI" = "Oil Price (WTI)",
  "CPIinflationrate" = "CPI Inflation",
  "logEPU" = "EPU",
  "GPRC" = "GPR",
  "USEMV" = "USEMV",
  "USMPU" = "USMPU"
)

# Helper function to get display name
get_display_name <- function(var_name) {
  return(var_name_mapping[[var_name]])
}

# =========================
# Helper Function to Generate IRFs
# =========================
generate_irf_plots <- function(shock_name) {
  shock_data <- as.data.frame(endog_data[[shock_name]])
  results <- lp_nl_iv(endog_data_japan,
                      lags_endog_nl     = 6,
                      shock             = shock_data,
                      exog_data         = exog_data,
                      lags_exog         = 4,
                      trend             = 0,
                      confint           = 1.96,
                      hor               = 24,
                      switching         = switching_variable,
                      use_logistic      = TRUE,
                      lag_switching     = TRUE,
                      use_hp            = FALSE,
                      gamma             = 3)
  
  plot_objs <- plot_nl(results)
  return(list(s1 = plot_objs$gg_s1, s2 = plot_objs$gg_s2))
}

# =========================
# Generate and Store IRFs for All Shocks
# =========================
plot_grid_s1 <- list()
plot_grid_s2 <- list()

for (shock in exog_vars) {
  plots <- generate_irf_plots(shock)
  
  for (i in seq_along(endog_vars)) {
    endog <- endog_vars[i]
    
    # Determine if this plot is in the first column (needs y-axis label)
    is_first_col <- shock == exog_vars[1]
    
    # Determine if this plot is in the last row (needs x-axis label)
    is_last_row <- i == length(endog_vars)
    
    # Create a more readable title with proper variable names
    endog_display <- get_display_name(endog)
    shock_display <- get_display_name(shock)
    plot_title <- paste0("Resp. of ", endog_display, " to a shock of ", shock_display)
    
    # Get the plot for this endogenous variable
    p1 <- plots$s1[[i]]
    p2 <- plots$s2[[i]]
    
    # Add appropriate axis labels based on position with improved formatting
    if (is_first_col) {
      p1 <- p1 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
      p2 <- p2 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + ylab("")
      p2 <- p2 + ylab("")
    }
    
    if (is_last_row) {
      p1 <- p1 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
      p2 <- p2 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + xlab("")
      p2 <- p2 + xlab("")
    }
    
    # Add improved title formatting with proper variable names
    p1 <- p1 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    p2 <- p2 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    # Store the modified plots
    plot_grid_s1[[paste0(endog, "_", shock)]] <- p1
    plot_grid_s2[[paste0(endog, "_", shock)]] <- p2
  }
}

# =========================
# Save IRF Grid Function
# =========================
plot_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Causality_Analysis/Output/IRF_Grids"
if (!dir.exists(plot_dir)) dir.create(plot_dir)

save_irf_grid <- function(plot_list, filename_png) {
  # Save individual plots first
  for (name in names(plot_list)) {
    p <- plot_list[[name]]
    file_path <- file.path(plot_dir, paste0(name, ".png"))
    ggsave(file_path, plot = p, width = 6, height = 5, dpi = 150)
  }
  
  # Create a list to hold plots in the correct order for the grid
  grid_plots <- list()
  
  # Fill the list with plots in the correct order
  for (i in seq_along(endog_vars)) {
    for (j in seq_along(exog_vars)) {
      endog <- endog_vars[i]
      shock <- exog_vars[j]
      plot_name <- paste0(endog, "_", shock)
      grid_plots <- c(grid_plots, list(plot_list[[plot_name]]))
    }
  }
  
  # Arrange plots in a grid
  arranged_plots <- do.call(arrangeGrob, c(grid_plots, list(ncol = length(exog_vars))))
  
  # Save as high-quality PNG
  ggsave(filename_png, arranged_plots, width = 16, height = 20, dpi = 300)
  
  # Convert to JPG as well
  jpg_filename <- sub(".png$", ".jpg", filename_png)
  ggsave(jpg_filename, arranged_plots, width = 16, height = 20, dpi = 300)
}

# =========================
# Save Both Regime Grids
# =========================
save_irf_grid(plot_grid_s1, "IRF_Grid_Regime1_HighIR_JAPAN_revised.png")
save_irf_grid(plot_grid_s2, "IRF_Grid_Regime2_LowIR_JAPAN_revised.png")

cat("\n=== IRF Generation Complete for JAPAN ===\n")
cat("Plot titles updated with proper variable names:\n")
cat("  - Unemployment Rate (was: Unemploymentrate)\n")
cat("  - REER (was: RealbroadEER)\n")
cat("  - SIR (was: ShorttermIR)\n")
cat("  - Oil Price (WTI) (was: OilpriceGlobalWTI)\n")
cat("  - CPI Inflation (was: CPIinflationrate)\n")
cat("  - EPU (was: logEPU)\n")
cat("  - GPRC, USEMV, USMPU (unchanged)\n")
cat("\nFiles saved in:", plot_dir, "\n")

############################## Country: UK #####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/uk")
getwd()

endog_data <- read.csv("all_mulvar_data_uk_v2.csv", header = TRUE)
endog_data$Date <- as.Date(endog_data$Date)
str(endog_data)
head(endog_data)
tail(endog_data)

endog_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")
switching_variable <- endog_data$ShorttermIR

endog_data_uk <- endog_data[, endog_vars]
exog_data <- endog_data[, exog_vars]

# =========================
# Variable Name Mapping
# =========================
var_name_mapping <- list(
  "Unemploymentrate" = "Unemployment Rate",
  "RealbroadEER" = "REER",
  "ShorttermIR" = "SIR",
  "OilpriceGlobalWTI" = "Oil Price (WTI)",
  "CPIinflationrate" = "CPI Inflation",
  "logEPU" = "EPU",
  "GPRC" = "GPR",
  "USEMV" = "USEMV",
  "USMPU" = "USMPU"
)

# Helper function to get display name
get_display_name <- function(var_name) {
  return(var_name_mapping[[var_name]])
}

# =========================
# Helper Function to Generate IRFs
# =========================
generate_irf_plots <- function(shock_name) {
  shock_data <- as.data.frame(endog_data[[shock_name]])
  results <- lp_nl_iv(endog_data_uk,
                      lags_endog_nl     = 6,
                      shock             = shock_data,
                      exog_data         = exog_data,
                      lags_exog         = 4,
                      trend             = 0,
                      confint           = 1.96,
                      hor               = 24,
                      switching         = switching_variable,
                      use_logistic      = TRUE,
                      lag_switching     = TRUE,
                      use_hp            = FALSE,
                      gamma             = 3)
  
  plot_objs <- plot_nl(results)
  return(list(s1 = plot_objs$gg_s1, s2 = plot_objs$gg_s2))
}

# =========================
# Generate and Store IRFs for All Shocks
# =========================
plot_grid_s1 <- list()
plot_grid_s2 <- list()

for (shock in exog_vars) {
  plots <- generate_irf_plots(shock)
  
  for (i in seq_along(endog_vars)) {
    endog <- endog_vars[i]
    
    # Determine if this plot is in the first column (needs y-axis label)
    is_first_col <- shock == exog_vars[1]
    
    # Determine if this plot is in the last row (needs x-axis label)
    is_last_row <- i == length(endog_vars)
    
    # Create a more readable title with proper variable names
    endog_display <- get_display_name(endog)
    shock_display <- get_display_name(shock)
    plot_title <- paste0("Resp. of ", endog_display, " to a shock of ", shock_display)
    
    # Get the plot for this endogenous variable
    p1 <- plots$s1[[i]]
    p2 <- plots$s2[[i]]
    
    # Add appropriate axis labels based on position with improved formatting
    if (is_first_col) {
      p1 <- p1 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
      p2 <- p2 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + ylab("")
      p2 <- p2 + ylab("")
    }
    
    if (is_last_row) {
      p1 <- p1 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
      p2 <- p2 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + xlab("")
      p2 <- p2 + xlab("")
    }
    
    # Add improved title formatting with proper variable names
    p1 <- p1 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    p2 <- p2 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    # Store the modified plots
    plot_grid_s1[[paste0(endog, "_", shock)]] <- p1
    plot_grid_s2[[paste0(endog, "_", shock)]] <- p2
  }
}

# =========================
# Save IRF Grid Function
# =========================
plot_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Causality_Analysis/Output/IRF_Grids"
if (!dir.exists(plot_dir)) dir.create(plot_dir)

save_irf_grid <- function(plot_list, filename_png) {
  # Save individual plots first
  for (name in names(plot_list)) {
    p <- plot_list[[name]]
    file_path <- file.path(plot_dir, paste0(name, ".png"))
    ggsave(file_path, plot = p, width = 6, height = 5, dpi = 150)
  }
  
  # Create a list to hold plots in the correct order for the grid
  grid_plots <- list()
  
  # Fill the list with plots in the correct order
  for (i in seq_along(endog_vars)) {
    for (j in seq_along(exog_vars)) {
      endog <- endog_vars[i]
      shock <- exog_vars[j]
      plot_name <- paste0(endog, "_", shock)
      grid_plots <- c(grid_plots, list(plot_list[[plot_name]]))
    }
  }
  
  # Arrange plots in a grid
  arranged_plots <- do.call(arrangeGrob, c(grid_plots, list(ncol = length(exog_vars))))
  
  # Save as high-quality PNG
  ggsave(filename_png, arranged_plots, width = 16, height = 20, dpi = 300)
  
  # Convert to JPG as well
  jpg_filename <- sub(".png$", ".jpg", filename_png)
  ggsave(jpg_filename, arranged_plots, width = 16, height = 20, dpi = 300)
}

# =========================
# Save Both Regime Grids
# =========================
save_irf_grid(plot_grid_s1, "IRF_Grid_Regime1_HighIR_UK_revised.png")
save_irf_grid(plot_grid_s2, "IRF_Grid_Regime2_LowIR_UK_revised.png")

cat("\n=== IRF Generation Complete for UK ===\n")
cat("Plot titles updated with proper variable names:\n")
cat("  - Unemployment Rate (was: Unemploymentrate)\n")
cat("  - REER (was: RealbroadEER)\n")
cat("  - SIR (was: ShorttermIR)\n")
cat("  - Oil Price (WTI) (was: OilpriceGlobalWTI)\n")
cat("  - CPI Inflation (was: CPIinflationrate)\n")
cat("  - EPU (was: logEPU)\n")
cat("  - GPRC, USEMV, USMPU (unchanged)\n")
cat("\nFiles saved in:", plot_dir, "\n")

############################## Country: ITALY #####################
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/italy")
getwd()

endog_data <- read.csv("all_mulvar_data_italy_v2.csv", header = TRUE)
endog_data$Date <- as.Date(endog_data$Date)
str(endog_data)
head(endog_data)
tail(endog_data)

endog_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exog_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")
switching_variable <- endog_data$ShorttermIR

endog_data_italy <- endog_data[, endog_vars]
exog_data <- endog_data[, exog_vars]

# =========================
# Variable Name Mapping
# =========================
var_name_mapping <- list(
  "Unemploymentrate" = "Unemployment Rate",
  "RealbroadEER" = "REER",
  "ShorttermIR" = "SIR",
  "OilpriceGlobalWTI" = "Oil Price (WTI)",
  "CPIinflationrate" = "CPI Inflation",
  "logEPU" = "EPU",
  "GPRC" = "GPR",
  "USEMV" = "USEMV",
  "USMPU" = "USMPU"
)

# Helper function to get display name
get_display_name <- function(var_name) {
  return(var_name_mapping[[var_name]])
}

# =========================
# Helper Function to Generate IRFs
# =========================
generate_irf_plots <- function(shock_name) {
  shock_data <- as.data.frame(endog_data[[shock_name]])
  results <- lp_nl_iv(endog_data_italy,
                      lags_endog_nl     = 6,
                      shock             = shock_data,
                      exog_data         = exog_data,
                      lags_exog         = 4,
                      trend             = 0,
                      confint           = 1.96,
                      hor               = 24,
                      switching         = switching_variable,
                      use_logistic      = TRUE,
                      lag_switching     = TRUE,
                      use_hp            = FALSE,
                      gamma             = 3)
  
  plot_objs <- plot_nl(results)
  return(list(s1 = plot_objs$gg_s1, s2 = plot_objs$gg_s2))
}

# =========================
# Generate and Store IRFs for All Shocks
# =========================
plot_grid_s1 <- list()
plot_grid_s2 <- list()

for (shock in exog_vars) {
  plots <- generate_irf_plots(shock)
  
  for (i in seq_along(endog_vars)) {
    endog <- endog_vars[i]
    
    # Determine if this plot is in the first column (needs y-axis label)
    is_first_col <- shock == exog_vars[1]
    
    # Determine if this plot is in the last row (needs x-axis label)
    is_last_row <- i == length(endog_vars)
    
    # Create a more readable title with proper variable names
    endog_display <- get_display_name(endog)
    shock_display <- get_display_name(shock)
    plot_title <- paste0("Resp. of ", endog_display, " to a shock of ", shock_display)
    
    # Get the plot for this endogenous variable
    p1 <- plots$s1[[i]]
    p2 <- plots$s2[[i]]
    
    # Add appropriate axis labels based on position with improved formatting
    if (is_first_col) {
      p1 <- p1 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
      p2 <- p2 + ylab("Impulse Response") + 
        theme(axis.title.y = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + ylab("")
      p2 <- p2 + ylab("")
    }
    
    if (is_last_row) {
      p1 <- p1 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
      p2 <- p2 + xlab("Months") + 
        theme(axis.title.x = element_text(size = 14, face = "bold"))
    } else {
      p1 <- p1 + xlab("")
      p2 <- p2 + xlab("")
    }
    
    # Add improved title formatting with proper variable names
    p1 <- p1 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    p2 <- p2 + ggtitle(plot_title) + 
      theme(plot.title = element_text(size = 10, face = "bold"))
    
    # Store the modified plots
    plot_grid_s1[[paste0(endog, "_", shock)]] <- p1
    plot_grid_s2[[paste0(endog, "_", shock)]] <- p2
  }
}

# =========================
# Save IRF Grid Function
# =========================
plot_dir <- "/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Causality_Analysis/Output/IRF_Grids"
if (!dir.exists(plot_dir)) dir.create(plot_dir)

save_irf_grid <- function(plot_list, filename_png) {
  # Save individual plots first
  for (name in names(plot_list)) {
    p <- plot_list[[name]]
    file_path <- file.path(plot_dir, paste0(name, ".png"))
    ggsave(file_path, plot = p, width = 6, height = 5, dpi = 150)
  }
  
  # Create a list to hold plots in the correct order for the grid
  grid_plots <- list()
  
  # Fill the list with plots in the correct order
  for (i in seq_along(endog_vars)) {
    for (j in seq_along(exog_vars)) {
      endog <- endog_vars[i]
      shock <- exog_vars[j]
      plot_name <- paste0(endog, "_", shock)
      grid_plots <- c(grid_plots, list(plot_list[[plot_name]]))
    }
  }
  
  # Arrange plots in a grid
  arranged_plots <- do.call(arrangeGrob, c(grid_plots, list(ncol = length(exog_vars))))
  
  # Save as high-quality PNG
  ggsave(filename_png, arranged_plots, width = 16, height = 20, dpi = 300)
  
  # Convert to JPG as well
  jpg_filename <- sub(".png$", ".jpg", filename_png)
  ggsave(jpg_filename, arranged_plots, width = 16, height = 20, dpi = 300)
}

# =========================
# Save Both Regime Grids
# =========================
save_irf_grid(plot_grid_s1, "IRF_Grid_Regime1_HighIR_ITALY_revised.png")
save_irf_grid(plot_grid_s2, "IRF_Grid_Regime2_LowIR_ITALY_revised.png")

cat("\n=== IRF Generation Complete for ITALY ===\n")
cat("Plot titles updated with proper variable names:\n")
cat("  - Unemployment Rate (was: Unemploymentrate)\n")
cat("  - REER (was: RealbroadEER)\n")
cat("  - SIR (was: ShorttermIR)\n")
cat("  - Oil Price (WTI) (was: OilpriceGlobalWTI)\n")
cat("  - CPI Inflation (was: CPIinflationrate)\n")
cat("  - EPU (was: logEPU)\n")
cat("  - GPRC, USEMV, USMPU (unchanged)\n")
cat("\nFiles saved in:", plot_dir, "\n")
############################# End of Code ############################






