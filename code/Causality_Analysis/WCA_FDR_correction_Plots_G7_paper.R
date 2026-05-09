###################### WCA - FDR charts : Training period ###################
############################## Country: CANADA #####################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/canada")
getwd()

# Load required libraries
library(biwavelet)
library(wavelets)
library(Cairo)
library(magick)
library(dplyr)
library(lmtest)
library(lubridate)

# Read the dataset
data_ts <- read.csv("all_mulvar_data_canada_v2.csv", header = TRUE)
data_ts$Date <- as.Date(data_ts$Date)
str(data_ts)

# =========================
# Filter data for training period only (1995M01 to 2022M03)
# =========================
training_start <- as.Date("1995-01-01")
training_end <- as.Date("2022-03-01")

# Filter data to training period
data_ts_train <- data_ts[data_ts$Date >= training_start & data_ts$Date <= training_end, ]
str(data_ts_train)

# Verify the filtered dataset
cat("Original dataset period:", min(data_ts$Date), "to", max(data_ts$Date), "\n")
cat("Training dataset period:", min(data_ts_train$Date), "to", max(data_ts_train$Date), "\n")
cat("Original observations:", nrow(data_ts), "\n")
cat("Training observations:", nrow(data_ts_train), "\n")

# Define variables
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Variable labels for titles
endogenous_labels <- c("Unemployment Rate", "REER", "SIR", "Oil Price (WTI)", "CPI Inflation")
# exogenous_labels <- c("logEPU", "GPRC", "USEMV", "USMPU")
exogenous_labels <- c("EPU", "GPR", "USEMV", "USMPU")

# Calculate time period information for training data
n_obs_train <- nrow(data_ts_train)
start_year_train <- year(min(data_ts_train$Date))
end_year_train <- year(max(data_ts_train$Date))
time_sequence_train <- 1:n_obs_train

# =========================
# FDR Correction Function (Aguiar-Conraria & Soares 2014 approach)
# =========================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  # Extract dimensions
  n_time <- ncol(wtc_result$rsq)
  n_scale <- nrow(wtc_result$rsq)
  
  # Initialize matrices
  p_values <- matrix(NA, nrow = n_scale, ncol = n_time)
  fdr_signif <- matrix(FALSE, nrow = n_scale, ncol = n_time)
  
  # For each scale, compute p-values and apply FDR correction
  for (i in 1:n_scale) {
    # Get the significance level for this scale
    sig_level <- wtc_result$signif[i]
    
    # Skip if sig_level is NA or 0
    if (is.na(sig_level) || sig_level == 0) {
      next
    }
    
    # Compute p-values for this scale
    scale_rsq <- wtc_result$rsq[i, ]
    scale_p_values <- rep(NA, n_time)
    
    for (j in 1:n_time) {
      if (!is.na(scale_rsq[j])) {
        # Compute p-value based on the null distribution
        # Under H0, the coherence follows a specific distribution
        # We use the fact that the significance level represents the critical value
        if (scale_rsq[j] >= sig_level) {
          # For significant coherence, estimate p-value
          # Using the relationship between coherence and chi-square distribution
          scale_p_values[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        } else {
          # For non-significant coherence, use a conservative p-value
          scale_p_values[j] <- min(1, (1 - scale_rsq[j]/sig_level))
        }
      }
    }
    
    p_values[i, ] <- scale_p_values
    
    # Apply Benjamini-Hochberg FDR correction for this scale
    valid_indices <- which(!is.na(scale_p_values))
    
    if (length(valid_indices) > 0) {
      # Extract valid p-values
      valid_p <- scale_p_values[valid_indices]
      
      # Apply FDR correction
      p_adjusted <- p.adjust(valid_p, method = "BH")
      
      # Mark significant points
      sig_indices <- valid_indices[p_adjusted < alpha]
      fdr_signif[i, sig_indices] <- TRUE
    }
  }
  
  # Return results
  return(list(
    fdr_signif = fdr_signif, 
    p_values = p_values,
    alpha = alpha
  ))
}

# =========================
# Modified WTC plotting function with FDR correction
# =========================
plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label, n_obs, start_year, end_year, 
                              main_title, file_name) {
  
  # CairoPNG(filename = file_name, width = 1600, height = 1200, res = 300)
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  
  # Set plotting parameters
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  
  # Create base plot without original significance contours
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
       lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       # ylab = "Scale", xlab = "Period", plot.cb = TRUE,
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE, 
       main = main_title, cex.main = 1.5, 
       # font.main = 2, font.lab = 2)
       font.main = 3, font.lab = 3)
  
  # Add FDR-corrected significance contours
  if (any(fdr_result$fdr_signif, na.rm = TRUE)) {
    # Create a smoothed version of the significance matrix for better contours
    sig_smooth <- fdr_result$fdr_signif
    
    # Add contours for FDR-corrected significant regions
    contour(wtc_result$t, wtc_result$period, t(sig_smooth), 
            levels = c(0.5), add = TRUE, col = "black", lwd = 2, 
            drawlabels = FALSE, method = "edge")
  }
  
  # Add grid lines
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  
  # Define x-axis labels
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks)) {
    year_labels <- year_labels[1:length(year_breaks)]
  }
  
  # Add time axis
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  
  # Add FDR correction note
  # mtext(paste0("FDR-corrected significance (α = ", fdr_result$alpha, ")"), 
  #       side = 1, line = 4, cex = 0.8, font = 3)
  
  dev.off()
}

# =========================
# Generate and Save WCC Plots with FDR Correction (Training Period Only)
# =========================
output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)

wcc_results <- list()
fdr_results <- list()
file_list <- c()

# Store all p-values for global FDR correction summary
all_p_values <- list()

# Create WCC plots for each endogenous-exogenous pair using training data only
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    y_var <- endogenous_vars[i]
    x_var <- exogenous_vars[j]
    y_label <- endogenous_labels[i]
    x_label <- exogenous_labels[j]
    
    pair_name <- paste(y_var, x_var, sep = "_x_")
    
    cat("Processing:", pair_name, "\n")
    
    # Prepare time series data using training period only
    t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
    t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
    
    # Specify the number of iterations for significance testing
    nrands <- 1000
    
    # Calculate wavelet coherence
    wtc_result <- wtc(t1, t2, nrands = nrands)
    wcc_results[[pair_name]] <- wtc_result
    
    # Apply FDR correction with alpha = 0.10
    fdr_result <- apply_fdr_correction(wtc_result, alpha = 0.10)
    fdr_results[[pair_name]] <- fdr_result
    
    # Store p-values for summary
    all_p_values[[pair_name]] <- fdr_result$p_values
    
    # Create output filename
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name), "_fdr_training.png")
    file_list <- c(file_list, out_file)
    
    # Generate individual WCC plot with FDR correction
    plot_wtc_with_fdr(wtc_result, fdr_result, y_label, x_label, 
                      n_obs_train, start_year_train, end_year_train,
                      # paste0(y_label, " vs ", x_label, " (Training: 1995-2022, FDR-corrected)"),
                      paste0(y_label, " vs ", x_label),
                      out_file)
  }
}

# =========================
# Combine All WCC Plots into One Grid Image (5x4) - Training Period with FDR
# =========================
library(magick)

# Clean file list and ensure correct order
file_list <- Filter(function(f) file.exists(f) && grepl("\\.png$", f), file_list)

# Sort files to maintain correct order (endogenous vars as rows, exogenous vars as columns)
ordered_files <- c()
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    pattern <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    matching_file <- file_list[grepl(pattern, file_list)]
    if (length(matching_file) > 0) {
      ordered_files <- c(ordered_files, matching_file[1])
    }
  }
}

# Group into rows of 4 (one row per endogenous variable)
rows <- split(ordered_files, ceiling(seq_along(ordered_files)/4))

# Build rows first
row_images <- lapply(rows, function(row_files) {
  if (length(row_files) > 0) {
    row_imgs <- image_read(row_files)
    image_append(image_join(row_imgs), stack = FALSE)
  }
})

# Remove any NULL entries
row_images <- row_images[!sapply(row_images, is.null)]

# Stack rows vertically
if (length(row_images) > 0) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  
  # Save the grid image with FDR correction specification
  image_write(grid_image, "WCC_Heatmaps_Grid_canada_fdr_training_revised.png")
  cat("Grid image with FDR correction saved as: WCC_Heatmaps_Grid_canada_fdr_training.png\n")
}

############################## Country: USA #####################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/usa")
getwd()

# Load required libraries
library(biwavelet)
library(wavelets)
library(Cairo)
library(magick)
library(dplyr)
library(lmtest)
library(lubridate)

# Read the dataset
data_ts <- read.csv("all_mulvar_data_usa_v2.csv", header = TRUE)
data_ts$Date <- as.Date(data_ts$Date)
str(data_ts)

# =========================
# Filter data for training period only (1995M01 to 2022M03)
# =========================
training_start <- as.Date("1995-01-01")
training_end <- as.Date("2022-03-01")

# Filter data to training period
data_ts_train <- data_ts[data_ts$Date >= training_start & data_ts$Date <= training_end, ]

# Verify the filtered dataset
cat("Original dataset period:", min(data_ts$Date), "to", max(data_ts$Date), "\n")
cat("Training dataset period:", min(data_ts_train$Date), "to", max(data_ts_train$Date), "\n")
cat("Original observations:", nrow(data_ts), "\n")
cat("Training observations:", nrow(data_ts_train), "\n")

# Define variables
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Variable labels for titles
# endogenous_labels <- c("Unemployment Rate", "Real broad EER", "Short-term IR", "Oil Price (WTI)", "CPI Inflation Rate")
endogenous_labels <- c("Unemployment Rate", "REER", "SIR", "Oil Price (WTI)", "CPI Inflation")
# exogenous_labels <- c("logEPU", "GPRC", "USEMV", "USMPU")
exogenous_labels <- c("EPU", "GPR", "USEMV", "USMPU")

# Calculate time period information for training data
n_obs_train <- nrow(data_ts_train)
start_year_train <- year(min(data_ts_train$Date))
end_year_train <- year(max(data_ts_train$Date))
time_sequence_train <- 1:n_obs_train

# =========================
# FDR Correction Function (Aguiar-Conraria & Soares 2014 approach)
# =========================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  # Extract dimensions
  n_time <- ncol(wtc_result$rsq)
  n_scale <- nrow(wtc_result$rsq)
  
  # Initialize matrices
  p_values <- matrix(NA, nrow = n_scale, ncol = n_time)
  fdr_signif <- matrix(FALSE, nrow = n_scale, ncol = n_time)
  
  # For each scale, compute p-values and apply FDR correction
  for (i in 1:n_scale) {
    # Get the significance level for this scale
    sig_level <- wtc_result$signif[i]
    
    # Skip if sig_level is NA or 0
    if (is.na(sig_level) || sig_level == 0) {
      next
    }
    
    # Compute p-values for this scale
    scale_rsq <- wtc_result$rsq[i, ]
    scale_p_values <- rep(NA, n_time)
    
    for (j in 1:n_time) {
      if (!is.na(scale_rsq[j])) {
        # Compute p-value based on the null distribution
        # Under H0, the coherence follows a specific distribution
        # We use the fact that the significance level represents the critical value
        if (scale_rsq[j] >= sig_level) {
          # For significant coherence, estimate p-value
          # Using the relationship between coherence and chi-square distribution
          scale_p_values[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        } else {
          # For non-significant coherence, use a conservative p-value
          scale_p_values[j] <- min(1, (1 - scale_rsq[j]/sig_level))
        }
      }
    }
    
    p_values[i, ] <- scale_p_values
    
    # Apply Benjamini-Hochberg FDR correction for this scale
    valid_indices <- which(!is.na(scale_p_values))
    
    if (length(valid_indices) > 0) {
      # Extract valid p-values
      valid_p <- scale_p_values[valid_indices]
      
      # Apply FDR correction
      p_adjusted <- p.adjust(valid_p, method = "BH")
      
      # Mark significant points
      sig_indices <- valid_indices[p_adjusted < alpha]
      fdr_signif[i, sig_indices] <- TRUE
    }
  }
  
  # Return results
  return(list(
    fdr_signif = fdr_signif, 
    p_values = p_values,
    alpha = alpha
  ))
}

# =========================
# Modified WTC plotting function with FDR correction
# =========================
plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label, n_obs, start_year, end_year, 
                              main_title, file_name) {
  
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  
  # Set plotting parameters
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  
  # Create base plot without original significance contours
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
       lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       # ylab = "Scale", xlab = "Period", plot.cb = TRUE, 
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE,
       main = main_title, cex.main = 1.5, 
       # font.main = 2, font.lab = 2)
       font.main = 3, font.lab = 2)
  
  # Add FDR-corrected significance contours
  if (any(fdr_result$fdr_signif, na.rm = TRUE)) {
    # Create a smoothed version of the significance matrix for better contours
    sig_smooth <- fdr_result$fdr_signif
    
    # Add contours for FDR-corrected significant regions
    contour(wtc_result$t, wtc_result$period, t(sig_smooth), 
            levels = c(0.5), add = TRUE, col = "black", lwd = 2, 
            drawlabels = FALSE, method = "edge")
  }
  
  # Add grid lines
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  
  # Define x-axis labels
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks)) {
    year_labels <- year_labels[1:length(year_breaks)]
  }
  
  # Add time axis
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  
  # Add FDR correction note
  # mtext(paste0("FDR-corrected significance (α = ", fdr_result$alpha, ")"), 
  #       side = 1, line = 4, cex = 0.8, font = 3)
  
  dev.off()
}

# =========================
# Generate and Save WCC Plots with FDR Correction (Training Period Only)
# =========================
output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)

wcc_results <- list()
fdr_results <- list()
file_list <- c()

# Store all p-values for global FDR correction summary
all_p_values <- list()

# Create WCC plots for each endogenous-exogenous pair using training data only
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    y_var <- endogenous_vars[i]
    x_var <- exogenous_vars[j]
    y_label <- endogenous_labels[i]
    x_label <- exogenous_labels[j]
    
    pair_name <- paste(y_var, x_var, sep = "_x_")
    
    cat("Processing:", pair_name, "\n")
    
    # Prepare time series data using training period only
    t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
    t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
    
    # Specify the number of iterations for significance testing
    nrands <- 1000
    
    # Calculate wavelet coherence
    wtc_result <- wtc(t1, t2, nrands = nrands)
    wcc_results[[pair_name]] <- wtc_result
    
    # Apply FDR correction with alpha = 0.10
    fdr_result <- apply_fdr_correction(wtc_result, alpha = 0.10)
    fdr_results[[pair_name]] <- fdr_result
    
    # Store p-values for summary
    all_p_values[[pair_name]] <- fdr_result$p_values
    
    # Create output filename
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name), "_fdr_training.png")
    file_list <- c(file_list, out_file)
    
    # Generate individual WCC plot with FDR correction
    plot_wtc_with_fdr(wtc_result, fdr_result, y_label, x_label, 
                      n_obs_train, start_year_train, end_year_train,
                      # paste0(y_label, " vs ", x_label, " (Training: 1995-2022, FDR-corrected)"),
                      paste0(y_label, " vs ", x_label),
                      out_file)
  }
}

# =========================
# Combine All WCC Plots into One Grid Image (5x4) - Training Period with FDR
# =========================
library(magick)

# Clean file list and ensure correct order
file_list <- Filter(function(f) file.exists(f) && grepl("\\.png$", f), file_list)

# Sort files to maintain correct order (endogenous vars as rows, exogenous vars as columns)
ordered_files <- c()
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    pattern <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    matching_file <- file_list[grepl(pattern, file_list)]
    if (length(matching_file) > 0) {
      ordered_files <- c(ordered_files, matching_file[1])
    }
  }
}

# Group into rows of 4 (one row per endogenous variable)
rows <- split(ordered_files, ceiling(seq_along(ordered_files)/4))

# Build rows first
row_images <- lapply(rows, function(row_files) {
  if (length(row_files) > 0) {
    row_imgs <- image_read(row_files)
    image_append(image_join(row_imgs), stack = FALSE)
  }
})

# Remove any NULL entries
row_images <- row_images[!sapply(row_images, is.null)]

# Stack rows vertically
if (length(row_images) > 0) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  
  # Save the grid image with FDR correction specification
  image_write(grid_image, "WCC_Heatmaps_Grid_usa_fdr_training_revised.png")
  cat("Grid image with FDR correction saved as: WCC_Heatmaps_Grid_usa_fdr_training.png\n")
}

############################## Country: FRANCE #####################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/france")
getwd()

# Load required libraries
library(biwavelet)
library(wavelets)
library(Cairo)
library(magick)
library(dplyr)
library(lmtest)
library(lubridate)

# Read the dataset
data_ts <- read.csv("all_mulvar_data_france_v2.csv", header = TRUE)
data_ts$Date <- as.Date(data_ts$Date)
str(data_ts)

# =========================
# Filter data for training period only (1995M01 to 2022M03)
# =========================
training_start <- as.Date("1995-01-01")
training_end <- as.Date("2022-03-01")

# Filter data to training period
data_ts_train <- data_ts[data_ts$Date >= training_start & data_ts$Date <= training_end, ]

# Verify the filtered dataset
cat("Original dataset period:", min(data_ts$Date), "to", max(data_ts$Date), "\n")
cat("Training dataset period:", min(data_ts_train$Date), "to", max(data_ts_train$Date), "\n")
cat("Original observations:", nrow(data_ts), "\n")
cat("Training observations:", nrow(data_ts_train), "\n")

# Define variables
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Variable labels for titles
# endogenous_labels <- c("Unemployment Rate", "Real broad EER", "Short-term IR", "Oil Price (WTI)", "CPI Inflation Rate")
# exogenous_labels <- c("logEPU", "GPRC", "USEMV", "USMPU")

endogenous_labels <- c("Unemployment Rate", "REER", "SIR", "Oil Price (WTI)", "CPI Inflation")
exogenous_labels <- c("EPU", "GPR", "USEMV", "USMPU")

# Calculate time period information for training data
n_obs_train <- nrow(data_ts_train)
start_year_train <- year(min(data_ts_train$Date))
end_year_train <- year(max(data_ts_train$Date))
time_sequence_train <- 1:n_obs_train

# =========================
# FDR Correction Function (Aguiar-Conraria & Soares 2014 approach)
# =========================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  # Extract dimensions
  n_time <- ncol(wtc_result$rsq)
  n_scale <- nrow(wtc_result$rsq)
  
  # Initialize matrices
  p_values <- matrix(NA, nrow = n_scale, ncol = n_time)
  fdr_signif <- matrix(FALSE, nrow = n_scale, ncol = n_time)
  
  # For each scale, compute p-values and apply FDR correction
  for (i in 1:n_scale) {
    # Get the significance level for this scale
    sig_level <- wtc_result$signif[i]
    
    # Skip if sig_level is NA or 0
    if (is.na(sig_level) || sig_level == 0) {
      next
    }
    
    # Compute p-values for this scale
    scale_rsq <- wtc_result$rsq[i, ]
    scale_p_values <- rep(NA, n_time)
    
    for (j in 1:n_time) {
      if (!is.na(scale_rsq[j])) {
        # Compute p-value based on the null distribution
        # Under H0, the coherence follows a specific distribution
        # We use the fact that the significance level represents the critical value
        if (scale_rsq[j] >= sig_level) {
          # For significant coherence, estimate p-value
          # Using the relationship between coherence and chi-square distribution
          scale_p_values[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        } else {
          # For non-significant coherence, use a conservative p-value
          scale_p_values[j] <- min(1, (1 - scale_rsq[j]/sig_level))
        }
      }
    }
    
    p_values[i, ] <- scale_p_values
    
    # Apply Benjamini-Hochberg FDR correction for this scale
    valid_indices <- which(!is.na(scale_p_values))
    
    if (length(valid_indices) > 0) {
      # Extract valid p-values
      valid_p <- scale_p_values[valid_indices]
      
      # Apply FDR correction
      p_adjusted <- p.adjust(valid_p, method = "BH")
      
      # Mark significant points
      sig_indices <- valid_indices[p_adjusted < alpha]
      fdr_signif[i, sig_indices] <- TRUE
    }
  }
  
  # Return results
  return(list(
    fdr_signif = fdr_signif, 
    p_values = p_values,
    alpha = alpha
  ))
}

# =========================
# Modified WTC plotting function with FDR correction
# =========================
plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label, n_obs, start_year, end_year, 
                              main_title, file_name) {
  
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  
  # Set plotting parameters
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  
  # Create base plot without original significance contours
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
       lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       # ylab = "Scale", xlab = "Period", plot.cb = TRUE, 
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE, 
       main = main_title, cex.main = 1.5, 
       # font.main = 2, font.lab = 2)
       font.main = 3, font.lab = 2)
  
  # Add FDR-corrected significance contours
  if (any(fdr_result$fdr_signif, na.rm = TRUE)) {
    # Create a smoothed version of the significance matrix for better contours
    sig_smooth <- fdr_result$fdr_signif
    
    # Add contours for FDR-corrected significant regions
    contour(wtc_result$t, wtc_result$period, t(sig_smooth), 
            levels = c(0.5), add = TRUE, col = "black", lwd = 2, 
            drawlabels = FALSE, method = "edge")
  }
  
  # Add grid lines
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  
  # Define x-axis labels
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks)) {
    year_labels <- year_labels[1:length(year_breaks)]
  }
  
  # Add time axis
  # axis(side = 3, at = year_breaks, labels = year_labels, font = 2)
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  
  # Add FDR correction note
  # mtext(paste0("FDR-corrected significance (α = ", fdr_result$alpha, ")"), 
  #       side = 1, line = 4, cex = 0.8, font = 3)
  
  dev.off()
}

# =========================
# Generate and Save WCC Plots with FDR Correction (Training Period Only)
# =========================
output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)

wcc_results <- list()
fdr_results <- list()
file_list <- c()

# Store all p-values for global FDR correction summary
all_p_values <- list()

# Create WCC plots for each endogenous-exogenous pair using training data only
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    y_var <- endogenous_vars[i]
    x_var <- exogenous_vars[j]
    y_label <- endogenous_labels[i]
    x_label <- exogenous_labels[j]
    
    pair_name <- paste(y_var, x_var, sep = "_x_")
    
    cat("Processing:", pair_name, "\n")
    
    # Prepare time series data using training period only
    t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
    t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
    
    # Specify the number of iterations for significance testing
    nrands <- 1000
    
    # Calculate wavelet coherence
    wtc_result <- wtc(t1, t2, nrands = nrands)
    wcc_results[[pair_name]] <- wtc_result
    
    # Apply FDR correction with alpha = 0.10
    fdr_result <- apply_fdr_correction(wtc_result, alpha = 0.10)
    fdr_results[[pair_name]] <- fdr_result
    
    # Store p-values for summary
    all_p_values[[pair_name]] <- fdr_result$p_values
    
    # Create output filename
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name), "_fdr_training.png")
    file_list <- c(file_list, out_file)
    
    # Generate individual WCC plot with FDR correction
    plot_wtc_with_fdr(wtc_result, fdr_result, y_label, x_label, 
                      n_obs_train, start_year_train, end_year_train,
                      # paste0(y_label, " vs ", x_label, " (Training: 1995-2022, FDR-corrected)"),
                      paste0(y_label, " vs ", x_label),
                      out_file)
  }
}

# =========================
# Combine All WCC Plots into One Grid Image (5x4) - Training Period with FDR
# =========================
library(magick)

# Clean file list and ensure correct order
file_list <- Filter(function(f) file.exists(f) && grepl("\\.png$", f), file_list)

# Sort files to maintain correct order (endogenous vars as rows, exogenous vars as columns)
ordered_files <- c()
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    pattern <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    matching_file <- file_list[grepl(pattern, file_list)]
    if (length(matching_file) > 0) {
      ordered_files <- c(ordered_files, matching_file[1])
    }
  }
}

# Group into rows of 4 (one row per endogenous variable)
rows <- split(ordered_files, ceiling(seq_along(ordered_files)/4))

# Build rows first
row_images <- lapply(rows, function(row_files) {
  if (length(row_files) > 0) {
    row_imgs <- image_read(row_files)
    image_append(image_join(row_imgs), stack = FALSE)
  }
})

# Remove any NULL entries
row_images <- row_images[!sapply(row_images, is.null)]

# Stack rows vertically
if (length(row_images) > 0) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  
  # Save the grid image with FDR correction specification
  image_write(grid_image, "WCC_Heatmaps_Grid_france_fdr_training_revised.png")
  cat("Grid image with FDR correction saved as: WCC_Heatmaps_Grid_france_fdr_training.png\n")
}

############################## Country: GERMANY #####################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/germany")
getwd()

# Load required libraries
library(biwavelet)
library(wavelets)
library(Cairo)
library(magick)
library(dplyr)
library(lmtest)
library(lubridate)

# Read the dataset
data_ts <- read.csv("all_mulvar_data_germany_v2.csv", header = TRUE)
data_ts$Date <- as.Date(data_ts$Date)
str(data_ts)

# =========================
# Filter data for training period only (1995M01 to 2022M03)
# =========================
training_start <- as.Date("1995-01-01")
training_end <- as.Date("2022-03-01")

# Filter data to training period
data_ts_train <- data_ts[data_ts$Date >= training_start & data_ts$Date <= training_end, ]

# Verify the filtered dataset
cat("Original dataset period:", min(data_ts$Date), "to", max(data_ts$Date), "\n")
cat("Training dataset period:", min(data_ts_train$Date), "to", max(data_ts_train$Date), "\n")
cat("Original observations:", nrow(data_ts), "\n")
cat("Training observations:", nrow(data_ts_train), "\n")

# Define variables
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Variable labels for titles
# endogenous_labels <- c("Unemployment Rate", "Real broad EER", "Short-term IR", "Oil Price (WTI)", "CPI Inflation Rate")
# exogenous_labels <- c("logEPU", "GPRC", "USEMV", "USMPU")

endogenous_labels <- c("Unemployment Rate", "REER", "SIR", "Oil Price (WTI)", "CPI Inflation")
exogenous_labels <- c("EPU", "GPR", "USEMV", "USMPU")

# Calculate time period information for training data
n_obs_train <- nrow(data_ts_train)
start_year_train <- year(min(data_ts_train$Date))
end_year_train <- year(max(data_ts_train$Date))
time_sequence_train <- 1:n_obs_train

# =========================
# FDR Correction Function (Aguiar-Conraria & Soares 2014 approach)
# =========================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  # Extract dimensions
  n_time <- ncol(wtc_result$rsq)
  n_scale <- nrow(wtc_result$rsq)
  
  # Initialize matrices
  p_values <- matrix(NA, nrow = n_scale, ncol = n_time)
  fdr_signif <- matrix(FALSE, nrow = n_scale, ncol = n_time)
  
  # For each scale, compute p-values and apply FDR correction
  for (i in 1:n_scale) {
    # Get the significance level for this scale
    sig_level <- wtc_result$signif[i]
    
    # Skip if sig_level is NA or 0
    if (is.na(sig_level) || sig_level == 0) {
      next
    }
    
    # Compute p-values for this scale
    scale_rsq <- wtc_result$rsq[i, ]
    scale_p_values <- rep(NA, n_time)
    
    for (j in 1:n_time) {
      if (!is.na(scale_rsq[j])) {
        # Compute p-value based on the null distribution
        # Under H0, the coherence follows a specific distribution
        # We use the fact that the significance level represents the critical value
        if (scale_rsq[j] >= sig_level) {
          # For significant coherence, estimate p-value
          # Using the relationship between coherence and chi-square distribution
          scale_p_values[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        } else {
          # For non-significant coherence, use a conservative p-value
          scale_p_values[j] <- min(1, (1 - scale_rsq[j]/sig_level))
        }
      }
    }
    
    p_values[i, ] <- scale_p_values
    
    # Apply Benjamini-Hochberg FDR correction for this scale
    valid_indices <- which(!is.na(scale_p_values))
    
    if (length(valid_indices) > 0) {
      # Extract valid p-values
      valid_p <- scale_p_values[valid_indices]
      
      # Apply FDR correction
      p_adjusted <- p.adjust(valid_p, method = "BH")
      
      # Mark significant points
      sig_indices <- valid_indices[p_adjusted < alpha]
      fdr_signif[i, sig_indices] <- TRUE
    }
  }
  
  # Return results
  return(list(
    fdr_signif = fdr_signif, 
    p_values = p_values,
    alpha = alpha
  ))
}

# =========================
# Modified WTC plotting function with FDR correction
# =========================
plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label, n_obs, start_year, end_year, 
                              main_title, file_name) {
  
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  
  # Set plotting parameters
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  
  # Create base plot without original significance contours
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
       lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       # ylab = "Scale", xlab = "Period", plot.cb = TRUE,
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE,
       main = main_title, cex.main = 1.5, 
       # font.main = 2, font.lab = 2)
       font.main = 3, font.lab = 2)
  
  # Add FDR-corrected significance contours
  if (any(fdr_result$fdr_signif, na.rm = TRUE)) {
    # Create a smoothed version of the significance matrix for better contours
    sig_smooth <- fdr_result$fdr_signif
    
    # Add contours for FDR-corrected significant regions
    contour(wtc_result$t, wtc_result$period, t(sig_smooth), 
            levels = c(0.5), add = TRUE, col = "black", lwd = 2, 
            drawlabels = FALSE, method = "edge")
  }
  
  # Add grid lines
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  
  # Define x-axis labels
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks)) {
    year_labels <- year_labels[1:length(year_breaks)]
  }
  
  # Add time axis
  # axis(side = 3, at = year_breaks, labels = year_labels, font = 2)
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  
  # Add FDR correction note
  # mtext(paste0("FDR-corrected significance (α = ", fdr_result$alpha, ")"), 
  #       side = 1, line = 4, cex = 0.8, font = 3)
  
  dev.off()
}

# =========================
# Generate and Save WCC Plots with FDR Correction (Training Period Only)
# =========================
output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)

wcc_results <- list()
fdr_results <- list()
file_list <- c()

# Store all p-values for global FDR correction summary
all_p_values <- list()

# Create WCC plots for each endogenous-exogenous pair using training data only
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    y_var <- endogenous_vars[i]
    x_var <- exogenous_vars[j]
    y_label <- endogenous_labels[i]
    x_label <- exogenous_labels[j]
    
    pair_name <- paste(y_var, x_var, sep = "_x_")
    
    cat("Processing:", pair_name, "\n")
    
    # Prepare time series data using training period only
    t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
    t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
    
    # Specify the number of iterations for significance testing
    nrands <- 1000
    
    # Calculate wavelet coherence
    wtc_result <- wtc(t1, t2, nrands = nrands)
    wcc_results[[pair_name]] <- wtc_result
    
    # Apply FDR correction with alpha = 0.10
    fdr_result <- apply_fdr_correction(wtc_result, alpha = 0.10)
    fdr_results[[pair_name]] <- fdr_result
    
    # Store p-values for summary
    all_p_values[[pair_name]] <- fdr_result$p_values
    
    # Create output filename
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name), "_fdr_training.png")
    file_list <- c(file_list, out_file)
    
    # Generate individual WCC plot with FDR correction
    plot_wtc_with_fdr(wtc_result, fdr_result, y_label, x_label, 
                      n_obs_train, start_year_train, end_year_train,
                      # paste0(y_label, " vs ", x_label, " (Training: 1995-2022, FDR-corrected)"),
                      paste0(y_label, " vs ", x_label),
                      out_file)
  }
}

# =========================
# Combine All WCC Plots into One Grid Image (5x4) - Training Period with FDR
# =========================
library(magick)

# Clean file list and ensure correct order
file_list <- Filter(function(f) file.exists(f) && grepl("\\.png$", f), file_list)

# Sort files to maintain correct order (endogenous vars as rows, exogenous vars as columns)
ordered_files <- c()
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    pattern <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    matching_file <- file_list[grepl(pattern, file_list)]
    if (length(matching_file) > 0) {
      ordered_files <- c(ordered_files, matching_file[1])
    }
  }
}

# Group into rows of 4 (one row per endogenous variable)
rows <- split(ordered_files, ceiling(seq_along(ordered_files)/4))

# Build rows first
row_images <- lapply(rows, function(row_files) {
  if (length(row_files) > 0) {
    row_imgs <- image_read(row_files)
    image_append(image_join(row_imgs), stack = FALSE)
  }
})

# Remove any NULL entries
row_images <- row_images[!sapply(row_images, is.null)]

# Stack rows vertically
if (length(row_images) > 0) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  
  # Save the grid image with FDR correction specification
  image_write(grid_image, "WCC_Heatmaps_Grid_germany_fdr_training_revised.png")
  cat("Grid image with FDR correction saved as: WCC_Heatmaps_Grid_germany_fdr_training.png\n")
}

############################## Country: JAPAN #####################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/japan")
getwd()

# Load required libraries
library(biwavelet)
library(wavelets)
library(Cairo)
library(magick)
library(dplyr)
library(lmtest)
library(lubridate)

# Read the dataset
data_ts <- read.csv("all_mulvar_data_japan_v2.csv", header = TRUE)
data_ts$Date <- as.Date(data_ts$Date)
str(data_ts)

# =========================
# Filter data for training period only (1995M01 to 2022M03)
# =========================
training_start <- as.Date("1995-01-01")
training_end <- as.Date("2022-03-01")

# Filter data to training period
data_ts_train <- data_ts[data_ts$Date >= training_start & data_ts$Date <= training_end, ]

# Verify the filtered dataset
cat("Original dataset period:", min(data_ts$Date), "to", max(data_ts$Date), "\n")
cat("Training dataset period:", min(data_ts_train$Date), "to", max(data_ts_train$Date), "\n")
cat("Original observations:", nrow(data_ts), "\n")
cat("Training observations:", nrow(data_ts_train), "\n")

# Define variables
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Variable labels for titles
# endogenous_labels <- c("Unemployment Rate", "Real broad EER", "Short-term IR", "Oil Price (WTI)", "CPI Inflation Rate")
# exogenous_labels <- c("logEPU", "GPRC", "USEMV", "USMPU")

endogenous_labels <- c("Unemployment Rate", "REER", "SIR", "Oil Price (WTI)", "CPI Inflation")
exogenous_labels <- c("EPU", "GPR", "USEMV", "USMPU")

# Calculate time period information for training data
n_obs_train <- nrow(data_ts_train)
start_year_train <- year(min(data_ts_train$Date))
end_year_train <- year(max(data_ts_train$Date))
time_sequence_train <- 1:n_obs_train

# =========================
# FDR Correction Function (Aguiar-Conraria & Soares 2014 approach)
# =========================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  # Extract dimensions
  n_time <- ncol(wtc_result$rsq)
  n_scale <- nrow(wtc_result$rsq)
  
  # Initialize matrices
  p_values <- matrix(NA, nrow = n_scale, ncol = n_time)
  fdr_signif <- matrix(FALSE, nrow = n_scale, ncol = n_time)
  
  # For each scale, compute p-values and apply FDR correction
  for (i in 1:n_scale) {
    # Get the significance level for this scale
    sig_level <- wtc_result$signif[i]
    
    # Skip if sig_level is NA or 0
    if (is.na(sig_level) || sig_level == 0) {
      next
    }
    
    # Compute p-values for this scale
    scale_rsq <- wtc_result$rsq[i, ]
    scale_p_values <- rep(NA, n_time)
    
    for (j in 1:n_time) {
      if (!is.na(scale_rsq[j])) {
        # Compute p-value based on the null distribution
        # Under H0, the coherence follows a specific distribution
        # We use the fact that the significance level represents the critical value
        if (scale_rsq[j] >= sig_level) {
          # For significant coherence, estimate p-value
          # Using the relationship between coherence and chi-square distribution
          scale_p_values[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        } else {
          # For non-significant coherence, use a conservative p-value
          scale_p_values[j] <- min(1, (1 - scale_rsq[j]/sig_level))
        }
      }
    }
    
    p_values[i, ] <- scale_p_values
    
    # Apply Benjamini-Hochberg FDR correction for this scale
    valid_indices <- which(!is.na(scale_p_values))
    
    if (length(valid_indices) > 0) {
      # Extract valid p-values
      valid_p <- scale_p_values[valid_indices]
      
      # Apply FDR correction
      p_adjusted <- p.adjust(valid_p, method = "BH")
      
      # Mark significant points
      sig_indices <- valid_indices[p_adjusted < alpha]
      fdr_signif[i, sig_indices] <- TRUE
    }
  }
  
  # Return results
  return(list(
    fdr_signif = fdr_signif, 
    p_values = p_values,
    alpha = alpha
  ))
}

# =========================
# Modified WTC plotting function with FDR correction
# =========================
plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label, n_obs, start_year, end_year, 
                              main_title, file_name) {
  
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  
  # Set plotting parameters
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  
  # Create base plot without original significance contours
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
       lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       # ylab = "Scale", xlab = "Period", plot.cb = TRUE, 
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE,
       main = main_title, cex.main = 1.5,
       font.main = 3, font.lab = 2)
       # font.main = 2, font.lab = 2)
  
  # Add FDR-corrected significance contours
  if (any(fdr_result$fdr_signif, na.rm = TRUE)) {
    # Create a smoothed version of the significance matrix for better contours
    sig_smooth <- fdr_result$fdr_signif
    
    # Add contours for FDR-corrected significant regions
    contour(wtc_result$t, wtc_result$period, t(sig_smooth), 
            levels = c(0.5), add = TRUE, col = "black", lwd = 2, 
            drawlabels = FALSE, method = "edge")
  }
  
  # Add grid lines
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  
  # Define x-axis labels
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks)) {
    year_labels <- year_labels[1:length(year_breaks)]
  }
  
  # Add time axis
  # axis(side = 3, at = year_breaks, labels = year_labels, font = 2)
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  
  # Add FDR correction note
  # mtext(paste0("FDR-corrected significance (α = ", fdr_result$alpha, ")"), 
  #       side = 1, line = 4, cex = 0.8, font = 3)
  
  dev.off()
}

# =========================
# Generate and Save WCC Plots with FDR Correction (Training Period Only)
# =========================
output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)

wcc_results <- list()
fdr_results <- list()
file_list <- c()

# Store all p-values for global FDR correction summary
all_p_values <- list()

# Create WCC plots for each endogenous-exogenous pair using training data only
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    y_var <- endogenous_vars[i]
    x_var <- exogenous_vars[j]
    y_label <- endogenous_labels[i]
    x_label <- exogenous_labels[j]
    
    pair_name <- paste(y_var, x_var, sep = "_x_")
    
    cat("Processing:", pair_name, "\n")
    
    # Prepare time series data using training period only
    t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
    t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
    
    # Specify the number of iterations for significance testing
    nrands <- 1000
    
    # Calculate wavelet coherence
    wtc_result <- wtc(t1, t2, nrands = nrands)
    wcc_results[[pair_name]] <- wtc_result
    
    # Apply FDR correction with alpha = 0.10
    fdr_result <- apply_fdr_correction(wtc_result, alpha = 0.10)
    fdr_results[[pair_name]] <- fdr_result
    
    # Store p-values for summary
    all_p_values[[pair_name]] <- fdr_result$p_values
    
    # Create output filename
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name), "_fdr_training.png")
    file_list <- c(file_list, out_file)
    
    # Generate individual WCC plot with FDR correction
    plot_wtc_with_fdr(wtc_result, fdr_result, y_label, x_label, 
                      n_obs_train, start_year_train, end_year_train,
                      # paste0(y_label, " vs ", x_label, " (Training: 1995-2022, FDR-corrected)"),
                      paste0(y_label, " vs ", x_label),
                      out_file)
  }
}

# =========================
# Combine All WCC Plots into One Grid Image (5x4) - Training Period with FDR
# =========================
library(magick)

# Clean file list and ensure correct order
file_list <- Filter(function(f) file.exists(f) && grepl("\\.png$", f), file_list)

# Sort files to maintain correct order (endogenous vars as rows, exogenous vars as columns)
ordered_files <- c()
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    pattern <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    matching_file <- file_list[grepl(pattern, file_list)]
    if (length(matching_file) > 0) {
      ordered_files <- c(ordered_files, matching_file[1])
    }
  }
}

# Group into rows of 4 (one row per endogenous variable)
rows <- split(ordered_files, ceiling(seq_along(ordered_files)/4))

# Build rows first
row_images <- lapply(rows, function(row_files) {
  if (length(row_files) > 0) {
    row_imgs <- image_read(row_files)
    image_append(image_join(row_imgs), stack = FALSE)
  }
})

# Remove any NULL entries
row_images <- row_images[!sapply(row_images, is.null)]

# Stack rows vertically
if (length(row_images) > 0) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  
  # Save the grid image with FDR correction specification
  image_write(grid_image, "WCC_Heatmaps_Grid_japan_fdr_training_revised.png")
  cat("Grid image with FDR correction saved as: WCC_Heatmaps_Grid_japan_fdr_training.png\n")
}

############################## Country: UK #####################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/uk")
getwd()

# Load required libraries
library(biwavelet)
library(wavelets)
library(Cairo)
library(magick)
library(dplyr)
library(lmtest)
library(lubridate)

# Read the dataset
data_ts <- read.csv("all_mulvar_data_uk_v2.csv", header = TRUE)
data_ts$Date <- as.Date(data_ts$Date)
str(data_ts)

# =========================
# Filter data for training period only (1995M01 to 2022M03)
# =========================
training_start <- as.Date("1995-01-01")
training_end <- as.Date("2022-03-01")

# Filter data to training period
data_ts_train <- data_ts[data_ts$Date >= training_start & data_ts$Date <= training_end, ]

# Verify the filtered dataset
cat("Original dataset period:", min(data_ts$Date), "to", max(data_ts$Date), "\n")
cat("Training dataset period:", min(data_ts_train$Date), "to", max(data_ts_train$Date), "\n")
cat("Original observations:", nrow(data_ts), "\n")
cat("Training observations:", nrow(data_ts_train), "\n")

# Define variables
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Variable labels for titles

# endogenous_labels <- c("Unemployment Rate", "Real broad EER", "Short-term IR", "Oil Price (WTI)", "CPI Inflation Rate")
# exogenous_labels <- c("logEPU", "GPRC", "USEMV", "USMPU")

endogenous_labels <- c("Unemployment Rate", "REER", "SIR", "Oil Price (WTI)", "CPI Inflation")
exogenous_labels <- c("EPU", "GPR", "USEMV", "USMPU")

# Calculate time period information for training data
n_obs_train <- nrow(data_ts_train)
start_year_train <- year(min(data_ts_train$Date))
end_year_train <- year(max(data_ts_train$Date))
time_sequence_train <- 1:n_obs_train

# =========================
# FDR Correction Function (Aguiar-Conraria & Soares 2014 approach)
# =========================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  # Extract dimensions
  n_time <- ncol(wtc_result$rsq)
  n_scale <- nrow(wtc_result$rsq)
  
  # Initialize matrices
  p_values <- matrix(NA, nrow = n_scale, ncol = n_time)
  fdr_signif <- matrix(FALSE, nrow = n_scale, ncol = n_time)
  
  # For each scale, compute p-values and apply FDR correction
  for (i in 1:n_scale) {
    # Get the significance level for this scale
    sig_level <- wtc_result$signif[i]
    
    # Skip if sig_level is NA or 0
    if (is.na(sig_level) || sig_level == 0) {
      next
    }
    
    # Compute p-values for this scale
    scale_rsq <- wtc_result$rsq[i, ]
    scale_p_values <- rep(NA, n_time)
    
    for (j in 1:n_time) {
      if (!is.na(scale_rsq[j])) {
        # Compute p-value based on the null distribution
        # Under H0, the coherence follows a specific distribution
        # We use the fact that the significance level represents the critical value
        if (scale_rsq[j] >= sig_level) {
          # For significant coherence, estimate p-value
          # Using the relationship between coherence and chi-square distribution
          scale_p_values[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        } else {
          # For non-significant coherence, use a conservative p-value
          scale_p_values[j] <- min(1, (1 - scale_rsq[j]/sig_level))
        }
      }
    }
    
    p_values[i, ] <- scale_p_values
    
    # Apply Benjamini-Hochberg FDR correction for this scale
    valid_indices <- which(!is.na(scale_p_values))
    
    if (length(valid_indices) > 0) {
      # Extract valid p-values
      valid_p <- scale_p_values[valid_indices]
      
      # Apply FDR correction
      p_adjusted <- p.adjust(valid_p, method = "BH")
      
      # Mark significant points
      sig_indices <- valid_indices[p_adjusted < alpha]
      fdr_signif[i, sig_indices] <- TRUE
    }
  }
  
  # Return results
  return(list(
    fdr_signif = fdr_signif, 
    p_values = p_values,
    alpha = alpha
  ))
}

# =========================
# Modified WTC plotting function with FDR correction
# =========================
plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label, n_obs, start_year, end_year, 
                              main_title, file_name) {
  
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  
  # Set plotting parameters
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  
  # Create base plot without original significance contours
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
       lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       # ylab = "Scale", xlab = "Period", plot.cb = TRUE,
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE,
       main = main_title, cex.main = 1.5,
       font.main = 3, font.lab = 2)
       # font.main = 2, font.lab = 2)
  
  # Add FDR-corrected significance contours
  if (any(fdr_result$fdr_signif, na.rm = TRUE)) {
    # Create a smoothed version of the significance matrix for better contours
    sig_smooth <- fdr_result$fdr_signif
    
    # Add contours for FDR-corrected significant regions
    contour(wtc_result$t, wtc_result$period, t(sig_smooth), 
            levels = c(0.5), add = TRUE, col = "black", lwd = 2, 
            drawlabels = FALSE, method = "edge")
  }
  
  # Add grid lines
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  
  # Define x-axis labels
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks)) {
    year_labels <- year_labels[1:length(year_breaks)]
  }
  
  # Add time axis
  # axis(side = 3, at = year_breaks, labels = year_labels, font = 2)
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  
  # Add FDR correction note
  # mtext(paste0("FDR-corrected significance (α = ", fdr_result$alpha, ")"), 
  #       side = 1, line = 4, cex = 0.8, font = 3)
  
  dev.off()
}

# =========================
# Generate and Save WCC Plots with FDR Correction (Training Period Only)
# =========================
output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)

wcc_results <- list()
fdr_results <- list()
file_list <- c()

# Store all p-values for global FDR correction summary
all_p_values <- list()

# Create WCC plots for each endogenous-exogenous pair using training data only
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    y_var <- endogenous_vars[i]
    x_var <- exogenous_vars[j]
    y_label <- endogenous_labels[i]
    x_label <- exogenous_labels[j]
    
    pair_name <- paste(y_var, x_var, sep = "_x_")
    
    cat("Processing:", pair_name, "\n")
    
    # Prepare time series data using training period only
    t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
    t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
    
    # Specify the number of iterations for significance testing
    nrands <- 1000
    
    # Calculate wavelet coherence
    wtc_result <- wtc(t1, t2, nrands = nrands)
    wcc_results[[pair_name]] <- wtc_result
    
    # Apply FDR correction with alpha = 0.10
    fdr_result <- apply_fdr_correction(wtc_result, alpha = 0.10)
    fdr_results[[pair_name]] <- fdr_result
    
    # Store p-values for summary
    all_p_values[[pair_name]] <- fdr_result$p_values
    
    # Create output filename
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name), "_fdr_training.png")
    file_list <- c(file_list, out_file)
    
    # Generate individual WCC plot with FDR correction
    plot_wtc_with_fdr(wtc_result, fdr_result, y_label, x_label, 
                      n_obs_train, start_year_train, end_year_train,
                      paste0(y_label, " vs ", x_label),
                      # paste0(y_label, " vs ", x_label, " (Training: 1995-2022, FDR-corrected)"),
                      out_file)
  }
}

# =========================
# Combine All WCC Plots into One Grid Image (5x4) - Training Period with FDR
# =========================
library(magick)

# Clean file list and ensure correct order
file_list <- Filter(function(f) file.exists(f) && grepl("\\.png$", f), file_list)

# Sort files to maintain correct order (endogenous vars as rows, exogenous vars as columns)
ordered_files <- c()
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    pattern <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    matching_file <- file_list[grepl(pattern, file_list)]
    if (length(matching_file) > 0) {
      ordered_files <- c(ordered_files, matching_file[1])
    }
  }
}

# Group into rows of 4 (one row per endogenous variable)
rows <- split(ordered_files, ceiling(seq_along(ordered_files)/4))

# Build rows first
row_images <- lapply(rows, function(row_files) {
  if (length(row_files) > 0) {
    row_imgs <- image_read(row_files)
    image_append(image_join(row_imgs), stack = FALSE)
  }
})

# Remove any NULL entries
row_images <- row_images[!sapply(row_images, is.null)]

# Stack rows vertically
if (length(row_images) > 0) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  
  # Save the grid image with FDR correction specification
  image_write(grid_image, "WCC_Heatmaps_Grid_uk_fdr_training_revised.png")
  cat("Grid image with FDR correction saved as: WCC_Heatmaps_Grid_uk_fdr_training.png\n")
}

############################## Country: ITALY #####################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/italy")
getwd()

# Load required libraries
library(biwavelet)
library(wavelets)
library(Cairo)
library(magick)
library(dplyr)
library(lmtest)
library(lubridate)

# Read the dataset
data_ts <- read.csv("all_mulvar_data_italy_v2.csv", header = TRUE)
data_ts$Date <- as.Date(data_ts$Date)
str(data_ts)

# =========================
# Filter data for training period only (1995M01 to 2022M03)
# =========================
training_start <- as.Date("1995-01-01")
training_end <- as.Date("2022-03-01")

# Filter data to training period
data_ts_train <- data_ts[data_ts$Date >= training_start & data_ts$Date <= training_end, ]

# Verify the filtered dataset
cat("Original dataset period:", min(data_ts$Date), "to", max(data_ts$Date), "\n")
cat("Training dataset period:", min(data_ts_train$Date), "to", max(data_ts_train$Date), "\n")
cat("Original observations:", nrow(data_ts), "\n")
cat("Training observations:", nrow(data_ts_train), "\n")

# Define variables
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars <- c("logEPU", "GPRC", "USEMV", "USMPU")

# Variable labels for titles
# endogenous_labels <- c("Unemployment Rate", "Real broad EER", "Short-term IR", "Oil Price (WTI)", "CPI Inflation Rate")
# exogenous_labels <- c("logEPU", "GPRC", "USEMV", "USMPU")

endogenous_labels <- c("Unemployment Rate", "REER", "SIR", "Oil Price (WTI)", "CPI Inflation")
exogenous_labels <- c("EPU", "GPR", "USEMV", "USMPU")

# Calculate time period information for training data
n_obs_train <- nrow(data_ts_train)
start_year_train <- year(min(data_ts_train$Date))
end_year_train <- year(max(data_ts_train$Date))
time_sequence_train <- 1:n_obs_train

# =========================
# FDR Correction Function (Aguiar-Conraria & Soares 2014 approach)
# =========================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  # Extract dimensions
  n_time <- ncol(wtc_result$rsq)
  n_scale <- nrow(wtc_result$rsq)
  
  # Initialize matrices
  p_values <- matrix(NA, nrow = n_scale, ncol = n_time)
  fdr_signif <- matrix(FALSE, nrow = n_scale, ncol = n_time)
  
  # For each scale, compute p-values and apply FDR correction
  for (i in 1:n_scale) {
    # Get the significance level for this scale
    sig_level <- wtc_result$signif[i]
    
    # Skip if sig_level is NA or 0
    if (is.na(sig_level) || sig_level == 0) {
      next
    }
    
    # Compute p-values for this scale
    scale_rsq <- wtc_result$rsq[i, ]
    scale_p_values <- rep(NA, n_time)
    
    for (j in 1:n_time) {
      if (!is.na(scale_rsq[j])) {
        # Compute p-value based on the null distribution
        # Under H0, the coherence follows a specific distribution
        # We use the fact that the significance level represents the critical value
        if (scale_rsq[j] >= sig_level) {
          # For significant coherence, estimate p-value
          # Using the relationship between coherence and chi-square distribution
          scale_p_values[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        } else {
          # For non-significant coherence, use a conservative p-value
          scale_p_values[j] <- min(1, (1 - scale_rsq[j]/sig_level))
        }
      }
    }
    
    p_values[i, ] <- scale_p_values
    
    # Apply Benjamini-Hochberg FDR correction for this scale
    valid_indices <- which(!is.na(scale_p_values))
    
    if (length(valid_indices) > 0) {
      # Extract valid p-values
      valid_p <- scale_p_values[valid_indices]
      
      # Apply FDR correction
      p_adjusted <- p.adjust(valid_p, method = "BH")
      
      # Mark significant points
      sig_indices <- valid_indices[p_adjusted < alpha]
      fdr_signif[i, sig_indices] <- TRUE
    }
  }
  
  # Return results
  return(list(
    fdr_signif = fdr_signif, 
    p_values = p_values,
    alpha = alpha
  ))
}

# =========================
# Modified WTC plotting function with FDR correction
# =========================
plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label, n_obs, start_year, end_year, 
                              main_title, file_name) {
  
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  
  # Set plotting parameters
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  
  # Create base plot without original significance contours
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
       lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       # ylab = "Scale", xlab = "Period", plot.cb = TRUE,
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE,
       main = main_title, cex.main = 1.5,
       font.main = 3, font.lab = 2)
       # font.main = 2, font.lab = 2)
  
  # Add FDR-corrected significance contours
  if (any(fdr_result$fdr_signif, na.rm = TRUE)) {
    # Create a smoothed version of the significance matrix for better contours
    sig_smooth <- fdr_result$fdr_signif
    
    # Add contours for FDR-corrected significant regions
    contour(wtc_result$t, wtc_result$period, t(sig_smooth), 
            levels = c(0.5), add = TRUE, col = "black", lwd = 2, 
            drawlabels = FALSE, method = "edge")
  }
  
  # Add grid lines
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  
  # Define x-axis labels
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks)) {
    year_labels <- year_labels[1:length(year_breaks)]
  }
  
  # Add time axis
  # axis(side = 3, at = year_breaks, labels = year_labels, font = 2)
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  
  # Add FDR correction note
  # mtext(paste0("FDR-corrected significance (α = ", fdr_result$alpha, ")"), 
  #       side = 1, line = 4, cex = 0.8, font = 3)
  
  dev.off()
}

# =========================
# Generate and Save WCC Plots with FDR Correction (Training Period Only)
# =========================
output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)

wcc_results <- list()
fdr_results <- list()
file_list <- c()

# Store all p-values for global FDR correction summary
all_p_values <- list()

# Create WCC plots for each endogenous-exogenous pair using training data only
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    y_var <- endogenous_vars[i]
    x_var <- exogenous_vars[j]
    y_label <- endogenous_labels[i]
    x_label <- exogenous_labels[j]
    
    pair_name <- paste(y_var, x_var, sep = "_x_")
    
    cat("Processing:", pair_name, "\n")
    
    # Prepare time series data using training period only
    t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
    t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
    
    # Specify the number of iterations for significance testing
    nrands <- 1000
    
    # Calculate wavelet coherence
    wtc_result <- wtc(t1, t2, nrands = nrands)
    wcc_results[[pair_name]] <- wtc_result
    
    # Apply FDR correction with alpha = 0.10
    fdr_result <- apply_fdr_correction(wtc_result, alpha = 0.10)
    fdr_results[[pair_name]] <- fdr_result
    
    # Store p-values for summary
    all_p_values[[pair_name]] <- fdr_result$p_values
    
    # Create output filename
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name), "_fdr_training.png")
    file_list <- c(file_list, out_file)
    
    # Generate individual WCC plot with FDR correction
    plot_wtc_with_fdr(wtc_result, fdr_result, y_label, x_label, 
                      n_obs_train, start_year_train, end_year_train,
                      paste0(y_label, " vs ", x_label),
                      # paste0(y_label, " vs ", x_label, " (Training: 1995-2022, FDR-corrected)"),
                      out_file)
  }
}

# =========================
# Combine All WCC Plots into One Grid Image (5x4) - Training Period with FDR
# =========================
library(magick)

# Clean file list and ensure correct order
file_list <- Filter(function(f) file.exists(f) && grepl("\\.png$", f), file_list)

# Sort files to maintain correct order (endogenous vars as rows, exogenous vars as columns)
ordered_files <- c()
for (i in 1:length(endogenous_vars)) {
  for (j in 1:length(exogenous_vars)) {
    pattern <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    matching_file <- file_list[grepl(pattern, file_list)]
    if (length(matching_file) > 0) {
      ordered_files <- c(ordered_files, matching_file[1])
    }
  }
}

# Group into rows of 4 (one row per endogenous variable)
rows <- split(ordered_files, ceiling(seq_along(ordered_files)/4))

# Build rows first
row_images <- lapply(rows, function(row_files) {
  if (length(row_files) > 0) {
    row_imgs <- image_read(row_files)
    image_append(image_join(row_imgs), stack = FALSE)
  }
})

# Remove any NULL entries
row_images <- row_images[!sapply(row_images, is.null)]

# Stack rows vertically
if (length(row_images) > 0) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  
  # Save the grid image with FDR correction specification
  image_write(grid_image, "WCC_Heatmaps_Grid_italy_fdr_training_revised.png")
  cat("Grid image with FDR correction saved as: WCC_Heatmaps_Grid_italy_fdr_training.png\n")
}
####################### End of Code: G7 Countries ###################


