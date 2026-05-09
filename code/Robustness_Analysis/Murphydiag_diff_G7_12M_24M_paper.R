############################### Murphydiagram: Canada: 12M and 24M #############################

# install.packages('murphydiagram')
# install.packages('gridExtra')
# install.packages('grid')
# install.packages('png')

library(murphydiagram)
library(gridExtra)
library(grid)
library(png)

setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()
# Variable meta info
var_list <- list(
  UR   = "Unemployment Rate",
  REER = "REER",
  SIR  = "SIR",
  OP   = "Oil Price (WTI)",
  CPI  = "CPI Inflation"
)

# Helper to create a single Murphy diagram plot as a grob
get_md_grob <- function(data, comp, var_label, horizon, show_ylab=FALSE) {
  comp_label <- ifelse(comp == "VARx", "VARx", "CatBoostx")
  main_title <- sprintf("SZBVARx vs %s: %s (%s)", comp_label, var_label, horizon)
  # main_title <- sprintf("SZBVARx vs %s (Canada): %s (%s)", comp_label, var_label, horizon)
  temp_file <- tempfile(fileext = ".png")
  png(temp_file, width = 600, height = 400, res = 120)
  murphydiagram_diff(
    data$SZBVAR, data[[comp]], data$test_data,
    lag_truncate = 1, conf_level = 0.90
  )
  title(main = main_title, cex.main = 0.8)
  if (show_ylab) {
    mtext("Extremal score differences", side = 2, line = 2.5, cex = 0.9, font = 2) # bold
  }
  dev.off()
  img <- png::readPNG(temp_file)
  grid::rasterGrob(img, interpolate = TRUE)
}

# Prepare the grobs in the requested order
all_grobs <- list()
for (i in seq_along(var_list)) {
  var <- names(var_list)[i]
  var_label <- var_list[[var]]
  data_12M <- read.csv(sprintf("canada_GR_results_baselines_%s_12M.csv", var), header = TRUE)
  data_24M <- read.csv(sprintf("canada_GR_results_baselines_%s_24M.csv", var), header = TRUE)
  # Only first column gets ylab
  show_ylab <- c(TRUE, FALSE, FALSE, FALSE)
  # 1st: SZBVAR vs VAR (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "VARx", var_label, "12-month", show_ylab=show_ylab[1])
  # 2nd: SZBVAR vs CatBoost (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "CatBoost", var_label, "12-month", show_ylab=show_ylab[2])
  # 3rd: SZBVAR vs VAR (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "VARx", var_label, "24-month", show_ylab=show_ylab[3])
  # 4th: SZBVAR vs CatBoost (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "CatBoost", var_label, "24-month", show_ylab=show_ylab[4])
}

# Custom legend grob (centered at the bottom of the chart)
legend_grob <- grobTree(
  # Black line (Extremal score difference)
  linesGrob(x = unit(c(0.30, 0.40), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "black", lwd = 2)),
  textGrob("Extremal score difference", x = unit(0.415, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9)),
  # Grey line (90% CI)
  linesGrob(x = unit(c(0.60, 0.70), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "grey50", lwd = 6)),
  textGrob("90% CI (all charts)", x = unit(0.715, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9))
)

# Arrange in 5x4 grid as specified, add legend and universal x-axis label
# png("MD_canada_overleaf_12M_24M_V2.png", width = 2400, height = 2200, res = 120)
png("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/MD_canada_12M_24M_paper.png", width = 2400, height = 2200, res = 120)
grid.arrange(
  arrangeGrob(grobs = all_grobs, nrow = 5, ncol = 4),
  # xlab_grob,
  legend_grob,
  heights = c(20, 1.2, 1.2)
)
dev.off()
cat("One big stacked Murphy diagram grid (5x4) with all requested features has been generated and saved as MD_Stacked_AllVars_5x4.png\n")

############################### Murphydiagram: USA: 12M and 24M ##############################
setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()
# Variable meta info
var_list <- list(
  UR   = "Unemployment Rate",
  REER = "REER",
  SIR  = "SIR",
  OP   = "Oil Price (WTI)",
  CPI  = "CPI Inflation"
)

# Helper to create a single Murphy diagram plot as a grob
get_md_grob <- function(data, comp, var_label, horizon, show_ylab=FALSE) {
  comp_label <- ifelse(comp == "VARx", "VARx", "CatBoostx")
  # main_title <- sprintf("SZBVARx vs %s (US): %s (%s)", comp_label, var_label, horizon)
  main_title <- sprintf("SZBVARx vs %s: %s (%s)", comp_label, var_label, horizon)
  temp_file <- tempfile(fileext = ".png")
  png(temp_file, width = 600, height = 400, res = 120)
  murphydiagram_diff(
    data$SZBVAR, data[[comp]], data$test_data,
    lag_truncate = 1, conf_level = 0.90
  )
  title(main = main_title, cex.main = 0.8)
  if (show_ylab) {
    mtext("Extremal score differences", side = 2, line = 2.5, cex = 0.9, font = 2) # bold
  }
  dev.off()
  img <- png::readPNG(temp_file)
  grid::rasterGrob(img, interpolate = TRUE)
}

# Prepare the grobs in the requested order
all_grobs <- list()
for (i in seq_along(var_list)) {
  var <- names(var_list)[i]
  var_label <- var_list[[var]]
  data_12M <- read.csv(sprintf("usa_GR_results_baselines_%s_12M.csv", var), header = TRUE)
  data_24M <- read.csv(sprintf("usa_GR_results_baselines_%s_24M.csv", var), header = TRUE)
  # Only first column gets ylab
  show_ylab <- c(TRUE, FALSE, FALSE, FALSE)
  # 1st: SZBVAR vs VAR (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "VARx", var_label, "12-month", show_ylab=show_ylab[1])
  # 2nd: SZBVAR vs CatBoost (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "CatBoost", var_label, "12-month", show_ylab=show_ylab[2])
  # 3rd: SZBVAR vs VAR (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "VARx", var_label, "24-month", show_ylab=show_ylab[3])
  # 4th: SZBVAR vs CatBoost (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "CatBoost", var_label, "24-month", show_ylab=show_ylab[4])
}


# Custom legend grob (centered at the bottom of the chart)
legend_grob <- grobTree(
  # Black line (Extremal score difference)
  linesGrob(x = unit(c(0.30, 0.40), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "black", lwd = 2)),
  textGrob("Extremal score difference", x = unit(0.415, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9)),
  # Grey line (90% CI)
  linesGrob(x = unit(c(0.60, 0.70), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "grey50", lwd = 6)),
  textGrob("90% CI (all charts)", x = unit(0.715, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9))
)

# Arrange in 5x4 grid as specified, add legend and universal x-axis label
# png("MD_usa_overleaf_12M_24M_V2.png", width = 2400, height = 2200, res = 120)
png("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/MD_usa_12M_24M_paper.png", width = 2400, height = 2200, res = 120)
grid.arrange(
  arrangeGrob(grobs = all_grobs, nrow = 5, ncol = 4),
  # xlab_grob,
  legend_grob,
  heights = c(20, 1.2, 1.2)
)
dev.off()
cat("One big stacked Murphy diagram grid (5x4) with all requested features has been generated and saved as MD_Stacked_AllVars_5x4.png\n")

############################### Murphydiagram: FRANCE: 12M and 24M ##############################
setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()
# Variable meta info
var_list <- list(
  UR   = "Unemployment Rate",
  REER = "REER",
  SIR  = "SIR",
  OP   = "Oil Price (WTI)",
  CPI  = "CPI Inflation"
)

# Helper to create a single Murphy diagram plot as a grob
get_md_grob <- function(data, comp, var_label, horizon, show_ylab=FALSE) {
  comp_label <- ifelse(comp == "VARx", "VARx", "CatBoostx")
  main_title <- sprintf("SZBVARx vs %s: %s (%s)", comp_label, var_label, horizon)
  temp_file <- tempfile(fileext = ".png")
  png(temp_file, width = 600, height = 400, res = 120)
  murphydiagram_diff(
    data$SZBVAR, data[[comp]], data$test_data,
    lag_truncate = 1, conf_level = 0.90
  )
  title(main = main_title, cex.main = 0.8)
  if (show_ylab) {
    mtext("Extremal score differences", side = 2, line = 2.5, cex = 0.9, font = 2) # bold
  }
  dev.off()
  img <- png::readPNG(temp_file)
  grid::rasterGrob(img, interpolate = TRUE)
}

# Prepare the grobs in the requested order
all_grobs <- list()
for (i in seq_along(var_list)) {
  var <- names(var_list)[i]
  var_label <- var_list[[var]]
  data_12M <- read.csv(sprintf("france_GR_results_baselines_%s_12M.csv", var), header = TRUE)
  data_24M <- read.csv(sprintf("france_GR_results_baselines_%s_24M.csv", var), header = TRUE)
  # Only first column gets ylab
  show_ylab <- c(TRUE, FALSE, FALSE, FALSE)
  # 1st: SZBVAR vs VAR (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "VARx", var_label, "12-month", show_ylab=show_ylab[1])
  # 2nd: SZBVAR vs CatBoost (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "CatBoost", var_label, "12-month", show_ylab=show_ylab[2])
  # 3rd: SZBVAR vs VAR (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "VARx", var_label, "24-month", show_ylab=show_ylab[3])
  # 4th: SZBVAR vs CatBoost (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "CatBoost", var_label, "24-month", show_ylab=show_ylab[4])
}


# Custom legend grob (centered at the bottom of the chart)
legend_grob <- grobTree(
  # Black line (Extremal score difference)
  linesGrob(x = unit(c(0.30, 0.40), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "black", lwd = 2)),
  textGrob("Extremal score difference", x = unit(0.415, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9)),
  # Grey line (90% CI)
  linesGrob(x = unit(c(0.60, 0.70), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "grey50", lwd = 6)),
  textGrob("90% CI (all charts)", x = unit(0.715, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9))
)

# Arrange in 5x4 grid as specified, add legend and universal x-axis label
png("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/MD_france_12M_24M_paper.png", width = 2400, height = 2200, res = 120)
grid.arrange(
  arrangeGrob(grobs = all_grobs, nrow = 5, ncol = 4),
  # xlab_grob,
  legend_grob,
  heights = c(20, 1.2, 1.2)
)
dev.off()
cat("One big stacked Murphy diagram grid (5x4) with all requested features has been generated and saved as MD_Stacked_AllVars_5x4.png\n")

############################### Murphydiagram: GERMANY: 12M and 24M ##############################
setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()

# Variable meta info
var_list <- list(
  UR   = "Unemployment Rate",
  REER = "REER",
  SIR  = "SIR",
  OP   = "Oil Price (WTI)",
  CPI  = "CPI Inflation"
)

# Helper to create a single Murphy diagram plot as a grob
get_md_grob <- function(data, comp, var_label, horizon, show_ylab=FALSE) {
  comp_label <- ifelse(comp == "VARx", "VARx", "CatBoostx")
  main_title <- sprintf("SZBVARx vs %s: %s (%s)", comp_label, var_label, horizon)
  temp_file <- tempfile(fileext = ".png")
  png(temp_file, width = 600, height = 400, res = 120)
  murphydiagram_diff(
    data$SZBVAR, data[[comp]], data$test_data,
    lag_truncate = 1, conf_level = 0.90
  )
  title(main = main_title, cex.main = 0.8)
  if (show_ylab) {
    mtext("Extremal score differences", side = 2, line = 2.5, cex = 0.9, font = 2) # bold
  }
  dev.off()
  img <- png::readPNG(temp_file)
  grid::rasterGrob(img, interpolate = TRUE)
}

# Prepare the grobs in the requested order
all_grobs <- list()
for (i in seq_along(var_list)) {
  var <- names(var_list)[i]
  var_label <- var_list[[var]]
  data_12M <- read.csv(sprintf("germany_GR_results_baselines_%s_12M.csv", var), header = TRUE)
  data_24M <- read.csv(sprintf("germany_GR_results_baselines_%s_24M.csv", var), header = TRUE)
  # Only first column gets ylab
  show_ylab <- c(TRUE, FALSE, FALSE, FALSE)
  # 1st: SZBVAR vs VAR (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "VARx", var_label, "12-month", show_ylab=show_ylab[1])
  # 2nd: SZBVAR vs CatBoost (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "CatBoost", var_label, "12-month", show_ylab=show_ylab[2])
  # 3rd: SZBVAR vs VAR (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "VARx", var_label, "24-month", show_ylab=show_ylab[3])
  # 4th: SZBVAR vs CatBoost (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "CatBoost", var_label, "24-month", show_ylab=show_ylab[4])
}


# Custom legend grob (centered at the bottom of the chart)
legend_grob <- grobTree(
  # Black line (Extremal score difference)
  linesGrob(x = unit(c(0.30, 0.40), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "black", lwd = 2)),
  textGrob("Extremal score difference", x = unit(0.415, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9)),
  # Grey line (90% CI)
  linesGrob(x = unit(c(0.60, 0.70), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "grey50", lwd = 6)),
  textGrob("90% CI (all charts)", x = unit(0.715, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9))
)

# Arrange in 5x4 grid as specified, add legend and universal x-axis label
png("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/MD_germany_12M_24M_paper.png", width = 2400, height = 2200, res = 120)
grid.arrange(
  arrangeGrob(grobs = all_grobs, nrow = 5, ncol = 4),
  # xlab_grob,
  legend_grob,
  heights = c(20, 1.2, 1.2)
)
dev.off()
cat("One big stacked Murphy diagram grid (5x4) with all requested features has been generated and saved as MD_Stacked_AllVars_5x4.png\n")

############################### Murphydiagram: JAPAN: 12M and 24M ##############################
setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()

# Variable meta info
var_list <- list(
  UR   = "Unemployment Rate",
  REER = "REER",
  SIR  = "SIR",
  OP   = "Oil Price (WTI)",
  CPI  = "CPI Inflation"
)

# Helper to create a single Murphy diagram plot as a grob
get_md_grob <- function(data, comp, var_label, horizon, show_ylab=FALSE) {
  comp_label <- ifelse(comp == "VARx", "VARx", "CatBoostx")
  main_title <- sprintf("SZBVARx vs %s: %s (%s)", comp_label, var_label, horizon)
  temp_file <- tempfile(fileext = ".png")
  png(temp_file, width = 600, height = 400, res = 120)
  murphydiagram_diff(
    data$SZBVAR, data[[comp]], data$test_data,
    lag_truncate = 1, conf_level = 0.90
  )
  title(main = main_title, cex.main = 0.8)
  if (show_ylab) {
    mtext("Extremal score differences", side = 2, line = 2.5, cex = 0.9, font = 2) # bold
  }
  dev.off()
  img <- png::readPNG(temp_file)
  grid::rasterGrob(img, interpolate = TRUE)
}

# Prepare the grobs in the requested order
all_grobs <- list()
for (i in seq_along(var_list)) {
  var <- names(var_list)[i]
  var_label <- var_list[[var]]
  data_12M <- read.csv(sprintf("japan_GR_results_baselines_%s_12M.csv", var), header = TRUE)
  data_24M <- read.csv(sprintf("japan_GR_results_baselines_%s_24M.csv", var), header = TRUE)
  # Only first column gets ylab
  show_ylab <- c(TRUE, FALSE, FALSE, FALSE)
  # 1st: SZBVAR vs VAR (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "VARx", var_label, "12-month", show_ylab=show_ylab[1])
  # 2nd: SZBVAR vs CatBoost (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "CatBoost", var_label, "12-month", show_ylab=show_ylab[2])
  # 3rd: SZBVAR vs VAR (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "VARx", var_label, "24-month", show_ylab=show_ylab[3])
  # 4th: SZBVAR vs CatBoost (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "CatBoost", var_label, "24-month", show_ylab=show_ylab[4])
}


# Custom legend grob (centered at the bottom of the chart)
legend_grob <- grobTree(
  # Black line (Extremal score difference)
  linesGrob(x = unit(c(0.30, 0.40), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "black", lwd = 2)),
  textGrob("Extremal score difference", x = unit(0.415, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9)),
  # Grey line (90% CI)
  linesGrob(x = unit(c(0.60, 0.70), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "grey50", lwd = 6)),
  textGrob("90% CI (all charts)", x = unit(0.715, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9))
)

# Arrange in 5x4 grid as specified, add legend and universal x-axis label
png("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/MD_japan_12M_24M_paper.png", width = 2400, height = 2200, res = 120)
grid.arrange(
  arrangeGrob(grobs = all_grobs, nrow = 5, ncol = 4),
  # xlab_grob,
  legend_grob,
  heights = c(20, 1.2, 1.2)
)
dev.off()
cat("One big stacked Murphy diagram grid (5x4) with all requested features has been generated and saved as MD_Stacked_AllVars_5x4.png\n")
############################### Murphydiagram: UK: 12M and 24M ##############################
setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()

# Variable meta info
var_list <- list(
  UR   = "Unemployment Rate",
  REER = "REER",
  SIR  = "SIR",
  OP   = "Oil Price (WTI)",
  CPI  = "CPI Inflation"
)

# Helper to create a single Murphy diagram plot as a grob
get_md_grob <- function(data, comp, var_label, horizon, show_ylab=FALSE) {
  comp_label <- ifelse(comp == "VARx", "VARx", "CatBoostx")
  main_title <- sprintf("SZBVARx vs %s: %s (%s)", comp_label, var_label, horizon)
  temp_file <- tempfile(fileext = ".png")
  png(temp_file, width = 600, height = 400, res = 120)
  murphydiagram_diff(
    data$SZBVAR, data[[comp]], data$test_data,
    # functional = "quantile",
    # equally_spaced = TRUE,
    lag_truncate = 1, # 4 
    conf_level = 0.90
  )
  title(main = main_title, cex.main = 0.8)
  if (show_ylab) {
    mtext("Extremal score differences", side = 2, line = 2.5, cex = 0.9, font = 2) # bold
  }
  dev.off()
  img <- png::readPNG(temp_file)
  grid::rasterGrob(img, interpolate = TRUE)
}

# Prepare the grobs in the requested order
all_grobs <- list()
for (i in seq_along(var_list)) {
  var <- names(var_list)[i]
  var_label <- var_list[[var]]
  data_12M <- read.csv(sprintf("uk_GR_results_baselines_%s_12M.csv", var), header = TRUE)
  data_24M <- read.csv(sprintf("uk_GR_results_baselines_%s_24M.csv", var), header = TRUE)
  # Only first column gets ylab
  show_ylab <- c(TRUE, FALSE, FALSE, FALSE)
  # 1st: SZBVAR vs VAR (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "VARx", var_label, "12-month", show_ylab=show_ylab[1])
  # 2nd: SZBVAR vs CatBoost (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "CatBoost", var_label, "12-month", show_ylab=show_ylab[2])
  # 3rd: SZBVAR vs VAR (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "VARx", var_label, "24-month", show_ylab=show_ylab[3])
  # 4th: SZBVAR vs CatBoost (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "CatBoost", var_label, "24-month", show_ylab=show_ylab[4])
}


# Custom legend grob (centered at the bottom of the chart)
legend_grob <- grobTree(
  # Black line (Extremal score difference)
  linesGrob(x = unit(c(0.30, 0.40), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "black", lwd = 2)),
  textGrob("Extremal score difference", x = unit(0.415, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9)),
  # Grey line (90% CI)
  linesGrob(x = unit(c(0.60, 0.70), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "grey50", lwd = 6)),
  textGrob("90% CI (all charts)", x = unit(0.715, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9))
)

# Arrange in 5x4 grid as specified, add legend and universal x-axis label
png("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/MD_uk_12M_24M_paper.png", width = 2400, height = 2200, res = 120)
grid.arrange(
  arrangeGrob(grobs = all_grobs, nrow = 5, ncol = 4),
  # xlab_grob,
  legend_grob,
  heights = c(20, 1.2, 1.2)
)
dev.off()
cat("One big stacked Murphy diagram grid (5x4) with all requested features has been generated and saved as MD_Stacked_AllVars_5x4.png\n")

############################### Murphydiagram: ITALY: 12M and 24M ##############################
setwd('/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Input_dataset')
getwd()

# Variable meta info
var_list <- list(
  UR   = "Unemployment Rate",
  REER = "REER",
  SIR  = "SIR",
  OP   = "Oil Price (WTI)",
  CPI  = "CPI Inflation"
)

# Helper to create a single Murphy diagram plot as a grob
get_md_grob <- function(data, comp, var_label, horizon, show_ylab=FALSE) {
  comp_label <- ifelse(comp == "VARx", "VARx", "CatBoostx")
  main_title <- sprintf("SZBVARx vs %s: %s (%s)", comp_label, var_label, horizon)
  temp_file <- tempfile(fileext = ".png")
  png(temp_file, width = 600, height = 400, res = 120)
  murphydiagram_diff(
    data$SZBVAR, data[[comp]], data$test_data,
    lag_truncate = 1, conf_level = 0.90
  )
  title(main = main_title, cex.main = 0.8)
  if (show_ylab) {
    mtext("Extremal score differences", side = 2, line = 2.5, cex = 0.9, font = 2) # bold
  }
  dev.off()
  img <- png::readPNG(temp_file)
  grid::rasterGrob(img, interpolate = TRUE)
}

# Prepare the grobs in the requested order
all_grobs <- list()
for (i in seq_along(var_list)) {
  var <- names(var_list)[i]
  var_label <- var_list[[var]]
  data_12M <- read.csv(sprintf("italy_GR_results_baselines_%s_12M.csv", var), header = TRUE)
  data_24M <- read.csv(sprintf("italy_GR_results_baselines_%s_24M.csv", var), header = TRUE)
  # Only first column gets ylab
  show_ylab <- c(TRUE, FALSE, FALSE, FALSE)
  # 1st: SZBVAR vs VAR (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "VARx", var_label, "12-month", show_ylab=show_ylab[1])
  # 2nd: SZBVAR vs CatBoost (12M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_12M, "CatBoost", var_label, "12-month", show_ylab=show_ylab[2])
  # 3rd: SZBVAR vs VAR (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "VARx", var_label, "24-month", show_ylab=show_ylab[3])
  # 4th: SZBVAR vs CatBoost (24M)
  all_grobs[[length(all_grobs)+1]] <- get_md_grob(data_24M, "CatBoost", var_label, "24-month", show_ylab=show_ylab[4])
}


# Custom legend grob (centered at the bottom of the chart)
legend_grob <- grobTree(
  # Black line (Extremal score difference)
  linesGrob(x = unit(c(0.30, 0.40), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "black", lwd = 2)),
  textGrob("Extremal score difference", x = unit(0.415, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9)),
  # Grey line (90% CI)
  linesGrob(x = unit(c(0.60, 0.70), "npc"), y = unit(c(0.7, 0.7), "npc"), gp = gpar(col = "grey50", lwd = 6)),
  textGrob("90% CI (all charts)", x = unit(0.715, "npc"), y = unit(0.7, "npc"), just = "left", gp = gpar(fontface = "bold", cex = 0.9))
)

# Arrange in 5x4 grid as specified, add legend and universal x-axis label
png("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/code/Robustness_Analysis/Results/MD_italy_12M_24M_paper.png", width = 2400, height = 2200, res = 120)
grid.arrange(
  arrangeGrob(grobs = all_grobs, nrow = 5, ncol = 4),
  # xlab_grob,
  legend_grob,
  heights = c(20, 1.2, 1.2)
)
dev.off()
cat("One big stacked Murphy diagram grid (5x4) with all requested features has been generated and saved as MD_Stacked_AllVars_5x4.png\n")
############################# End of Code #########################



