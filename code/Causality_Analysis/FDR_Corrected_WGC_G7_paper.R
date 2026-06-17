# =====================================================================
#  CANADA -- FDR-corrected WCA + Wavelet Granger Causality (WGC)
#  Tests per (Endogenous, Exogenous, Scale, Direction):
#     1. Linear Granger F-test (lmtest::grangertest)
#     2. NN-Granger            (NlinTS::nlin_causality.test)
#  Training window: 1995-01-01 to 2022-03-01
# =====================================================================
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/canada")
getwd()

# ---- Packages -------------------------------------------------------
suppressPackageStartupMessages({
  library(biwavelet); library(wavelets); library(Cairo); library(magick)
  library(dplyr);     library(tidyr);    library(lmtest); library(lubridate)
  library(NlinTS)
})
stopifnot(utils::packageVersion("NlinTS") >= "1.4.0")
cat("NlinTS version:", as.character(utils::packageVersion("NlinTS")), "\n")

# ---- Read and subset data ------------------------------------------
data_ts <- read.csv("all_mulvar_data_canada_v2.csv",
                    header = TRUE, check.names = FALSE)
data_ts$Date <- as.Date(data_ts$Date)

training_start <- as.Date("1995-01-01")
training_end   <- as.Date("2022-03-01")
data_ts_train  <- data_ts[data_ts$Date >= training_start &
                            data_ts$Date <= training_end, ]

cat("Training period:", as.character(min(data_ts_train$Date)), "to",
    as.character(max(data_ts_train$Date)),
    "  (", nrow(data_ts_train), "obs )\n")

# ---- Variable specification ----------------------------------------
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR",
                     "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars  <- c("logEPU", "GPRC", "USEMV", "USMPU")
endogenous_labels <- c("Unemployment Rate", "REER", "SIR",
                       "Oil Price (WTI)", "CPI Inflation")
exogenous_labels  <- c("EPU", "GPR", "USEMV", "USMPU")

n_obs_train         <- nrow(data_ts_train)
start_year_train    <- year(min(data_ts_train$Date))
end_year_train      <- year(max(data_ts_train$Date))
time_sequence_train <- seq_len(n_obs_train)

# =====================================================================
#  PART A -- WAVELET COHERENCE (WCA) with scale-wise FDR correction
# =====================================================================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  n_time  <- ncol(wtc_result$rsq); n_scale <- nrow(wtc_result$rsq)
  p_values   <- matrix(NA, n_scale, n_time)
  fdr_signif <- matrix(FALSE, n_scale, n_time)
  for (i in seq_len(n_scale)) {
    sig_level <- wtc_result$signif[i]
    if (is.na(sig_level) || sig_level == 0) next
    scale_rsq <- wtc_result$rsq[i, ]; scale_p <- rep(NA_real_, n_time)
    for (j in seq_len(n_time)) {
      if (!is.na(scale_rsq[j])) {
        if (scale_rsq[j] >= sig_level)
          scale_p[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        else
          scale_p[j] <- min(1, 1 - scale_rsq[j] / sig_level)
      }
    }
    p_values[i, ] <- scale_p
    valid <- which(!is.na(scale_p))
    if (length(valid) > 0) {
      p_adj <- p.adjust(scale_p[valid], method = "BH")
      fdr_signif[i, valid[p_adj < alpha]] <- TRUE
    }
  }
  list(fdr_signif = fdr_signif, p_values = p_values, alpha = alpha)
}

plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label,
                              n_obs, start_year, end_year,
                              main_title, file_name) {
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey",
       lwd.coi = 2, lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE,
       main = main_title, cex.main = 1.5, font.main = 3, font.lab = 3)
  if (any(fdr_result$fdr_signif, na.rm = TRUE))
    contour(wtc_result$t, wtc_result$period, t(fdr_result$fdr_signif),
            levels = c(0.5), add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE, method = "edge")
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks))
    year_labels <- year_labels[seq_along(year_breaks)]
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  dev.off()
}

output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)
wca_ckpt_dir <- "WCA_checkpoints_canada"
if (!dir.exists(wca_ckpt_dir)) dir.create(wca_ckpt_dir)

wcc_results <- list(); fdr_results <- list(); file_list <- c()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    y_var <- endogenous_vars[i]; x_var <- exogenous_vars[j]
    y_lab <- endogenous_labels[i]; x_lab <- exogenous_labels[j]
    pair_name <- paste(y_var, x_var, sep = "_x_")
    ckpt <- file.path(wca_ckpt_dir, paste0(pair_name, ".rds"))
    
    if (file.exists(ckpt)) {
      cat("WCA: CACHED ", pair_name, "\n")
      blob <- readRDS(ckpt)
      wcc_results[[pair_name]] <- blob$wtc
      fdr_results[[pair_name]] <- blob$fdr
    } else {
      cat("WCA:", pair_name, "\n")
      t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
      t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
      wtc_r <- wtc(t1, t2, nrands = 1000)
      fdr_r <- apply_fdr_correction(wtc_r, alpha = 0.10)
      saveRDS(list(wtc = wtc_r, fdr = fdr_r), ckpt)
      wcc_results[[pair_name]] <- wtc_r
      fdr_results[[pair_name]] <- fdr_r
    }
    
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name),
                       "_fdr_training.png")
    file_list <- c(file_list, out_file)
    plot_wtc_with_fdr(wcc_results[[pair_name]], fdr_results[[pair_name]],
                      y_lab, x_lab, n_obs_train,
                      start_year_train, end_year_train,
                      paste0(y_lab, " vs ", x_lab), out_file)
  }
}

# ---- Grid composite -------------------------------------------------
ordered_files <- c()
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pat <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    hit <- file_list[grepl(pat, file_list)]
    if (length(hit)) ordered_files <- c(ordered_files, hit[1])
  }
rows <- split(ordered_files, ceiling(seq_along(ordered_files) / 4))
row_images <- lapply(rows, function(rf)
  if (length(rf)) image_append(image_join(image_read(rf)), stack = FALSE))
row_images <- row_images[!sapply(row_images, is.null)]
if (length(row_images)) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  image_write(grid_image, "WCC_Heatmaps_Grid_canada_fdr_training_revised.png")
}

# ---- WCA FDR-impact summary ----------------------------------------
fdr_summary <- data.frame(Pair = character(), Total_Tests = integer(),
                          Significant_Original = integer(),
                          Significant_FDR = integer(),
                          FDR_Reduction_Percent = numeric(),
                          stringsAsFactors = FALSE)
for (pn in names(wcc_results)) {
  w <- wcc_results[[pn]]; f <- fdr_results[[pn]]
  orig_sig <- 0
  for (i in seq_len(nrow(w$rsq)))
    if (!is.na(w$signif[i]) && w$signif[i] > 0)
      orig_sig <- orig_sig + sum(w$rsq[i, ] >= w$signif[i], na.rm = TRUE)
  fdr_sig    <- sum(f$fdr_signif, na.rm = TRUE)
  total_test <- sum(!is.na(w$rsq))
  red_pct    <- if (orig_sig > 0) (orig_sig - fdr_sig) / orig_sig * 100 else 0
  fdr_summary <- rbind(fdr_summary, data.frame(
    Pair = pn, Total_Tests = total_test,
    Significant_Original = orig_sig, Significant_FDR = fdr_sig,
    FDR_Reduction_Percent = round(red_pct, 2)))
}
write.csv(fdr_summary, "FDR_Correction_Summary_canada_training.csv",
          row.names = FALSE)

# ---- WCA summary tables --------------------------------------------
label_strength <- function(v) {
  if (is.na(v)) return("NA")
  if (v > 0.50) "Strong" else if (v > 0.25) "Moderate" else "Weak"
}
summary_matrix     <- matrix(NA, length(endogenous_vars), length(exogenous_vars),
                             dimnames = list(endogenous_labels, exogenous_labels))
summary_matrix_fdr <- summary_matrix
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pn <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    w  <- wcc_results[[pn]]; f <- fdr_results[[pn]]
    if (is.null(w)) next
    coi_mask <- matrix(1, nrow(w$rsq), ncol(w$rsq))
    coi_mask[w$coi < w$period] <- NA
    rsq_v <- w$rsq * coi_mask
    avg   <- mean(rsq_v, na.rm = TRUE)
    summary_matrix[endogenous_labels[i], exogenous_labels[j]] <-
      sprintf("%.2f (%s)", avg, label_strength(avg))
    rsq_f <- rsq_v; rsq_f[!f$fdr_signif] <- NA
    avg_f <- mean(rsq_f, na.rm = TRUE)
    summary_matrix_fdr[endogenous_labels[i], exogenous_labels[j]] <-
      if (is.na(avg_f)) "NS"
    else sprintf("%.2f (%s)", avg_f, label_strength(avg_f))
  }
write.csv(cbind(Endogenous = rownames(summary_matrix), summary_matrix),
          "WCC_Summary_Table_Original_canada_training.csv", row.names = FALSE)
write.csv(cbind(Endogenous = rownames(summary_matrix_fdr), summary_matrix_fdr),
          "WCC_Summary_Table_FDR_canada_training.csv", row.names = FALSE)

# =====================================================================
#  PART B -- WAVELET GRANGER CAUSALITY (Linear F + NN-GC only)
# =====================================================================
WAVELET <- "la8"; LEVELS <- 6
LIN_LAGS <- 2;    NN_LAGS <- 2

nn_gc_test <- function(ts_to, ts_from, lags = NN_LAGS,
                       layers_univ = c(2), layers_biv = c(4),
                       iters = 50, lr = 0.01, algo = "sgd",
                       batch = 10, bias = TRUE, seed = 1) {
  ts_to <- as.numeric(ts_to); ts_from <- as.numeric(ts_from)
  res <- NlinTS::nlin_causality.test(
    ts1 = ts_to, ts2 = ts_from, lag = lags,
    LayersUniv = layers_univ, LayersBiv = layers_biv,
    iters = iters, learningRate = lr, algo = algo,
    batch_size = batch, bias = bias, seed = seed)
  stat <- tryCatch(res$Ftest,  error = function(e) NA_real_)
  pval <- tryCatch(res$pvalue, error = function(e) NA_real_)
  if (is.null(stat) || length(stat) == 0) stat <- NA_real_
  if (is.null(pval) || length(pval) == 0) pval <- NA_real_
  c(stat = unname(stat), p = unname(pval))
}

lin_gc_test <- function(ts_to, ts_from, lags = LIN_LAGS) {
  df <- data.frame(Y = as.numeric(ts_to), X = as.numeric(ts_from))
  t  <- lmtest::grangertest(Y ~ X, order = lags, data = df)
  c(stat = unname(t$F[2]), p = unname(t$`Pr(>F)`[2]))
}

safe_run <- function(label, expr)
  tryCatch(expr, error = function(e) {
    message(sprintf("  [WARN] %s failed: %s", label, conditionMessage(e)))
    c(stat = NA_real_, p = NA_real_)
  })

wavelet_gc_full <- function(ts_y, ts_x, y_label, x_label,
                            wavelet = WAVELET, levels = LEVELS,
                            lags = LIN_LAGS) {
  m_y <- wavelets::modwt(as.numeric(ts_y), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  m_x <- wavelets::modwt(as.numeric(ts_x), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  n_orig <- length(ts_y); rows <- list()
  for (i in seq_len(levels)) {
    dy <- as.numeric(m_y@W[[i]]); dx <- as.numeric(m_x@W[[i]])
    if (length(dy) > n_orig) dy <- dy[seq_len(n_orig)]
    if (length(dx) > n_orig) dx <- dx[seq_len(n_orig)]
    df <- na.omit(data.frame(Y = dy, X = dx))
    sc <- paste0("D", i); enough <- nrow(df) > (3 * lags + 5)
    if (enough) {
      lin_fwd <- safe_run(sprintf("Lin X->Y %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$Y, df$X, lags))
      lin_rev <- safe_run(sprintf("Lin Y->X %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$X, df$Y, lags))
      nn_fwd  <- safe_run(sprintf("NN  X->Y %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$Y, df$X, lags))
      nn_rev  <- safe_run(sprintf("NN  Y->X %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$X, df$Y, lags))
    } else {
      lin_fwd <- lin_rev <- nn_fwd <- nn_rev <-
        c(stat = NA_real_, p = NA_real_)
    }
    mk <- function(dir, test, r) data.frame(
      Endogenous = y_label, Exogenous = x_label, Scale = sc,
      Direction = dir, Test = test,
      Statistic = round(unname(r["stat"]), 4),
      p_value   = round(unname(r["p"]),    4),
      stringsAsFactors = FALSE)
    rows[[length(rows)+1]] <- mk("X_to_Y","Linear_F", lin_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","Linear_F", lin_rev)
    rows[[length(rows)+1]] <- mk("X_to_Y","NN_GC",    nn_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","NN_GC",    nn_rev)
  }
  do.call(rbind, rows)
}

# ---- Run all 20 pairs with checkpointing ---------------------------
wgc_ckpt_dir <- "WGC_checkpoints_canada"
if (!dir.exists(wgc_ckpt_dir)) dir.create(wgc_ckpt_dir)

results_list <- list(); pair_id <- 0
total_pairs  <- length(endogenous_vars) * length(exogenous_vars)
t0 <- Sys.time()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    pair_id <- pair_id + 1
    ckpt_file <- file.path(wgc_ckpt_dir,
                           sprintf("pair_%02d_%s_x_%s.rds", pair_id,
                                   endogenous_vars[i], exogenous_vars[j]))
    if (file.exists(ckpt_file)) {
      cat(sprintf("[%2d/%d] %-22s <- %-6s  CACHED\n",
                  pair_id, total_pairs,
                  endogenous_labels[i], exogenous_labels[j]))
      results_list[[length(results_list)+1]] <- readRDS(ckpt_file)
      next
    }
    tp <- Sys.time()
    cat(sprintf("[%2d/%d] %-22s <- %-6s  ... ",
                pair_id, total_pairs,
                endogenous_labels[i], exogenous_labels[j]))
    flush.console()
    res <- wavelet_gc_full(
      ts_y    = data_ts_train[[endogenous_vars[i]]],
      ts_x    = data_ts_train[[exogenous_vars[j]]],
      y_label = endogenous_labels[i],
      x_label = exogenous_labels[j])
    saveRDS(res, ckpt_file)
    results_list[[length(results_list)+1]] <- res
    cat(sprintf("done in %.1f sec\n",
                as.numeric(difftime(Sys.time(), tp, units = "secs"))))
    flush.console()
  }
}
cat("WGC total elapsed:",
    round(difftime(Sys.time(), t0, units = "mins"), 2), "minutes\n")

wgc_all <- do.call(rbind, results_list)

# Raw dump before FDR (safety net)
write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_canada_training_raw.csv",
          row.names = FALSE)

# ---- BH-FDR within (Test, Direction) -------------------------------
wgc_all <- wgc_all %>%
  group_by(Test, Direction) %>%
  mutate(p_value_fdr = p.adjust(p_value, method = "BH")) %>%
  ungroup() %>%
  mutate(Significant_Original = ifelse(is.na(p_value), NA,
                                       ifelse(p_value     < 0.10, "Yes", "No")),
         Significant_FDR      = ifelse(is.na(p_value_fdr), NA,
                                       ifelse(p_value_fdr < 0.10, "Yes", "No")),
         p_value_fdr = round(p_value_fdr, 4)) %>%
  as.data.frame()

write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_canada_training.csv",
          row.names = FALSE)

# ---- Console summaries ---------------------------------------------
cat("\n--- WGC significance (FDR q < 0.10) by Test x Direction ---\n")
print(wgc_all %>% group_by(Test, Direction) %>%
        summarise(N = n(),
                  N_valid = sum(!is.na(p_value)),
                  Sig_orig = sum(Significant_Original == "Yes", na.rm = TRUE),
                  Sig_FDR  = sum(Significant_FDR      == "Yes", na.rm = TRUE),
                  .groups = "drop"))

cat("\n--- WGC significance by Endogenous x Test (FDR) ---\n")
print(wgc_all %>% group_by(Endogenous, Test) %>%
        summarise(Sig_FDR = sum(Significant_FDR == "Yes", na.rm = TRUE),
                  .groups = "drop") %>%
        pivot_wider(names_from = Test, values_from = Sig_FDR,
                    values_fill = 0))

# =====================================================================
#  PART C -- WCA FDR-impact reporting & example comparison plot
# =====================================================================
total_orig <- sum(fdr_summary$Significant_Original)
total_fdr  <- sum(fdr_summary$Significant_FDR)
cat("\n=== WCA FDR Impact ===\n")
cat("Total significant pixels (original):     ", total_orig, "\n")
cat("Total significant pixels (FDR-corrected):", total_fdr, "\n")
cat("Reduction:",
    round(if (total_orig > 0) (total_orig - total_fdr) / total_orig * 100
          else 0, 2), "%\n")

ex_pair <- paste(endogenous_vars[1], exogenous_vars[1], sep = "_x_")
if (ex_pair %in% names(wcc_results)) {
  CairoPNG("FDR_Comparison_Example.png", width = 2400, height = 1200, res = 150)
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 2,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "Original (α = 0.05)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 0,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "FDR-corrected (α = 0.10)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  if (any(fdr_results[[ex_pair]]$fdr_signif, na.rm = TRUE))
    contour(wcc_results[[ex_pair]]$t, wcc_results[[ex_pair]]$period,
            t(fdr_results[[ex_pair]]$fdr_signif),
            levels = 0.5, add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE)
  mtext(paste(endogenous_labels[1], "vs", exogenous_labels[1],
              "- FDR Correction Comparison"),
        outer = TRUE, cex = 1.8, font = 2)
  dev.off()
}

cat("\n=== Done ===\n")
cat("Outputs:\n")
cat(" - WCC plots/grid:  WCC_FDR_Charts_Training/, WCC_Heatmaps_Grid_canada_fdr_training_revised.png\n")
cat(" - WCA summaries:   FDR_Correction_Summary_canada_training.csv, WCC_Summary_Table_{Original,FDR}_canada_training.csv\n")
cat(" - WGC raw:         Wavelet_NL_Granger_Causality_canada_training_raw.csv\n")
cat(" - WGC final (FDR): Wavelet_NL_Granger_Causality_canada_training.csv  (480 rows)\n")
cat(" - WCA example:     FDR_Comparison_Example.png\n")
cat(" - Checkpoints:     WCA_checkpoints_canada/, WGC_checkpoints_canada/\n")
################## End of Code: Canada #################

# =====================================================================
#  USA -- FDR-corrected WCA + Wavelet Granger Causality (WGC)
#  Tests per (Endogenous, Exogenous, Scale, Direction):
#     1. Linear Granger F-test (lmtest::grangertest)
#     2. NN-Granger            (NlinTS::nlin_causality.test)
#  Training window: 1995-01-01 to 2022-03-01
# =====================================================================
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/usa")
getwd()

# ---- Packages -------------------------------------------------------
suppressPackageStartupMessages({
  library(biwavelet); library(wavelets); library(Cairo); library(magick)
  library(dplyr);     library(tidyr);    library(lmtest); library(lubridate)
  library(NlinTS)
})
stopifnot(utils::packageVersion("NlinTS") >= "1.4.0")
cat("NlinTS version:", as.character(utils::packageVersion("NlinTS")), "\n")

# ---- Read and subset data ------------------------------------------
data_ts <- read.csv("all_mulvar_data_usa_v2.csv",
                    header = TRUE, check.names = FALSE)
data_ts$Date <- as.Date(data_ts$Date)

training_start <- as.Date("1995-01-01")
training_end   <- as.Date("2022-03-01")
data_ts_train  <- data_ts[data_ts$Date >= training_start &
                            data_ts$Date <= training_end, ]

cat("Training period:", as.character(min(data_ts_train$Date)), "to",
    as.character(max(data_ts_train$Date)),
    "  (", nrow(data_ts_train), "obs )\n")

# ---- Variable specification ----------------------------------------
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR",
                     "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars  <- c("logEPU", "GPRC", "USEMV", "USMPU")
endogenous_labels <- c("Unemployment Rate", "REER", "SIR",
                       "Oil Price (WTI)", "CPI Inflation")
exogenous_labels  <- c("EPU", "GPR", "USEMV", "USMPU")

n_obs_train         <- nrow(data_ts_train)
start_year_train    <- year(min(data_ts_train$Date))
end_year_train      <- year(max(data_ts_train$Date))
time_sequence_train <- seq_len(n_obs_train)

# =====================================================================
#  PART A -- WAVELET COHERENCE (WCA) with scale-wise FDR correction
# =====================================================================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  n_time  <- ncol(wtc_result$rsq); n_scale <- nrow(wtc_result$rsq)
  p_values   <- matrix(NA, n_scale, n_time)
  fdr_signif <- matrix(FALSE, n_scale, n_time)
  for (i in seq_len(n_scale)) {
    sig_level <- wtc_result$signif[i]
    if (is.na(sig_level) || sig_level == 0) next
    scale_rsq <- wtc_result$rsq[i, ]; scale_p <- rep(NA_real_, n_time)
    for (j in seq_len(n_time)) {
      if (!is.na(scale_rsq[j])) {
        if (scale_rsq[j] >= sig_level)
          scale_p[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        else
          scale_p[j] <- min(1, 1 - scale_rsq[j] / sig_level)
      }
    }
    p_values[i, ] <- scale_p
    valid <- which(!is.na(scale_p))
    if (length(valid) > 0) {
      p_adj <- p.adjust(scale_p[valid], method = "BH")
      fdr_signif[i, valid[p_adj < alpha]] <- TRUE
    }
  }
  list(fdr_signif = fdr_signif, p_values = p_values, alpha = alpha)
}

plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label,
                              n_obs, start_year, end_year,
                              main_title, file_name) {
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey",
       lwd.coi = 2, lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE,
       main = main_title, cex.main = 1.5, font.main = 3, font.lab = 3)
  if (any(fdr_result$fdr_signif, na.rm = TRUE))
    contour(wtc_result$t, wtc_result$period, t(fdr_result$fdr_signif),
            levels = c(0.5), add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE, method = "edge")
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks))
    year_labels <- year_labels[seq_along(year_breaks)]
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  dev.off()
}

output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)
wca_ckpt_dir <- "WCA_checkpoints_usa"
if (!dir.exists(wca_ckpt_dir)) dir.create(wca_ckpt_dir)

wcc_results <- list(); fdr_results <- list(); file_list <- c()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    y_var <- endogenous_vars[i]; x_var <- exogenous_vars[j]
    y_lab <- endogenous_labels[i]; x_lab <- exogenous_labels[j]
    pair_name <- paste(y_var, x_var, sep = "_x_")
    ckpt <- file.path(wca_ckpt_dir, paste0(pair_name, ".rds"))
    
    if (file.exists(ckpt)) {
      cat("WCA: CACHED ", pair_name, "\n")
      blob <- readRDS(ckpt)
      wcc_results[[pair_name]] <- blob$wtc
      fdr_results[[pair_name]] <- blob$fdr
    } else {
      cat("WCA:", pair_name, "\n")
      t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
      t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
      wtc_r <- wtc(t1, t2, nrands = 1000)
      fdr_r <- apply_fdr_correction(wtc_r, alpha = 0.10)
      saveRDS(list(wtc = wtc_r, fdr = fdr_r), ckpt)
      wcc_results[[pair_name]] <- wtc_r
      fdr_results[[pair_name]] <- fdr_r
    }
    
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name),
                       "_fdr_training.png")
    file_list <- c(file_list, out_file)
    plot_wtc_with_fdr(wcc_results[[pair_name]], fdr_results[[pair_name]],
                      y_lab, x_lab, n_obs_train,
                      start_year_train, end_year_train,
                      paste0(y_lab, " vs ", x_lab), out_file)
  }
}

# ---- Grid composite -------------------------------------------------
ordered_files <- c()
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pat <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    hit <- file_list[grepl(pat, file_list)]
    if (length(hit)) ordered_files <- c(ordered_files, hit[1])
  }
rows <- split(ordered_files, ceiling(seq_along(ordered_files) / 4))
row_images <- lapply(rows, function(rf)
  if (length(rf)) image_append(image_join(image_read(rf)), stack = FALSE))
row_images <- row_images[!sapply(row_images, is.null)]
if (length(row_images)) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  image_write(grid_image, "WCC_Heatmaps_Grid_usa_fdr_training_revised.png")
}

# ---- WCA FDR-impact summary ----------------------------------------
fdr_summary <- data.frame(Pair = character(), Total_Tests = integer(),
                          Significant_Original = integer(),
                          Significant_FDR = integer(),
                          FDR_Reduction_Percent = numeric(),
                          stringsAsFactors = FALSE)
for (pn in names(wcc_results)) {
  w <- wcc_results[[pn]]; f <- fdr_results[[pn]]
  orig_sig <- 0
  for (i in seq_len(nrow(w$rsq)))
    if (!is.na(w$signif[i]) && w$signif[i] > 0)
      orig_sig <- orig_sig + sum(w$rsq[i, ] >= w$signif[i], na.rm = TRUE)
  fdr_sig    <- sum(f$fdr_signif, na.rm = TRUE)
  total_test <- sum(!is.na(w$rsq))
  red_pct    <- if (orig_sig > 0) (orig_sig - fdr_sig) / orig_sig * 100 else 0
  fdr_summary <- rbind(fdr_summary, data.frame(
    Pair = pn, Total_Tests = total_test,
    Significant_Original = orig_sig, Significant_FDR = fdr_sig,
    FDR_Reduction_Percent = round(red_pct, 2)))
}
write.csv(fdr_summary, "FDR_Correction_Summary_usa_training.csv",
          row.names = FALSE)

# ---- WCA summary tables --------------------------------------------
label_strength <- function(v) {
  if (is.na(v)) return("NA")
  if (v > 0.50) "Strong" else if (v > 0.25) "Moderate" else "Weak"
}
summary_matrix     <- matrix(NA, length(endogenous_vars), length(exogenous_vars),
                             dimnames = list(endogenous_labels, exogenous_labels))
summary_matrix_fdr <- summary_matrix
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pn <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    w  <- wcc_results[[pn]]; f <- fdr_results[[pn]]
    if (is.null(w)) next
    coi_mask <- matrix(1, nrow(w$rsq), ncol(w$rsq))
    coi_mask[w$coi < w$period] <- NA
    rsq_v <- w$rsq * coi_mask
    avg   <- mean(rsq_v, na.rm = TRUE)
    summary_matrix[endogenous_labels[i], exogenous_labels[j]] <-
      sprintf("%.2f (%s)", avg, label_strength(avg))
    rsq_f <- rsq_v; rsq_f[!f$fdr_signif] <- NA
    avg_f <- mean(rsq_f, na.rm = TRUE)
    summary_matrix_fdr[endogenous_labels[i], exogenous_labels[j]] <-
      if (is.na(avg_f)) "NS"
    else sprintf("%.2f (%s)", avg_f, label_strength(avg_f))
  }
write.csv(cbind(Endogenous = rownames(summary_matrix), summary_matrix),
          "WCC_Summary_Table_Original_usa_training.csv", row.names = FALSE)
write.csv(cbind(Endogenous = rownames(summary_matrix_fdr), summary_matrix_fdr),
          "WCC_Summary_Table_FDR_usa_training.csv", row.names = FALSE)

# =====================================================================
#  PART B -- WAVELET GRANGER CAUSALITY (Linear F + NN-GC only)
# =====================================================================
WAVELET <- "la8"; LEVELS <- 6
LIN_LAGS <- 2;    NN_LAGS <- 2

nn_gc_test <- function(ts_to, ts_from, lags = NN_LAGS,
                       layers_univ = c(2), layers_biv = c(4),
                       iters = 50, lr = 0.01, algo = "sgd",
                       batch = 10, bias = TRUE, seed = 1) {
  ts_to <- as.numeric(ts_to); ts_from <- as.numeric(ts_from)
  res <- NlinTS::nlin_causality.test(
    ts1 = ts_to, ts2 = ts_from, lag = lags,
    LayersUniv = layers_univ, LayersBiv = layers_biv,
    iters = iters, learningRate = lr, algo = algo,
    batch_size = batch, bias = bias, seed = seed)
  stat <- tryCatch(res$Ftest,  error = function(e) NA_real_)
  pval <- tryCatch(res$pvalue, error = function(e) NA_real_)
  if (is.null(stat) || length(stat) == 0) stat <- NA_real_
  if (is.null(pval) || length(pval) == 0) pval <- NA_real_
  c(stat = unname(stat), p = unname(pval))
}

lin_gc_test <- function(ts_to, ts_from, lags = LIN_LAGS) {
  df <- data.frame(Y = as.numeric(ts_to), X = as.numeric(ts_from))
  t  <- lmtest::grangertest(Y ~ X, order = lags, data = df)
  c(stat = unname(t$F[2]), p = unname(t$`Pr(>F)`[2]))
}

safe_run <- function(label, expr)
  tryCatch(expr, error = function(e) {
    message(sprintf("  [WARN] %s failed: %s", label, conditionMessage(e)))
    c(stat = NA_real_, p = NA_real_)
  })

wavelet_gc_full <- function(ts_y, ts_x, y_label, x_label,
                            wavelet = WAVELET, levels = LEVELS,
                            lags = LIN_LAGS) {
  m_y <- wavelets::modwt(as.numeric(ts_y), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  m_x <- wavelets::modwt(as.numeric(ts_x), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  n_orig <- length(ts_y); rows <- list()
  for (i in seq_len(levels)) {
    dy <- as.numeric(m_y@W[[i]]); dx <- as.numeric(m_x@W[[i]])
    if (length(dy) > n_orig) dy <- dy[seq_len(n_orig)]
    if (length(dx) > n_orig) dx <- dx[seq_len(n_orig)]
    df <- na.omit(data.frame(Y = dy, X = dx))
    sc <- paste0("D", i); enough <- nrow(df) > (3 * lags + 5)
    if (enough) {
      lin_fwd <- safe_run(sprintf("Lin X->Y %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$Y, df$X, lags))
      lin_rev <- safe_run(sprintf("Lin Y->X %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$X, df$Y, lags))
      nn_fwd  <- safe_run(sprintf("NN  X->Y %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$Y, df$X, lags))
      nn_rev  <- safe_run(sprintf("NN  Y->X %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$X, df$Y, lags))
    } else {
      lin_fwd <- lin_rev <- nn_fwd <- nn_rev <-
        c(stat = NA_real_, p = NA_real_)
    }
    mk <- function(dir, test, r) data.frame(
      Endogenous = y_label, Exogenous = x_label, Scale = sc,
      Direction = dir, Test = test,
      Statistic = round(unname(r["stat"]), 4),
      p_value   = round(unname(r["p"]),    4),
      stringsAsFactors = FALSE)
    rows[[length(rows)+1]] <- mk("X_to_Y","Linear_F", lin_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","Linear_F", lin_rev)
    rows[[length(rows)+1]] <- mk("X_to_Y","NN_GC",    nn_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","NN_GC",    nn_rev)
  }
  do.call(rbind, rows)
}

# ---- Run all 20 pairs with checkpointing ---------------------------
wgc_ckpt_dir <- "WGC_checkpoints_usa"
if (!dir.exists(wgc_ckpt_dir)) dir.create(wgc_ckpt_dir)

results_list <- list(); pair_id <- 0
total_pairs  <- length(endogenous_vars) * length(exogenous_vars)
t0 <- Sys.time()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    pair_id <- pair_id + 1
    ckpt_file <- file.path(wgc_ckpt_dir,
                           sprintf("pair_%02d_%s_x_%s.rds", pair_id,
                                   endogenous_vars[i], exogenous_vars[j]))
    if (file.exists(ckpt_file)) {
      cat(sprintf("[%2d/%d] %-22s <- %-6s  CACHED\n",
                  pair_id, total_pairs,
                  endogenous_labels[i], exogenous_labels[j]))
      results_list[[length(results_list)+1]] <- readRDS(ckpt_file)
      next
    }
    tp <- Sys.time()
    cat(sprintf("[%2d/%d] %-22s <- %-6s  ... ",
                pair_id, total_pairs,
                endogenous_labels[i], exogenous_labels[j]))
    flush.console()
    res <- wavelet_gc_full(
      ts_y    = data_ts_train[[endogenous_vars[i]]],
      ts_x    = data_ts_train[[exogenous_vars[j]]],
      y_label = endogenous_labels[i],
      x_label = exogenous_labels[j])
    saveRDS(res, ckpt_file)
    results_list[[length(results_list)+1]] <- res
    cat(sprintf("done in %.1f sec\n",
                as.numeric(difftime(Sys.time(), tp, units = "secs"))))
    flush.console()
  }
}
cat("WGC total elapsed:",
    round(difftime(Sys.time(), t0, units = "mins"), 2), "minutes\n")

wgc_all <- do.call(rbind, results_list)

# Raw dump before FDR (safety net)
write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_usa_training_raw.csv",
          row.names = FALSE)

# ---- BH-FDR within (Test, Direction) -------------------------------
wgc_all <- wgc_all %>%
  group_by(Test, Direction) %>%
  mutate(p_value_fdr = p.adjust(p_value, method = "BH")) %>%
  ungroup() %>%
  mutate(Significant_Original = ifelse(is.na(p_value), NA,
                                       ifelse(p_value     < 0.10, "Yes", "No")),
         Significant_FDR      = ifelse(is.na(p_value_fdr), NA,
                                       ifelse(p_value_fdr < 0.10, "Yes", "No")),
         p_value_fdr = round(p_value_fdr, 4)) %>%
  as.data.frame()

write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_usa_training.csv",
          row.names = FALSE)

# ---- Console summaries ---------------------------------------------
cat("\n--- WGC significance (FDR q < 0.10) by Test x Direction ---\n")
print(wgc_all %>% group_by(Test, Direction) %>%
        summarise(N = n(),
                  N_valid = sum(!is.na(p_value)),
                  Sig_orig = sum(Significant_Original == "Yes", na.rm = TRUE),
                  Sig_FDR  = sum(Significant_FDR      == "Yes", na.rm = TRUE),
                  .groups = "drop"))

cat("\n--- WGC significance by Endogenous x Test (FDR) ---\n")
print(wgc_all %>% group_by(Endogenous, Test) %>%
        summarise(Sig_FDR = sum(Significant_FDR == "Yes", na.rm = TRUE),
                  .groups = "drop") %>%
        pivot_wider(names_from = Test, values_from = Sig_FDR,
                    values_fill = 0))

# =====================================================================
#  PART C -- WCA FDR-impact reporting & example comparison plot
# =====================================================================
total_orig <- sum(fdr_summary$Significant_Original)
total_fdr  <- sum(fdr_summary$Significant_FDR)
cat("\n=== WCA FDR Impact ===\n")
cat("Total significant pixels (original):     ", total_orig, "\n")
cat("Total significant pixels (FDR-corrected):", total_fdr, "\n")
cat("Reduction:",
    round(if (total_orig > 0) (total_orig - total_fdr) / total_orig * 100
          else 0, 2), "%\n")

ex_pair <- paste(endogenous_vars[1], exogenous_vars[1], sep = "_x_")
if (ex_pair %in% names(wcc_results)) {
  CairoPNG("FDR_Comparison_Example.png", width = 2400, height = 1200, res = 150)
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 2,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "Original (α = 0.05)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 0,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "FDR-corrected (α = 0.10)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  if (any(fdr_results[[ex_pair]]$fdr_signif, na.rm = TRUE))
    contour(wcc_results[[ex_pair]]$t, wcc_results[[ex_pair]]$period,
            t(fdr_results[[ex_pair]]$fdr_signif),
            levels = 0.5, add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE)
  mtext(paste(endogenous_labels[1], "vs", exogenous_labels[1],
              "- FDR Correction Comparison"),
        outer = TRUE, cex = 1.8, font = 2)
  dev.off()
}

cat("\n=== Done ===\n")
cat("Outputs:\n")
cat(" - WCC plots/grid:  WCC_FDR_Charts_Training/, WCC_Heatmaps_Grid_usa_fdr_training_revised.png\n")
cat(" - WCA summaries:   FDR_Correction_Summary_usa_training.csv, WCC_Summary_Table_{Original,FDR}_usa_training.csv\n")
cat(" - WGC raw:         Wavelet_NL_Granger_Causality_usa_training_raw.csv\n")
cat(" - WGC final (FDR): Wavelet_NL_Granger_Causality_usa_training.csv  (480 rows)\n")
cat(" - WCA example:     FDR_Comparison_Example.png\n")
cat(" - Checkpoints:     WCA_checkpoints_usa/, WGC_checkpoints_usa/\n")
################## End of Code: USA #################

# =====================================================================
#  FRANCE -- FDR-corrected WCA + Wavelet Granger Causality (WGC)
#  Tests per (Endogenous, Exogenous, Scale, Direction):
#     1. Linear Granger F-test (lmtest::grangertest)
#     2. NN-Granger            (NlinTS::nlin_causality.test)
#  Training window: 1995-01-01 to 2022-03-01
# =====================================================================
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/france")
getwd()
# ---- Packages -------------------------------------------------------
suppressPackageStartupMessages({
  library(biwavelet); library(wavelets); library(Cairo); library(magick)
  library(dplyr);     library(tidyr);    library(lmtest); library(lubridate)
  library(NlinTS)
})
stopifnot(utils::packageVersion("NlinTS") >= "1.4.0")
cat("NlinTS version:", as.character(utils::packageVersion("NlinTS")), "\n")

# ---- Read and subset data ------------------------------------------
data_ts <- read.csv("all_mulvar_data_france_v2.csv",
                    header = TRUE, check.names = FALSE)
data_ts$Date <- as.Date(data_ts$Date)

training_start <- as.Date("1995-01-01")
training_end   <- as.Date("2022-03-01")
data_ts_train  <- data_ts[data_ts$Date >= training_start &
                            data_ts$Date <= training_end, ]

cat("Training period:", as.character(min(data_ts_train$Date)), "to",
    as.character(max(data_ts_train$Date)),
    "  (", nrow(data_ts_train), "obs )\n")

# ---- Variable specification ----------------------------------------
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR",
                     "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars  <- c("logEPU", "GPRC", "USEMV", "USMPU")
endogenous_labels <- c("Unemployment Rate", "REER", "SIR",
                       "Oil Price (WTI)", "CPI Inflation")
exogenous_labels  <- c("EPU", "GPR", "USEMV", "USMPU")

n_obs_train         <- nrow(data_ts_train)
start_year_train    <- year(min(data_ts_train$Date))
end_year_train      <- year(max(data_ts_train$Date))
time_sequence_train <- seq_len(n_obs_train)

# =====================================================================
#  PART A -- WAVELET COHERENCE (WCA) with scale-wise FDR correction
# =====================================================================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  n_time  <- ncol(wtc_result$rsq); n_scale <- nrow(wtc_result$rsq)
  p_values   <- matrix(NA, n_scale, n_time)
  fdr_signif <- matrix(FALSE, n_scale, n_time)
  for (i in seq_len(n_scale)) {
    sig_level <- wtc_result$signif[i]
    if (is.na(sig_level) || sig_level == 0) next
    scale_rsq <- wtc_result$rsq[i, ]; scale_p <- rep(NA_real_, n_time)
    for (j in seq_len(n_time)) {
      if (!is.na(scale_rsq[j])) {
        if (scale_rsq[j] >= sig_level)
          scale_p[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        else
          scale_p[j] <- min(1, 1 - scale_rsq[j] / sig_level)
      }
    }
    p_values[i, ] <- scale_p
    valid <- which(!is.na(scale_p))
    if (length(valid) > 0) {
      p_adj <- p.adjust(scale_p[valid], method = "BH")
      fdr_signif[i, valid[p_adj < alpha]] <- TRUE
    }
  }
  list(fdr_signif = fdr_signif, p_values = p_values, alpha = alpha)
}

plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label,
                              n_obs, start_year, end_year,
                              main_title, file_name) {
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey",
       lwd.coi = 2, lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE,
       main = main_title, cex.main = 1.5, font.main = 3, font.lab = 3)
  if (any(fdr_result$fdr_signif, na.rm = TRUE))
    contour(wtc_result$t, wtc_result$period, t(fdr_result$fdr_signif),
            levels = c(0.5), add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE, method = "edge")
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks))
    year_labels <- year_labels[seq_along(year_breaks)]
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  dev.off()
}

output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)
wca_ckpt_dir <- "WCA_checkpoints_france"
if (!dir.exists(wca_ckpt_dir)) dir.create(wca_ckpt_dir)

wcc_results <- list(); fdr_results <- list(); file_list <- c()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    y_var <- endogenous_vars[i]; x_var <- exogenous_vars[j]
    y_lab <- endogenous_labels[i]; x_lab <- exogenous_labels[j]
    pair_name <- paste(y_var, x_var, sep = "_x_")
    ckpt <- file.path(wca_ckpt_dir, paste0(pair_name, ".rds"))
    
    if (file.exists(ckpt)) {
      cat("WCA: CACHED ", pair_name, "\n")
      blob <- readRDS(ckpt)
      wcc_results[[pair_name]] <- blob$wtc
      fdr_results[[pair_name]] <- blob$fdr
    } else {
      cat("WCA:", pair_name, "\n")
      t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
      t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
      wtc_r <- wtc(t1, t2, nrands = 1000)
      fdr_r <- apply_fdr_correction(wtc_r, alpha = 0.10)
      saveRDS(list(wtc = wtc_r, fdr = fdr_r), ckpt)
      wcc_results[[pair_name]] <- wtc_r
      fdr_results[[pair_name]] <- fdr_r
    }
    
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name),
                       "_fdr_training.png")
    file_list <- c(file_list, out_file)
    plot_wtc_with_fdr(wcc_results[[pair_name]], fdr_results[[pair_name]],
                      y_lab, x_lab, n_obs_train,
                      start_year_train, end_year_train,
                      paste0(y_lab, " vs ", x_lab), out_file)
  }
}

# ---- Grid composite -------------------------------------------------
ordered_files <- c()
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pat <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    hit <- file_list[grepl(pat, file_list)]
    if (length(hit)) ordered_files <- c(ordered_files, hit[1])
  }
rows <- split(ordered_files, ceiling(seq_along(ordered_files) / 4))
row_images <- lapply(rows, function(rf)
  if (length(rf)) image_append(image_join(image_read(rf)), stack = FALSE))
row_images <- row_images[!sapply(row_images, is.null)]
if (length(row_images)) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  image_write(grid_image, "WCC_Heatmaps_Grid_france_fdr_training_revised.png")
}

# ---- WCA FDR-impact summary ----------------------------------------
fdr_summary <- data.frame(Pair = character(), Total_Tests = integer(),
                          Significant_Original = integer(),
                          Significant_FDR = integer(),
                          FDR_Reduction_Percent = numeric(),
                          stringsAsFactors = FALSE)
for (pn in names(wcc_results)) {
  w <- wcc_results[[pn]]; f <- fdr_results[[pn]]
  orig_sig <- 0
  for (i in seq_len(nrow(w$rsq)))
    if (!is.na(w$signif[i]) && w$signif[i] > 0)
      orig_sig <- orig_sig + sum(w$rsq[i, ] >= w$signif[i], na.rm = TRUE)
  fdr_sig    <- sum(f$fdr_signif, na.rm = TRUE)
  total_test <- sum(!is.na(w$rsq))
  red_pct    <- if (orig_sig > 0) (orig_sig - fdr_sig) / orig_sig * 100 else 0
  fdr_summary <- rbind(fdr_summary, data.frame(
    Pair = pn, Total_Tests = total_test,
    Significant_Original = orig_sig, Significant_FDR = fdr_sig,
    FDR_Reduction_Percent = round(red_pct, 2)))
}
write.csv(fdr_summary, "FDR_Correction_Summary_france_training.csv",
          row.names = FALSE)

# ---- WCA summary tables --------------------------------------------
label_strength <- function(v) {
  if (is.na(v)) return("NA")
  if (v > 0.50) "Strong" else if (v > 0.25) "Moderate" else "Weak"
}
summary_matrix     <- matrix(NA, length(endogenous_vars), length(exogenous_vars),
                             dimnames = list(endogenous_labels, exogenous_labels))
summary_matrix_fdr <- summary_matrix
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pn <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    w  <- wcc_results[[pn]]; f <- fdr_results[[pn]]
    if (is.null(w)) next
    coi_mask <- matrix(1, nrow(w$rsq), ncol(w$rsq))
    coi_mask[w$coi < w$period] <- NA
    rsq_v <- w$rsq * coi_mask
    avg   <- mean(rsq_v, na.rm = TRUE)
    summary_matrix[endogenous_labels[i], exogenous_labels[j]] <-
      sprintf("%.2f (%s)", avg, label_strength(avg))
    rsq_f <- rsq_v; rsq_f[!f$fdr_signif] <- NA
    avg_f <- mean(rsq_f, na.rm = TRUE)
    summary_matrix_fdr[endogenous_labels[i], exogenous_labels[j]] <-
      if (is.na(avg_f)) "NS"
    else sprintf("%.2f (%s)", avg_f, label_strength(avg_f))
  }
write.csv(cbind(Endogenous = rownames(summary_matrix), summary_matrix),
          "WCC_Summary_Table_Original_france_training.csv", row.names = FALSE)
write.csv(cbind(Endogenous = rownames(summary_matrix_fdr), summary_matrix_fdr),
          "WCC_Summary_Table_FDR_france_training.csv", row.names = FALSE)

# =====================================================================
#  PART B -- WAVELET GRANGER CAUSALITY (Linear F + NN-GC only)
# =====================================================================
WAVELET <- "la8"; LEVELS <- 6
LIN_LAGS <- 2;    NN_LAGS <- 2

nn_gc_test <- function(ts_to, ts_from, lags = NN_LAGS,
                       layers_univ = c(2), layers_biv = c(4),
                       iters = 50, lr = 0.01, algo = "sgd",
                       batch = 10, bias = TRUE, seed = 1) {
  ts_to <- as.numeric(ts_to); ts_from <- as.numeric(ts_from)
  res <- NlinTS::nlin_causality.test(
    ts1 = ts_to, ts2 = ts_from, lag = lags,
    LayersUniv = layers_univ, LayersBiv = layers_biv,
    iters = iters, learningRate = lr, algo = algo,
    batch_size = batch, bias = bias, seed = seed)
  stat <- tryCatch(res$Ftest,  error = function(e) NA_real_)
  pval <- tryCatch(res$pvalue, error = function(e) NA_real_)
  if (is.null(stat) || length(stat) == 0) stat <- NA_real_
  if (is.null(pval) || length(pval) == 0) pval <- NA_real_
  c(stat = unname(stat), p = unname(pval))
}

lin_gc_test <- function(ts_to, ts_from, lags = LIN_LAGS) {
  df <- data.frame(Y = as.numeric(ts_to), X = as.numeric(ts_from))
  t  <- lmtest::grangertest(Y ~ X, order = lags, data = df)
  c(stat = unname(t$F[2]), p = unname(t$`Pr(>F)`[2]))
}

safe_run <- function(label, expr)
  tryCatch(expr, error = function(e) {
    message(sprintf("  [WARN] %s failed: %s", label, conditionMessage(e)))
    c(stat = NA_real_, p = NA_real_)
  })

wavelet_gc_full <- function(ts_y, ts_x, y_label, x_label,
                            wavelet = WAVELET, levels = LEVELS,
                            lags = LIN_LAGS) {
  m_y <- wavelets::modwt(as.numeric(ts_y), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  m_x <- wavelets::modwt(as.numeric(ts_x), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  n_orig <- length(ts_y); rows <- list()
  for (i in seq_len(levels)) {
    dy <- as.numeric(m_y@W[[i]]); dx <- as.numeric(m_x@W[[i]])
    if (length(dy) > n_orig) dy <- dy[seq_len(n_orig)]
    if (length(dx) > n_orig) dx <- dx[seq_len(n_orig)]
    df <- na.omit(data.frame(Y = dy, X = dx))
    sc <- paste0("D", i); enough <- nrow(df) > (3 * lags + 5)
    if (enough) {
      lin_fwd <- safe_run(sprintf("Lin X->Y %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$Y, df$X, lags))
      lin_rev <- safe_run(sprintf("Lin Y->X %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$X, df$Y, lags))
      nn_fwd  <- safe_run(sprintf("NN  X->Y %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$Y, df$X, lags))
      nn_rev  <- safe_run(sprintf("NN  Y->X %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$X, df$Y, lags))
    } else {
      lin_fwd <- lin_rev <- nn_fwd <- nn_rev <-
        c(stat = NA_real_, p = NA_real_)
    }
    mk <- function(dir, test, r) data.frame(
      Endogenous = y_label, Exogenous = x_label, Scale = sc,
      Direction = dir, Test = test,
      Statistic = round(unname(r["stat"]), 4),
      p_value   = round(unname(r["p"]),    4),
      stringsAsFactors = FALSE)
    rows[[length(rows)+1]] <- mk("X_to_Y","Linear_F", lin_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","Linear_F", lin_rev)
    rows[[length(rows)+1]] <- mk("X_to_Y","NN_GC",    nn_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","NN_GC",    nn_rev)
  }
  do.call(rbind, rows)
}

# ---- Run all 20 pairs with checkpointing ---------------------------
wgc_ckpt_dir <- "WGC_checkpoints_france"
if (!dir.exists(wgc_ckpt_dir)) dir.create(wgc_ckpt_dir)

results_list <- list(); pair_id <- 0
total_pairs  <- length(endogenous_vars) * length(exogenous_vars)
t0 <- Sys.time()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    pair_id <- pair_id + 1
    ckpt_file <- file.path(wgc_ckpt_dir,
                           sprintf("pair_%02d_%s_x_%s.rds", pair_id,
                                   endogenous_vars[i], exogenous_vars[j]))
    if (file.exists(ckpt_file)) {
      cat(sprintf("[%2d/%d] %-22s <- %-6s  CACHED\n",
                  pair_id, total_pairs,
                  endogenous_labels[i], exogenous_labels[j]))
      results_list[[length(results_list)+1]] <- readRDS(ckpt_file)
      next
    }
    tp <- Sys.time()
    cat(sprintf("[%2d/%d] %-22s <- %-6s  ... ",
                pair_id, total_pairs,
                endogenous_labels[i], exogenous_labels[j]))
    flush.console()
    res <- wavelet_gc_full(
      ts_y    = data_ts_train[[endogenous_vars[i]]],
      ts_x    = data_ts_train[[exogenous_vars[j]]],
      y_label = endogenous_labels[i],
      x_label = exogenous_labels[j])
    saveRDS(res, ckpt_file)
    results_list[[length(results_list)+1]] <- res
    cat(sprintf("done in %.1f sec\n",
                as.numeric(difftime(Sys.time(), tp, units = "secs"))))
    flush.console()
  }
}
cat("WGC total elapsed:",
    round(difftime(Sys.time(), t0, units = "mins"), 2), "minutes\n")

wgc_all <- do.call(rbind, results_list)

# Raw dump before FDR (safety net)
write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_france_training_raw.csv",
          row.names = FALSE)

# ---- BH-FDR within (Test, Direction) -------------------------------
wgc_all <- wgc_all %>%
  group_by(Test, Direction) %>%
  mutate(p_value_fdr = p.adjust(p_value, method = "BH")) %>%
  ungroup() %>%
  mutate(Significant_Original = ifelse(is.na(p_value), NA,
                                       ifelse(p_value     < 0.10, "Yes", "No")),
         Significant_FDR      = ifelse(is.na(p_value_fdr), NA,
                                       ifelse(p_value_fdr < 0.10, "Yes", "No")),
         p_value_fdr = round(p_value_fdr, 4)) %>%
  as.data.frame()

write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_france_training.csv",
          row.names = FALSE)

# ---- Console summaries ---------------------------------------------
cat("\n--- WGC significance (FDR q < 0.10) by Test x Direction ---\n")
print(wgc_all %>% group_by(Test, Direction) %>%
        summarise(N = n(),
                  N_valid = sum(!is.na(p_value)),
                  Sig_orig = sum(Significant_Original == "Yes", na.rm = TRUE),
                  Sig_FDR  = sum(Significant_FDR      == "Yes", na.rm = TRUE),
                  .groups = "drop"))

cat("\n--- WGC significance by Endogenous x Test (FDR) ---\n")
print(wgc_all %>% group_by(Endogenous, Test) %>%
        summarise(Sig_FDR = sum(Significant_FDR == "Yes", na.rm = TRUE),
                  .groups = "drop") %>%
        pivot_wider(names_from = Test, values_from = Sig_FDR,
                    values_fill = 0))

# =====================================================================
#  PART C -- WCA FDR-impact reporting & example comparison plot
# =====================================================================
total_orig <- sum(fdr_summary$Significant_Original)
total_fdr  <- sum(fdr_summary$Significant_FDR)
cat("\n=== WCA FDR Impact ===\n")
cat("Total significant pixels (original):     ", total_orig, "\n")
cat("Total significant pixels (FDR-corrected):", total_fdr, "\n")
cat("Reduction:",
    round(if (total_orig > 0) (total_orig - total_fdr) / total_orig * 100
          else 0, 2), "%\n")

ex_pair <- paste(endogenous_vars[1], exogenous_vars[1], sep = "_x_")
if (ex_pair %in% names(wcc_results)) {
  CairoPNG("FDR_Comparison_Example.png", width = 2400, height = 1200, res = 150)
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 2,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "Original (α = 0.05)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 0,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "FDR-corrected (α = 0.10)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  if (any(fdr_results[[ex_pair]]$fdr_signif, na.rm = TRUE))
    contour(wcc_results[[ex_pair]]$t, wcc_results[[ex_pair]]$period,
            t(fdr_results[[ex_pair]]$fdr_signif),
            levels = 0.5, add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE)
  mtext(paste(endogenous_labels[1], "vs", exogenous_labels[1],
              "- FDR Correction Comparison"),
        outer = TRUE, cex = 1.8, font = 2)
  dev.off()
}

cat("\n=== Done ===\n")
cat("Outputs:\n")
cat(" - WCC plots/grid:  WCC_FDR_Charts_Training/, WCC_Heatmaps_Grid_france_fdr_training_revised.png\n")
cat(" - WCA summaries:   FDR_Correction_Summary_france_training.csv, WCC_Summary_Table_{Original,FDR}_france_training.csv\n")
cat(" - WGC raw:         Wavelet_NL_Granger_Causality_france_training_raw.csv\n")
cat(" - WGC final (FDR): Wavelet_NL_Granger_Causality_france_training.csv  (480 rows)\n")
cat(" - WCA example:     FDR_Comparison_Example.png\n")
cat(" - Checkpoints:     WCA_checkpoints_france/, WGC_checkpoints_france/\n")
################## End of Code: France #################

# =====================================================================
#  GERMANY -- FDR-corrected WCA + Wavelet Granger Causality (WGC)
#  Tests per (Endogenous, Exogenous, Scale, Direction):
#     1. Linear Granger F-test (lmtest::grangertest)
#     2. NN-Granger            (NlinTS::nlin_causality.test)
#  Training window: 1995-01-01 to 2022-03-01
# =====================================================================
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/germany")
getwd()

# ---- Packages -------------------------------------------------------
suppressPackageStartupMessages({
  library(biwavelet); library(wavelets); library(Cairo); library(magick)
  library(dplyr);     library(tidyr);    library(lmtest); library(lubridate)
  library(NlinTS)
})
stopifnot(utils::packageVersion("NlinTS") >= "1.4.0")
cat("NlinTS version:", as.character(utils::packageVersion("NlinTS")), "\n")

# ---- Read and subset data ------------------------------------------
data_ts <- read.csv("all_mulvar_data_germany_v2.csv",
                    header = TRUE, check.names = FALSE)
data_ts$Date <- as.Date(data_ts$Date)

training_start <- as.Date("1995-01-01")
training_end   <- as.Date("2022-03-01")
data_ts_train  <- data_ts[data_ts$Date >= training_start &
                            data_ts$Date <= training_end, ]

cat("Training period:", as.character(min(data_ts_train$Date)), "to",
    as.character(max(data_ts_train$Date)),
    "  (", nrow(data_ts_train), "obs )\n")

# ---- Variable specification ----------------------------------------
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR",
                     "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars  <- c("logEPU", "GPRC", "USEMV", "USMPU")
endogenous_labels <- c("Unemployment Rate", "REER", "SIR",
                       "Oil Price (WTI)", "CPI Inflation")
exogenous_labels  <- c("EPU", "GPR", "USEMV", "USMPU")

n_obs_train         <- nrow(data_ts_train)
start_year_train    <- year(min(data_ts_train$Date))
end_year_train      <- year(max(data_ts_train$Date))
time_sequence_train <- seq_len(n_obs_train)

# =====================================================================
#  PART A -- WAVELET COHERENCE (WCA) with scale-wise FDR correction
# =====================================================================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  n_time  <- ncol(wtc_result$rsq); n_scale <- nrow(wtc_result$rsq)
  p_values   <- matrix(NA, n_scale, n_time)
  fdr_signif <- matrix(FALSE, n_scale, n_time)
  for (i in seq_len(n_scale)) {
    sig_level <- wtc_result$signif[i]
    if (is.na(sig_level) || sig_level == 0) next
    scale_rsq <- wtc_result$rsq[i, ]; scale_p <- rep(NA_real_, n_time)
    for (j in seq_len(n_time)) {
      if (!is.na(scale_rsq[j])) {
        if (scale_rsq[j] >= sig_level)
          scale_p[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        else
          scale_p[j] <- min(1, 1 - scale_rsq[j] / sig_level)
      }
    }
    p_values[i, ] <- scale_p
    valid <- which(!is.na(scale_p))
    if (length(valid) > 0) {
      p_adj <- p.adjust(scale_p[valid], method = "BH")
      fdr_signif[i, valid[p_adj < alpha]] <- TRUE
    }
  }
  list(fdr_signif = fdr_signif, p_values = p_values, alpha = alpha)
}

plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label,
                              n_obs, start_year, end_year,
                              main_title, file_name) {
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey",
       lwd.coi = 2, lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE,
       main = main_title, cex.main = 1.5, font.main = 3, font.lab = 3)
  if (any(fdr_result$fdr_signif, na.rm = TRUE))
    contour(wtc_result$t, wtc_result$period, t(fdr_result$fdr_signif),
            levels = c(0.5), add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE, method = "edge")
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks))
    year_labels <- year_labels[seq_along(year_breaks)]
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  dev.off()
}

output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)
wca_ckpt_dir <- "WCA_checkpoints_germany"
if (!dir.exists(wca_ckpt_dir)) dir.create(wca_ckpt_dir)

wcc_results <- list(); fdr_results <- list(); file_list <- c()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    y_var <- endogenous_vars[i]; x_var <- exogenous_vars[j]
    y_lab <- endogenous_labels[i]; x_lab <- exogenous_labels[j]
    pair_name <- paste(y_var, x_var, sep = "_x_")
    ckpt <- file.path(wca_ckpt_dir, paste0(pair_name, ".rds"))
    
    if (file.exists(ckpt)) {
      cat("WCA: CACHED ", pair_name, "\n")
      blob <- readRDS(ckpt)
      wcc_results[[pair_name]] <- blob$wtc
      fdr_results[[pair_name]] <- blob$fdr
    } else {
      cat("WCA:", pair_name, "\n")
      t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
      t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
      wtc_r <- wtc(t1, t2, nrands = 1000)
      fdr_r <- apply_fdr_correction(wtc_r, alpha = 0.10)
      saveRDS(list(wtc = wtc_r, fdr = fdr_r), ckpt)
      wcc_results[[pair_name]] <- wtc_r
      fdr_results[[pair_name]] <- fdr_r
    }
    
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name),
                       "_fdr_training.png")
    file_list <- c(file_list, out_file)
    plot_wtc_with_fdr(wcc_results[[pair_name]], fdr_results[[pair_name]],
                      y_lab, x_lab, n_obs_train,
                      start_year_train, end_year_train,
                      paste0(y_lab, " vs ", x_lab), out_file)
  }
}

# ---- Grid composite -------------------------------------------------
ordered_files <- c()
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pat <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    hit <- file_list[grepl(pat, file_list)]
    if (length(hit)) ordered_files <- c(ordered_files, hit[1])
  }
rows <- split(ordered_files, ceiling(seq_along(ordered_files) / 4))
row_images <- lapply(rows, function(rf)
  if (length(rf)) image_append(image_join(image_read(rf)), stack = FALSE))
row_images <- row_images[!sapply(row_images, is.null)]
if (length(row_images)) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  image_write(grid_image, "WCC_Heatmaps_Grid_germany_fdr_training_revised.png")
}

# ---- WCA FDR-impact summary ----------------------------------------
fdr_summary <- data.frame(Pair = character(), Total_Tests = integer(),
                          Significant_Original = integer(),
                          Significant_FDR = integer(),
                          FDR_Reduction_Percent = numeric(),
                          stringsAsFactors = FALSE)
for (pn in names(wcc_results)) {
  w <- wcc_results[[pn]]; f <- fdr_results[[pn]]
  orig_sig <- 0
  for (i in seq_len(nrow(w$rsq)))
    if (!is.na(w$signif[i]) && w$signif[i] > 0)
      orig_sig <- orig_sig + sum(w$rsq[i, ] >= w$signif[i], na.rm = TRUE)
  fdr_sig    <- sum(f$fdr_signif, na.rm = TRUE)
  total_test <- sum(!is.na(w$rsq))
  red_pct    <- if (orig_sig > 0) (orig_sig - fdr_sig) / orig_sig * 100 else 0
  fdr_summary <- rbind(fdr_summary, data.frame(
    Pair = pn, Total_Tests = total_test,
    Significant_Original = orig_sig, Significant_FDR = fdr_sig,
    FDR_Reduction_Percent = round(red_pct, 2)))
}
write.csv(fdr_summary, "FDR_Correction_Summary_germany_training.csv",
          row.names = FALSE)

# ---- WCA summary tables --------------------------------------------
label_strength <- function(v) {
  if (is.na(v)) return("NA")
  if (v > 0.50) "Strong" else if (v > 0.25) "Moderate" else "Weak"
}
summary_matrix     <- matrix(NA, length(endogenous_vars), length(exogenous_vars),
                             dimnames = list(endogenous_labels, exogenous_labels))
summary_matrix_fdr <- summary_matrix
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pn <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    w  <- wcc_results[[pn]]; f <- fdr_results[[pn]]
    if (is.null(w)) next
    coi_mask <- matrix(1, nrow(w$rsq), ncol(w$rsq))
    coi_mask[w$coi < w$period] <- NA
    rsq_v <- w$rsq * coi_mask
    avg   <- mean(rsq_v, na.rm = TRUE)
    summary_matrix[endogenous_labels[i], exogenous_labels[j]] <-
      sprintf("%.2f (%s)", avg, label_strength(avg))
    rsq_f <- rsq_v; rsq_f[!f$fdr_signif] <- NA
    avg_f <- mean(rsq_f, na.rm = TRUE)
    summary_matrix_fdr[endogenous_labels[i], exogenous_labels[j]] <-
      if (is.na(avg_f)) "NS"
    else sprintf("%.2f (%s)", avg_f, label_strength(avg_f))
  }
write.csv(cbind(Endogenous = rownames(summary_matrix), summary_matrix),
          "WCC_Summary_Table_Original_germany_training.csv", row.names = FALSE)
write.csv(cbind(Endogenous = rownames(summary_matrix_fdr), summary_matrix_fdr),
          "WCC_Summary_Table_FDR_germany_training.csv", row.names = FALSE)

# =====================================================================
#  PART B -- WAVELET GRANGER CAUSALITY (Linear F + NN-GC only)
# =====================================================================
WAVELET <- "la8"; LEVELS <- 6
LIN_LAGS <- 2;    NN_LAGS <- 2

nn_gc_test <- function(ts_to, ts_from, lags = NN_LAGS,
                       layers_univ = c(2), layers_biv = c(4),
                       iters = 50, lr = 0.01, algo = "sgd",
                       batch = 10, bias = TRUE, seed = 1) {
  ts_to <- as.numeric(ts_to); ts_from <- as.numeric(ts_from)
  res <- NlinTS::nlin_causality.test(
    ts1 = ts_to, ts2 = ts_from, lag = lags,
    LayersUniv = layers_univ, LayersBiv = layers_biv,
    iters = iters, learningRate = lr, algo = algo,
    batch_size = batch, bias = bias, seed = seed)
  stat <- tryCatch(res$Ftest,  error = function(e) NA_real_)
  pval <- tryCatch(res$pvalue, error = function(e) NA_real_)
  if (is.null(stat) || length(stat) == 0) stat <- NA_real_
  if (is.null(pval) || length(pval) == 0) pval <- NA_real_
  c(stat = unname(stat), p = unname(pval))
}

lin_gc_test <- function(ts_to, ts_from, lags = LIN_LAGS) {
  df <- data.frame(Y = as.numeric(ts_to), X = as.numeric(ts_from))
  t  <- lmtest::grangertest(Y ~ X, order = lags, data = df)
  c(stat = unname(t$F[2]), p = unname(t$`Pr(>F)`[2]))
}

safe_run <- function(label, expr)
  tryCatch(expr, error = function(e) {
    message(sprintf("  [WARN] %s failed: %s", label, conditionMessage(e)))
    c(stat = NA_real_, p = NA_real_)
  })

wavelet_gc_full <- function(ts_y, ts_x, y_label, x_label,
                            wavelet = WAVELET, levels = LEVELS,
                            lags = LIN_LAGS) {
  m_y <- wavelets::modwt(as.numeric(ts_y), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  m_x <- wavelets::modwt(as.numeric(ts_x), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  n_orig <- length(ts_y); rows <- list()
  for (i in seq_len(levels)) {
    dy <- as.numeric(m_y@W[[i]]); dx <- as.numeric(m_x@W[[i]])
    if (length(dy) > n_orig) dy <- dy[seq_len(n_orig)]
    if (length(dx) > n_orig) dx <- dx[seq_len(n_orig)]
    df <- na.omit(data.frame(Y = dy, X = dx))
    sc <- paste0("D", i); enough <- nrow(df) > (3 * lags + 5)
    if (enough) {
      lin_fwd <- safe_run(sprintf("Lin X->Y %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$Y, df$X, lags))
      lin_rev <- safe_run(sprintf("Lin Y->X %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$X, df$Y, lags))
      nn_fwd  <- safe_run(sprintf("NN  X->Y %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$Y, df$X, lags))
      nn_rev  <- safe_run(sprintf("NN  Y->X %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$X, df$Y, lags))
    } else {
      lin_fwd <- lin_rev <- nn_fwd <- nn_rev <-
        c(stat = NA_real_, p = NA_real_)
    }
    mk <- function(dir, test, r) data.frame(
      Endogenous = y_label, Exogenous = x_label, Scale = sc,
      Direction = dir, Test = test,
      Statistic = round(unname(r["stat"]), 4),
      p_value   = round(unname(r["p"]),    4),
      stringsAsFactors = FALSE)
    rows[[length(rows)+1]] <- mk("X_to_Y","Linear_F", lin_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","Linear_F", lin_rev)
    rows[[length(rows)+1]] <- mk("X_to_Y","NN_GC",    nn_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","NN_GC",    nn_rev)
  }
  do.call(rbind, rows)
}

# ---- Run all 20 pairs with checkpointing ---------------------------
wgc_ckpt_dir <- "WGC_checkpoints_germany"
if (!dir.exists(wgc_ckpt_dir)) dir.create(wgc_ckpt_dir)

results_list <- list(); pair_id <- 0
total_pairs  <- length(endogenous_vars) * length(exogenous_vars)
t0 <- Sys.time()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    pair_id <- pair_id + 1
    ckpt_file <- file.path(wgc_ckpt_dir,
                           sprintf("pair_%02d_%s_x_%s.rds", pair_id,
                                   endogenous_vars[i], exogenous_vars[j]))
    if (file.exists(ckpt_file)) {
      cat(sprintf("[%2d/%d] %-22s <- %-6s  CACHED\n",
                  pair_id, total_pairs,
                  endogenous_labels[i], exogenous_labels[j]))
      results_list[[length(results_list)+1]] <- readRDS(ckpt_file)
      next
    }
    tp <- Sys.time()
    cat(sprintf("[%2d/%d] %-22s <- %-6s  ... ",
                pair_id, total_pairs,
                endogenous_labels[i], exogenous_labels[j]))
    flush.console()
    res <- wavelet_gc_full(
      ts_y    = data_ts_train[[endogenous_vars[i]]],
      ts_x    = data_ts_train[[exogenous_vars[j]]],
      y_label = endogenous_labels[i],
      x_label = exogenous_labels[j])
    saveRDS(res, ckpt_file)
    results_list[[length(results_list)+1]] <- res
    cat(sprintf("done in %.1f sec\n",
                as.numeric(difftime(Sys.time(), tp, units = "secs"))))
    flush.console()
  }
}
cat("WGC total elapsed:",
    round(difftime(Sys.time(), t0, units = "mins"), 2), "minutes\n")

wgc_all <- do.call(rbind, results_list)

# Raw dump before FDR (safety net)
write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_germany_training_raw.csv",
          row.names = FALSE)

# ---- BH-FDR within (Test, Direction) -------------------------------
wgc_all <- wgc_all %>%
  group_by(Test, Direction) %>%
  mutate(p_value_fdr = p.adjust(p_value, method = "BH")) %>%
  ungroup() %>%
  mutate(Significant_Original = ifelse(is.na(p_value), NA,
                                       ifelse(p_value     < 0.10, "Yes", "No")),
         Significant_FDR      = ifelse(is.na(p_value_fdr), NA,
                                       ifelse(p_value_fdr < 0.10, "Yes", "No")),
         p_value_fdr = round(p_value_fdr, 4)) %>%
  as.data.frame()

write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_germany_training.csv",
          row.names = FALSE)

# ---- Console summaries ---------------------------------------------
cat("\n--- WGC significance (FDR q < 0.10) by Test x Direction ---\n")
print(wgc_all %>% group_by(Test, Direction) %>%
        summarise(N = n(),
                  N_valid = sum(!is.na(p_value)),
                  Sig_orig = sum(Significant_Original == "Yes", na.rm = TRUE),
                  Sig_FDR  = sum(Significant_FDR      == "Yes", na.rm = TRUE),
                  .groups = "drop"))

cat("\n--- WGC significance by Endogenous x Test (FDR) ---\n")
print(wgc_all %>% group_by(Endogenous, Test) %>%
        summarise(Sig_FDR = sum(Significant_FDR == "Yes", na.rm = TRUE),
                  .groups = "drop") %>%
        pivot_wider(names_from = Test, values_from = Sig_FDR,
                    values_fill = 0))

# =====================================================================
#  PART C -- WCA FDR-impact reporting & example comparison plot
# =====================================================================
total_orig <- sum(fdr_summary$Significant_Original)
total_fdr  <- sum(fdr_summary$Significant_FDR)
cat("\n=== WCA FDR Impact ===\n")
cat("Total significant pixels (original):     ", total_orig, "\n")
cat("Total significant pixels (FDR-corrected):", total_fdr, "\n")
cat("Reduction:",
    round(if (total_orig > 0) (total_orig - total_fdr) / total_orig * 100
          else 0, 2), "%\n")

ex_pair <- paste(endogenous_vars[1], exogenous_vars[1], sep = "_x_")
if (ex_pair %in% names(wcc_results)) {
  CairoPNG("FDR_Comparison_Example.png", width = 2400, height = 1200, res = 150)
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 2,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "Original (α = 0.05)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 0,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "FDR-corrected (α = 0.10)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  if (any(fdr_results[[ex_pair]]$fdr_signif, na.rm = TRUE))
    contour(wcc_results[[ex_pair]]$t, wcc_results[[ex_pair]]$period,
            t(fdr_results[[ex_pair]]$fdr_signif),
            levels = 0.5, add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE)
  mtext(paste(endogenous_labels[1], "vs", exogenous_labels[1],
              "- FDR Correction Comparison"),
        outer = TRUE, cex = 1.8, font = 2)
  dev.off()
}

cat("\n=== Done ===\n")
cat("Outputs:\n")
cat(" - WCC plots/grid:  WCC_FDR_Charts_Training/, WCC_Heatmaps_Grid_germany_fdr_training_revised.png\n")
cat(" - WCA summaries:   FDR_Correction_Summary_germany_training.csv, WCC_Summary_Table_{Original,FDR}_germany_training.csv\n")
cat(" - WGC raw:         Wavelet_NL_Granger_Causality_germany_training_raw.csv\n")
cat(" - WGC final (FDR): Wavelet_NL_Granger_Causality_germany_training.csv  (480 rows)\n")
cat(" - WCA example:     FDR_Comparison_Example.png\n")
cat(" - Checkpoints:     WCA_checkpoints_germany/, WGC_checkpoints_germany/\n")
################## End of Code: Germany #################

# =====================================================================
#  JAPAN -- FDR-corrected WCA + Wavelet Granger Causality (WGC)
#  Tests per (Endogenous, Exogenous, Scale, Direction):
#     1. Linear Granger F-test (lmtest::grangertest)
#     2. NN-Granger            (NlinTS::nlin_causality.test)
#  Training window: 1995-01-01 to 2022-03-01
# =====================================================================
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/japan")
getwd()

# ---- Packages -------------------------------------------------------
suppressPackageStartupMessages({
  library(biwavelet); library(wavelets); library(Cairo); library(magick)
  library(dplyr);     library(tidyr);    library(lmtest); library(lubridate)
  library(NlinTS)
})
stopifnot(utils::packageVersion("NlinTS") >= "1.4.0")
cat("NlinTS version:", as.character(utils::packageVersion("NlinTS")), "\n")

# ---- Read and subset data ------------------------------------------
data_ts <- read.csv("all_mulvar_data_japan_v2.csv",
                    header = TRUE, check.names = FALSE)
data_ts$Date <- as.Date(data_ts$Date)

training_start <- as.Date("1995-01-01")
training_end   <- as.Date("2022-03-01")
data_ts_train  <- data_ts[data_ts$Date >= training_start &
                            data_ts$Date <= training_end, ]

cat("Training period:", as.character(min(data_ts_train$Date)), "to",
    as.character(max(data_ts_train$Date)),
    "  (", nrow(data_ts_train), "obs )\n")

# ---- Variable specification ----------------------------------------
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR",
                     "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars  <- c("logEPU", "GPRC", "USEMV", "USMPU")
endogenous_labels <- c("Unemployment Rate", "REER", "SIR",
                       "Oil Price (WTI)", "CPI Inflation")
exogenous_labels  <- c("EPU", "GPR", "USEMV", "USMPU")

n_obs_train         <- nrow(data_ts_train)
start_year_train    <- year(min(data_ts_train$Date))
end_year_train      <- year(max(data_ts_train$Date))
time_sequence_train <- seq_len(n_obs_train)

# =====================================================================
#  PART A -- WAVELET COHERENCE (WCA) with scale-wise FDR correction
# =====================================================================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  n_time  <- ncol(wtc_result$rsq); n_scale <- nrow(wtc_result$rsq)
  p_values   <- matrix(NA, n_scale, n_time)
  fdr_signif <- matrix(FALSE, n_scale, n_time)
  for (i in seq_len(n_scale)) {
    sig_level <- wtc_result$signif[i]
    if (is.na(sig_level) || sig_level == 0) next
    scale_rsq <- wtc_result$rsq[i, ]; scale_p <- rep(NA_real_, n_time)
    for (j in seq_len(n_time)) {
      if (!is.na(scale_rsq[j])) {
        if (scale_rsq[j] >= sig_level)
          scale_p[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        else
          scale_p[j] <- min(1, 1 - scale_rsq[j] / sig_level)
      }
    }
    p_values[i, ] <- scale_p
    valid <- which(!is.na(scale_p))
    if (length(valid) > 0) {
      p_adj <- p.adjust(scale_p[valid], method = "BH")
      fdr_signif[i, valid[p_adj < alpha]] <- TRUE
    }
  }
  list(fdr_signif = fdr_signif, p_values = p_values, alpha = alpha)
}

plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label,
                              n_obs, start_year, end_year,
                              main_title, file_name) {
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey",
       lwd.coi = 2, lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE,
       main = main_title, cex.main = 1.5, font.main = 3, font.lab = 3)
  if (any(fdr_result$fdr_signif, na.rm = TRUE))
    contour(wtc_result$t, wtc_result$period, t(fdr_result$fdr_signif),
            levels = c(0.5), add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE, method = "edge")
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks))
    year_labels <- year_labels[seq_along(year_breaks)]
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  dev.off()
}

output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)
wca_ckpt_dir <- "WCA_checkpoints_japan"
if (!dir.exists(wca_ckpt_dir)) dir.create(wca_ckpt_dir)

wcc_results <- list(); fdr_results <- list(); file_list <- c()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    y_var <- endogenous_vars[i]; x_var <- exogenous_vars[j]
    y_lab <- endogenous_labels[i]; x_lab <- exogenous_labels[j]
    pair_name <- paste(y_var, x_var, sep = "_x_")
    ckpt <- file.path(wca_ckpt_dir, paste0(pair_name, ".rds"))
    
    if (file.exists(ckpt)) {
      cat("WCA: CACHED ", pair_name, "\n")
      blob <- readRDS(ckpt)
      wcc_results[[pair_name]] <- blob$wtc
      fdr_results[[pair_name]] <- blob$fdr
    } else {
      cat("WCA:", pair_name, "\n")
      t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
      t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
      wtc_r <- wtc(t1, t2, nrands = 1000)
      fdr_r <- apply_fdr_correction(wtc_r, alpha = 0.10)
      saveRDS(list(wtc = wtc_r, fdr = fdr_r), ckpt)
      wcc_results[[pair_name]] <- wtc_r
      fdr_results[[pair_name]] <- fdr_r
    }
    
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name),
                       "_fdr_training.png")
    file_list <- c(file_list, out_file)
    plot_wtc_with_fdr(wcc_results[[pair_name]], fdr_results[[pair_name]],
                      y_lab, x_lab, n_obs_train,
                      start_year_train, end_year_train,
                      paste0(y_lab, " vs ", x_lab), out_file)
  }
}

# ---- Grid composite -------------------------------------------------
ordered_files <- c()
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pat <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    hit <- file_list[grepl(pat, file_list)]
    if (length(hit)) ordered_files <- c(ordered_files, hit[1])
  }
rows <- split(ordered_files, ceiling(seq_along(ordered_files) / 4))
row_images <- lapply(rows, function(rf)
  if (length(rf)) image_append(image_join(image_read(rf)), stack = FALSE))
row_images <- row_images[!sapply(row_images, is.null)]
if (length(row_images)) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  image_write(grid_image, "WCC_Heatmaps_Grid_japan_fdr_training_revised.png")
}

# ---- WCA FDR-impact summary ----------------------------------------
fdr_summary <- data.frame(Pair = character(), Total_Tests = integer(),
                          Significant_Original = integer(),
                          Significant_FDR = integer(),
                          FDR_Reduction_Percent = numeric(),
                          stringsAsFactors = FALSE)
for (pn in names(wcc_results)) {
  w <- wcc_results[[pn]]; f <- fdr_results[[pn]]
  orig_sig <- 0
  for (i in seq_len(nrow(w$rsq)))
    if (!is.na(w$signif[i]) && w$signif[i] > 0)
      orig_sig <- orig_sig + sum(w$rsq[i, ] >= w$signif[i], na.rm = TRUE)
  fdr_sig    <- sum(f$fdr_signif, na.rm = TRUE)
  total_test <- sum(!is.na(w$rsq))
  red_pct    <- if (orig_sig > 0) (orig_sig - fdr_sig) / orig_sig * 100 else 0
  fdr_summary <- rbind(fdr_summary, data.frame(
    Pair = pn, Total_Tests = total_test,
    Significant_Original = orig_sig, Significant_FDR = fdr_sig,
    FDR_Reduction_Percent = round(red_pct, 2)))
}
write.csv(fdr_summary, "FDR_Correction_Summary_japan_training.csv",
          row.names = FALSE)

# ---- WCA summary tables --------------------------------------------
label_strength <- function(v) {
  if (is.na(v)) return("NA")
  if (v > 0.50) "Strong" else if (v > 0.25) "Moderate" else "Weak"
}
summary_matrix     <- matrix(NA, length(endogenous_vars), length(exogenous_vars),
                             dimnames = list(endogenous_labels, exogenous_labels))
summary_matrix_fdr <- summary_matrix
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pn <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    w  <- wcc_results[[pn]]; f <- fdr_results[[pn]]
    if (is.null(w)) next
    coi_mask <- matrix(1, nrow(w$rsq), ncol(w$rsq))
    coi_mask[w$coi < w$period] <- NA
    rsq_v <- w$rsq * coi_mask
    avg   <- mean(rsq_v, na.rm = TRUE)
    summary_matrix[endogenous_labels[i], exogenous_labels[j]] <-
      sprintf("%.2f (%s)", avg, label_strength(avg))
    rsq_f <- rsq_v; rsq_f[!f$fdr_signif] <- NA
    avg_f <- mean(rsq_f, na.rm = TRUE)
    summary_matrix_fdr[endogenous_labels[i], exogenous_labels[j]] <-
      if (is.na(avg_f)) "NS"
    else sprintf("%.2f (%s)", avg_f, label_strength(avg_f))
  }
write.csv(cbind(Endogenous = rownames(summary_matrix), summary_matrix),
          "WCC_Summary_Table_Original_japan_training.csv", row.names = FALSE)
write.csv(cbind(Endogenous = rownames(summary_matrix_fdr), summary_matrix_fdr),
          "WCC_Summary_Table_FDR_japan_training.csv", row.names = FALSE)

# =====================================================================
#  PART B -- WAVELET GRANGER CAUSALITY (Linear F + NN-GC only)
# =====================================================================
WAVELET <- "la8"; LEVELS <- 6
LIN_LAGS <- 2;    NN_LAGS <- 2

nn_gc_test <- function(ts_to, ts_from, lags = NN_LAGS,
                       layers_univ = c(2), layers_biv = c(4),
                       iters = 50, lr = 0.01, algo = "sgd",
                       batch = 10, bias = TRUE, seed = 1) {
  ts_to <- as.numeric(ts_to); ts_from <- as.numeric(ts_from)
  res <- NlinTS::nlin_causality.test(
    ts1 = ts_to, ts2 = ts_from, lag = lags,
    LayersUniv = layers_univ, LayersBiv = layers_biv,
    iters = iters, learningRate = lr, algo = algo,
    batch_size = batch, bias = bias, seed = seed)
  stat <- tryCatch(res$Ftest,  error = function(e) NA_real_)
  pval <- tryCatch(res$pvalue, error = function(e) NA_real_)
  if (is.null(stat) || length(stat) == 0) stat <- NA_real_
  if (is.null(pval) || length(pval) == 0) pval <- NA_real_
  c(stat = unname(stat), p = unname(pval))
}

lin_gc_test <- function(ts_to, ts_from, lags = LIN_LAGS) {
  df <- data.frame(Y = as.numeric(ts_to), X = as.numeric(ts_from))
  t  <- lmtest::grangertest(Y ~ X, order = lags, data = df)
  c(stat = unname(t$F[2]), p = unname(t$`Pr(>F)`[2]))
}

safe_run <- function(label, expr)
  tryCatch(expr, error = function(e) {
    message(sprintf("  [WARN] %s failed: %s", label, conditionMessage(e)))
    c(stat = NA_real_, p = NA_real_)
  })

wavelet_gc_full <- function(ts_y, ts_x, y_label, x_label,
                            wavelet = WAVELET, levels = LEVELS,
                            lags = LIN_LAGS) {
  m_y <- wavelets::modwt(as.numeric(ts_y), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  m_x <- wavelets::modwt(as.numeric(ts_x), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  n_orig <- length(ts_y); rows <- list()
  for (i in seq_len(levels)) {
    dy <- as.numeric(m_y@W[[i]]); dx <- as.numeric(m_x@W[[i]])
    if (length(dy) > n_orig) dy <- dy[seq_len(n_orig)]
    if (length(dx) > n_orig) dx <- dx[seq_len(n_orig)]
    df <- na.omit(data.frame(Y = dy, X = dx))
    sc <- paste0("D", i); enough <- nrow(df) > (3 * lags + 5)
    if (enough) {
      lin_fwd <- safe_run(sprintf("Lin X->Y %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$Y, df$X, lags))
      lin_rev <- safe_run(sprintf("Lin Y->X %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$X, df$Y, lags))
      nn_fwd  <- safe_run(sprintf("NN  X->Y %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$Y, df$X, lags))
      nn_rev  <- safe_run(sprintf("NN  Y->X %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$X, df$Y, lags))
    } else {
      lin_fwd <- lin_rev <- nn_fwd <- nn_rev <-
        c(stat = NA_real_, p = NA_real_)
    }
    mk <- function(dir, test, r) data.frame(
      Endogenous = y_label, Exogenous = x_label, Scale = sc,
      Direction = dir, Test = test,
      Statistic = round(unname(r["stat"]), 4),
      p_value   = round(unname(r["p"]),    4),
      stringsAsFactors = FALSE)
    rows[[length(rows)+1]] <- mk("X_to_Y","Linear_F", lin_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","Linear_F", lin_rev)
    rows[[length(rows)+1]] <- mk("X_to_Y","NN_GC",    nn_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","NN_GC",    nn_rev)
  }
  do.call(rbind, rows)
}

# ---- Run all 20 pairs with checkpointing ---------------------------
wgc_ckpt_dir <- "WGC_checkpoints_japan"
if (!dir.exists(wgc_ckpt_dir)) dir.create(wgc_ckpt_dir)

results_list <- list(); pair_id <- 0
total_pairs  <- length(endogenous_vars) * length(exogenous_vars)
t0 <- Sys.time()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    pair_id <- pair_id + 1
    ckpt_file <- file.path(wgc_ckpt_dir,
                           sprintf("pair_%02d_%s_x_%s.rds", pair_id,
                                   endogenous_vars[i], exogenous_vars[j]))
    if (file.exists(ckpt_file)) {
      cat(sprintf("[%2d/%d] %-22s <- %-6s  CACHED\n",
                  pair_id, total_pairs,
                  endogenous_labels[i], exogenous_labels[j]))
      results_list[[length(results_list)+1]] <- readRDS(ckpt_file)
      next
    }
    tp <- Sys.time()
    cat(sprintf("[%2d/%d] %-22s <- %-6s  ... ",
                pair_id, total_pairs,
                endogenous_labels[i], exogenous_labels[j]))
    flush.console()
    res <- wavelet_gc_full(
      ts_y    = data_ts_train[[endogenous_vars[i]]],
      ts_x    = data_ts_train[[exogenous_vars[j]]],
      y_label = endogenous_labels[i],
      x_label = exogenous_labels[j])
    saveRDS(res, ckpt_file)
    results_list[[length(results_list)+1]] <- res
    cat(sprintf("done in %.1f sec\n",
                as.numeric(difftime(Sys.time(), tp, units = "secs"))))
    flush.console()
  }
}
cat("WGC total elapsed:",
    round(difftime(Sys.time(), t0, units = "mins"), 2), "minutes\n")

wgc_all <- do.call(rbind, results_list)

# Raw dump before FDR (safety net)
write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_japan_training_raw.csv",
          row.names = FALSE)

# ---- BH-FDR within (Test, Direction) -------------------------------
wgc_all <- wgc_all %>%
  group_by(Test, Direction) %>%
  mutate(p_value_fdr = p.adjust(p_value, method = "BH")) %>%
  ungroup() %>%
  mutate(Significant_Original = ifelse(is.na(p_value), NA,
                                       ifelse(p_value     < 0.10, "Yes", "No")),
         Significant_FDR      = ifelse(is.na(p_value_fdr), NA,
                                       ifelse(p_value_fdr < 0.10, "Yes", "No")),
         p_value_fdr = round(p_value_fdr, 4)) %>%
  as.data.frame()

write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_japan_training.csv",
          row.names = FALSE)

# ---- Console summaries ---------------------------------------------
cat("\n--- WGC significance (FDR q < 0.10) by Test x Direction ---\n")
print(wgc_all %>% group_by(Test, Direction) %>%
        summarise(N = n(),
                  N_valid = sum(!is.na(p_value)),
                  Sig_orig = sum(Significant_Original == "Yes", na.rm = TRUE),
                  Sig_FDR  = sum(Significant_FDR      == "Yes", na.rm = TRUE),
                  .groups = "drop"))

cat("\n--- WGC significance by Endogenous x Test (FDR) ---\n")
print(wgc_all %>% group_by(Endogenous, Test) %>%
        summarise(Sig_FDR = sum(Significant_FDR == "Yes", na.rm = TRUE),
                  .groups = "drop") %>%
        pivot_wider(names_from = Test, values_from = Sig_FDR,
                    values_fill = 0))

# =====================================================================
#  PART C -- WCA FDR-impact reporting & example comparison plot
# =====================================================================
total_orig <- sum(fdr_summary$Significant_Original)
total_fdr  <- sum(fdr_summary$Significant_FDR)
cat("\n=== WCA FDR Impact ===\n")
cat("Total significant pixels (original):     ", total_orig, "\n")
cat("Total significant pixels (FDR-corrected):", total_fdr, "\n")
cat("Reduction:",
    round(if (total_orig > 0) (total_orig - total_fdr) / total_orig * 100
          else 0, 2), "%\n")

ex_pair <- paste(endogenous_vars[1], exogenous_vars[1], sep = "_x_")
if (ex_pair %in% names(wcc_results)) {
  CairoPNG("FDR_Comparison_Example.png", width = 2400, height = 1200, res = 150)
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 2,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "Original (α = 0.05)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 0,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "FDR-corrected (α = 0.10)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  if (any(fdr_results[[ex_pair]]$fdr_signif, na.rm = TRUE))
    contour(wcc_results[[ex_pair]]$t, wcc_results[[ex_pair]]$period,
            t(fdr_results[[ex_pair]]$fdr_signif),
            levels = 0.5, add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE)
  mtext(paste(endogenous_labels[1], "vs", exogenous_labels[1],
              "- FDR Correction Comparison"),
        outer = TRUE, cex = 1.8, font = 2)
  dev.off()
}

cat("\n=== Done ===\n")
cat("Outputs:\n")
cat(" - WCC plots/grid:  WCC_FDR_Charts_Training/, WCC_Heatmaps_Grid_japan_fdr_training_revised.png\n")
cat(" - WCA summaries:   FDR_Correction_Summary_japan_training.csv, WCC_Summary_Table_{Original,FDR}_japan_training.csv\n")
cat(" - WGC raw:         Wavelet_NL_Granger_Causality_japan_training_raw.csv\n")
cat(" - WGC final (FDR): Wavelet_NL_Granger_Causality_japan_training.csv  (480 rows)\n")
cat(" - WCA example:     FDR_Comparison_Example.png\n")
cat(" - Checkpoints:     WCA_checkpoints_japan/, WGC_checkpoints_japan/\n")
################## End of Code: Japan #################

# =====================================================================
#  UK -- FDR-corrected WCA + Wavelet Granger Causality (WGC)
#  Tests per (Endogenous, Exogenous, Scale, Direction):
#     1. Linear Granger F-test (lmtest::grangertest)
#     2. NN-Granger            (NlinTS::nlin_causality.test)
#  Training window: 1995-01-01 to 2022-03-01
# =====================================================================
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/uk")
getwd()

# ---- Packages -------------------------------------------------------
suppressPackageStartupMessages({
  library(biwavelet); library(wavelets); library(Cairo); library(magick)
  library(dplyr);     library(tidyr);    library(lmtest); library(lubridate)
  library(NlinTS)
})
stopifnot(utils::packageVersion("NlinTS") >= "1.4.0")
cat("NlinTS version:", as.character(utils::packageVersion("NlinTS")), "\n")

# ---- Read and subset data ------------------------------------------
data_ts <- read.csv("all_mulvar_data_uk_v2.csv",
                    header = TRUE, check.names = FALSE)
data_ts$Date <- as.Date(data_ts$Date)

training_start <- as.Date("1995-01-01")
training_end   <- as.Date("2022-03-01")
data_ts_train  <- data_ts[data_ts$Date >= training_start &
                            data_ts$Date <= training_end, ]

cat("Training period:", as.character(min(data_ts_train$Date)), "to",
    as.character(max(data_ts_train$Date)),
    "  (", nrow(data_ts_train), "obs )\n")

# ---- Variable specification ----------------------------------------
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR",
                     "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars  <- c("logEPU", "GPRC", "USEMV", "USMPU")
endogenous_labels <- c("Unemployment Rate", "REER", "SIR",
                       "Oil Price (WTI)", "CPI Inflation")
exogenous_labels  <- c("EPU", "GPR", "USEMV", "USMPU")

n_obs_train         <- nrow(data_ts_train)
start_year_train    <- year(min(data_ts_train$Date))
end_year_train      <- year(max(data_ts_train$Date))
time_sequence_train <- seq_len(n_obs_train)

# =====================================================================
#  PART A -- WAVELET COHERENCE (WCA) with scale-wise FDR correction
# =====================================================================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  n_time  <- ncol(wtc_result$rsq); n_scale <- nrow(wtc_result$rsq)
  p_values   <- matrix(NA, n_scale, n_time)
  fdr_signif <- matrix(FALSE, n_scale, n_time)
  for (i in seq_len(n_scale)) {
    sig_level <- wtc_result$signif[i]
    if (is.na(sig_level) || sig_level == 0) next
    scale_rsq <- wtc_result$rsq[i, ]; scale_p <- rep(NA_real_, n_time)
    for (j in seq_len(n_time)) {
      if (!is.na(scale_rsq[j])) {
        if (scale_rsq[j] >= sig_level)
          scale_p[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        else
          scale_p[j] <- min(1, 1 - scale_rsq[j] / sig_level)
      }
    }
    p_values[i, ] <- scale_p
    valid <- which(!is.na(scale_p))
    if (length(valid) > 0) {
      p_adj <- p.adjust(scale_p[valid], method = "BH")
      fdr_signif[i, valid[p_adj < alpha]] <- TRUE
    }
  }
  list(fdr_signif = fdr_signif, p_values = p_values, alpha = alpha)
}

plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label,
                              n_obs, start_year, end_year,
                              main_title, file_name) {
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey",
       lwd.coi = 2, lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE,
       main = main_title, cex.main = 1.5, font.main = 3, font.lab = 3)
  if (any(fdr_result$fdr_signif, na.rm = TRUE))
    contour(wtc_result$t, wtc_result$period, t(fdr_result$fdr_signif),
            levels = c(0.5), add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE, method = "edge")
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks))
    year_labels <- year_labels[seq_along(year_breaks)]
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  dev.off()
}

output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)
wca_ckpt_dir <- "WCA_checkpoints_uk"
if (!dir.exists(wca_ckpt_dir)) dir.create(wca_ckpt_dir)

wcc_results <- list(); fdr_results <- list(); file_list <- c()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    y_var <- endogenous_vars[i]; x_var <- exogenous_vars[j]
    y_lab <- endogenous_labels[i]; x_lab <- exogenous_labels[j]
    pair_name <- paste(y_var, x_var, sep = "_x_")
    ckpt <- file.path(wca_ckpt_dir, paste0(pair_name, ".rds"))
    
    if (file.exists(ckpt)) {
      cat("WCA: CACHED ", pair_name, "\n")
      blob <- readRDS(ckpt)
      wcc_results[[pair_name]] <- blob$wtc
      fdr_results[[pair_name]] <- blob$fdr
    } else {
      cat("WCA:", pair_name, "\n")
      t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
      t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
      wtc_r <- wtc(t1, t2, nrands = 1000)
      fdr_r <- apply_fdr_correction(wtc_r, alpha = 0.10)
      saveRDS(list(wtc = wtc_r, fdr = fdr_r), ckpt)
      wcc_results[[pair_name]] <- wtc_r
      fdr_results[[pair_name]] <- fdr_r
    }
    
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name),
                       "_fdr_training.png")
    file_list <- c(file_list, out_file)
    plot_wtc_with_fdr(wcc_results[[pair_name]], fdr_results[[pair_name]],
                      y_lab, x_lab, n_obs_train,
                      start_year_train, end_year_train,
                      paste0(y_lab, " vs ", x_lab), out_file)
  }
}

# ---- Grid composite -------------------------------------------------
ordered_files <- c()
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pat <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    hit <- file_list[grepl(pat, file_list)]
    if (length(hit)) ordered_files <- c(ordered_files, hit[1])
  }
rows <- split(ordered_files, ceiling(seq_along(ordered_files) / 4))
row_images <- lapply(rows, function(rf)
  if (length(rf)) image_append(image_join(image_read(rf)), stack = FALSE))
row_images <- row_images[!sapply(row_images, is.null)]
if (length(row_images)) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  image_write(grid_image, "WCC_Heatmaps_Grid_uk_fdr_training_revised.png")
}

# ---- WCA FDR-impact summary ----------------------------------------
fdr_summary <- data.frame(Pair = character(), Total_Tests = integer(),
                          Significant_Original = integer(),
                          Significant_FDR = integer(),
                          FDR_Reduction_Percent = numeric(),
                          stringsAsFactors = FALSE)
for (pn in names(wcc_results)) {
  w <- wcc_results[[pn]]; f <- fdr_results[[pn]]
  orig_sig <- 0
  for (i in seq_len(nrow(w$rsq)))
    if (!is.na(w$signif[i]) && w$signif[i] > 0)
      orig_sig <- orig_sig + sum(w$rsq[i, ] >= w$signif[i], na.rm = TRUE)
  fdr_sig    <- sum(f$fdr_signif, na.rm = TRUE)
  total_test <- sum(!is.na(w$rsq))
  red_pct    <- if (orig_sig > 0) (orig_sig - fdr_sig) / orig_sig * 100 else 0
  fdr_summary <- rbind(fdr_summary, data.frame(
    Pair = pn, Total_Tests = total_test,
    Significant_Original = orig_sig, Significant_FDR = fdr_sig,
    FDR_Reduction_Percent = round(red_pct, 2)))
}
write.csv(fdr_summary, "FDR_Correction_Summary_uk_training.csv",
          row.names = FALSE)

# ---- WCA summary tables --------------------------------------------
label_strength <- function(v) {
  if (is.na(v)) return("NA")
  if (v > 0.50) "Strong" else if (v > 0.25) "Moderate" else "Weak"
}
summary_matrix     <- matrix(NA, length(endogenous_vars), length(exogenous_vars),
                             dimnames = list(endogenous_labels, exogenous_labels))
summary_matrix_fdr <- summary_matrix
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pn <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    w  <- wcc_results[[pn]]; f <- fdr_results[[pn]]
    if (is.null(w)) next
    coi_mask <- matrix(1, nrow(w$rsq), ncol(w$rsq))
    coi_mask[w$coi < w$period] <- NA
    rsq_v <- w$rsq * coi_mask
    avg   <- mean(rsq_v, na.rm = TRUE)
    summary_matrix[endogenous_labels[i], exogenous_labels[j]] <-
      sprintf("%.2f (%s)", avg, label_strength(avg))
    rsq_f <- rsq_v; rsq_f[!f$fdr_signif] <- NA
    avg_f <- mean(rsq_f, na.rm = TRUE)
    summary_matrix_fdr[endogenous_labels[i], exogenous_labels[j]] <-
      if (is.na(avg_f)) "NS"
    else sprintf("%.2f (%s)", avg_f, label_strength(avg_f))
  }
write.csv(cbind(Endogenous = rownames(summary_matrix), summary_matrix),
          "WCC_Summary_Table_Original_uk_training.csv", row.names = FALSE)
write.csv(cbind(Endogenous = rownames(summary_matrix_fdr), summary_matrix_fdr),
          "WCC_Summary_Table_FDR_uk_training.csv", row.names = FALSE)

# =====================================================================
#  PART B -- WAVELET GRANGER CAUSALITY (Linear F + NN-GC only)
# =====================================================================
WAVELET <- "la8"; LEVELS <- 6
LIN_LAGS <- 2;    NN_LAGS <- 2

nn_gc_test <- function(ts_to, ts_from, lags = NN_LAGS,
                       layers_univ = c(2), layers_biv = c(4),
                       iters = 50, lr = 0.01, algo = "sgd",
                       batch = 10, bias = TRUE, seed = 1) {
  ts_to <- as.numeric(ts_to); ts_from <- as.numeric(ts_from)
  res <- NlinTS::nlin_causality.test(
    ts1 = ts_to, ts2 = ts_from, lag = lags,
    LayersUniv = layers_univ, LayersBiv = layers_biv,
    iters = iters, learningRate = lr, algo = algo,
    batch_size = batch, bias = bias, seed = seed)
  stat <- tryCatch(res$Ftest,  error = function(e) NA_real_)
  pval <- tryCatch(res$pvalue, error = function(e) NA_real_)
  if (is.null(stat) || length(stat) == 0) stat <- NA_real_
  if (is.null(pval) || length(pval) == 0) pval <- NA_real_
  c(stat = unname(stat), p = unname(pval))
}

lin_gc_test <- function(ts_to, ts_from, lags = LIN_LAGS) {
  df <- data.frame(Y = as.numeric(ts_to), X = as.numeric(ts_from))
  t  <- lmtest::grangertest(Y ~ X, order = lags, data = df)
  c(stat = unname(t$F[2]), p = unname(t$`Pr(>F)`[2]))
}

safe_run <- function(label, expr)
  tryCatch(expr, error = function(e) {
    message(sprintf("  [WARN] %s failed: %s", label, conditionMessage(e)))
    c(stat = NA_real_, p = NA_real_)
  })

wavelet_gc_full <- function(ts_y, ts_x, y_label, x_label,
                            wavelet = WAVELET, levels = LEVELS,
                            lags = LIN_LAGS) {
  m_y <- wavelets::modwt(as.numeric(ts_y), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  m_x <- wavelets::modwt(as.numeric(ts_x), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  n_orig <- length(ts_y); rows <- list()
  for (i in seq_len(levels)) {
    dy <- as.numeric(m_y@W[[i]]); dx <- as.numeric(m_x@W[[i]])
    if (length(dy) > n_orig) dy <- dy[seq_len(n_orig)]
    if (length(dx) > n_orig) dx <- dx[seq_len(n_orig)]
    df <- na.omit(data.frame(Y = dy, X = dx))
    sc <- paste0("D", i); enough <- nrow(df) > (3 * lags + 5)
    if (enough) {
      lin_fwd <- safe_run(sprintf("Lin X->Y %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$Y, df$X, lags))
      lin_rev <- safe_run(sprintf("Lin Y->X %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$X, df$Y, lags))
      nn_fwd  <- safe_run(sprintf("NN  X->Y %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$Y, df$X, lags))
      nn_rev  <- safe_run(sprintf("NN  Y->X %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$X, df$Y, lags))
    } else {
      lin_fwd <- lin_rev <- nn_fwd <- nn_rev <-
        c(stat = NA_real_, p = NA_real_)
    }
    mk <- function(dir, test, r) data.frame(
      Endogenous = y_label, Exogenous = x_label, Scale = sc,
      Direction = dir, Test = test,
      Statistic = round(unname(r["stat"]), 4),
      p_value   = round(unname(r["p"]),    4),
      stringsAsFactors = FALSE)
    rows[[length(rows)+1]] <- mk("X_to_Y","Linear_F", lin_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","Linear_F", lin_rev)
    rows[[length(rows)+1]] <- mk("X_to_Y","NN_GC",    nn_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","NN_GC",    nn_rev)
  }
  do.call(rbind, rows)
}

# ---- Run all 20 pairs with checkpointing ---------------------------
wgc_ckpt_dir <- "WGC_checkpoints_uk"
if (!dir.exists(wgc_ckpt_dir)) dir.create(wgc_ckpt_dir)

results_list <- list(); pair_id <- 0
total_pairs  <- length(endogenous_vars) * length(exogenous_vars)
t0 <- Sys.time()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    pair_id <- pair_id + 1
    ckpt_file <- file.path(wgc_ckpt_dir,
                           sprintf("pair_%02d_%s_x_%s.rds", pair_id,
                                   endogenous_vars[i], exogenous_vars[j]))
    if (file.exists(ckpt_file)) {
      cat(sprintf("[%2d/%d] %-22s <- %-6s  CACHED\n",
                  pair_id, total_pairs,
                  endogenous_labels[i], exogenous_labels[j]))
      results_list[[length(results_list)+1]] <- readRDS(ckpt_file)
      next
    }
    tp <- Sys.time()
    cat(sprintf("[%2d/%d] %-22s <- %-6s  ... ",
                pair_id, total_pairs,
                endogenous_labels[i], exogenous_labels[j]))
    flush.console()
    res <- wavelet_gc_full(
      ts_y    = data_ts_train[[endogenous_vars[i]]],
      ts_x    = data_ts_train[[exogenous_vars[j]]],
      y_label = endogenous_labels[i],
      x_label = exogenous_labels[j])
    saveRDS(res, ckpt_file)
    results_list[[length(results_list)+1]] <- res
    cat(sprintf("done in %.1f sec\n",
                as.numeric(difftime(Sys.time(), tp, units = "secs"))))
    flush.console()
  }
}
cat("WGC total elapsed:",
    round(difftime(Sys.time(), t0, units = "mins"), 2), "minutes\n")

wgc_all <- do.call(rbind, results_list)

# Raw dump before FDR (safety net)
write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_uk_training_raw.csv",
          row.names = FALSE)

# ---- BH-FDR within (Test, Direction) -------------------------------
wgc_all <- wgc_all %>%
  group_by(Test, Direction) %>%
  mutate(p_value_fdr = p.adjust(p_value, method = "BH")) %>%
  ungroup() %>%
  mutate(Significant_Original = ifelse(is.na(p_value), NA,
                                       ifelse(p_value     < 0.10, "Yes", "No")),
         Significant_FDR      = ifelse(is.na(p_value_fdr), NA,
                                       ifelse(p_value_fdr < 0.10, "Yes", "No")),
         p_value_fdr = round(p_value_fdr, 4)) %>%
  as.data.frame()

write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_uk_training.csv",
          row.names = FALSE)

# ---- Console summaries ---------------------------------------------
cat("\n--- WGC significance (FDR q < 0.10) by Test x Direction ---\n")
print(wgc_all %>% group_by(Test, Direction) %>%
        summarise(N = n(),
                  N_valid = sum(!is.na(p_value)),
                  Sig_orig = sum(Significant_Original == "Yes", na.rm = TRUE),
                  Sig_FDR  = sum(Significant_FDR      == "Yes", na.rm = TRUE),
                  .groups = "drop"))

cat("\n--- WGC significance by Endogenous x Test (FDR) ---\n")
print(wgc_all %>% group_by(Endogenous, Test) %>%
        summarise(Sig_FDR = sum(Significant_FDR == "Yes", na.rm = TRUE),
                  .groups = "drop") %>%
        pivot_wider(names_from = Test, values_from = Sig_FDR,
                    values_fill = 0))

# =====================================================================
#  PART C -- WCA FDR-impact reporting & example comparison plot
# =====================================================================
total_orig <- sum(fdr_summary$Significant_Original)
total_fdr  <- sum(fdr_summary$Significant_FDR)
cat("\n=== WCA FDR Impact ===\n")
cat("Total significant pixels (original):     ", total_orig, "\n")
cat("Total significant pixels (FDR-corrected):", total_fdr, "\n")
cat("Reduction:",
    round(if (total_orig > 0) (total_orig - total_fdr) / total_orig * 100
          else 0, 2), "%\n")

ex_pair <- paste(endogenous_vars[1], exogenous_vars[1], sep = "_x_")
if (ex_pair %in% names(wcc_results)) {
  CairoPNG("FDR_Comparison_Example.png", width = 2400, height = 1200, res = 150)
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 2,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "Original (α = 0.05)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 0,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "FDR-corrected (α = 0.10)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  if (any(fdr_results[[ex_pair]]$fdr_signif, na.rm = TRUE))
    contour(wcc_results[[ex_pair]]$t, wcc_results[[ex_pair]]$period,
            t(fdr_results[[ex_pair]]$fdr_signif),
            levels = 0.5, add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE)
  mtext(paste(endogenous_labels[1], "vs", exogenous_labels[1],
              "- FDR Correction Comparison"),
        outer = TRUE, cex = 1.8, font = 2)
  dev.off()
}

cat("\n=== Done ===\n")
cat("Outputs:\n")
cat(" - WCC plots/grid:  WCC_FDR_Charts_Training/, WCC_Heatmaps_Grid_uk_fdr_training_revised.png\n")
cat(" - WCA summaries:   FDR_Correction_Summary_uk_training.csv, WCC_Summary_Table_{Original,FDR}_uk_training.csv\n")
cat(" - WGC raw:         Wavelet_NL_Granger_Causality_uk_training_raw.csv\n")
cat(" - WGC final (FDR): Wavelet_NL_Granger_Causality_uk_training.csv  (480 rows)\n")
cat(" - WCA example:     FDR_Comparison_Example.png\n")
cat(" - Checkpoints:     WCA_checkpoints_uk/, WGC_checkpoints_uk/\n")
################## End of Code: UK #################

# =====================================================================
#  ITALY -- FDR-corrected WCA + Wavelet Granger Causality (WGC)
#  Tests per (Endogenous, Exogenous, Scale, Direction):
#     1. Linear Granger F-test (lmtest::grangertest)
#     2. NN-Granger            (NlinTS::nlin_causality.test)
#  Training window: 1995-01-01 to 2022-03-01
# =====================================================================
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/italy")
getwd()

# ---- Packages -------------------------------------------------------
suppressPackageStartupMessages({
  library(biwavelet); library(wavelets); library(Cairo); library(magick)
  library(dplyr);     library(tidyr);    library(lmtest); library(lubridate)
  library(NlinTS)
})
stopifnot(utils::packageVersion("NlinTS") >= "1.4.0")
cat("NlinTS version:", as.character(utils::packageVersion("NlinTS")), "\n")

# ---- Read and subset data ------------------------------------------
data_ts <- read.csv("all_mulvar_data_italy_v2.csv",
                    header = TRUE, check.names = FALSE)
data_ts$Date <- as.Date(data_ts$Date)

training_start <- as.Date("1995-01-01")
training_end   <- as.Date("2022-03-01")
data_ts_train  <- data_ts[data_ts$Date >= training_start &
                            data_ts$Date <= training_end, ]

cat("Training period:", as.character(min(data_ts_train$Date)), "to",
    as.character(max(data_ts_train$Date)),
    "  (", nrow(data_ts_train), "obs )\n")

# ---- Variable specification ----------------------------------------
endogenous_vars <- c("Unemploymentrate", "RealbroadEER", "ShorttermIR",
                     "OilpriceGlobalWTI", "CPIinflationrate")
exogenous_vars  <- c("logEPU", "GPRC", "USEMV", "USMPU")
endogenous_labels <- c("Unemployment Rate", "REER", "SIR",
                       "Oil Price (WTI)", "CPI Inflation")
exogenous_labels  <- c("EPU", "GPR", "USEMV", "USMPU")

n_obs_train         <- nrow(data_ts_train)
start_year_train    <- year(min(data_ts_train$Date))
end_year_train      <- year(max(data_ts_train$Date))
time_sequence_train <- seq_len(n_obs_train)

# =====================================================================
#  PART A -- WAVELET COHERENCE (WCA) with scale-wise FDR correction
# =====================================================================
apply_fdr_correction <- function(wtc_result, alpha = 0.10) {
  n_time  <- ncol(wtc_result$rsq); n_scale <- nrow(wtc_result$rsq)
  p_values   <- matrix(NA, n_scale, n_time)
  fdr_signif <- matrix(FALSE, n_scale, n_time)
  for (i in seq_len(n_scale)) {
    sig_level <- wtc_result$signif[i]
    if (is.na(sig_level) || sig_level == 0) next
    scale_rsq <- wtc_result$rsq[i, ]; scale_p <- rep(NA_real_, n_time)
    for (j in seq_len(n_time)) {
      if (!is.na(scale_rsq[j])) {
        if (scale_rsq[j] >= sig_level)
          scale_p[j] <- 1 - pchisq(scale_rsq[j] * 2, df = 2)
        else
          scale_p[j] <- min(1, 1 - scale_rsq[j] / sig_level)
      }
    }
    p_values[i, ] <- scale_p
    valid <- which(!is.na(scale_p))
    if (length(valid) > 0) {
      p_adj <- p.adjust(scale_p[valid], method = "BH")
      fdr_signif[i, valid[p_adj < alpha]] <- TRUE
    }
  }
  list(fdr_signif = fdr_signif, p_values = p_values, alpha = alpha)
}

plot_wtc_with_fdr <- function(wtc_result, fdr_result, y_label, x_label,
                              n_obs, start_year, end_year,
                              main_title, file_name) {
  CairoPNG(filename = file_name, width = 1600, height = 1200, res = 150)
  par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
  plot(wtc_result, plot.phase = TRUE, lty.coi = 1, col.coi = "grey",
       lwd.coi = 2, lwd.sig = 0, arrow.lwd = 0.03, arrow.len = 0.12,
       ylab = "Scale", xlab = "Frequency", plot.cb = TRUE,
       main = main_title, cex.main = 1.5, font.main = 3, font.lab = 3)
  if (any(fdr_result$fdr_signif, na.rm = TRUE))
    contour(wtc_result$t, wtc_result$period, t(fdr_result$fdr_signif),
            levels = c(0.5), add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE, method = "edge")
  abline(v = seq(12, n_obs, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)
  year_breaks <- seq(0, n_obs, 12)
  year_labels <- seq(start_year, end_year, 1)
  if (length(year_labels) > length(year_breaks))
    year_labels <- year_labels[seq_along(year_breaks)]
  axis(side = 3, at = year_breaks, labels = year_labels, font = 3)
  dev.off()
}

output_dir <- "WCC_FDR_Charts_Training"
if (!dir.exists(output_dir)) dir.create(output_dir)
wca_ckpt_dir <- "WCA_checkpoints_italy"
if (!dir.exists(wca_ckpt_dir)) dir.create(wca_ckpt_dir)

wcc_results <- list(); fdr_results <- list(); file_list <- c()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    y_var <- endogenous_vars[i]; x_var <- exogenous_vars[j]
    y_lab <- endogenous_labels[i]; x_lab <- exogenous_labels[j]
    pair_name <- paste(y_var, x_var, sep = "_x_")
    ckpt <- file.path(wca_ckpt_dir, paste0(pair_name, ".rds"))
    
    if (file.exists(ckpt)) {
      cat("WCA: CACHED ", pair_name, "\n")
      blob <- readRDS(ckpt)
      wcc_results[[pair_name]] <- blob$wtc
      fdr_results[[pair_name]] <- blob$fdr
    } else {
      cat("WCA:", pair_name, "\n")
      t1 <- cbind(time_sequence_train, data_ts_train[[y_var]])
      t2 <- cbind(time_sequence_train, data_ts_train[[x_var]])
      wtc_r <- wtc(t1, t2, nrands = 1000)
      fdr_r <- apply_fdr_correction(wtc_r, alpha = 0.10)
      saveRDS(list(wtc = wtc_r, fdr = fdr_r), ckpt)
      wcc_results[[pair_name]] <- wtc_r
      fdr_results[[pair_name]] <- fdr_r
    }
    
    out_file <- paste0(output_dir, "/", gsub(" ", "_", pair_name),
                       "_fdr_training.png")
    file_list <- c(file_list, out_file)
    plot_wtc_with_fdr(wcc_results[[pair_name]], fdr_results[[pair_name]],
                      y_lab, x_lab, n_obs_train,
                      start_year_train, end_year_train,
                      paste0(y_lab, " vs ", x_lab), out_file)
  }
}

# ---- Grid composite -------------------------------------------------
ordered_files <- c()
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pat <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    hit <- file_list[grepl(pat, file_list)]
    if (length(hit)) ordered_files <- c(ordered_files, hit[1])
  }
rows <- split(ordered_files, ceiling(seq_along(ordered_files) / 4))
row_images <- lapply(rows, function(rf)
  if (length(rf)) image_append(image_join(image_read(rf)), stack = FALSE))
row_images <- row_images[!sapply(row_images, is.null)]
if (length(row_images)) {
  grid_image <- image_append(image_join(row_images), stack = TRUE)
  image_write(grid_image, "WCC_Heatmaps_Grid_italy_fdr_training_revised.png")
}

# ---- WCA FDR-impact summary ----------------------------------------
fdr_summary <- data.frame(Pair = character(), Total_Tests = integer(),
                          Significant_Original = integer(),
                          Significant_FDR = integer(),
                          FDR_Reduction_Percent = numeric(),
                          stringsAsFactors = FALSE)
for (pn in names(wcc_results)) {
  w <- wcc_results[[pn]]; f <- fdr_results[[pn]]
  orig_sig <- 0
  for (i in seq_len(nrow(w$rsq)))
    if (!is.na(w$signif[i]) && w$signif[i] > 0)
      orig_sig <- orig_sig + sum(w$rsq[i, ] >= w$signif[i], na.rm = TRUE)
  fdr_sig    <- sum(f$fdr_signif, na.rm = TRUE)
  total_test <- sum(!is.na(w$rsq))
  red_pct    <- if (orig_sig > 0) (orig_sig - fdr_sig) / orig_sig * 100 else 0
  fdr_summary <- rbind(fdr_summary, data.frame(
    Pair = pn, Total_Tests = total_test,
    Significant_Original = orig_sig, Significant_FDR = fdr_sig,
    FDR_Reduction_Percent = round(red_pct, 2)))
}
write.csv(fdr_summary, "FDR_Correction_Summary_italy_training.csv",
          row.names = FALSE)

# ---- WCA summary tables --------------------------------------------
label_strength <- function(v) {
  if (is.na(v)) return("NA")
  if (v > 0.50) "Strong" else if (v > 0.25) "Moderate" else "Weak"
}
summary_matrix     <- matrix(NA, length(endogenous_vars), length(exogenous_vars),
                             dimnames = list(endogenous_labels, exogenous_labels))
summary_matrix_fdr <- summary_matrix
for (i in seq_along(endogenous_vars))
  for (j in seq_along(exogenous_vars)) {
    pn <- paste(endogenous_vars[i], exogenous_vars[j], sep = "_x_")
    w  <- wcc_results[[pn]]; f <- fdr_results[[pn]]
    if (is.null(w)) next
    coi_mask <- matrix(1, nrow(w$rsq), ncol(w$rsq))
    coi_mask[w$coi < w$period] <- NA
    rsq_v <- w$rsq * coi_mask
    avg   <- mean(rsq_v, na.rm = TRUE)
    summary_matrix[endogenous_labels[i], exogenous_labels[j]] <-
      sprintf("%.2f (%s)", avg, label_strength(avg))
    rsq_f <- rsq_v; rsq_f[!f$fdr_signif] <- NA
    avg_f <- mean(rsq_f, na.rm = TRUE)
    summary_matrix_fdr[endogenous_labels[i], exogenous_labels[j]] <-
      if (is.na(avg_f)) "NS"
    else sprintf("%.2f (%s)", avg_f, label_strength(avg_f))
  }
write.csv(cbind(Endogenous = rownames(summary_matrix), summary_matrix),
          "WCC_Summary_Table_Original_italy_training.csv", row.names = FALSE)
write.csv(cbind(Endogenous = rownames(summary_matrix_fdr), summary_matrix_fdr),
          "WCC_Summary_Table_FDR_italy_training.csv", row.names = FALSE)

# =====================================================================
#  PART B -- WAVELET GRANGER CAUSALITY (Linear F + NN-GC only)
# =====================================================================
WAVELET <- "la8"; LEVELS <- 6
LIN_LAGS <- 2;    NN_LAGS <- 2

nn_gc_test <- function(ts_to, ts_from, lags = NN_LAGS,
                       layers_univ = c(2), layers_biv = c(4),
                       iters = 50, lr = 0.01, algo = "sgd",
                       batch = 10, bias = TRUE, seed = 1) {
  ts_to <- as.numeric(ts_to); ts_from <- as.numeric(ts_from)
  res <- NlinTS::nlin_causality.test(
    ts1 = ts_to, ts2 = ts_from, lag = lags,
    LayersUniv = layers_univ, LayersBiv = layers_biv,
    iters = iters, learningRate = lr, algo = algo,
    batch_size = batch, bias = bias, seed = seed)
  stat <- tryCatch(res$Ftest,  error = function(e) NA_real_)
  pval <- tryCatch(res$pvalue, error = function(e) NA_real_)
  if (is.null(stat) || length(stat) == 0) stat <- NA_real_
  if (is.null(pval) || length(pval) == 0) pval <- NA_real_
  c(stat = unname(stat), p = unname(pval))
}

lin_gc_test <- function(ts_to, ts_from, lags = LIN_LAGS) {
  df <- data.frame(Y = as.numeric(ts_to), X = as.numeric(ts_from))
  t  <- lmtest::grangertest(Y ~ X, order = lags, data = df)
  c(stat = unname(t$F[2]), p = unname(t$`Pr(>F)`[2]))
}

safe_run <- function(label, expr)
  tryCatch(expr, error = function(e) {
    message(sprintf("  [WARN] %s failed: %s", label, conditionMessage(e)))
    c(stat = NA_real_, p = NA_real_)
  })

wavelet_gc_full <- function(ts_y, ts_x, y_label, x_label,
                            wavelet = WAVELET, levels = LEVELS,
                            lags = LIN_LAGS) {
  m_y <- wavelets::modwt(as.numeric(ts_y), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  m_x <- wavelets::modwt(as.numeric(ts_x), filter = wavelet,
                         n.levels = levels, boundary = "reflection")
  n_orig <- length(ts_y); rows <- list()
  for (i in seq_len(levels)) {
    dy <- as.numeric(m_y@W[[i]]); dx <- as.numeric(m_x@W[[i]])
    if (length(dy) > n_orig) dy <- dy[seq_len(n_orig)]
    if (length(dx) > n_orig) dx <- dx[seq_len(n_orig)]
    df <- na.omit(data.frame(Y = dy, X = dx))
    sc <- paste0("D", i); enough <- nrow(df) > (3 * lags + 5)
    if (enough) {
      lin_fwd <- safe_run(sprintf("Lin X->Y %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$Y, df$X, lags))
      lin_rev <- safe_run(sprintf("Lin Y->X %s %s<-%s", sc, y_label, x_label),
                          lin_gc_test(df$X, df$Y, lags))
      nn_fwd  <- safe_run(sprintf("NN  X->Y %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$Y, df$X, lags))
      nn_rev  <- safe_run(sprintf("NN  Y->X %s %s<-%s", sc, y_label, x_label),
                          nn_gc_test(df$X, df$Y, lags))
    } else {
      lin_fwd <- lin_rev <- nn_fwd <- nn_rev <-
        c(stat = NA_real_, p = NA_real_)
    }
    mk <- function(dir, test, r) data.frame(
      Endogenous = y_label, Exogenous = x_label, Scale = sc,
      Direction = dir, Test = test,
      Statistic = round(unname(r["stat"]), 4),
      p_value   = round(unname(r["p"]),    4),
      stringsAsFactors = FALSE)
    rows[[length(rows)+1]] <- mk("X_to_Y","Linear_F", lin_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","Linear_F", lin_rev)
    rows[[length(rows)+1]] <- mk("X_to_Y","NN_GC",    nn_fwd)
    rows[[length(rows)+1]] <- mk("Y_to_X","NN_GC",    nn_rev)
  }
  do.call(rbind, rows)
}

# ---- Run all 20 pairs with checkpointing ---------------------------
wgc_ckpt_dir <- "WGC_checkpoints_italy"
if (!dir.exists(wgc_ckpt_dir)) dir.create(wgc_ckpt_dir)

results_list <- list(); pair_id <- 0
total_pairs  <- length(endogenous_vars) * length(exogenous_vars)
t0 <- Sys.time()

for (i in seq_along(endogenous_vars)) {
  for (j in seq_along(exogenous_vars)) {
    pair_id <- pair_id + 1
    ckpt_file <- file.path(wgc_ckpt_dir,
                           sprintf("pair_%02d_%s_x_%s.rds", pair_id,
                                   endogenous_vars[i], exogenous_vars[j]))
    if (file.exists(ckpt_file)) {
      cat(sprintf("[%2d/%d] %-22s <- %-6s  CACHED\n",
                  pair_id, total_pairs,
                  endogenous_labels[i], exogenous_labels[j]))
      results_list[[length(results_list)+1]] <- readRDS(ckpt_file)
      next
    }
    tp <- Sys.time()
    cat(sprintf("[%2d/%d] %-22s <- %-6s  ... ",
                pair_id, total_pairs,
                endogenous_labels[i], exogenous_labels[j]))
    flush.console()
    res <- wavelet_gc_full(
      ts_y    = data_ts_train[[endogenous_vars[i]]],
      ts_x    = data_ts_train[[exogenous_vars[j]]],
      y_label = endogenous_labels[i],
      x_label = exogenous_labels[j])
    saveRDS(res, ckpt_file)
    results_list[[length(results_list)+1]] <- res
    cat(sprintf("done in %.1f sec\n",
                as.numeric(difftime(Sys.time(), tp, units = "secs"))))
    flush.console()
  }
}
cat("WGC total elapsed:",
    round(difftime(Sys.time(), t0, units = "mins"), 2), "minutes\n")

wgc_all <- do.call(rbind, results_list)

# Raw dump before FDR (safety net)
write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_italy_training_raw.csv",
          row.names = FALSE)

# ---- BH-FDR within (Test, Direction) -------------------------------
wgc_all <- wgc_all %>%
  group_by(Test, Direction) %>%
  mutate(p_value_fdr = p.adjust(p_value, method = "BH")) %>%
  ungroup() %>%
  mutate(Significant_Original = ifelse(is.na(p_value), NA,
                                       ifelse(p_value     < 0.10, "Yes", "No")),
         Significant_FDR      = ifelse(is.na(p_value_fdr), NA,
                                       ifelse(p_value_fdr < 0.10, "Yes", "No")),
         p_value_fdr = round(p_value_fdr, 4)) %>%
  as.data.frame()

write.csv(wgc_all,
          "Wavelet_NL_Granger_Causality_italy_training.csv",
          row.names = FALSE)

# ---- Console summaries ---------------------------------------------
cat("\n--- WGC significance (FDR q < 0.10) by Test x Direction ---\n")
print(wgc_all %>% group_by(Test, Direction) %>%
        summarise(N = n(),
                  N_valid = sum(!is.na(p_value)),
                  Sig_orig = sum(Significant_Original == "Yes", na.rm = TRUE),
                  Sig_FDR  = sum(Significant_FDR      == "Yes", na.rm = TRUE),
                  .groups = "drop"))

cat("\n--- WGC significance by Endogenous x Test (FDR) ---\n")
print(wgc_all %>% group_by(Endogenous, Test) %>%
        summarise(Sig_FDR = sum(Significant_FDR == "Yes", na.rm = TRUE),
                  .groups = "drop") %>%
        pivot_wider(names_from = Test, values_from = Sig_FDR,
                    values_fill = 0))

# =====================================================================
#  PART C -- WCA FDR-impact reporting & example comparison plot
# =====================================================================
total_orig <- sum(fdr_summary$Significant_Original)
total_fdr  <- sum(fdr_summary$Significant_FDR)
cat("\n=== WCA FDR Impact ===\n")
cat("Total significant pixels (original):     ", total_orig, "\n")
cat("Total significant pixels (FDR-corrected):", total_fdr, "\n")
cat("Reduction:",
    round(if (total_orig > 0) (total_orig - total_fdr) / total_orig * 100
          else 0, 2), "%\n")

ex_pair <- paste(endogenous_vars[1], exogenous_vars[1], sep = "_x_")
if (ex_pair %in% names(wcc_results)) {
  CairoPNG("FDR_Comparison_Example.png", width = 2400, height = 1200, res = 150)
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 2,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "Original (α = 0.05)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  plot(wcc_results[[ex_pair]], plot.phase = TRUE, lty.coi = 1,
       col.coi = "grey", lwd.coi = 2, lwd.sig = 0,
       arrow.lwd = 0.03, arrow.len = 0.12, plot.cb = TRUE,
       ylab = "Scale", xlab = "Period",
       main = "FDR-corrected (α = 0.10)", cex.main = 1.5,
       font.main = 2, font.lab = 2)
  if (any(fdr_results[[ex_pair]]$fdr_signif, na.rm = TRUE))
    contour(wcc_results[[ex_pair]]$t, wcc_results[[ex_pair]]$period,
            t(fdr_results[[ex_pair]]$fdr_signif),
            levels = 0.5, add = TRUE, col = "black", lwd = 2,
            drawlabels = FALSE)
  mtext(paste(endogenous_labels[1], "vs", exogenous_labels[1],
              "- FDR Correction Comparison"),
        outer = TRUE, cex = 1.8, font = 2)
  dev.off()
}

cat("\n=== Done ===\n")
cat("Outputs:\n")
cat(" - WCC plots/grid:  WCC_FDR_Charts_Training/, WCC_Heatmaps_Grid_italy_fdr_training_revised.png\n")
cat(" - WCA summaries:   FDR_Correction_Summary_italy_training.csv, WCC_Summary_Table_{Original,FDR}_italy_training.csv\n")
cat(" - WGC raw:         Wavelet_NL_Granger_Causality_italy_training_raw.csv\n")
cat(" - WGC final (FDR): Wavelet_NL_Granger_Causality_italy_training.csv  (480 rows)\n")
cat(" - WCA example:     FDR_Comparison_Example.png\n")
cat(" - Checkpoints:     WCA_checkpoints_italy/, WGC_checkpoints_italy/\n")
################## End of Code: Italy #################