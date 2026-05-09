#### SZBVAR Model: CANADA: 12M and 24M ahead - forecasts  ####
###################### Source code : szbvar function + forecast.szbvar function ##################
# Revised instruction
# Instruction: 1. First execute "szbvarx_orchestrator_utils_G7_paper.R" code module before running the following code block
##################### End of the Source Code #####################################################
#### SZBVAR Model: CANADA: 12M and 24M ahead - forecasts with HP Tuning ####
# Load required libraries
library(vars)
library(forecast)
library(Metrics)
library(dplyr)
library(tseries)
library(parallel)

# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/canada")
getwd()

###################### Source code : szbvar function + forecast.szbvar function ##################
# Comments:
#   - Following codes should be run before running the remaining code blocks.
#   - "szbvar.R", "szbsvar.R" , "forecast.R", and "forecast_msbvar.R"
#   - location: /Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper/code/R_codes_G7_countries/Algorithms/USA or for all the remaining countries in G7
##################### End of the Source Code #####################################################

########################### SZBVARx Model with HPs ##################
# Read the dataset
var.canada <- read.csv("all_mulvar_data_canada_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.canada$Date <- as.Date(var.canada$Date)
str(var.canada)


# Creation of datasets for 12M forecasts
var.canada.12M.train <- var.canada[1:327,]
var.canada.12M.val <- var.canada[328:339,]
var.canada.12M.test <- var.canada[340:351,]
var.canada.12M.full.train <- var.canada[1:339,]

# Creation of datasets for 24M forecasts
var.canada.24M.train <- var.canada[1:303,]
var.canada.24M.val <- var.canada[304:327,]
var.canada.24M.test <- var.canada[328:351,]
var.canada.24M.full.train <- var.canada[1:327,]


# Check for stationarity (using 12M full train data as an example) - Optional
for(col in c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate", 
             "logEPU", "GPRC", "USEMV", "USMPU")) {
  print(paste("KPSS test for", col))
  print(kpss.test(var.canada.12M.full.train[[col]], null="Trend"))
}


# Function to fit SZBVAR model and generate forecasts: 12M {based on best HPs}
fit_and_forecast_szbvar_12M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 1,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.2,
                  lambda1 = 0.05,
                  lambda3 = 1,
                  lambda4 = 0.1,
                  lambda5 = 0,
                  mu5 = 1,
                  mu6 = 0,
                  prior = 0 # Normal-Wishart prior,
                  # prior = 1 # Normal-flat prior,
                  # prior = 2 # flat-flat prior (i.e., akin to MLE)
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}

# Function to fit SZBVAR model and generate forecasts: 24M {based on best HPs}
fit_and_forecast_szbvar_24M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 1,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.6,
                  lambda1 = 0.05,
                  lambda3 = 1,
                  lambda4 = 0.1,
                  lambda5 = 1,
                  mu5 = 1,
                  mu6 = 1,
                  prior = 0 # Normal-Wishart prior,
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}


# 12M Forecasts
results_12M <- fit_and_forecast_szbvar_12M(var.canada.12M.full.train, var.canada.12M.test)
results_12M
# $forecasts
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         5.321021     98.59409    4.787213          73.79209         4.274800
# [2,]         5.406251     98.27829    4.783357          72.77942         4.221312
# [3,]         5.339979     98.45350    4.795603          73.22148         4.183475
# [4,]         5.386799     98.29073    4.792534          72.64381         4.123455
# [5,]         5.286124     98.72541    4.811482          73.81448         4.119531
# [6,]         5.272983     98.93492    4.823734          74.25155         4.084171
# [7,]         5.391611     98.94680    4.821223          73.94821         4.008136
# [8,]         5.464857     99.10320    4.824078          74.08216         3.941818
# [9,]         5.556011     99.04657    4.826787          73.69100         3.891961
# [10,]         5.824503     99.03266    4.807977          73.00027         3.746877
# [11,]         5.744756     99.23320    4.812413          73.59253         3.726897
# [12,]         5.979250     98.73413    4.788698          71.87340         3.631999
# 24M Forecasts
results_24M <- fit_and_forecast_szbvar_24M(var.canada.24M.full.train, var.canada.24M.test)
results_24M
# $forecasts
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         6.030345    103.91683   0.8774233         104.07437         6.447507
# [2,]         6.293372    102.95065   0.8736473         100.42017         6.264433
# [3,]         6.541316    101.90009   0.8809909          96.60180         6.083575
# [4,]         6.589111    101.38143   0.9035830          94.54762         5.956301
# [5,]         6.674964    100.69538   0.9453821          91.95750         5.826724
# [6,]         6.673101    100.61621   0.9588318          91.10310         5.722959
# [7,]         6.763438     99.96339   0.9818711          88.66257         5.597992
# [8,]         6.804644     99.95117   0.9764432          87.91506         5.495524
# [9,]         6.784494     99.72982   1.0115821          86.77499         5.406489
# [10,]         6.806103     99.21622   1.0569622          84.83901         5.304112
# [11,]         6.763658     98.84369   1.1099746          83.40068         5.222344
# [12,]         6.794174     98.19679   1.1528060          81.16963         5.122211
# [13,]         6.730065     97.78755   1.2127645          79.72178         5.046171
# [14,]         6.609759     97.69265   1.2612642          79.21199         4.979429
# [15,]         6.558427     97.45411   1.3035653          78.20933         4.898803
# [16,]         6.494193     97.58058   1.3231786          78.13757         4.842123
# [17,]         6.418325     97.46831   1.3643915          77.52267         4.778574
# [18,]         6.418511     97.21349   1.3794533          76.44113         4.706305
# [19,]         6.372000     97.10998   1.4103733          75.81546         4.641706
# [20,]         6.244993     97.34349   1.4391021          76.20867         4.604362
# [21,]         6.260799     97.57215   1.4072424          76.36356         4.535509
# [22,]         6.272136     97.58015   1.3815046          76.04618         4.452531
# [23,]         6.100200     97.50540   1.3756611          76.06498         4.390396
# [24,]         5.988436     97.60601   1.3105800          76.49880         4.319345

### SZBVARx: CnDAN -  Credible PPIs ####
suppressPackageStartupMessages({ 
  library(MASS)
  if (!requireNamespace("digest", quietly = TRUE)) {
    message("Note: 'digest' package not available; verification hashes will be skipped")
  }
})

## ========================= USER SETTINGS ================================= ##
endog_vars <- c("Unemploymentrate","RealbroadEER","ShorttermIR",
                "OilpriceGlobalWTI","CPIinflationrate")
exog_vars  <- c("logEPU","GPRC","USEMV","USMPU")

# Admissible supports (edit per variable if needed; NA = unbounded)
support_bounds <- list(
  Unemploymentrate   = c(0, 100),
  RealbroadEER       = c(0,  NA),
  ShorttermIR        = c(NA, NA),
  OilpriceGlobalWTI  = c(0,  NA),
  CPIinflationrate   = c(NA, NA)
)

gamma_level <- 0.50
n_sim <- 1000
SEED <- 1234
COUNTRY <- "canada"  # Country identifier
## ======================================================================== ##


## ============================ UTILITIES ================================= ##
.num <- function(x) as.numeric(x)

.normalize_ar_cube <- function(Araw, m, p){
  d <- dim(Araw)
  if (length(d)!=3) stop("model$ar.coefs must be 3D")
  if (all(d == c(m,m,p))) return(Araw)
  if (all(d == c(p,m,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[L,,]; return(out) }
  if (all(d == c(m,p,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[,L,]; return(out) }
  stop("Unexpected ar.coefs dims: ", paste(d, collapse="x"))
}

.normalize_B <- function(Braw, m){
  dm <- dim(Braw); if (is.null(dm)) stop("model$exog.coefs must be a matrix when exog_vars != 0")
  if (dm[1]==m) return(Braw)     # (m x k)
  if (dm[2]==m) return(t(Braw))  # (k x m) -> (m x k)
  stop("exog.coefs incompatible dims: ", paste(dm, collapse="x"))
}

.exog_tail_from_training <- function(train_df, exog_vars, H){
  # Uses last H training observations as future exogenous path
  # Valid for: (1) known policy paths, (2) scenario analysis, (3) persistence assumption
  start <- nrow(train_df) - H + 1
  if (start < 1) stop("H exceeds training length.")
  as.matrix(train_df[start:nrow(train_df), exog_vars, drop=FALSE])
}

# One-step VARX with shock
.varx_step <- function(Yhist, A, cvec, B, z, Rchol){
  p <- dim(A)[3]; m <- length(cvec)
  y <- cvec
  for (L in 1:p) y <- y + A[,,L] %*% .num(Yhist[nrow(Yhist)-L+1, ])
  if (!is.null(B) && !is.null(z)) y <- y + B %*% .num(z)
  zstd <- rnorm(m); eps <- t(Rchol) %*% zstd  # chol returns upper triangular R
  .num(y + eps)
}

# Full H-step simulation path (proper MA accumulation)
.simulate_varx_path <- function(Yinit, A, cvec, B, Zfut, Rchol){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    z <- if (ncol(Zfut)>0) Zfut[h, ] else NULL
    y_next <- .varx_step(Yh, A, cvec, B, z, Rchol)
    out[h,] <- y_next
    Yh <- rbind(Yh, y_next)
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Deterministic path (ε≡0) for snap-centering
.det_path <- function(Yinit, A, cvec, B, Zfut){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    y <- cvec
    for (L in 1:p) y <- y + A[,,L] %*% .num(Yh[nrow(Yh)-L+1, ])
    if (!is.null(B) && ncol(Zfut)>0) y <- y + B %*% .num(Zfut[h, ])
    out[h,] <- .num(y)
    Yh <- rbind(Yh, t(y))
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Truncate simulated draws to admissible support
.truncate_draws <- function(x, lb, ub){
  keep <- rep(TRUE, length(x))
  if (!is.na(lb)) keep <- keep & (x >= lb)
  if (!is.na(ub)) keep <- keep & (x <= ub)
  x[keep]
}

# PF-anchored shortest (HPD-style) gamma-interval from draws within support
# Returns [L,U], acceptance rate, and a status flag
.pf_anchored_interval <- function(draws, pf, gamma, lb = NA_real_, ub = NA_real_, 
                                  var_name = "", horizon = 0){
  n_total <- length(draws)
  # Truncate to support
  x <- .truncate_draws(draws, lb, ub)
  acc_rate <- length(x) / max(1L, n_total)
  
  # Warn if low acceptance rate
  if (acc_rate < 0.5 && var_name != "") {
    warning(sprintf("%s horizon %d: accept_rate = %.3f (effective coverage = %.1f%%)", 
                    var_name, horizon, acc_rate, gamma * acc_rate * 100),
            call. = FALSE)
  }
  
  if (length(x) == 0L) {  # all mass out-of-support
    return(list(L = pf, U = pf, accept_rate = 0, status = "degenerate"))
  }
  
  x <- sort(x)
  n <- length(x)
  k <- max(1L, ceiling(gamma * n))
  
  # If PF outside the truncated range, anchor at PF by extending to nearest window
  if (pf <= x[1]) {
    j <- min(n, k)
    L <- pf; U <- x[j]
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="left-anchored"))
  }
  if (pf >= x[n]) {
    i <- max(1L, n - k + 1)
    L <- x[i]; U <- pf
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="right-anchored"))
  }
  
  # Sliding-window HPD subject to "must contain PF"
  bestL <- x[1]; bestU <- x[min(n,k)]; bestW <- bestU - bestL; found <- FALSE
  j <- k
  for (i in 1:(n - k + 1)) {
    j <- i + k - 1
    L <- x[i]; U <- x[j]
    if (pf >= L && pf <= U) {
      w <- U - L
      if (!found || w < bestW) { bestW <- w; bestL <- L; bestU <- U; found <- TRUE }
    }
  }
  if (found) {
    return(list(L = bestL, U = bestU, accept_rate = acc_rate, status = "window"))
  }
  
  # Fallback: expand the closest window to include PF
  # (Preserves ≥ gamma mass, includes PF; width may be slightly larger than HPD)
  # Choose window whose center is closest to PF
  centers <- sapply(1:(n - k + 1), function(i) (x[i] + x[i+k-1]) / 2)
  idx <- which.min(abs(centers - pf))
  L <- min(x[idx], pf); U <- max(x[idx + k - 1], pf)
  list(L = L, U = U, accept_rate = acc_rate, status = "expanded")
}
## ======================================================================== ##


## ====================== MAIN: PPIs (PF-ANCHORED) ========================= ##
# Uses fitted szbvarx model, training data, and SAVED point forecasts
generate_ppi_pf_anchored <- function(model, train_df,
                                     endog_vars, exog_vars,
                                     point_forecasts,
                                     support_bounds,
                                     gamma = 0.50, n_sim = 1000,
                                     seed = NULL){
  
  # Set seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
    message(sprintf("  → RNG seed set to %d for reproducibility", seed))
  }
  
  # Enforce model ordering via eqnames
  eq <- attr(model, "eqnames"); if (is.null(eq)) eq <- colnames(point_forecasts)
  if (is.null(eq)) stop("Cannot infer model eqnames; set colnames(point_forecasts).")
  if (!all(eq %in% endog_vars)) stop("endog_vars missing: ", paste(setdiff(eq, endog_vars), collapse=", "))
  endog_vars <- eq
  if (!all(colnames(point_forecasts) == endog_vars))
    point_forecasts <- as.matrix(point_forecasts[, endog_vars, drop=FALSE])
  
  # Extract components
  p      <- as.integer(model$p)
  cvec   <- .num(model$intercept)
  Araw   <- model$ar.coefs
  Braw   <- if (!is.null(model$exog.coefs)) model$exog.coefs else NULL
  Sigma  <- as.matrix(model$mean.S)
  m <- length(endog_vars); H <- nrow(point_forecasts)
  if (length(cvec) != m) stop("Intercept length != number of endogenous series.")
  
  A <- .normalize_ar_cube(Araw, m, p)
  B <- if (!is.null(Braw) && length(exog_vars) > 0) .normalize_B(Braw, m) else NULL
  
  # Robust chol for Sigma with warning
  Rchol <- tryCatch({
    chol(Sigma)
  }, error = function(e) {
    warning("Sigma near-singular; adding jitter 1e-10 to diagonal", call. = FALSE)
    chol(Sigma + diag(1e-10, nrow(Sigma)))
  })
  
  # Initial history & exog FUT path (training tail; no leakage)
  Y <- as.matrix(train_df[, endog_vars, drop=FALSE])
  Yinit <- Y[(nrow(Y)-p+1):nrow(Y), , drop=FALSE]
  Zfut <- if (length(exog_vars)==0) matrix(numeric(0), H, 0) else {
    zt <- .exog_tail_from_training(train_df, exog_vars, H); colnames(zt) <- exog_vars; zt
  }
  
  # Deterministic path (ε=0) + snap-centering delta
  det <- .det_path(Yinit, A, cvec, B, Zfut)
  colnames(det) <- endog_vars
  delta <- point_forecasts - det  # H x m
  
  # Validate snap-centering (diagnostic)
  max_delta <- max(abs(delta))
  if (max_delta > 1e-3) {
    message(sprintf("  → Max snap-centering delta = %.4f (PF differs from deterministic path)", max_delta))
    message("     This is expected if PF comes from HP-tuned model with different settings")
  }
  
  # Monte Carlo with proper recursion
  sims <- array(NA_real_, dim = c(H, m, n_sim))
  dimnames(sims) <- list(NULL, endog_vars, NULL)
  for (s in 1:n_sim) {
    path <- .simulate_varx_path(Yinit, A, cvec, B, Zfut, Rchol)
    sims[,,s] <- path + delta   # snap-center to PF
  }
  
  # Build PF-anchored credible intervals after support truncation
  results <- vector("list", m); names(results) <- endog_vars
  avgw <- setNames(numeric(m), endog_vars)
  acc_avg <- setNames(numeric(m), endog_vars)
  
  for (j in seq_len(m)) {
    var <- endog_vars[j]
    b <- support_bounds[[var]]
    lb <- if (is.null(b)) NA_real_ else b[1]
    ub <- if (is.null(b)) NA_real_ else b[2]
    
    lower <- upper <- acc <- rep(NA_real_, H)
    for (h in 1:H) {
      draws <- sims[h, j, ]
      pf    <- as.numeric(point_forecasts[h, j])
      ans   <- .pf_anchored_interval(draws, pf, gamma, lb, ub, var, h)
      L <- ans$L; U <- ans$U
      if (U < L) { tmp <- U; U <- L; L <- tmp }  # logical consistency
      lower[h] <- L; upper[h] <- U; acc[h] <- ans$accept_rate
    }
    
    df <- data.frame(
      horizon        = 1:H,
      point_forecast = as.numeric(point_forecasts[, j]),
      lower_50       = lower,
      upper_50       = upper,
      interval_width = upper - lower,
      accept_rate    = acc,
      effective_coverage = gamma * acc  # Adjusted for truncation
    )
    results[[var]] <- df
    avgw[j] <- mean(df$interval_width, na.rm = TRUE)
    acc_avg[j] <- mean(df$accept_rate, na.rm = TRUE)
  }
  
  list(intervals = results,
       avg_widths = avgw,
       accept_rates = acc_avg,
       sims = sims)
}
## ======================================================================== ##


## ============================ RUN & EXPORT =============================== ##
## Enhanced with full reproducibility controls

cat("\n")
cat("================================================================================\n")
cat("  SZBVARX Forecast-Consistent Credible Intervals - PRODUCTION RUN\n")
cat(sprintf("  Country: %s\n", toupper(COUNTRY)))
cat("================================================================================\n\n")

# Capture initial RNG state
if (exists(".Random.seed")) {
  rng_state_initial <- .Random.seed
} else {
  set.seed(NULL)  # Initialize RNG
  rng_state_initial <- .Random.seed
}

# Display reproducibility info
cat("=== Reproducibility Configuration ===\n")
cat(sprintf("Country:      %s\n", toupper(COUNTRY)))
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n", gamma_level))
cat(sprintf("R version:    %s\n", R.version.string))
cat(sprintf("MASS version: %s\n", packageVersion("MASS")))
cat(sprintf("Platform:     %s\n", R.version$platform))
cat(sprintf("Timestamp:    %s\n", Sys.time()))
cat("======================================\n\n")

# Expected to exist in the environment (CANADA-specific):
# results_12M$model, results_12M$forecasts, var.canada.12M.full.train
# results_24M$model, results_24M$forecasts, var.canada.24M.full.train

cat("Validating input data...\n")
stopifnot(all(colnames(results_12M$forecasts) %in% endog_vars),
          all(colnames(results_24M$forecasts) %in% endog_vars))

# Align forecasts to eqnames
eq12 <- attr(results_12M$model, "eqnames"); if (is.null(eq12)) eq12 <- colnames(results_12M$forecasts)
eq24 <- attr(results_24M$model, "eqnames"); if (is.null(eq24)) eq24 <- colnames(results_24M$forecasts)
results_12M$forecasts <- as.matrix(results_12M$forecasts[, eq12, drop=FALSE])
results_24M$forecasts <- as.matrix(results_24M$forecasts[, eq24, drop=FALSE])
cat("  ✓ Input validation passed\n\n")

# Generate PF-anchored, support-respecting PPIs with explicit seeds
cat("Generating 12M prediction intervals...\n")
set.seed(SEED)  # Explicit seed for 12M
ppi_12M <- generate_ppi_pf_anchored(
  model = results_12M$model,
  train_df = var.canada.12M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_12M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED
)
cat("  ✓ 12M intervals complete\n\n")

cat("Generating 24M prediction intervals...\n")
set.seed(SEED + 1000)  # Different seed for 24M (ensures independence)
ppi_24M <- generate_ppi_pf_anchored(
  model = results_24M$model,
  train_df = var.canada.24M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_24M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED + 1000
)
cat("  ✓ 24M intervals complete\n\n")

# Consolidation helper
.consolidate <- function(ppi_obj, horizon_label){
  do.call(rbind, lapply(names(ppi_obj$intervals), function(v){
    d <- ppi_obj$intervals[[v]]
    d$variable <- v
    d$forecast_horizon <- horizon_label
    d$actual_value <- NA_real_
    d$covered <- NA_integer_
    d[, c("forecast_horizon","variable","horizon",
          "point_forecast","actual_value",
          "lower_50","upper_50","interval_width","covered","accept_rate","effective_coverage")]
  }))
}

cat("Consolidating results...\n")
cons12 <- .consolidate(ppi_12M, "12M")
cons24 <- .consolidate(ppi_24M, "24M")
consALL <- rbind(cons12, cons24)

# Write outputs (CANADA-specific filenames)
cat("Writing output files...\n")
write.csv(cons12, sprintf("credible_intervals_szbvarx_12M_all_variables_%s.csv", COUNTRY), row.names = FALSE)
write.csv(cons24, sprintf("credible_intervals_szbvarx_24M_all_variables_%s.csv", COUNTRY), row.names = FALSE)
write.csv(consALL, sprintf("credible_intervals_szbvarx_all_variables_%s.csv", COUNTRY), row.names = FALSE)

summary_table <- data.frame(
  variable          = names(ppi_12M$avg_widths),
  country           = COUNTRY,
  avg_width_12M     = ppi_12M$avg_widths,
  accept_12M        = ppi_12M$accept_rates,
  avg_width_24M     = ppi_24M$avg_widths,
  accept_24M        = ppi_24M$accept_rates,
  avg_width_overall = (ppi_12M$avg_widths + ppi_24M$avg_widths)/2,
  row.names = NULL
)
write.csv(summary_table, sprintf("average_interval_widths_szbvarx_%s.csv", COUNTRY), row.names = FALSE)
cat("  ✓ CSV files written\n\n")

# Capture final RNG state
rng_state_final <- .Random.seed

# Save reproducibility metadata
cat("Saving reproducibility metadata...\n")
repro_metadata <- list(
  country = COUNTRY,
  seed = SEED,
  n_sim = n_sim,
  gamma = gamma_level,
  endog_vars = endog_vars,
  exog_vars = exog_vars,
  support_bounds = support_bounds,
  rng_state_initial = rng_state_initial,
  rng_state_final = rng_state_final,
  timestamp = Sys.time(),
  session_info = sessionInfo(),
  r_version = R.version.string,
  platform = R.version$platform
)
saveRDS(repro_metadata, sprintf("reproducibility_metadata_szbvarx_ppi_%s.rds", COUNTRY))
cat(sprintf("  ✓ Metadata saved: reproducibility_metadata_szbvarx_ppi_%s.rds\n", COUNTRY))

# Compute verification hashes
if (requireNamespace("digest", quietly = TRUE)) {
  cat("Computing verification hashes...\n")
  library(digest)
  verification <- data.frame(
    object = c("ppi_12M_intervals", "ppi_24M_intervals", "cons12", "cons24", "summary_table"),
    hash_sha256 = c(
      digest(ppi_12M$intervals, algo = "sha256"),
      digest(ppi_24M$intervals, algo = "sha256"),
      digest(cons12, algo = "sha256"),
      digest(cons24, algo = "sha256"),
      digest(summary_table, algo = "sha256")
    ),
    country = COUNTRY,
    seed = SEED,
    n_sim = n_sim,
    gamma = gamma_level,
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  write.csv(verification, sprintf("verification_hashes_szbvarx_ppi_%s.csv", COUNTRY), row.names = FALSE)
  cat(sprintf("  ✓ Hashes saved: verification_hashes_szbvarx_ppi_%s.csv\n\n", COUNTRY))
  
  cat("=== Verification Hashes (SHA-256) ===\n")
  print(verification[, c("object", "hash_sha256")], row.names = FALSE)
  cat("======================================\n\n")
}

# Save session info
cat("Saving session info...\n")
sink(sprintf("session_info_szbvarx_ppi_%s.txt", COUNTRY))
cat("================================================================================\n")
cat(sprintf("  SZBVARX PPI Generation - Session Info (%s)\n", toupper(COUNTRY)))
cat("================================================================================\n\n")
cat(sprintf("Country:      %s\n", toupper(COUNTRY)))
cat(sprintf("Date:         %s\n", Sys.time()))
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n\n", gamma_level))
cat("Variables:\n")
cat(sprintf("  Endogenous: %s\n", paste(endog_vars, collapse=", ")))
cat(sprintf("  Exogenous:  %s\n\n", paste(exog_vars, collapse=", ")))
cat("Support bounds:\n")
for (v in names(support_bounds)) {
  b <- support_bounds[[v]]
  cat(sprintf("  %s: [%s, %s]\n", v, 
              ifelse(is.na(b[1]), "-Inf", b[1]),
              ifelse(is.na(b[2]), "+Inf", b[2])))
}
cat("\n")
cat("--------------------------------------------------------------------------------\n")
cat("R Session Info:\n")
cat("--------------------------------------------------------------------------------\n")
print(sessionInfo())
sink()
cat(sprintf("  ✓ Session info saved: session_info_szbvarx_ppi_%s.txt\n\n", COUNTRY))

# Summary statistics
cat("================================================================================\n")
cat("  SUMMARY STATISTICS\n")
cat("================================================================================\n\n")
cat("Average Interval Widths:\n")
print(summary_table, row.names = FALSE)
cat("\n")
cat("Average Acceptance Rates (Support Truncation):\n")
cat(sprintf("  12M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_12M$accept_rates), mean(ppi_12M$accept_rates) * gamma_level * 100))
cat(sprintf("  24M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_24M$accept_rates), mean(ppi_24M$accept_rates) * gamma_level * 100))
cat("\n")

cat("================================================================================\n")
cat("  DONE: PF-anchored, support-respecting credible intervals exported\n")
cat("================================================================================\n\n")

cat("Output files:\n")
cat(sprintf("  • credible_intervals_szbvarx_12M_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • credible_intervals_szbvarx_24M_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • credible_intervals_szbvarx_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • average_interval_widths_szbvarx_%s.csv\n", COUNTRY))
cat(sprintf("  • reproducibility_metadata_szbvarx_ppi_%s.rds\n", COUNTRY))
cat(sprintf("  • session_info_szbvarx_ppi_%s.txt\n", COUNTRY))
if (requireNamespace("digest", quietly = TRUE)) {
  cat(sprintf("  • verification_hashes_szbvarx_ppi_%s.csv\n", COUNTRY))
}
cat("\n")
cat("To verify reproducibility, re-run this script and compare hashes.\n")
cat(sprintf("To restore RNG state: .Random.seed <- readRDS('reproducibility_metadata_szbvarx_ppi_%s.rds')$rng_state_initial\n", COUNTRY))
cat("\n")
####################### End Of Code ##############################

#### SZBVAR Model: USA: 12M and 24M ahead - forecasts  ####
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/usa")
getwd()

########################### SZBVARx Model with HPs ##################
# Read the dataset
var.usa <- read.csv("all_mulvar_data_usa_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.usa$Date <- as.Date(var.usa$Date)
str(var.usa)


# Creation of datasets for 12M forecasts
var.usa.12M.train <- var.usa[1:327,]
var.usa.12M.val <- var.usa[328:339,]
var.usa.12M.test <- var.usa[340:351,]
var.usa.12M.full.train <- var.usa[1:339,]

# Creation of datasets for 24M forecasts
var.usa.24M.train <- var.usa[1:303,]
var.usa.24M.val <- var.usa[304:327,]
var.usa.24M.test <- var.usa[328:351,]
var.usa.24M.full.train <- var.usa[1:327,]


# Check for stationarity (using 12M full train data as an example) - Optional
for(col in c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate", 
             "logEPU", "GPRC", "USEMV", "USMPU")) {
  print(paste("KPSS test for", col))
  print(kpss.test(var.usa.12M.full.train[[col]], null="Trend"))
}


# Function to fit SZBVAR model and generate forecasts: 12M {based on best HPs}
fit_and_forecast_szbvar_12M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 4,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.2,
                  lambda1 = 0.05,
                  lambda3 = 3,
                  lambda4 = 0.1,
                  lambda5 = 0.5,
                  mu5 = 0.5,
                  mu6 = 0.5,
                  prior = 0 # Normal-Wishart prior,
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}

# Function to fit SZBVAR model and generate forecasts: 24M {based on best HPs}
fit_and_forecast_szbvar_24M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 4,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.2,
                  lambda1 = 0.1,
                  lambda3 = 1,
                  lambda4 = 0.1,
                  lambda5 = 0,
                  mu5 = 1,
                  mu6 = 0,
                  prior = 0 # Normal-Wishart prior,
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}


# 12M Forecasts
results_12M <- fit_and_forecast_szbvar_12M(var.usa.12M.full.train, var.usa.12M.test)
results_12M
# $forecasts
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         3.613051     106.8543    4.919479          73.62626         4.954784
# [2,]         3.704545     107.0846    4.907830          72.59121         4.869942
# [3,]         3.766158     106.9300    4.905529          73.26619         4.862381
# [4,]         3.896775     106.9699    4.878913          72.72545         4.796676
# [5,]         3.908105     106.7629    4.892018          73.98898         4.820142
# [6,]         3.986850     106.5579    4.878637          74.73092         4.812936
# [7,]         4.162202     106.3191    4.815919          74.75935         4.756637
# [8,]         4.339268     105.9727    4.758208          75.25226         4.725179
# [9,]         4.460580     105.8893    4.729359          75.12205         4.677416
# [10,]         4.859507     105.2403    4.571560          74.94021         4.583455
# [11,]         4.892545     105.1553    4.570137          75.42902         4.572465
# [12,]         5.118569     105.2467    4.491269          73.76306         4.444565
# 24M Forecasts
results_24M <- fit_and_forecast_szbvar_24M(var.usa.24M.full.train, var.usa.24M.test)
results_24M
# $forecasts
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         4.136928     103.2548   0.6915469         104.90885         8.426655
# [2,]         4.452973     104.0716   0.6675159         101.94705         8.310867
# [3,]         4.691300     104.9536   0.6737191          98.33722         8.152070
# [4,]         4.862240     105.6728   0.7119366          96.90079         8.082251
# [5,]         4.930581     106.4827   0.8005654          94.93172         7.988342
# [6,]         5.050445     106.8256   0.8104858          94.84182         7.983835
# [7,]         5.140943     107.5779   0.8759660          92.98723         7.896250
# [8,]         5.336937     107.7983   0.8400444          93.29884         7.915977
# [9,]         5.376623     108.3173   0.9150947          93.29199         7.913400
# [10,]         5.347640     109.0239   1.0294360          92.24680         7.857201
# [11,]         5.256148     109.6979   1.1740861          91.46647         7.814304
# [12,]         5.179387     110.5015   1.3148393          89.51767         7.716358
# [13,]         5.017754     111.2283   1.4909194          88.46606         7.653797
# [14,]         4.918596     111.7071   1.6158564          88.59602         7.648513
# [15,]         4.855561     112.1991   1.7230855          88.17013         7.624202
# [16,]         4.863141     112.4455   1.7828765          89.00138         7.664501
# [17,]         4.760264     112.8990   1.9031540          89.13677         7.661905
# [18,]         4.732468     113.4035   1.9914286          88.50854         7.628254
# [19,]         4.674783     113.7979   2.0868853          88.49036         7.624129
# [20,]         4.608349     114.0397   2.1782482          89.82328         7.679334
# [21,]         4.727436     114.0392   2.1314724          90.74053         7.720397
# [22,]         4.783549     114.1693   2.1069841          90.71061         7.709190
# [23,]         4.656063     114.5772   2.1809451          90.13059         7.650356
# [24,]         4.642327     114.7509   2.1456759          89.95696         7.596154

################# SZBVARx : USA - Credible PPIs ##########################
suppressPackageStartupMessages({ 
  library(MASS)
  if (!requireNamespace("digest", quietly = TRUE)) {
    message("Note: 'digest' package not available; verification hashes will be skipped")
  }
})

## ========================= USER SETTINGS ================================= ##
endog_vars <- c("Unemploymentrate","RealbroadEER","ShorttermIR",
                "OilpriceGlobalWTI","CPIinflationrate")
exog_vars  <- c("logEPU","GPRC","USEMV","USMPU")

# Admissible supports (edit per variable if needed; NA = unbounded)
support_bounds <- list(
  Unemploymentrate   = c(0, 100),
  RealbroadEER       = c(0,  NA),
  ShorttermIR        = c(NA, NA),
  OilpriceGlobalWTI  = c(0,  NA),
  CPIinflationrate   = c(NA, NA)
)

gamma_level <- 0.50
n_sim <- 1000
SEED <- 1234
## ======================================================================== ##


## ============================ UTILITIES ================================= ##
.num <- function(x) as.numeric(x)

.normalize_ar_cube <- function(Araw, m, p){
  d <- dim(Araw)
  if (length(d)!=3) stop("model$ar.coefs must be 3D")
  if (all(d == c(m,m,p))) return(Araw)
  if (all(d == c(p,m,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[L,,]; return(out) }
  if (all(d == c(m,p,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[,L,]; return(out) }
  stop("Unexpected ar.coefs dims: ", paste(d, collapse="x"))
}

.normalize_B <- function(Braw, m){
  dm <- dim(Braw); if (is.null(dm)) stop("model$exog.coefs must be a matrix when exog_vars != 0")
  if (dm[1]==m) return(Braw)     # (m x k)
  if (dm[2]==m) return(t(Braw))  # (k x m) -> (m x k)
  stop("exog.coefs incompatible dims: ", paste(dm, collapse="x"))
}

.exog_tail_from_training <- function(train_df, exog_vars, H){
  # Uses last H training observations as future exogenous path
  # Valid for: (1) known policy paths, (2) scenario analysis, (3) persistence assumption
  start <- nrow(train_df) - H + 1
  if (start < 1) stop("H exceeds training length.")
  as.matrix(train_df[start:nrow(train_df), exog_vars, drop=FALSE])
}

# One-step VARX with shock
.varx_step <- function(Yhist, A, cvec, B, z, Rchol){
  p <- dim(A)[3]; m <- length(cvec)
  y <- cvec
  for (L in 1:p) y <- y + A[,,L] %*% .num(Yhist[nrow(Yhist)-L+1, ])
  if (!is.null(B) && !is.null(z)) y <- y + B %*% .num(z)
  zstd <- rnorm(m); eps <- t(Rchol) %*% zstd  # chol returns upper triangular R
  .num(y + eps)
}

# Full H-step simulation path (proper MA accumulation)
.simulate_varx_path <- function(Yinit, A, cvec, B, Zfut, Rchol){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    z <- if (ncol(Zfut)>0) Zfut[h, ] else NULL
    y_next <- .varx_step(Yh, A, cvec, B, z, Rchol)
    out[h,] <- y_next
    Yh <- rbind(Yh, y_next)
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Deterministic path (ε≡0) for snap-centering
.det_path <- function(Yinit, A, cvec, B, Zfut){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    y <- cvec
    for (L in 1:p) y <- y + A[,,L] %*% .num(Yh[nrow(Yh)-L+1, ])
    if (!is.null(B) && ncol(Zfut)>0) y <- y + B %*% .num(Zfut[h, ])
    out[h,] <- .num(y)
    Yh <- rbind(Yh, t(y))
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Truncate simulated draws to admissible support
.truncate_draws <- function(x, lb, ub){
  keep <- rep(TRUE, length(x))
  if (!is.na(lb)) keep <- keep & (x >= lb)
  if (!is.na(ub)) keep <- keep & (x <= ub)
  x[keep]
}

# PF-anchored shortest (HPD-style) gamma-interval from draws within support
# Returns [L,U], acceptance rate, and a status flag
.pf_anchored_interval <- function(draws, pf, gamma, lb = NA_real_, ub = NA_real_, 
                                  var_name = "", horizon = 0){
  n_total <- length(draws)
  # Truncate to support
  x <- .truncate_draws(draws, lb, ub)
  acc_rate <- length(x) / max(1L, n_total)
  
  # Warn if low acceptance rate
  if (acc_rate < 0.5 && var_name != "") {
    warning(sprintf("%s horizon %d: accept_rate = %.3f (effective coverage = %.1f%%)", 
                    var_name, horizon, acc_rate, gamma * acc_rate * 100),
            call. = FALSE)
  }
  
  if (length(x) == 0L) {  # all mass out-of-support
    return(list(L = pf, U = pf, accept_rate = 0, status = "degenerate"))
  }
  
  x <- sort(x)
  n <- length(x)
  k <- max(1L, ceiling(gamma * n))
  
  # If PF outside the truncated range, anchor at PF by extending to nearest window
  if (pf <= x[1]) {
    j <- min(n, k)
    L <- pf; U <- x[j]
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="left-anchored"))
  }
  if (pf >= x[n]) {
    i <- max(1L, n - k + 1)
    L <- x[i]; U <- pf
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="right-anchored"))
  }
  
  # Sliding-window HPD subject to "must contain PF"
  bestL <- x[1]; bestU <- x[min(n,k)]; bestW <- bestU - bestL; found <- FALSE
  j <- k
  for (i in 1:(n - k + 1)) {
    j <- i + k - 1
    L <- x[i]; U <- x[j]
    if (pf >= L && pf <= U) {
      w <- U - L
      if (!found || w < bestW) { bestW <- w; bestL <- L; bestU <- U; found <- TRUE }
    }
  }
  if (found) {
    return(list(L = bestL, U = bestU, accept_rate = acc_rate, status = "window"))
  }
  
  # Fallback: expand the closest window to include PF
  # (Preserves ≥ gamma mass, includes PF; width may be slightly larger than HPD)
  # Choose window whose center is closest to PF
  centers <- sapply(1:(n - k + 1), function(i) (x[i] + x[i+k-1]) / 2)
  idx <- which.min(abs(centers - pf))
  L <- min(x[idx], pf); U <- max(x[idx + k - 1], pf)
  list(L = L, U = U, accept_rate = acc_rate, status = "expanded")
}
## ======================================================================== ##


## ====================== MAIN: PPIs (PF-ANCHORED) ========================= ##
# Uses fitted szbvarx model, training data, and SAVED point forecasts
generate_ppi_pf_anchored <- function(model, train_df,
                                     endog_vars, exog_vars,
                                     point_forecasts,
                                     support_bounds,
                                     gamma = 0.50, n_sim = 1000,
                                     seed = NULL){
  
  # Set seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
    message(sprintf("  → RNG seed set to %d for reproducibility", seed))
  }
  
  # Enforce model ordering via eqnames
  eq <- attr(model, "eqnames"); if (is.null(eq)) eq <- colnames(point_forecasts)
  if (is.null(eq)) stop("Cannot infer model eqnames; set colnames(point_forecasts).")
  if (!all(eq %in% endog_vars)) stop("endog_vars missing: ", paste(setdiff(eq, endog_vars), collapse=", "))
  endog_vars <- eq
  if (!all(colnames(point_forecasts) == endog_vars))
    point_forecasts <- as.matrix(point_forecasts[, endog_vars, drop=FALSE])
  
  # Extract components
  p      <- as.integer(model$p)
  cvec   <- .num(model$intercept)
  Araw   <- model$ar.coefs
  Braw   <- if (!is.null(model$exog.coefs)) model$exog.coefs else NULL
  Sigma  <- as.matrix(model$mean.S)
  m <- length(endog_vars); H <- nrow(point_forecasts)
  if (length(cvec) != m) stop("Intercept length != number of endogenous series.")
  
  A <- .normalize_ar_cube(Araw, m, p)
  B <- if (!is.null(Braw) && length(exog_vars) > 0) .normalize_B(Braw, m) else NULL
  
  # Robust chol for Sigma with warning
  Rchol <- tryCatch({
    chol(Sigma)
  }, error = function(e) {
    warning("Sigma near-singular; adding jitter 1e-10 to diagonal", call. = FALSE)
    chol(Sigma + diag(1e-10, nrow(Sigma)))
  })
  
  # Initial history & exog FUT path (training tail; no leakage)
  Y <- as.matrix(train_df[, endog_vars, drop=FALSE])
  Yinit <- Y[(nrow(Y)-p+1):nrow(Y), , drop=FALSE]
  Zfut <- if (length(exog_vars)==0) matrix(numeric(0), H, 0) else {
    zt <- .exog_tail_from_training(train_df, exog_vars, H); colnames(zt) <- exog_vars; zt
  }
  
  # Deterministic path (ε=0) + snap-centering delta
  det <- .det_path(Yinit, A, cvec, B, Zfut)
  colnames(det) <- endog_vars
  delta <- point_forecasts - det  # H x m
  
  # Validate snap-centering (diagnostic)
  max_delta <- max(abs(delta))
  if (max_delta > 1e-3) {
    message(sprintf("  → Max snap-centering delta = %.4f (PF differs from deterministic path)", max_delta))
    message("     This is expected if PF comes from HP-tuned model with different settings")
  }
  
  # Monte Carlo with proper recursion
  sims <- array(NA_real_, dim = c(H, m, n_sim))
  dimnames(sims) <- list(NULL, endog_vars, NULL)
  for (s in 1:n_sim) {
    path <- .simulate_varx_path(Yinit, A, cvec, B, Zfut, Rchol)
    sims[,,s] <- path + delta   # snap-center to PF
  }
  
  # Build PF-anchored credible intervals after support truncation
  results <- vector("list", m); names(results) <- endog_vars
  avgw <- setNames(numeric(m), endog_vars)
  acc_avg <- setNames(numeric(m), endog_vars)
  
  for (j in seq_len(m)) {
    var <- endog_vars[j]
    b <- support_bounds[[var]]
    lb <- if (is.null(b)) NA_real_ else b[1]
    ub <- if (is.null(b)) NA_real_ else b[2]
    
    lower <- upper <- acc <- rep(NA_real_, H)
    for (h in 1:H) {
      draws <- sims[h, j, ]
      pf    <- as.numeric(point_forecasts[h, j])
      ans   <- .pf_anchored_interval(draws, pf, gamma, lb, ub, var, h)
      L <- ans$L; U <- ans$U
      if (U < L) { tmp <- U; U <- L; L <- tmp }  # logical consistency
      lower[h] <- L; upper[h] <- U; acc[h] <- ans$accept_rate
    }
    
    df <- data.frame(
      horizon        = 1:H,
      point_forecast = as.numeric(point_forecasts[, j]),
      lower_50       = lower,
      upper_50       = upper,
      interval_width = upper - lower,
      accept_rate    = acc,
      effective_coverage = gamma * acc  # Adjusted for truncation
    )
    results[[var]] <- df
    avgw[j] <- mean(df$interval_width, na.rm = TRUE)
    acc_avg[j] <- mean(df$accept_rate, na.rm = TRUE)
  }
  
  list(intervals = results,
       avg_widths = avgw,
       accept_rates = acc_avg,
       sims = sims)
}
## ======================================================================== ##


## ============================ RUN & EXPORT =============================== ##
## Enhanced with full reproducibility controls

cat("\n")
cat("================================================================================\n")
cat("  SZBVARX Forecast-Consistent Credible Intervals - PRODUCTION RUN\n")
cat("================================================================================\n\n")

# Capture initial RNG state
if (exists(".Random.seed")) {
  rng_state_initial <- .Random.seed
} else {
  set.seed(NULL)  # Initialize RNG
  rng_state_initial <- .Random.seed
}

# Display reproducibility info
cat("=== Reproducibility Configuration ===\n")
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n", gamma_level))
cat(sprintf("R version:    %s\n", R.version.string))
cat(sprintf("MASS version: %s\n", packageVersion("MASS")))
cat(sprintf("Platform:     %s\n", R.version$platform))
cat(sprintf("Timestamp:    %s\n", Sys.time()))
cat("======================================\n\n")

# Expected to exist in the environment:
# results_12M$model, results_12M$forecasts, var.usa.12M.full.train
# results_24M$model, results_24M$forecasts, var.usa.24M.full.train

cat("Validating input data...\n")
stopifnot(all(colnames(results_12M$forecasts) %in% endog_vars),
          all(colnames(results_24M$forecasts) %in% endog_vars))

# Align forecasts to eqnames
eq12 <- attr(results_12M$model, "eqnames"); if (is.null(eq12)) eq12 <- colnames(results_12M$forecasts)
eq24 <- attr(results_24M$model, "eqnames"); if (is.null(eq24)) eq24 <- colnames(results_24M$forecasts)
results_12M$forecasts <- as.matrix(results_12M$forecasts[, eq12, drop=FALSE])
results_24M$forecasts <- as.matrix(results_24M$forecasts[, eq24, drop=FALSE])
cat("  ✓ Input validation passed\n\n")

# Generate PF-anchored, support-respecting PPIs with explicit seeds
cat("Generating 12M prediction intervals...\n")
set.seed(SEED)  # Explicit seed for 12M
ppi_12M <- generate_ppi_pf_anchored(
  model = results_12M$model,
  train_df = var.usa.12M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_12M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED
)
cat("  ✓ 12M intervals complete\n\n")

cat("Generating 24M prediction intervals...\n")
set.seed(SEED + 1000)  # Different seed for 24M (ensures independence)
ppi_24M <- generate_ppi_pf_anchored(
  model = results_24M$model,
  train_df = var.usa.24M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_24M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED + 1000
)
cat("  ✓ 24M intervals complete\n\n")

# Consolidation helper
.consolidate <- function(ppi_obj, horizon_label){
  do.call(rbind, lapply(names(ppi_obj$intervals), function(v){
    d <- ppi_obj$intervals[[v]]
    d$variable <- v
    d$forecast_horizon <- horizon_label
    d$actual_value <- NA_real_
    d$covered <- NA_integer_
    d[, c("forecast_horizon","variable","horizon",
          "point_forecast","actual_value",
          "lower_50","upper_50","interval_width","covered","accept_rate","effective_coverage")]
  }))
}

cat("Consolidating results...\n")
cons12 <- .consolidate(ppi_12M, "12M")
cons24 <- .consolidate(ppi_24M, "24M")
consALL <- rbind(cons12, cons24)

# Write outputs
cat("Writing output files...\n")
write.csv(cons12, "credible_intervals_szbvarx_12M_all_variables_usa.csv", row.names = FALSE)
write.csv(cons24, "credible_intervals_szbvarx_24M_all_variables_usa.csv", row.names = FALSE)
write.csv(consALL,"credible_intervals_szbvarx_all_variables_usa.csv", row.names = FALSE)

summary_table <- data.frame(
  variable          = names(ppi_12M$avg_widths),
  country           = "usa",
  avg_width_12M     = ppi_12M$avg_widths,
  accept_12M        = ppi_12M$accept_rates,
  avg_width_24M     = ppi_24M$avg_widths,
  accept_24M        = ppi_24M$accept_rates,
  avg_width_overall = (ppi_12M$avg_widths + ppi_24M$avg_widths)/2,
  row.names = NULL
)
write.csv(summary_table, "average_interval_widths_szbvarx_usa.csv", row.names = FALSE)
cat("  ✓ CSV files written\n\n")

# Capture final RNG state
rng_state_final <- .Random.seed

# Save reproducibility metadata
cat("Saving reproducibility metadata...\n")
repro_metadata <- list(
  seed = SEED,
  n_sim = n_sim,
  gamma = gamma_level,
  endog_vars = endog_vars,
  exog_vars = exog_vars,
  support_bounds = support_bounds,
  rng_state_initial = rng_state_initial,
  rng_state_final = rng_state_final,
  timestamp = Sys.time(),
  session_info = sessionInfo(),
  r_version = R.version.string,
  platform = R.version$platform
)
saveRDS(repro_metadata, "reproducibility_metadata_szbvarx_ppi.rds")
cat("  ✓ Metadata saved: reproducibility_metadata_szbvarx_ppi.rds\n")

# Compute verification hashes
if (requireNamespace("digest", quietly = TRUE)) {
  cat("Computing verification hashes...\n")
  library(digest)
  verification <- data.frame(
    object = c("ppi_12M_intervals", "ppi_24M_intervals", "cons12", "cons24", "summary_table"),
    hash_sha256 = c(
      digest(ppi_12M$intervals, algo = "sha256"),
      digest(ppi_24M$intervals, algo = "sha256"),
      digest(cons12, algo = "sha256"),
      digest(cons24, algo = "sha256"),
      digest(summary_table, algo = "sha256")
    ),
    seed = SEED,
    n_sim = n_sim,
    gamma = gamma_level,
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  write.csv(verification, "verification_hashes_szbvarx_ppi.csv", row.names = FALSE)
  cat("  ✓ Hashes saved: verification_hashes_szbvarx_ppi.csv\n\n")
  
  cat("=== Verification Hashes (SHA-256) ===\n")
  print(verification[, c("object", "hash_sha256")], row.names = FALSE)
  cat("======================================\n\n")
}

# Save session info
cat("Saving session info...\n")
sink("session_info_szbvarx_ppi.txt")
cat("================================================================================\n")
cat("  SZBVARX PPI Generation - Session Info\n")
cat("================================================================================\n\n")
cat(sprintf("Date:         %s\n", Sys.time()))
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n\n", gamma_level))
cat("Variables:\n")
cat(sprintf("  Endogenous: %s\n", paste(endog_vars, collapse=", ")))
cat(sprintf("  Exogenous:  %s\n\n", paste(exog_vars, collapse=", ")))
cat("Support bounds:\n")
for (v in names(support_bounds)) {
  b <- support_bounds[[v]]
  cat(sprintf("  %s: [%s, %s]\n", v, 
              ifelse(is.na(b[1]), "-Inf", b[1]),
              ifelse(is.na(b[2]), "+Inf", b[2])))
}
cat("\n")
cat("--------------------------------------------------------------------------------\n")
cat("R Session Info:\n")
cat("--------------------------------------------------------------------------------\n")
print(sessionInfo())
sink()
cat("  ✓ Session info saved: session_info_szbvarx_ppi.txt\n\n")

# Summary statistics
cat("================================================================================\n")
cat("  SUMMARY STATISTICS\n")
cat("================================================================================\n\n")
cat("Average Interval Widths:\n")
print(summary_table, row.names = FALSE)
cat("\n")
cat("Average Acceptance Rates (Support Truncation):\n")
cat(sprintf("  12M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_12M$accept_rates), mean(ppi_12M$accept_rates) * gamma_level * 100))
cat(sprintf("  24M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_24M$accept_rates), mean(ppi_24M$accept_rates) * gamma_level * 100))
cat("\n")

cat("================================================================================\n")
cat("  DONE: PF-anchored, support-respecting credible intervals exported\n")
cat("================================================================================\n\n")

cat("Output files:\n")
cat("  • credible_intervals_szbvarx_12M_all_variables_usa.csv\n")
cat("  • credible_intervals_szbvarx_24M_all_variables_usa.csv\n")
cat("  • credible_intervals_szbvarx_all_variables_usa.csv\n")
cat("  • average_interval_widths_szbvarx_usa.csv\n")
cat("  • reproducibility_metadata_szbvarx_ppi.rds\n")
cat("  • session_info_szbvarx_ppi.txt\n")
if (requireNamespace("digest", quietly = TRUE)) {
  cat("  • verification_hashes_szbvarx_ppi.csv\n")
}
cat("\n")
cat("To verify reproducibility, re-run this script and compare hashes.\n")
cat("To restore RNG state: .Random.seed <- readRDS('reproducibility_metadata_szbvarx_ppi.rds')$rng_state_initial\n")
cat("\n")

####################### End Of Code ##############################

#### SZBVAR Model: FRANCE: 12M and 24M ahead - forecasts  ####
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/france")
getwd()

########################### SZBVARx Model with HPs ##################
# Read the dataset
var.france <- read.csv("all_mulvar_data_france_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.france$Date <- as.Date(var.france$Date)
str(var.france)


# Creation of datasets for 12M forecasts
var.france.12M.train <- var.france[1:327,]
var.france.12M.val <- var.france[328:339,]
var.france.12M.test <- var.france[340:351,]
var.france.12M.full.train <- var.france[1:339,]

# Creation of datasets for 24M forecasts
var.france.24M.train <- var.france[1:303,]
var.france.24M.val <- var.france[304:327,]
var.france.24M.test <- var.france[328:351,]
var.france.24M.full.train <- var.france[1:327,]


# Check for stationarity (using 12M full train data as an example) - Optional
for(col in c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate", 
             "logEPU", "GPRC", "USEMV", "USMPU")) {
  print(paste("KPSS test for", col))
  print(kpss.test(var.france.12M.full.train[[col]], null="Trend"))
}


# Function to fit SZBVAR model and generate forecasts: 12M {based on best HPs}
fit_and_forecast_szbvar_12M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 1,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.2,
                  lambda1 = 0.05,
                  lambda3 = 1,
                  lambda4 = 0.1,
                  lambda5 = 0,
                  mu5 = 1,
                  mu6 = 0,
                  prior = 0 # Normal-Wishart prior,
                  # prior = 1 # Normal-flat prior,
                  # prior = 2 # flat-flat prior (i.e., akin to MLE)
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}

# Function to fit SZBVAR model and generate forecasts: 24M {based on best HPs}
fit_and_forecast_szbvar_24M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 3,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.8,
                  lambda1 = 0.2,
                  lambda3 = 1,
                  lambda4 = 0.5,
                  lambda5 = 0,
                  mu5 = 0,
                  mu6 = 0.5,
                  prior = 0 # Normal-Wishart prior,
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}


# 12M Forecasts
results_12M <- fit_and_forecast_szbvar_12M(var.france.12M.full.train, var.france.12M.test)
results_12M
# $forecasts
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         6.988024     96.07047    2.938701          73.41112         5.748765
# [2,]         6.991407     96.07172    2.967269          72.46178         5.753136
# [3,]         6.980558     96.05717    3.001155          73.16557         5.822669
# [4,]         6.992196     96.13553    3.043799          72.85311         5.837418
# [5,]         6.982623     96.13898    3.097555          73.57330         5.886082
# [6,]         6.989463     96.18825    3.141988          73.93735         5.933945
# [7,]         7.027578     96.34833    3.174200          73.92417         5.983919
# [8,]         7.065616     96.52982    3.220220          74.22616         6.031489
# [9,]         7.085668     96.62556    3.268193          73.87419         6.045594
# [10,]         7.197887     97.08179    3.303988          73.72470         6.096844
# [11,]         7.187140     97.06333    3.347110          73.98057         6.139705
# [12,]         7.237794     97.25975    3.384666          72.46494         6.116870
# 24M Forecasts
results_24M <- fit_and_forecast_szbvar_24M(var.france.24M.full.train, var.france.24M.test)
results_24M
# $forecasts
# Unemploymentrate RealbroadEER  ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         7.373340     96.19184 -0.283178774         108.13243         4.489822
# [2,]         7.345300     96.20039 -0.111410194         104.46821         4.310264
# [3,]         7.310583     96.23625 -0.022394020          99.37390         4.041701
# [4,]         7.265224     96.18848  0.030912960          95.35989         3.779509
# [5,]         7.219729     96.08840  0.087935186          91.30684         3.531774
# [6,]         7.177761     96.17501  0.047681708          89.41542         3.379637
# [7,]         7.146792     96.18853  0.053114967          86.33722         3.216434
# [8,]         7.139901     96.33613  0.005629257          85.32606         3.108534
# [9,]         7.122338     96.30294  0.018495846          84.05575         2.990375
# [10,]         7.103654     96.30432  0.054328671          82.28952         2.859265
# [11,]         7.075504     96.21653  0.101231369          80.70644         2.738225
# [12,]         7.054552     96.16987  0.147757438          78.41790         2.617910
# [13,]         7.026861     96.01525  0.205104282          76.68327         2.506738
# [14,]         7.007597     95.94943  0.229562467          76.18419         2.434582
# [15,]         7.001513     95.92807  0.253467406          75.73516         2.377539
# [16,]         7.006293     96.01069  0.251105166          76.63703         2.345535
# [17,]         7.006405     96.00870  0.272363979          77.01556         2.315337
# [18,]         7.018434     96.00357  0.292927533          76.57465         2.288758
# [19,]         7.022960     96.01886  0.297771593          76.76315         2.264472
# [20,]         7.015611     96.00536  0.274614079          77.85120         2.279650
# [21,]         7.056748     96.18019  0.221384199          79.10863         2.317412
# [22,]         7.107950     96.26059  0.178093046          79.34500         2.355915
# [23,]         7.133916     96.10860  0.152627877          78.16662         2.388232
# [24,]         7.180817     96.10158  0.068772478          76.95353         2.441319

####  SZBVARx: France Credible PPIs  ####

suppressPackageStartupMessages({ 
  library(MASS)
  if (!requireNamespace("digest", quietly = TRUE)) {
    message("Note: 'digest' package not available; verification hashes will be skipped")
  }
})

## ========================= USER SETTINGS ================================= ##
endog_vars <- c("Unemploymentrate","RealbroadEER","ShorttermIR",
                "OilpriceGlobalWTI","CPIinflationrate")
exog_vars  <- c("logEPU","GPRC","USEMV","USMPU")

# Admissible supports (edit per variable if needed; NA = unbounded)
support_bounds <- list(
  Unemploymentrate   = c(0, 100),
  RealbroadEER       = c(0,  NA),
  ShorttermIR        = c(NA, NA),
  OilpriceGlobalWTI  = c(0,  NA),
  CPIinflationrate   = c(NA, NA)
)

gamma_level <- 0.50
n_sim <- 1000
SEED <- 1234
COUNTRY <- "france"  # Country identifier
## ======================================================================== ##


## ============================ UTILITIES ================================= ##
.num <- function(x) as.numeric(x)

.normalize_ar_cube <- function(Araw, m, p){
  d <- dim(Araw)
  if (length(d)!=3) stop("model$ar.coefs must be 3D")
  if (all(d == c(m,m,p))) return(Araw)
  if (all(d == c(p,m,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[L,,]; return(out) }
  if (all(d == c(m,p,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[,L,]; return(out) }
  stop("Unexpected ar.coefs dims: ", paste(d, collapse="x"))
}

.normalize_B <- function(Braw, m){
  dm <- dim(Braw); if (is.null(dm)) stop("model$exog.coefs must be a matrix when exog_vars != 0")
  if (dm[1]==m) return(Braw)     # (m x k)
  if (dm[2]==m) return(t(Braw))  # (k x m) -> (m x k)
  stop("exog.coefs incompatible dims: ", paste(dm, collapse="x"))
}

.exog_tail_from_training <- function(train_df, exog_vars, H){
  # Uses last H training observations as future exogenous path
  # Valid for: (1) known policy paths, (2) scenario analysis, (3) persistence assumption
  start <- nrow(train_df) - H + 1
  if (start < 1) stop("H exceeds training length.")
  as.matrix(train_df[start:nrow(train_df), exog_vars, drop=FALSE])
}

# One-step VARX with shock
.varx_step <- function(Yhist, A, cvec, B, z, Rchol){
  p <- dim(A)[3]; m <- length(cvec)
  y <- cvec
  for (L in 1:p) y <- y + A[,,L] %*% .num(Yhist[nrow(Yhist)-L+1, ])
  if (!is.null(B) && !is.null(z)) y <- y + B %*% .num(z)
  zstd <- rnorm(m); eps <- t(Rchol) %*% zstd  # chol returns upper triangular R
  .num(y + eps)
}

# Full H-step simulation path (proper MA accumulation)
.simulate_varx_path <- function(Yinit, A, cvec, B, Zfut, Rchol){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    z <- if (ncol(Zfut)>0) Zfut[h, ] else NULL
    y_next <- .varx_step(Yh, A, cvec, B, z, Rchol)
    out[h,] <- y_next
    Yh <- rbind(Yh, y_next)
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Deterministic path (ε≡0) for snap-centering
.det_path <- function(Yinit, A, cvec, B, Zfut){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    y <- cvec
    for (L in 1:p) y <- y + A[,,L] %*% .num(Yh[nrow(Yh)-L+1, ])
    if (!is.null(B) && ncol(Zfut)>0) y <- y + B %*% .num(Zfut[h, ])
    out[h,] <- .num(y)
    Yh <- rbind(Yh, t(y))
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Truncate simulated draws to admissible support
.truncate_draws <- function(x, lb, ub){
  keep <- rep(TRUE, length(x))
  if (!is.na(lb)) keep <- keep & (x >= lb)
  if (!is.na(ub)) keep <- keep & (x <= ub)
  x[keep]
}

# PF-anchored shortest (HPD-style) gamma-interval from draws within support
# Returns [L,U], acceptance rate, and a status flag
.pf_anchored_interval <- function(draws, pf, gamma, lb = NA_real_, ub = NA_real_, 
                                  var_name = "", horizon = 0){
  n_total <- length(draws)
  # Truncate to support
  x <- .truncate_draws(draws, lb, ub)
  acc_rate <- length(x) / max(1L, n_total)
  
  # Warn if low acceptance rate
  if (acc_rate < 0.5 && var_name != "") {
    warning(sprintf("%s horizon %d: accept_rate = %.3f (effective coverage = %.1f%%)", 
                    var_name, horizon, acc_rate, gamma * acc_rate * 100),
            call. = FALSE)
  }
  
  if (length(x) == 0L) {  # all mass out-of-support
    return(list(L = pf, U = pf, accept_rate = 0, status = "degenerate"))
  }
  
  x <- sort(x)
  n <- length(x)
  k <- max(1L, ceiling(gamma * n))
  
  # If PF outside the truncated range, anchor at PF by extending to nearest window
  if (pf <= x[1]) {
    j <- min(n, k)
    L <- pf; U <- x[j]
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="left-anchored"))
  }
  if (pf >= x[n]) {
    i <- max(1L, n - k + 1)
    L <- x[i]; U <- pf
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="right-anchored"))
  }
  
  # Sliding-window HPD subject to "must contain PF"
  bestL <- x[1]; bestU <- x[min(n,k)]; bestW <- bestU - bestL; found <- FALSE
  j <- k
  for (i in 1:(n - k + 1)) {
    j <- i + k - 1
    L <- x[i]; U <- x[j]
    if (pf >= L && pf <= U) {
      w <- U - L
      if (!found || w < bestW) { bestW <- w; bestL <- L; bestU <- U; found <- TRUE }
    }
  }
  if (found) {
    return(list(L = bestL, U = bestU, accept_rate = acc_rate, status = "window"))
  }
  
  # Fallback: expand the closest window to include PF
  # (Preserves ≥ gamma mass, includes PF; width may be slightly larger than HPD)
  # Choose window whose center is closest to PF
  centers <- sapply(1:(n - k + 1), function(i) (x[i] + x[i+k-1]) / 2)
  idx <- which.min(abs(centers - pf))
  L <- min(x[idx], pf); U <- max(x[idx + k - 1], pf)
  list(L = L, U = U, accept_rate = acc_rate, status = "expanded")
}
## ======================================================================== ##


## ====================== MAIN: PPIs (PF-ANCHORED) ========================= ##
# Uses fitted szbvarx model, training data, and SAVED point forecasts
generate_ppi_pf_anchored <- function(model, train_df,
                                     endog_vars, exog_vars,
                                     point_forecasts,
                                     support_bounds,
                                     gamma = 0.50, n_sim = 1000,
                                     seed = NULL){
  
  # Set seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
    message(sprintf("  → RNG seed set to %d for reproducibility", seed))
  }
  
  # Enforce model ordering via eqnames
  eq <- attr(model, "eqnames"); if (is.null(eq)) eq <- colnames(point_forecasts)
  if (is.null(eq)) stop("Cannot infer model eqnames; set colnames(point_forecasts).")
  if (!all(eq %in% endog_vars)) stop("endog_vars missing: ", paste(setdiff(eq, endog_vars), collapse=", "))
  endog_vars <- eq
  if (!all(colnames(point_forecasts) == endog_vars))
    point_forecasts <- as.matrix(point_forecasts[, endog_vars, drop=FALSE])
  
  # Extract components
  p      <- as.integer(model$p)
  cvec   <- .num(model$intercept)
  Araw   <- model$ar.coefs
  Braw   <- if (!is.null(model$exog.coefs)) model$exog.coefs else NULL
  Sigma  <- as.matrix(model$mean.S)
  m <- length(endog_vars); H <- nrow(point_forecasts)
  if (length(cvec) != m) stop("Intercept length != number of endogenous series.")
  
  A <- .normalize_ar_cube(Araw, m, p)
  B <- if (!is.null(Braw) && length(exog_vars) > 0) .normalize_B(Braw, m) else NULL
  
  # Robust chol for Sigma with warning
  Rchol <- tryCatch({
    chol(Sigma)
  }, error = function(e) {
    warning("Sigma near-singular; adding jitter 1e-10 to diagonal", call. = FALSE)
    chol(Sigma + diag(1e-10, nrow(Sigma)))
  })
  
  # Initial history & exog FUT path (training tail; no leakage)
  Y <- as.matrix(train_df[, endog_vars, drop=FALSE])
  Yinit <- Y[(nrow(Y)-p+1):nrow(Y), , drop=FALSE]
  Zfut <- if (length(exog_vars)==0) matrix(numeric(0), H, 0) else {
    zt <- .exog_tail_from_training(train_df, exog_vars, H); colnames(zt) <- exog_vars; zt
  }
  
  # Deterministic path (ε=0) + snap-centering delta
  det <- .det_path(Yinit, A, cvec, B, Zfut)
  colnames(det) <- endog_vars
  delta <- point_forecasts - det  # H x m
  
  # Validate snap-centering (diagnostic)
  max_delta <- max(abs(delta))
  if (max_delta > 1e-3) {
    message(sprintf("  → Max snap-centering delta = %.4f (PF differs from deterministic path)", max_delta))
    message("     This is expected if PF comes from HP-tuned model with different settings")
  }
  
  # Monte Carlo with proper recursion
  sims <- array(NA_real_, dim = c(H, m, n_sim))
  dimnames(sims) <- list(NULL, endog_vars, NULL)
  for (s in 1:n_sim) {
    path <- .simulate_varx_path(Yinit, A, cvec, B, Zfut, Rchol)
    sims[,,s] <- path + delta   # snap-center to PF
  }
  
  # Build PF-anchored credible intervals after support truncation
  results <- vector("list", m); names(results) <- endog_vars
  avgw <- setNames(numeric(m), endog_vars)
  acc_avg <- setNames(numeric(m), endog_vars)
  
  for (j in seq_len(m)) {
    var <- endog_vars[j]
    b <- support_bounds[[var]]
    lb <- if (is.null(b)) NA_real_ else b[1]
    ub <- if (is.null(b)) NA_real_ else b[2]
    
    lower <- upper <- acc <- rep(NA_real_, H)
    for (h in 1:H) {
      draws <- sims[h, j, ]
      pf    <- as.numeric(point_forecasts[h, j])
      ans   <- .pf_anchored_interval(draws, pf, gamma, lb, ub, var, h)
      L <- ans$L; U <- ans$U
      if (U < L) { tmp <- U; U <- L; L <- tmp }  # logical consistency
      lower[h] <- L; upper[h] <- U; acc[h] <- ans$accept_rate
    }
    
    df <- data.frame(
      horizon        = 1:H,
      point_forecast = as.numeric(point_forecasts[, j]),
      lower_50       = lower,
      upper_50       = upper,
      interval_width = upper - lower,
      accept_rate    = acc,
      effective_coverage = gamma * acc  # Adjusted for truncation
    )
    results[[var]] <- df
    avgw[j] <- mean(df$interval_width, na.rm = TRUE)
    acc_avg[j] <- mean(df$accept_rate, na.rm = TRUE)
  }
  
  list(intervals = results,
       avg_widths = avgw,
       accept_rates = acc_avg,
       sims = sims)
}
## ======================================================================== ##


## ============================ RUN & EXPORT =============================== ##
## Enhanced with full reproducibility controls

cat("\n")
cat("================================================================================\n")
cat("  SZBVARX Forecast-Consistent Credible Intervals - PRODUCTION RUN\n")
cat(sprintf("  Country: %s\n", toupper(COUNTRY)))
cat("================================================================================\n\n")

# Capture initial RNG state
if (exists(".Random.seed")) {
  rng_state_initial <- .Random.seed
} else {
  set.seed(NULL)  # Initialize RNG
  rng_state_initial <- .Random.seed
}

# Display reproducibility info
cat("=== Reproducibility Configuration ===\n")
cat(sprintf("Country:      %s\n", toupper(COUNTRY)))
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n", gamma_level))
cat(sprintf("R version:    %s\n", R.version.string))
cat(sprintf("MASS version: %s\n", packageVersion("MASS")))
cat(sprintf("Platform:     %s\n", R.version$platform))
cat(sprintf("Timestamp:    %s\n", Sys.time()))
cat("======================================\n\n")

# Expected to exist in the environment (FRANCE-specific):
# results_12M$model, results_12M$forecasts, var.france.12M.full.train
# results_24M$model, results_24M$forecasts, var.france.24M.full.train

cat("Validating input data...\n")
stopifnot(all(colnames(results_12M$forecasts) %in% endog_vars),
          all(colnames(results_24M$forecasts) %in% endog_vars))

# Align forecasts to eqnames
eq12 <- attr(results_12M$model, "eqnames"); if (is.null(eq12)) eq12 <- colnames(results_12M$forecasts)
eq24 <- attr(results_24M$model, "eqnames"); if (is.null(eq24)) eq24 <- colnames(results_24M$forecasts)
results_12M$forecasts <- as.matrix(results_12M$forecasts[, eq12, drop=FALSE])
results_24M$forecasts <- as.matrix(results_24M$forecasts[, eq24, drop=FALSE])
cat("  ✓ Input validation passed\n\n")

# Generate PF-anchored, support-respecting PPIs with explicit seeds
cat("Generating 12M prediction intervals...\n")
set.seed(SEED)  # Explicit seed for 12M
ppi_12M <- generate_ppi_pf_anchored(
  model = results_12M$model,
  train_df = var.france.12M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_12M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED
)
cat("  ✓ 12M intervals complete\n\n")

cat("Generating 24M prediction intervals...\n")
set.seed(SEED + 1000)  # Different seed for 24M (ensures independence)
ppi_24M <- generate_ppi_pf_anchored(
  model = results_24M$model,
  train_df = var.france.24M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_24M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED + 1000
)
cat("  ✓ 24M intervals complete\n\n")

# Consolidation helper
.consolidate <- function(ppi_obj, horizon_label){
  do.call(rbind, lapply(names(ppi_obj$intervals), function(v){
    d <- ppi_obj$intervals[[v]]
    d$variable <- v
    d$forecast_horizon <- horizon_label
    d$actual_value <- NA_real_
    d$covered <- NA_integer_
    d[, c("forecast_horizon","variable","horizon",
          "point_forecast","actual_value",
          "lower_50","upper_50","interval_width","covered","accept_rate","effective_coverage")]
  }))
}

cat("Consolidating results...\n")
cons12 <- .consolidate(ppi_12M, "12M")
cons24 <- .consolidate(ppi_24M, "24M")
consALL <- rbind(cons12, cons24)

# Write outputs (FRANCE-specific filenames)
cat("Writing output files...\n")
write.csv(cons12, sprintf("credible_intervals_szbvarx_12M_all_variables_%s.csv", COUNTRY), row.names = FALSE)
write.csv(cons24, sprintf("credible_intervals_szbvarx_24M_all_variables_%s.csv", COUNTRY), row.names = FALSE)
write.csv(consALL, sprintf("credible_intervals_szbvarx_all_variables_%s.csv", COUNTRY), row.names = FALSE)

summary_table <- data.frame(
  variable          = names(ppi_12M$avg_widths),
  country           = COUNTRY,
  avg_width_12M     = ppi_12M$avg_widths,
  accept_12M        = ppi_12M$accept_rates,
  avg_width_24M     = ppi_24M$avg_widths,
  accept_24M        = ppi_24M$accept_rates,
  avg_width_overall = (ppi_12M$avg_widths + ppi_24M$avg_widths)/2,
  row.names = NULL
)
write.csv(summary_table, sprintf("average_interval_widths_szbvarx_%s.csv", COUNTRY), row.names = FALSE)
cat("  ✓ CSV files written\n\n")

# Capture final RNG state
rng_state_final <- .Random.seed

# Save reproducibility metadata
cat("Saving reproducibility metadata...\n")
repro_metadata <- list(
  country = COUNTRY,
  seed = SEED,
  n_sim = n_sim,
  gamma = gamma_level,
  endog_vars = endog_vars,
  exog_vars = exog_vars,
  support_bounds = support_bounds,
  rng_state_initial = rng_state_initial,
  rng_state_final = rng_state_final,
  timestamp = Sys.time(),
  session_info = sessionInfo(),
  r_version = R.version.string,
  platform = R.version$platform
)
saveRDS(repro_metadata, sprintf("reproducibility_metadata_szbvarx_ppi_%s.rds", COUNTRY))
cat(sprintf("  ✓ Metadata saved: reproducibility_metadata_szbvarx_ppi_%s.rds\n", COUNTRY))

# Compute verification hashes
if (requireNamespace("digest", quietly = TRUE)) {
  cat("Computing verification hashes...\n")
  library(digest)
  verification <- data.frame(
    object = c("ppi_12M_intervals", "ppi_24M_intervals", "cons12", "cons24", "summary_table"),
    hash_sha256 = c(
      digest(ppi_12M$intervals, algo = "sha256"),
      digest(ppi_24M$intervals, algo = "sha256"),
      digest(cons12, algo = "sha256"),
      digest(cons24, algo = "sha256"),
      digest(summary_table, algo = "sha256")
    ),
    country = COUNTRY,
    seed = SEED,
    n_sim = n_sim,
    gamma = gamma_level,
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  write.csv(verification, sprintf("verification_hashes_szbvarx_ppi_%s.csv", COUNTRY), row.names = FALSE)
  cat(sprintf("  ✓ Hashes saved: verification_hashes_szbvarx_ppi_%s.csv\n\n", COUNTRY))
  
  cat("=== Verification Hashes (SHA-256) ===\n")
  print(verification[, c("object", "hash_sha256")], row.names = FALSE)
  cat("======================================\n\n")
}

# Save session info
cat("Saving session info...\n")
sink(sprintf("session_info_szbvarx_ppi_%s.txt", COUNTRY))
cat("================================================================================\n")
cat(sprintf("  SZBVARX PPI Generation - Session Info (%s)\n", toupper(COUNTRY)))
cat("================================================================================\n\n")
cat(sprintf("Country:      %s\n", toupper(COUNTRY)))
cat(sprintf("Date:         %s\n", Sys.time()))
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n\n", gamma_level))
cat("Variables:\n")
cat(sprintf("  Endogenous: %s\n", paste(endog_vars, collapse=", ")))
cat(sprintf("  Exogenous:  %s\n\n", paste(exog_vars, collapse=", ")))
cat("Support bounds:\n")
for (v in names(support_bounds)) {
  b <- support_bounds[[v]]
  cat(sprintf("  %s: [%s, %s]\n", v, 
              ifelse(is.na(b[1]), "-Inf", b[1]),
              ifelse(is.na(b[2]), "+Inf", b[2])))
}
cat("\n")
cat("--------------------------------------------------------------------------------\n")
cat("R Session Info:\n")
cat("--------------------------------------------------------------------------------\n")
print(sessionInfo())
sink()
cat(sprintf("  ✓ Session info saved: session_info_szbvarx_ppi_%s.txt\n\n", COUNTRY))

# Summary statistics
cat("================================================================================\n")
cat("  SUMMARY STATISTICS\n")
cat("================================================================================\n\n")
cat("Average Interval Widths:\n")
print(summary_table, row.names = FALSE)
cat("\n")
cat("Average Acceptance Rates (Support Truncation):\n")
cat(sprintf("  12M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_12M$accept_rates), mean(ppi_12M$accept_rates) * gamma_level * 100))
cat(sprintf("  24M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_24M$accept_rates), mean(ppi_24M$accept_rates) * gamma_level * 100))
cat("\n")

cat("================================================================================\n")
cat("  DONE: PF-anchored, support-respecting credible intervals exported\n")
cat("================================================================================\n\n")

cat("Output files:\n")
cat(sprintf("  • credible_intervals_szbvarx_12M_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • credible_intervals_szbvarx_24M_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • credible_intervals_szbvarx_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • average_interval_widths_szbvarx_%s.csv\n", COUNTRY))
cat(sprintf("  • reproducibility_metadata_szbvarx_ppi_%s.rds\n", COUNTRY))
cat(sprintf("  • session_info_szbvarx_ppi_%s.txt\n", COUNTRY))
if (requireNamespace("digest", quietly = TRUE)) {
  cat(sprintf("  • verification_hashes_szbvarx_ppi_%s.csv\n", COUNTRY))
}
cat("\n")
cat("To verify reproducibility, re-run this script and compare hashes.\n")
cat(sprintf("To restore RNG state: .Random.seed <- readRDS('reproducibility_metadata_szbvarx_ppi_%s.rds')$rng_state_initial\n", COUNTRY))
cat("\n")
########################## End of Code ##########################

#### SZBVAR Model: GERMANY: 12M and 24M ahead - forecasts  ####
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/germany")
getwd()

########################### SZBVARx Model with HPs ##################
# Read the dataset
var.germany <- read.csv("all_mulvar_data_germany_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.germany$Date <- as.Date(var.germany$Date)
str(var.germany)


# Creation of datasets for 12M forecasts
var.germany.12M.train <- var.germany[1:327,]
var.germany.12M.val <- var.germany[328:339,]
var.germany.12M.test <- var.germany[340:351,]
var.germany.12M.full.train <- var.germany[1:339,]

# Creation of datasets for 24M forecasts
var.germany.24M.train <- var.germany[1:303,]
var.germany.24M.val <- var.germany[304:327,]
var.germany.24M.test <- var.germany[328:351,]
var.germany.24M.full.train <- var.germany[1:327,]


# Check for stationarity (using 12M full train data as an example) - Optional
for(col in c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate", 
             "logEPU", "GPRC", "USEMV", "USMPU")) {
  print(paste("KPSS test for", col))
  print(kpss.test(var.germany.12M.full.train[[col]], null="Trend"))
}


# Function to fit SZBVAR model and generate forecasts: 12M {based on best HPs}
fit_and_forecast_szbvar_12M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 1,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.2,
                  lambda1 = 0.05,
                  lambda3 = 1,
                  lambda4 = 0.1,
                  lambda5 = 0,
                  mu5 = 1,
                  mu6 = 0,
                  prior = 0 # Normal-Wishart prior,
                  # prior = 1 # Normal-flat prior,
                  # prior = 2 # flat-flat prior (i.e., akin to MLE)
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}

# Function to fit SZBVAR model and generate forecasts: 24M {based on best HPs}
fit_and_forecast_szbvar_24M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 4,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.4,
                  lambda1 = 0.05,
                  lambda3 = 2,
                  lambda4 = 0.5,
                  lambda5 = 0,
                  mu5 = 1,
                  mu6 = 0.5,
                  prior = 0 # Normal-Wishart prior,
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}


# 12M Forecasts
results_12M <- fit_and_forecast_szbvar_12M(var.germany.12M.full.train, var.germany.12M.test)
results_12M
# $forecasts
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         2.893242     99.25902    2.972832          74.71375         7.606948
# [2,]         2.885656     99.23300    3.021844          74.16730         7.666231
# [3,]         2.891178     99.15876    3.088916          75.47858         7.830540
# [4,]         2.898928     99.18181    3.143253          75.35184         7.890889
# [5,]         2.897077     99.13528    3.217904          76.67920         7.993962
# [6,]         2.909219     99.11255    3.287610          77.89956         8.125478
# [7,]         2.954619     99.25990    3.344057          78.06472         8.186523
# [8,]         2.999813     99.40190    3.406418          78.62318         8.252265
# [9,]         3.012706     99.48168    3.467137          78.35998         8.268864
# [10,]         3.134406     99.90554    3.510305          78.07469         8.297152
# [11,]         3.128282     99.87014    3.580352          78.50980         8.375385
# [12,]         3.158549    100.06168    3.621564          76.72230         8.327642
# 24M Forecasts
results_24M <- fit_and_forecast_szbvar_24M(var.germany.24M.full.train, var.germany.24M.test)
results_24M
# $forecasts
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         2.991405     98.69361  -0.5089668         104.94839         5.673235
# [2,]         2.974347     98.92724  -0.5152221         102.05350         5.503430
# [3,]         2.948067     99.14890  -0.5104106          98.68689         5.310720
# [4,]         2.909288     99.21682  -0.4976628          96.99559         5.196627
# [5,]         2.848713     99.22527  -0.4696149          95.12872         5.095047
# [6,]         2.835789     99.22950  -0.4748429          95.27276         5.073884
# [7,]         2.783205     99.25908  -0.4652595          93.65489         4.996446
# [8,]         2.794222     99.33223  -0.4857604          93.70779         4.949608
# [9,]         2.749473     99.27877  -0.4662601          93.21486         4.901790
# [10,]         2.688115     99.24022  -0.4362980          91.74935         4.815468
# [11,]         2.612886     99.11859  -0.4008721          90.91931         4.773641
# [12,]         2.537181     99.05752  -0.3703143          89.18566         4.698912
# [13,]         2.449876     98.89029  -0.3314993          88.41117         4.676137
# [14,]         2.395583     98.74244  -0.3051471          88.40318         4.665540
# [15,]         2.349105     98.64850  -0.2852037          87.90577         4.626321
# [16,]         2.325195     98.55059  -0.2855746          88.53672         4.626757
# [17,]         2.268906     98.39171  -0.2725192          88.88337         4.647469
# [18,]         2.227250     98.33339  -0.2665606          88.25962         4.607765
# [19,]         2.185978     98.22152  -0.2576535          88.31847         4.597691
# [20,]         2.142035     98.00401  -0.2559760          90.01722         4.689709
# [21,]         2.165351     97.99427  -0.3030428          91.57093         4.757894
# [22,]         2.175755     97.98391  -0.3443027          92.69549         4.831341
# [23,]         2.122812     97.76217  -0.3631833          94.72300         5.050867
# [24,]         2.125662     97.68736  -0.4265353          97.18611         5.276773

################### SZBVARx: Germany -  Credible PPIs ###############################
suppressPackageStartupMessages({ 
  library(MASS)
  if (!requireNamespace("digest", quietly = TRUE)) {
    message("Note: 'digest' package not available; verification hashes will be skipped")
  }
})

## ========================= USER SETTINGS ================================= ##
endog_vars <- c("Unemploymentrate","RealbroadEER","ShorttermIR",
                "OilpriceGlobalWTI","CPIinflationrate")
exog_vars  <- c("logEPU","GPRC","USEMV","USMPU")

# Admissible supports (edit per variable if needed; NA = unbounded)
support_bounds <- list(
  Unemploymentrate   = c(0, 100),
  RealbroadEER       = c(0,  NA),
  ShorttermIR        = c(NA, NA),
  OilpriceGlobalWTI  = c(0,  NA),
  CPIinflationrate   = c(NA, NA)
)

gamma_level <- 0.50
n_sim <- 1000
SEED <- 1234
COUNTRY <- "germany"  # Country identifier
## ======================================================================== ##


## ============================ UTILITIES ================================= ##
.num <- function(x) as.numeric(x)

.normalize_ar_cube <- function(Araw, m, p){
  d <- dim(Araw)
  if (length(d)!=3) stop("model$ar.coefs must be 3D")
  if (all(d == c(m,m,p))) return(Araw)
  if (all(d == c(p,m,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[L,,]; return(out) }
  if (all(d == c(m,p,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[,L,]; return(out) }
  stop("Unexpected ar.coefs dims: ", paste(d, collapse="x"))
}

.normalize_B <- function(Braw, m){
  dm <- dim(Braw); if (is.null(dm)) stop("model$exog.coefs must be a matrix when exog_vars != 0")
  if (dm[1]==m) return(Braw)     # (m x k)
  if (dm[2]==m) return(t(Braw))  # (k x m) -> (m x k)
  stop("exog.coefs incompatible dims: ", paste(dm, collapse="x"))
}

.exog_tail_from_training <- function(train_df, exog_vars, H){
  # Uses last H training observations as future exogenous path
  # Valid for: (1) known policy paths, (2) scenario analysis, (3) persistence assumption
  start <- nrow(train_df) - H + 1
  if (start < 1) stop("H exceeds training length.")
  as.matrix(train_df[start:nrow(train_df), exog_vars, drop=FALSE])
}

# One-step VARX with shock
.varx_step <- function(Yhist, A, cvec, B, z, Rchol){
  p <- dim(A)[3]; m <- length(cvec)
  y <- cvec
  for (L in 1:p) y <- y + A[,,L] %*% .num(Yhist[nrow(Yhist)-L+1, ])
  if (!is.null(B) && !is.null(z)) y <- y + B %*% .num(z)
  zstd <- rnorm(m); eps <- t(Rchol) %*% zstd  # chol returns upper triangular R
  .num(y + eps)
}

# Full H-step simulation path (proper MA accumulation)
.simulate_varx_path <- function(Yinit, A, cvec, B, Zfut, Rchol){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    z <- if (ncol(Zfut)>0) Zfut[h, ] else NULL
    y_next <- .varx_step(Yh, A, cvec, B, z, Rchol)
    out[h,] <- y_next
    Yh <- rbind(Yh, y_next)
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Deterministic path (ε≡0) for snap-centering
.det_path <- function(Yinit, A, cvec, B, Zfut){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    y <- cvec
    for (L in 1:p) y <- y + A[,,L] %*% .num(Yh[nrow(Yh)-L+1, ])
    if (!is.null(B) && ncol(Zfut)>0) y <- y + B %*% .num(Zfut[h, ])
    out[h,] <- .num(y)
    Yh <- rbind(Yh, t(y))
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Truncate simulated draws to admissible support
.truncate_draws <- function(x, lb, ub){
  keep <- rep(TRUE, length(x))
  if (!is.na(lb)) keep <- keep & (x >= lb)
  if (!is.na(ub)) keep <- keep & (x <= ub)
  x[keep]
}

# PF-anchored shortest (HPD-style) gamma-interval from draws within support
# Returns [L,U], acceptance rate, and a status flag
.pf_anchored_interval <- function(draws, pf, gamma, lb = NA_real_, ub = NA_real_, 
                                  var_name = "", horizon = 0){
  n_total <- length(draws)
  # Truncate to support
  x <- .truncate_draws(draws, lb, ub)
  acc_rate <- length(x) / max(1L, n_total)
  
  # Warn if low acceptance rate
  if (acc_rate < 0.5 && var_name != "") {
    warning(sprintf("%s horizon %d: accept_rate = %.3f (effective coverage = %.1f%%)", 
                    var_name, horizon, acc_rate, gamma * acc_rate * 100),
            call. = FALSE)
  }
  
  if (length(x) == 0L) {  # all mass out-of-support
    return(list(L = pf, U = pf, accept_rate = 0, status = "degenerate"))
  }
  
  x <- sort(x)
  n <- length(x)
  k <- max(1L, ceiling(gamma * n))
  
  # If PF outside the truncated range, anchor at PF by extending to nearest window
  if (pf <= x[1]) {
    j <- min(n, k)
    L <- pf; U <- x[j]
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="left-anchored"))
  }
  if (pf >= x[n]) {
    i <- max(1L, n - k + 1)
    L <- x[i]; U <- pf
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="right-anchored"))
  }
  
  # Sliding-window HPD subject to "must contain PF"
  bestL <- x[1]; bestU <- x[min(n,k)]; bestW <- bestU - bestL; found <- FALSE
  j <- k
  for (i in 1:(n - k + 1)) {
    j <- i + k - 1
    L <- x[i]; U <- x[j]
    if (pf >= L && pf <= U) {
      w <- U - L
      if (!found || w < bestW) { bestW <- w; bestL <- L; bestU <- U; found <- TRUE }
    }
  }
  if (found) {
    return(list(L = bestL, U = bestU, accept_rate = acc_rate, status = "window"))
  }
  
  # Fallback: expand the closest window to include PF
  # (Preserves ≥ gamma mass, includes PF; width may be slightly larger than HPD)
  # Choose window whose center is closest to PF
  centers <- sapply(1:(n - k + 1), function(i) (x[i] + x[i+k-1]) / 2)
  idx <- which.min(abs(centers - pf))
  L <- min(x[idx], pf); U <- max(x[idx + k - 1], pf)
  list(L = L, U = U, accept_rate = acc_rate, status = "expanded")
}
## ======================================================================== ##


## ====================== MAIN: PPIs (PF-ANCHORED) ========================= ##
# Uses fitted szbvarx model, training data, and SAVED point forecasts
generate_ppi_pf_anchored <- function(model, train_df,
                                     endog_vars, exog_vars,
                                     point_forecasts,
                                     support_bounds,
                                     gamma = 0.50, n_sim = 1000,
                                     seed = NULL){
  
  # Set seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
    message(sprintf("  → RNG seed set to %d for reproducibility", seed))
  }
  
  # Enforce model ordering via eqnames
  eq <- attr(model, "eqnames"); if (is.null(eq)) eq <- colnames(point_forecasts)
  if (is.null(eq)) stop("Cannot infer model eqnames; set colnames(point_forecasts).")
  if (!all(eq %in% endog_vars)) stop("endog_vars missing: ", paste(setdiff(eq, endog_vars), collapse=", "))
  endog_vars <- eq
  if (!all(colnames(point_forecasts) == endog_vars))
    point_forecasts <- as.matrix(point_forecasts[, endog_vars, drop=FALSE])
  
  # Extract components
  p      <- as.integer(model$p)
  cvec   <- .num(model$intercept)
  Araw   <- model$ar.coefs
  Braw   <- if (!is.null(model$exog.coefs)) model$exog.coefs else NULL
  Sigma  <- as.matrix(model$mean.S)
  m <- length(endog_vars); H <- nrow(point_forecasts)
  if (length(cvec) != m) stop("Intercept length != number of endogenous series.")
  
  A <- .normalize_ar_cube(Araw, m, p)
  B <- if (!is.null(Braw) && length(exog_vars) > 0) .normalize_B(Braw, m) else NULL
  
  # Robust chol for Sigma with warning
  Rchol <- tryCatch({
    chol(Sigma)
  }, error = function(e) {
    warning("Sigma near-singular; adding jitter 1e-10 to diagonal", call. = FALSE)
    chol(Sigma + diag(1e-10, nrow(Sigma)))
  })
  
  # Initial history & exog FUT path (training tail; no leakage)
  Y <- as.matrix(train_df[, endog_vars, drop=FALSE])
  Yinit <- Y[(nrow(Y)-p+1):nrow(Y), , drop=FALSE]
  Zfut <- if (length(exog_vars)==0) matrix(numeric(0), H, 0) else {
    zt <- .exog_tail_from_training(train_df, exog_vars, H); colnames(zt) <- exog_vars; zt
  }
  
  # Deterministic path (ε=0) + snap-centering delta
  det <- .det_path(Yinit, A, cvec, B, Zfut)
  colnames(det) <- endog_vars
  delta <- point_forecasts - det  # H x m
  
  # Validate snap-centering (diagnostic)
  max_delta <- max(abs(delta))
  if (max_delta > 1e-3) {
    message(sprintf("  → Max snap-centering delta = %.4f (PF differs from deterministic path)", max_delta))
    message("     This is expected if PF comes from HP-tuned model with different settings")
  }
  
  # Monte Carlo with proper recursion
  sims <- array(NA_real_, dim = c(H, m, n_sim))
  dimnames(sims) <- list(NULL, endog_vars, NULL)
  for (s in 1:n_sim) {
    path <- .simulate_varx_path(Yinit, A, cvec, B, Zfut, Rchol)
    sims[,,s] <- path + delta   # snap-center to PF
  }
  
  # Build PF-anchored credible intervals after support truncation
  results <- vector("list", m); names(results) <- endog_vars
  avgw <- setNames(numeric(m), endog_vars)
  acc_avg <- setNames(numeric(m), endog_vars)
  
  for (j in seq_len(m)) {
    var <- endog_vars[j]
    b <- support_bounds[[var]]
    lb <- if (is.null(b)) NA_real_ else b[1]
    ub <- if (is.null(b)) NA_real_ else b[2]
    
    lower <- upper <- acc <- rep(NA_real_, H)
    for (h in 1:H) {
      draws <- sims[h, j, ]
      pf    <- as.numeric(point_forecasts[h, j])
      ans   <- .pf_anchored_interval(draws, pf, gamma, lb, ub, var, h)
      L <- ans$L; U <- ans$U
      if (U < L) { tmp <- U; U <- L; L <- tmp }  # logical consistency
      lower[h] <- L; upper[h] <- U; acc[h] <- ans$accept_rate
    }
    
    df <- data.frame(
      horizon        = 1:H,
      point_forecast = as.numeric(point_forecasts[, j]),
      lower_50       = lower,
      upper_50       = upper,
      interval_width = upper - lower,
      accept_rate    = acc,
      effective_coverage = gamma * acc  # Adjusted for truncation
    )
    results[[var]] <- df
    avgw[j] <- mean(df$interval_width, na.rm = TRUE)
    acc_avg[j] <- mean(df$accept_rate, na.rm = TRUE)
  }
  
  list(intervals = results,
       avg_widths = avgw,
       accept_rates = acc_avg,
       sims = sims)
}
## ======================================================================== ##


## ============================ RUN & EXPORT =============================== ##
## Enhanced with full reproducibility controls

cat("\n")
cat("================================================================================\n")
cat("  SZBVARX Forecast-Consistent Credible Intervals - PRODUCTION RUN\n")
cat(sprintf("  Country: %s\n", toupper(COUNTRY)))
cat("================================================================================\n\n")

# Capture initial RNG state
if (exists(".Random.seed")) {
  rng_state_initial <- .Random.seed
} else {
  set.seed(NULL)  # Initialize RNG
  rng_state_initial <- .Random.seed
}

# Display reproducibility info
cat("=== Reproducibility Configuration ===\n")
cat(sprintf("Country:      %s\n", toupper(COUNTRY)))
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n", gamma_level))
cat(sprintf("R version:    %s\n", R.version.string))
cat(sprintf("MASS version: %s\n", packageVersion("MASS")))
cat(sprintf("Platform:     %s\n", R.version$platform))
cat(sprintf("Timestamp:    %s\n", Sys.time()))
cat("======================================\n\n")

# Expected to exist in the environment (GERMANY-specific):
# results_12M$model, results_12M$forecasts, var.germany.12M.full.train
# results_24M$model, results_24M$forecasts, var.germany.24M.full.train

cat("Validating input data...\n")
stopifnot(all(colnames(results_12M$forecasts) %in% endog_vars),
          all(colnames(results_24M$forecasts) %in% endog_vars))

# Align forecasts to eqnames
eq12 <- attr(results_12M$model, "eqnames"); if (is.null(eq12)) eq12 <- colnames(results_12M$forecasts)
eq24 <- attr(results_24M$model, "eqnames"); if (is.null(eq24)) eq24 <- colnames(results_24M$forecasts)
results_12M$forecasts <- as.matrix(results_12M$forecasts[, eq12, drop=FALSE])
results_24M$forecasts <- as.matrix(results_24M$forecasts[, eq24, drop=FALSE])
cat("  ✓ Input validation passed\n\n")

# Generate PF-anchored, support-respecting PPIs with explicit seeds
cat("Generating 12M prediction intervals...\n")
set.seed(SEED)  # Explicit seed for 12M
ppi_12M <- generate_ppi_pf_anchored(
  model = results_12M$model,
  train_df = var.germany.12M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_12M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED
)
cat("  ✓ 12M intervals complete\n\n")

cat("Generating 24M prediction intervals...\n")
set.seed(SEED + 1000)  # Different seed for 24M (ensures independence)
ppi_24M <- generate_ppi_pf_anchored(
  model = results_24M$model,
  train_df = var.germany.24M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_24M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED + 1000
)
cat("  ✓ 24M intervals complete\n\n")

# Consolidation helper
.consolidate <- function(ppi_obj, horizon_label){
  do.call(rbind, lapply(names(ppi_obj$intervals), function(v){
    d <- ppi_obj$intervals[[v]]
    d$variable <- v
    d$forecast_horizon <- horizon_label
    d$actual_value <- NA_real_
    d$covered <- NA_integer_
    d[, c("forecast_horizon","variable","horizon",
          "point_forecast","actual_value",
          "lower_50","upper_50","interval_width","covered","accept_rate","effective_coverage")]
  }))
}

cat("Consolidating results...\n")
cons12 <- .consolidate(ppi_12M, "12M")
cons24 <- .consolidate(ppi_24M, "24M")
consALL <- rbind(cons12, cons24)

# Write outputs (GERMANY-specific filenames)
cat("Writing output files...\n")
write.csv(cons12, sprintf("credible_intervals_szbvarx_12M_all_variables_%s.csv", COUNTRY), row.names = FALSE)
write.csv(cons24, sprintf("credible_intervals_szbvarx_24M_all_variables_%s.csv", COUNTRY), row.names = FALSE)
write.csv(consALL, sprintf("credible_intervals_szbvarx_all_variables_%s.csv", COUNTRY), row.names = FALSE)

summary_table <- data.frame(
  variable          = names(ppi_12M$avg_widths),
  country           = COUNTRY,
  avg_width_12M     = ppi_12M$avg_widths,
  accept_12M        = ppi_12M$accept_rates,
  avg_width_24M     = ppi_24M$avg_widths,
  accept_24M        = ppi_24M$accept_rates,
  avg_width_overall = (ppi_12M$avg_widths + ppi_24M$avg_widths)/2,
  row.names = NULL
)
write.csv(summary_table, sprintf("average_interval_widths_szbvarx_%s.csv", COUNTRY), row.names = FALSE)
cat("  ✓ CSV files written\n\n")

# Capture final RNG state
rng_state_final <- .Random.seed

# Save reproducibility metadata
cat("Saving reproducibility metadata...\n")
repro_metadata <- list(
  country = COUNTRY,
  seed = SEED,
  n_sim = n_sim,
  gamma = gamma_level,
  endog_vars = endog_vars,
  exog_vars = exog_vars,
  support_bounds = support_bounds,
  rng_state_initial = rng_state_initial,
  rng_state_final = rng_state_final,
  timestamp = Sys.time(),
  session_info = sessionInfo(),
  r_version = R.version.string,
  platform = R.version$platform
)
saveRDS(repro_metadata, sprintf("reproducibility_metadata_szbvarx_ppi_%s.rds", COUNTRY))
cat(sprintf("  ✓ Metadata saved: reproducibility_metadata_szbvarx_ppi_%s.rds\n", COUNTRY))

# Compute verification hashes
if (requireNamespace("digest", quietly = TRUE)) {
  cat("Computing verification hashes...\n")
  library(digest)
  verification <- data.frame(
    object = c("ppi_12M_intervals", "ppi_24M_intervals", "cons12", "cons24", "summary_table"),
    hash_sha256 = c(
      digest(ppi_12M$intervals, algo = "sha256"),
      digest(ppi_24M$intervals, algo = "sha256"),
      digest(cons12, algo = "sha256"),
      digest(cons24, algo = "sha256"),
      digest(summary_table, algo = "sha256")
    ),
    country = COUNTRY,
    seed = SEED,
    n_sim = n_sim,
    gamma = gamma_level,
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  write.csv(verification, sprintf("verification_hashes_szbvarx_ppi_%s.csv", COUNTRY), row.names = FALSE)
  cat(sprintf("  ✓ Hashes saved: verification_hashes_szbvarx_ppi_%s.csv\n\n", COUNTRY))
  
  cat("=== Verification Hashes (SHA-256) ===\n")
  print(verification[, c("object", "hash_sha256")], row.names = FALSE)
  cat("======================================\n\n")
}

# Save session info
cat("Saving session info...\n")
sink(sprintf("session_info_szbvarx_ppi_%s.txt", COUNTRY))
cat("================================================================================\n")
cat(sprintf("  SZBVARX PPI Generation - Session Info (%s)\n", toupper(COUNTRY)))
cat("================================================================================\n\n")
cat(sprintf("Country:      %s\n", toupper(COUNTRY)))
cat(sprintf("Date:         %s\n", Sys.time()))
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n\n", gamma_level))
cat("Variables:\n")
cat(sprintf("  Endogenous: %s\n", paste(endog_vars, collapse=", ")))
cat(sprintf("  Exogenous:  %s\n\n", paste(exog_vars, collapse=", ")))
cat("Support bounds:\n")
for (v in names(support_bounds)) {
  b <- support_bounds[[v]]
  cat(sprintf("  %s: [%s, %s]\n", v, 
              ifelse(is.na(b[1]), "-Inf", b[1]),
              ifelse(is.na(b[2]), "+Inf", b[2])))
}
cat("\n")
cat("--------------------------------------------------------------------------------\n")
cat("R Session Info:\n")
cat("--------------------------------------------------------------------------------\n")
print(sessionInfo())
sink()
cat(sprintf("  ✓ Session info saved: session_info_szbvarx_ppi_%s.txt\n\n", COUNTRY))

# Summary statistics
cat("================================================================================\n")
cat("  SUMMARY STATISTICS\n")
cat("================================================================================\n\n")
cat("Average Interval Widths:\n")
print(summary_table, row.names = FALSE)
cat("\n")
cat("Average Acceptance Rates (Support Truncation):\n")
cat(sprintf("  12M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_12M$accept_rates), mean(ppi_12M$accept_rates) * gamma_level * 100))
cat(sprintf("  24M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_24M$accept_rates), mean(ppi_24M$accept_rates) * gamma_level * 100))
cat("\n")

cat("================================================================================\n")
cat("  DONE: PF-anchored, support-respecting credible intervals exported\n")
cat("================================================================================\n\n")

cat("Output files:\n")
cat(sprintf("  • credible_intervals_szbvarx_12M_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • credible_intervals_szbvarx_24M_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • credible_intervals_szbvarx_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • average_interval_widths_szbvarx_%s.csv\n", COUNTRY))
cat(sprintf("  • reproducibility_metadata_szbvarx_ppi_%s.rds\n", COUNTRY))
cat(sprintf("  • session_info_szbvarx_ppi_%s.txt\n", COUNTRY))
if (requireNamespace("digest", quietly = TRUE)) {
  cat(sprintf("  • verification_hashes_szbvarx_ppi_%s.csv\n", COUNTRY))
}
cat("\n")
cat("To verify reproducibility, re-run this script and compare hashes.\n")
cat(sprintf("To restore RNG state: .Random.seed <- readRDS('reproducibility_metadata_szbvarx_ppi_%s.rds')$rng_state_initial\n", COUNTRY))
cat("\n")

################## End of Code ##########################

#### SZBVAR Model: JAPAN: 12M and 24M ahead - forecasts  ####
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/japan")
getwd()

########################### SZBVARx Model with HPs ##################
# Read the dataset
var.japan <- read.csv("all_mulvar_data_japan_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.japan$Date <- as.Date(var.japan$Date)
str(var.japan)


# Creation of datasets for 12M forecasts
var.japan.12M.train <- var.japan[1:327,]
var.japan.12M.val <- var.japan[328:339,]
var.japan.12M.test <- var.japan[340:351,]
var.japan.12M.full.train <- var.japan[1:339,]

# Creation of datasets for 24M forecasts
var.japan.24M.train <- var.japan[1:303,]
var.japan.24M.val <- var.japan[304:327,]
var.japan.24M.test <- var.japan[328:351,]
var.japan.24M.full.train <- var.japan[1:327,]


# Check for stationarity (using 12M full train data as an example) - Optional
for(col in c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate", 
             "logEPU", "GPRC", "USEMV", "USMPU")) {
  print(paste("KPSS test for", col))
  print(kpss.test(var.japan.12M.full.train[[col]], null="Trend"))
}


# Function to fit SZBVAR model and generate forecasts: 12M {based on best HPs}
fit_and_forecast_szbvar_12M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 1,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.2,
                  lambda1 = 0.05,
                  lambda3 = 1,
                  lambda4 = 0.1,
                  lambda5 = 0,
                  mu5 = 1,
                  mu6 = 0,
                  prior = 0 # Normal-Wishart prior,
                  # prior = 1 # Normal-flat prior,
                  # prior = 2 # flat-flat prior (i.e., akin to MLE)
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}

# Function to fit SZBVAR model and generate forecasts: 24M {based on best HPs}
fit_and_forecast_szbvar_24M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 2,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.2,
                  lambda1 = 0.05,
                  lambda3 = 1,
                  lambda4 = 0.1,
                  lambda5 = 0,
                  mu5 = 0.5,
                  mu6 = 0,
                  prior = 0 # Normal-Wishart prior,
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}


# 12M Forecasts
results_12M <- fit_and_forecast_szbvar_12M(var.japan.12M.full.train, var.japan.12M.test)
results_12M
# $forecasts
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         2.810290     77.30181 -0.03129346          72.46452         3.293448
# [2,]         2.843128     77.97433 -0.03739538          70.59470         3.301702
# [3,]         2.848200     77.23942 -0.03640252          70.55619         3.361412
# [4,]         2.872809     77.64514 -0.03874461          68.71774         3.390100
# [5,]         2.871446     76.52579 -0.03726344          69.41259         3.453477
# [6,]         2.879969     75.99371 -0.03510356          69.02982         3.519545
# [7,]         2.908744     75.87593 -0.03253136          68.17077         3.614566
# [8,]         2.928456     75.46942 -0.02802437          67.58525         3.715696
# [9,]         2.949330     75.51732 -0.02867159          66.28625         3.758752
# [10,]         2.996380     75.65615 -0.01841255          64.83321         3.939745
# [11,]         3.005088     75.27947 -0.01960509          64.10716         3.965720
# [12,]         3.056206     76.52003 -0.02433282          61.08093         3.988688
# 24M Forecasts
results_24M <- fit_and_forecast_szbvar_24M(var.japan.24M.full.train, var.japan.24M.test)
results_24M
# $forecasts
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         2.648207     86.00657 -0.02224800         104.69512         1.225858
# [2,]         2.685707     87.57725 -0.02431324         101.61398         1.246322
# [3,]         2.724133     89.43603 -0.02886110          98.08999         1.243431
# [4,]         2.737429     90.12459 -0.03125257          96.37446         1.251881
# [5,]         2.748108     91.02868 -0.03641791          94.23625         1.229680
# [6,]         2.750063     90.93164 -0.03286083          93.68086         1.280494
# [7,]         2.767393     92.16920 -0.03731756          91.01494         1.262125
# [8,]         2.773880     92.05085 -0.03203914          90.49411         1.328392
# [9,]         2.770195     92.18869 -0.03296182          89.43679         1.330529
# [10,]         2.772854     92.78911 -0.03809541          87.67769         1.299657
# [11,]         2.766598     93.03061 -0.04360104          86.49707         1.264108
# [12,]         2.776334     93.98103 -0.05234479          84.41862         1.214073
# [13,]         2.769175     94.29366 -0.05969371          83.18259         1.163004
# [14,]         2.757812     94.17288 -0.06180439          82.61864         1.153034
# [15,]         2.753534     94.30510 -0.06438727          81.67768         1.141609
# [16,]         2.741876     93.76776 -0.06215804          81.77173         1.169408
# [17,]         2.731164     93.63653 -0.06389649          81.19912         1.160801
# [18,]         2.734037     93.68384 -0.06756051          80.58829         1.155471
# [19,]         2.724979     93.26850 -0.06889508          80.51420         1.156788
# [20,]         2.703862     92.32764 -0.06668395          81.19546         1.179891
# [21,]         2.710378     91.83768 -0.05941953          81.44433         1.267278
# [22,]         2.724392     91.76400 -0.05487177          81.05102         1.334653
# [23,]         2.735759     91.81497 -0.05669625          80.62187         1.356836
# [24,]         2.764263     91.74258 -0.05330215          80.60323         1.441937

#################### SZBVARx: Japan - Credible PPIs #######################
suppressPackageStartupMessages({ 
  library(MASS)
  if (!requireNamespace("digest", quietly = TRUE)) {
    message("Note: 'digest' package not available; verification hashes will be skipped")
  }
})

## ========================= USER SETTINGS ================================= ##
endog_vars <- c("Unemploymentrate","RealbroadEER","ShorttermIR",
                "OilpriceGlobalWTI","CPIinflationrate")
exog_vars  <- c("logEPU","GPRC","USEMV","USMPU")

# Admissible supports (edit per variable if needed; NA = unbounded)
support_bounds <- list(
  Unemploymentrate   = c(0, 100),
  RealbroadEER       = c(0,  NA),
  ShorttermIR        = c(NA, NA),
  OilpriceGlobalWTI  = c(0,  NA),
  CPIinflationrate   = c(NA, NA)
)

gamma_level <- 0.50
n_sim <- 1000
SEED <- 1234
COUNTRY <- "japan"  # Country identifier
## ======================================================================== ##


## ============================ UTILITIES ================================= ##
.num <- function(x) as.numeric(x)

.normalize_ar_cube <- function(Araw, m, p){
  d <- dim(Araw)
  if (length(d)!=3) stop("model$ar.coefs must be 3D")
  if (all(d == c(m,m,p))) return(Araw)
  if (all(d == c(p,m,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[L,,]; return(out) }
  if (all(d == c(m,p,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[,L,]; return(out) }
  stop("Unexpected ar.coefs dims: ", paste(d, collapse="x"))
}

.normalize_B <- function(Braw, m){
  dm <- dim(Braw); if (is.null(dm)) stop("model$exog.coefs must be a matrix when exog_vars != 0")
  if (dm[1]==m) return(Braw)     # (m x k)
  if (dm[2]==m) return(t(Braw))  # (k x m) -> (m x k)
  stop("exog.coefs incompatible dims: ", paste(dm, collapse="x"))
}

.exog_tail_from_training <- function(train_df, exog_vars, H){
  # Uses last H training observations as future exogenous path
  # Valid for: (1) known policy paths, (2) scenario analysis, (3) persistence assumption
  start <- nrow(train_df) - H + 1
  if (start < 1) stop("H exceeds training length.")
  as.matrix(train_df[start:nrow(train_df), exog_vars, drop=FALSE])
}

# One-step VARX with shock
.varx_step <- function(Yhist, A, cvec, B, z, Rchol){
  p <- dim(A)[3]; m <- length(cvec)
  y <- cvec
  for (L in 1:p) y <- y + A[,,L] %*% .num(Yhist[nrow(Yhist)-L+1, ])
  if (!is.null(B) && !is.null(z)) y <- y + B %*% .num(z)
  zstd <- rnorm(m); eps <- t(Rchol) %*% zstd  # chol returns upper triangular R
  .num(y + eps)
}

# Full H-step simulation path (proper MA accumulation)
.simulate_varx_path <- function(Yinit, A, cvec, B, Zfut, Rchol){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    z <- if (ncol(Zfut)>0) Zfut[h, ] else NULL
    y_next <- .varx_step(Yh, A, cvec, B, z, Rchol)
    out[h,] <- y_next
    Yh <- rbind(Yh, y_next)
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Deterministic path (ε≡0) for snap-centering
.det_path <- function(Yinit, A, cvec, B, Zfut){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    y <- cvec
    for (L in 1:p) y <- y + A[,,L] %*% .num(Yh[nrow(Yh)-L+1, ])
    if (!is.null(B) && ncol(Zfut)>0) y <- y + B %*% .num(Zfut[h, ])
    out[h,] <- .num(y)
    Yh <- rbind(Yh, t(y))
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Truncate simulated draws to admissible support
.truncate_draws <- function(x, lb, ub){
  keep <- rep(TRUE, length(x))
  if (!is.na(lb)) keep <- keep & (x >= lb)
  if (!is.na(ub)) keep <- keep & (x <= ub)
  x[keep]
}

# PF-anchored shortest (HPD-style) gamma-interval from draws within support
# Returns [L,U], acceptance rate, and a status flag
.pf_anchored_interval <- function(draws, pf, gamma, lb = NA_real_, ub = NA_real_, 
                                  var_name = "", horizon = 0){
  n_total <- length(draws)
  # Truncate to support
  x <- .truncate_draws(draws, lb, ub)
  acc_rate <- length(x) / max(1L, n_total)
  
  # Warn if low acceptance rate
  if (acc_rate < 0.5 && var_name != "") {
    warning(sprintf("%s horizon %d: accept_rate = %.3f (effective coverage = %.1f%%)", 
                    var_name, horizon, acc_rate, gamma * acc_rate * 100),
            call. = FALSE)
  }
  
  if (length(x) == 0L) {  # all mass out-of-support
    return(list(L = pf, U = pf, accept_rate = 0, status = "degenerate"))
  }
  
  x <- sort(x)
  n <- length(x)
  k <- max(1L, ceiling(gamma * n))
  
  # If PF outside the truncated range, anchor at PF by extending to nearest window
  if (pf <= x[1]) {
    j <- min(n, k)
    L <- pf; U <- x[j]
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="left-anchored"))
  }
  if (pf >= x[n]) {
    i <- max(1L, n - k + 1)
    L <- x[i]; U <- pf
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="right-anchored"))
  }
  
  # Sliding-window HPD subject to "must contain PF"
  bestL <- x[1]; bestU <- x[min(n,k)]; bestW <- bestU - bestL; found <- FALSE
  j <- k
  for (i in 1:(n - k + 1)) {
    j <- i + k - 1
    L <- x[i]; U <- x[j]
    if (pf >= L && pf <= U) {
      w <- U - L
      if (!found || w < bestW) { bestW <- w; bestL <- L; bestU <- U; found <- TRUE }
    }
  }
  if (found) {
    return(list(L = bestL, U = bestU, accept_rate = acc_rate, status = "window"))
  }
  
  # Fallback: expand the closest window to include PF
  # (Preserves ≥ gamma mass, includes PF; width may be slightly larger than HPD)
  # Choose window whose center is closest to PF
  centers <- sapply(1:(n - k + 1), function(i) (x[i] + x[i+k-1]) / 2)
  idx <- which.min(abs(centers - pf))
  L <- min(x[idx], pf); U <- max(x[idx + k - 1], pf)
  list(L = L, U = U, accept_rate = acc_rate, status = "expanded")
}
## ======================================================================== ##


## ====================== MAIN: PPIs (PF-ANCHORED) ========================= ##
# Uses fitted szbvarx model, training data, and SAVED point forecasts
generate_ppi_pf_anchored <- function(model, train_df,
                                     endog_vars, exog_vars,
                                     point_forecasts,
                                     support_bounds,
                                     gamma = 0.50, n_sim = 1000,
                                     seed = NULL){
  
  # Set seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
    message(sprintf("  → RNG seed set to %d for reproducibility", seed))
  }
  
  # Enforce model ordering via eqnames
  eq <- attr(model, "eqnames"); if (is.null(eq)) eq <- colnames(point_forecasts)
  if (is.null(eq)) stop("Cannot infer model eqnames; set colnames(point_forecasts).")
  if (!all(eq %in% endog_vars)) stop("endog_vars missing: ", paste(setdiff(eq, endog_vars), collapse=", "))
  endog_vars <- eq
  if (!all(colnames(point_forecasts) == endog_vars))
    point_forecasts <- as.matrix(point_forecasts[, endog_vars, drop=FALSE])
  
  # Extract components
  p      <- as.integer(model$p)
  cvec   <- .num(model$intercept)
  Araw   <- model$ar.coefs
  Braw   <- if (!is.null(model$exog.coefs)) model$exog.coefs else NULL
  Sigma  <- as.matrix(model$mean.S)
  m <- length(endog_vars); H <- nrow(point_forecasts)
  if (length(cvec) != m) stop("Intercept length != number of endogenous series.")
  
  A <- .normalize_ar_cube(Araw, m, p)
  B <- if (!is.null(Braw) && length(exog_vars) > 0) .normalize_B(Braw, m) else NULL
  
  # Robust chol for Sigma with warning
  Rchol <- tryCatch({
    chol(Sigma)
  }, error = function(e) {
    warning("Sigma near-singular; adding jitter 1e-10 to diagonal", call. = FALSE)
    chol(Sigma + diag(1e-10, nrow(Sigma)))
  })
  
  # Initial history & exog FUT path (training tail; no leakage)
  Y <- as.matrix(train_df[, endog_vars, drop=FALSE])
  Yinit <- Y[(nrow(Y)-p+1):nrow(Y), , drop=FALSE]
  Zfut <- if (length(exog_vars)==0) matrix(numeric(0), H, 0) else {
    zt <- .exog_tail_from_training(train_df, exog_vars, H); colnames(zt) <- exog_vars; zt
  }
  
  # Deterministic path (ε=0) + snap-centering delta
  det <- .det_path(Yinit, A, cvec, B, Zfut)
  colnames(det) <- endog_vars
  delta <- point_forecasts - det  # H x m
  
  # Validate snap-centering (diagnostic)
  max_delta <- max(abs(delta))
  if (max_delta > 1e-3) {
    message(sprintf("  → Max snap-centering delta = %.4f (PF differs from deterministic path)", max_delta))
    message("     This is expected if PF comes from HP-tuned model with different settings")
  }
  
  # Monte Carlo with proper recursion
  sims <- array(NA_real_, dim = c(H, m, n_sim))
  dimnames(sims) <- list(NULL, endog_vars, NULL)
  for (s in 1:n_sim) {
    path <- .simulate_varx_path(Yinit, A, cvec, B, Zfut, Rchol)
    sims[,,s] <- path + delta   # snap-center to PF
  }
  
  # Build PF-anchored credible intervals after support truncation
  results <- vector("list", m); names(results) <- endog_vars
  avgw <- setNames(numeric(m), endog_vars)
  acc_avg <- setNames(numeric(m), endog_vars)
  
  for (j in seq_len(m)) {
    var <- endog_vars[j]
    b <- support_bounds[[var]]
    lb <- if (is.null(b)) NA_real_ else b[1]
    ub <- if (is.null(b)) NA_real_ else b[2]
    
    lower <- upper <- acc <- rep(NA_real_, H)
    for (h in 1:H) {
      draws <- sims[h, j, ]
      pf    <- as.numeric(point_forecasts[h, j])
      ans   <- .pf_anchored_interval(draws, pf, gamma, lb, ub, var, h)
      L <- ans$L; U <- ans$U
      if (U < L) { tmp <- U; U <- L; L <- tmp }  # logical consistency
      lower[h] <- L; upper[h] <- U; acc[h] <- ans$accept_rate
    }
    
    df <- data.frame(
      horizon        = 1:H,
      point_forecast = as.numeric(point_forecasts[, j]),
      lower_50       = lower,
      upper_50       = upper,
      interval_width = upper - lower,
      accept_rate    = acc,
      effective_coverage = gamma * acc  # Adjusted for truncation
    )
    results[[var]] <- df
    avgw[j] <- mean(df$interval_width, na.rm = TRUE)
    acc_avg[j] <- mean(df$accept_rate, na.rm = TRUE)
  }
  
  list(intervals = results,
       avg_widths = avgw,
       accept_rates = acc_avg,
       sims = sims)
}
## ======================================================================== ##


## ============================ RUN & EXPORT =============================== ##
## Enhanced with full reproducibility controls

cat("\n")
cat("================================================================================\n")
cat("  SZBVARX Forecast-Consistent Credible Intervals - PRODUCTION RUN\n")
cat(sprintf("  Country: %s\n", toupper(COUNTRY)))
cat("================================================================================\n\n")

# Capture initial RNG state
if (exists(".Random.seed")) {
  rng_state_initial <- .Random.seed
} else {
  set.seed(NULL)  # Initialize RNG
  rng_state_initial <- .Random.seed
}

# Display reproducibility info
cat("=== Reproducibility Configuration ===\n")
cat(sprintf("Country:      %s\n", toupper(COUNTRY)))
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n", gamma_level))
cat(sprintf("R version:    %s\n", R.version.string))
cat(sprintf("MASS version: %s\n", packageVersion("MASS")))
cat(sprintf("Platform:     %s\n", R.version$platform))
cat(sprintf("Timestamp:    %s\n", Sys.time()))
cat("======================================\n\n")

# Expected to exist in the environment (JAPAN-specific):
# results_12M$model, results_12M$forecasts, var.japan.12M.full.train
# results_24M$model, results_24M$forecasts, var.japan.24M.full.train

cat("Validating input data...\n")
stopifnot(all(colnames(results_12M$forecasts) %in% endog_vars),
          all(colnames(results_24M$forecasts) %in% endog_vars))

# Align forecasts to eqnames
eq12 <- attr(results_12M$model, "eqnames"); if (is.null(eq12)) eq12 <- colnames(results_12M$forecasts)
eq24 <- attr(results_24M$model, "eqnames"); if (is.null(eq24)) eq24 <- colnames(results_24M$forecasts)
results_12M$forecasts <- as.matrix(results_12M$forecasts[, eq12, drop=FALSE])
results_24M$forecasts <- as.matrix(results_24M$forecasts[, eq24, drop=FALSE])
cat("  ✓ Input validation passed\n\n")

# Generate PF-anchored, support-respecting PPIs with explicit seeds
cat("Generating 12M prediction intervals...\n")
set.seed(SEED)  # Explicit seed for 12M
ppi_12M <- generate_ppi_pf_anchored(
  model = results_12M$model,
  train_df = var.japan.12M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_12M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED
)
cat("  ✓ 12M intervals complete\n\n")

cat("Generating 24M prediction intervals...\n")
set.seed(SEED + 1000)  # Different seed for 24M (ensures independence)
ppi_24M <- generate_ppi_pf_anchored(
  model = results_24M$model,
  train_df = var.japan.24M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_24M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED + 1000
)
cat("  ✓ 24M intervals complete\n\n")

# Consolidation helper
.consolidate <- function(ppi_obj, horizon_label){
  do.call(rbind, lapply(names(ppi_obj$intervals), function(v){
    d <- ppi_obj$intervals[[v]]
    d$variable <- v
    d$forecast_horizon <- horizon_label
    d$actual_value <- NA_real_
    d$covered <- NA_integer_
    d[, c("forecast_horizon","variable","horizon",
          "point_forecast","actual_value",
          "lower_50","upper_50","interval_width","covered","accept_rate","effective_coverage")]
  }))
}

cat("Consolidating results...\n")
cons12 <- .consolidate(ppi_12M, "12M")
cons24 <- .consolidate(ppi_24M, "24M")
consALL <- rbind(cons12, cons24)

# Write outputs (JAPAN-specific filenames)
cat("Writing output files...\n")
write.csv(cons12, sprintf("credible_intervals_szbvarx_12M_all_variables_%s.csv", COUNTRY), row.names = FALSE)
write.csv(cons24, sprintf("credible_intervals_szbvarx_24M_all_variables_%s.csv", COUNTRY), row.names = FALSE)
write.csv(consALL, sprintf("credible_intervals_szbvarx_all_variables_%s.csv", COUNTRY), row.names = FALSE)

summary_table <- data.frame(
  variable          = names(ppi_12M$avg_widths),
  country           = COUNTRY,
  avg_width_12M     = ppi_12M$avg_widths,
  accept_12M        = ppi_12M$accept_rates,
  avg_width_24M     = ppi_24M$avg_widths,
  accept_24M        = ppi_24M$accept_rates,
  avg_width_overall = (ppi_12M$avg_widths + ppi_24M$avg_widths)/2,
  row.names = NULL
)
write.csv(summary_table, sprintf("average_interval_widths_szbvarx_%s.csv", COUNTRY), row.names = FALSE)
cat("  ✓ CSV files written\n\n")

# Capture final RNG state
rng_state_final <- .Random.seed

# Save reproducibility metadata
cat("Saving reproducibility metadata...\n")
repro_metadata <- list(
  country = COUNTRY,
  seed = SEED,
  n_sim = n_sim,
  gamma = gamma_level,
  endog_vars = endog_vars,
  exog_vars = exog_vars,
  support_bounds = support_bounds,
  rng_state_initial = rng_state_initial,
  rng_state_final = rng_state_final,
  timestamp = Sys.time(),
  session_info = sessionInfo(),
  r_version = R.version.string,
  platform = R.version$platform
)
saveRDS(repro_metadata, sprintf("reproducibility_metadata_szbvarx_ppi_%s.rds", COUNTRY))
cat(sprintf("  ✓ Metadata saved: reproducibility_metadata_szbvarx_ppi_%s.rds\n", COUNTRY))

# Compute verification hashes
if (requireNamespace("digest", quietly = TRUE)) {
  cat("Computing verification hashes...\n")
  library(digest)
  verification <- data.frame(
    object = c("ppi_12M_intervals", "ppi_24M_intervals", "cons12", "cons24", "summary_table"),
    hash_sha256 = c(
      digest(ppi_12M$intervals, algo = "sha256"),
      digest(ppi_24M$intervals, algo = "sha256"),
      digest(cons12, algo = "sha256"),
      digest(cons24, algo = "sha256"),
      digest(summary_table, algo = "sha256")
    ),
    country = COUNTRY,
    seed = SEED,
    n_sim = n_sim,
    gamma = gamma_level,
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  write.csv(verification, sprintf("verification_hashes_szbvarx_ppi_%s.csv", COUNTRY), row.names = FALSE)
  cat(sprintf("  ✓ Hashes saved: verification_hashes_szbvarx_ppi_%s.csv\n\n", COUNTRY))
  
  cat("=== Verification Hashes (SHA-256) ===\n")
  print(verification[, c("object", "hash_sha256")], row.names = FALSE)
  cat("======================================\n\n")
}

# Save session info
cat("Saving session info...\n")
sink(sprintf("session_info_szbvarx_ppi_%s.txt", COUNTRY))
cat("================================================================================\n")
cat(sprintf("  SZBVARX PPI Generation - Session Info (%s)\n", toupper(COUNTRY)))
cat("================================================================================\n\n")
cat(sprintf("Country:      %s\n", toupper(COUNTRY)))
cat(sprintf("Date:         %s\n", Sys.time()))
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n\n", gamma_level))
cat("Variables:\n")
cat(sprintf("  Endogenous: %s\n", paste(endog_vars, collapse=", ")))
cat(sprintf("  Exogenous:  %s\n\n", paste(exog_vars, collapse=", ")))
cat("Support bounds:\n")
for (v in names(support_bounds)) {
  b <- support_bounds[[v]]
  cat(sprintf("  %s: [%s, %s]\n", v, 
              ifelse(is.na(b[1]), "-Inf", b[1]),
              ifelse(is.na(b[2]), "+Inf", b[2])))
}
cat("\n")
cat("--------------------------------------------------------------------------------\n")
cat("R Session Info:\n")
cat("--------------------------------------------------------------------------------\n")
print(sessionInfo())
sink()
cat(sprintf("  ✓ Session info saved: session_info_szbvarx_ppi_%s.txt\n\n", COUNTRY))

# Summary statistics
cat("================================================================================\n")
cat("  SUMMARY STATISTICS\n")
cat("================================================================================\n\n")
cat("Average Interval Widths:\n")
print(summary_table, row.names = FALSE)
cat("\n")
cat("Average Acceptance Rates (Support Truncation):\n")
cat(sprintf("  12M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_12M$accept_rates), mean(ppi_12M$accept_rates) * gamma_level * 100))
cat(sprintf("  24M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_24M$accept_rates), mean(ppi_24M$accept_rates) * gamma_level * 100))
cat("\n")

cat("================================================================================\n")
cat("  DONE: PF-anchored, support-respecting credible intervals exported\n")
cat("================================================================================\n\n")

cat("Output files:\n")
cat(sprintf("  • credible_intervals_szbvarx_12M_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • credible_intervals_szbvarx_24M_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • credible_intervals_szbvarx_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • average_interval_widths_szbvarx_%s.csv\n", COUNTRY))
cat(sprintf("  • reproducibility_metadata_szbvarx_ppi_%s.rds\n", COUNTRY))
cat(sprintf("  • session_info_szbvarx_ppi_%s.txt\n", COUNTRY))
if (requireNamespace("digest", quietly = TRUE)) {
  cat(sprintf("  • verification_hashes_szbvarx_ppi_%s.csv\n", COUNTRY))
}
cat("\n")
cat("To verify reproducibility, re-run this script and compare hashes.\n")
cat(sprintf("To restore RNG state: .Random.seed <- readRDS('reproducibility_metadata_szbvarx_ppi_%s.rds')$rng_state_initial\n", COUNTRY))
cat("\n")
#################### End of Code ####################

#### SZBVAR Model: UK: 12M and 24M ahead - forecasts  ####

# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/uk")
getwd()

########################### SZBVARx Model with HPs ##################
# Read the dataset
var.uk <- read.csv("all_mulvar_data_uk_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.uk$Date <- as.Date(var.uk$Date)
str(var.uk)


# Creation of datasets for 12M forecasts
var.uk.12M.train <- var.uk[1:327,]
var.uk.12M.val <- var.uk[328:339,]
var.uk.12M.test <- var.uk[340:351,]
var.uk.12M.full.train <- var.uk[1:339,]

# Creation of datasets for 24M forecasts
var.uk.24M.train <- var.uk[1:303,]
var.uk.24M.val <- var.uk[304:327,]
var.uk.24M.test <- var.uk[328:351,]
var.uk.24M.full.train <- var.uk[1:327,]


# Check for stationarity (using 12M full train data as an example) - Optional
for(col in c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate", 
             "logEPU", "GPRC", "USEMV", "USMPU")) {
  print(paste("KPSS test for", col))
  print(kpss.test(var.uk.12M.full.train[[col]], null="Trend"))
}


# Function to fit SZBVAR model and generate forecasts: 12M {based on best HPs}
fit_and_forecast_szbvar_12M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 1,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.2,
                  lambda1 = 0.05,
                  lambda3 = 1,
                  lambda4 = 0.1,
                  lambda5 = 0,
                  mu5 = 1,
                  mu6 = 0,
                  prior = 0 # Normal-Wishart prior,
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}

# Function to fit SZBVAR model and generate forecasts: 24M {based on best HPs}
fit_and_forecast_szbvar_24M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 4,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.2,
                  lambda1 = 0.05,
                  lambda3 = 1,
                  lambda4 = 0.5,
                  lambda5 = 0,
                  mu5 = 1,
                  mu6 = 1,
                  prior = 0 # Normal-Wishart prior,
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}


# 12M Forecasts
results_12M <- fit_and_forecast_szbvar_12M(var.uk.12M.full.train, var.uk.12M.test)
results_12M
# $forecasts
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         3.682256    102.97740    4.414234          73.63456         8.937474
# [2,]         3.685327    102.75344    4.461043          72.39087         8.987744
# [3,]         3.674981    102.58796    4.555076          72.85714         9.087343
# [4,]         3.681467    102.29974    4.614889          72.03431         9.148710
# [5,]         3.669738    102.25357    4.725005          72.83943         9.253000
# [6,]         3.662916    102.04069    4.822050          73.31617         9.354210
# [7,]         3.672803    101.46917    4.891379          72.96558         9.440555
# [8,]         3.681069    100.93786    4.973797          72.95611         9.535500
# [9,]         3.690143    100.60932    5.044649          72.23078         9.603486
# [10,]         3.726405     99.33421    5.090675          71.28457         9.691388
# [11,]         3.725688     99.22095    5.181566          71.10182         9.769757
# [12,]         3.755467     98.60149    5.205271          68.77602         9.796291
# 24M Forecasts
results_24M <- fit_and_forecast_szbvar_24M(var.uk.24M.full.train, var.uk.24M.test)
results_24M
# $forecasts
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         3.650625    103.21116   0.8661360         105.45978         6.179182
# [2,]         3.695672    102.44763   0.7582164         102.86176         6.163601
# [3,]         3.740433    101.80042   0.6572603          99.89620         6.132698
# [4,]         3.764659    101.48209   0.6032460          98.54607         6.134954
# [5,]         3.788179    101.34527   0.5622812          96.81500         6.122001
# [6,]         3.801469    100.97021   0.5175260          96.99529         6.163611
# [7,]         3.823533    100.76081   0.4665560          95.46522         6.157476
# [8,]         3.839502    100.23871   0.4059871          95.87288         6.208412
# [9,]         3.852998    100.16588   0.3827119          95.50293         6.228242
# [10,]         3.867798    100.19345   0.3606182          94.40419         6.226257
# [11,]         3.875308    100.38031   0.3615330          93.61510         6.226415
# [12,]         3.890338    100.48663   0.3431053          91.99696         6.206590
# [13,]         3.894196    100.79982   0.3555323          91.13515         6.199955
# [14,]         3.896692    100.93868   0.3642100          91.05285         6.215300
# [15,]         3.905749    100.97969   0.3564839          90.62087         6.224656
# [16,]         3.909664    100.92764   0.3470836          91.37165         6.269028
# [17,]         3.908874    101.06885   0.3559212          91.56044         6.292036
# [18,]         3.916173    101.03955   0.3413128          91.13422         6.302687
# [19,]         3.920284    101.08789   0.3372041          91.32939         6.329408
# [20,]         3.909450    101.22390   0.3561985          92.76545         6.386002
# [21,]         3.916407    100.66644   0.3070294          93.96224         6.451661
# [22,]         3.924280    100.10137   0.2576954          94.42474         6.495097
# [23,]         3.915486     99.89219   0.2541465          94.40421         6.510760
# [24,]         3.903620     99.17402   0.2143893          95.02515         6.551003

############################ SZBVARx : UK - Credible PPIs ##################
suppressPackageStartupMessages({ 
  library(MASS)
  if (!requireNamespace("digest", quietly = TRUE)) {
    message("Note: 'digest' package not available; verification hashes will be skipped")
  }
})

## ========================= USER SETTINGS ================================= ##
endog_vars <- c("Unemploymentrate","RealbroadEER","ShorttermIR",
                "OilpriceGlobalWTI","CPIinflationrate")
exog_vars  <- c("logEPU","GPRC","USEMV","USMPU")

# Admissible supports (edit per variable if needed; NA = unbounded)
support_bounds <- list(
  Unemploymentrate   = c(0, 100),
  RealbroadEER       = c(0,  NA),
  ShorttermIR        = c(NA, NA),
  OilpriceGlobalWTI  = c(0,  NA),
  CPIinflationrate   = c(NA, NA)
)

gamma_level <- 0.50
n_sim <- 1000
SEED <- 1234
COUNTRY <- "uk"  # Country identifier
## ======================================================================== ##


## ============================ UTILITIES ================================= ##
.num <- function(x) as.numeric(x)

.normalize_ar_cube <- function(Araw, m, p){
  d <- dim(Araw)
  if (length(d)!=3) stop("model$ar.coefs must be 3D")
  if (all(d == c(m,m,p))) return(Araw)
  if (all(d == c(p,m,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[L,,]; return(out) }
  if (all(d == c(m,p,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[,L,]; return(out) }
  stop("Unexpected ar.coefs dims: ", paste(d, collapse="x"))
}

.normalize_B <- function(Braw, m){
  dm <- dim(Braw); if (is.null(dm)) stop("model$exog.coefs must be a matrix when exog_vars != 0")
  if (dm[1]==m) return(Braw)     # (m x k)
  if (dm[2]==m) return(t(Braw))  # (k x m) -> (m x k)
  stop("exog.coefs incompatible dims: ", paste(dm, collapse="x"))
}

.exog_tail_from_training <- function(train_df, exog_vars, H){
  # Uses last H training observations as future exogenous path
  # Valid for: (1) known policy paths, (2) scenario analysis, (3) persistence assumption
  start <- nrow(train_df) - H + 1
  if (start < 1) stop("H exceeds training length.")
  as.matrix(train_df[start:nrow(train_df), exog_vars, drop=FALSE])
}

# One-step VARX with shock
.varx_step <- function(Yhist, A, cvec, B, z, Rchol){
  p <- dim(A)[3]; m <- length(cvec)
  y <- cvec
  for (L in 1:p) y <- y + A[,,L] %*% .num(Yhist[nrow(Yhist)-L+1, ])
  if (!is.null(B) && !is.null(z)) y <- y + B %*% .num(z)
  zstd <- rnorm(m); eps <- t(Rchol) %*% zstd  # chol returns upper triangular R
  .num(y + eps)
}

# Full H-step simulation path (proper MA accumulation)
.simulate_varx_path <- function(Yinit, A, cvec, B, Zfut, Rchol){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    z <- if (ncol(Zfut)>0) Zfut[h, ] else NULL
    y_next <- .varx_step(Yh, A, cvec, B, z, Rchol)
    out[h,] <- y_next
    Yh <- rbind(Yh, y_next)
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Deterministic path (ε≡0) for snap-centering
.det_path <- function(Yinit, A, cvec, B, Zfut){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    y <- cvec
    for (L in 1:p) y <- y + A[,,L] %*% .num(Yh[nrow(Yh)-L+1, ])
    if (!is.null(B) && ncol(Zfut)>0) y <- y + B %*% .num(Zfut[h, ])
    out[h,] <- .num(y)
    Yh <- rbind(Yh, t(y))
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Truncate simulated draws to admissible support
.truncate_draws <- function(x, lb, ub){
  keep <- rep(TRUE, length(x))
  if (!is.na(lb)) keep <- keep & (x >= lb)
  if (!is.na(ub)) keep <- keep & (x <= ub)
  x[keep]
}

# PF-anchored shortest (HPD-style) gamma-interval from draws within support
# Returns [L,U], acceptance rate, and a status flag
.pf_anchored_interval <- function(draws, pf, gamma, lb = NA_real_, ub = NA_real_, 
                                  var_name = "", horizon = 0){
  n_total <- length(draws)
  # Truncate to support
  x <- .truncate_draws(draws, lb, ub)
  acc_rate <- length(x) / max(1L, n_total)
  
  # Warn if low acceptance rate
  if (acc_rate < 0.5 && var_name != "") {
    warning(sprintf("%s horizon %d: accept_rate = %.3f (effective coverage = %.1f%%)", 
                    var_name, horizon, acc_rate, gamma * acc_rate * 100),
            call. = FALSE)
  }
  
  if (length(x) == 0L) {  # all mass out-of-support
    return(list(L = pf, U = pf, accept_rate = 0, status = "degenerate"))
  }
  
  x <- sort(x)
  n <- length(x)
  k <- max(1L, ceiling(gamma * n))
  
  # If PF outside the truncated range, anchor at PF by extending to nearest window
  if (pf <= x[1]) {
    j <- min(n, k)
    L <- pf; U <- x[j]
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="left-anchored"))
  }
  if (pf >= x[n]) {
    i <- max(1L, n - k + 1)
    L <- x[i]; U <- pf
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="right-anchored"))
  }
  
  # Sliding-window HPD subject to "must contain PF"
  bestL <- x[1]; bestU <- x[min(n,k)]; bestW <- bestU - bestL; found <- FALSE
  j <- k
  for (i in 1:(n - k + 1)) {
    j <- i + k - 1
    L <- x[i]; U <- x[j]
    if (pf >= L && pf <= U) {
      w <- U - L
      if (!found || w < bestW) { bestW <- w; bestL <- L; bestU <- U; found <- TRUE }
    }
  }
  if (found) {
    return(list(L = bestL, U = bestU, accept_rate = acc_rate, status = "window"))
  }
  
  # Fallback: expand the closest window to include PF
  # (Preserves ≥ gamma mass, includes PF; width may be slightly larger than HPD)
  # Choose window whose center is closest to PF
  centers <- sapply(1:(n - k + 1), function(i) (x[i] + x[i+k-1]) / 2)
  idx <- which.min(abs(centers - pf))
  L <- min(x[idx], pf); U <- max(x[idx + k - 1], pf)
  list(L = L, U = U, accept_rate = acc_rate, status = "expanded")
}
## ======================================================================== ##


## ====================== MAIN: PPIs (PF-ANCHORED) ========================= ##
# Uses fitted szbvarx model, training data, and SAVED point forecasts
generate_ppi_pf_anchored <- function(model, train_df,
                                     endog_vars, exog_vars,
                                     point_forecasts,
                                     support_bounds,
                                     gamma = 0.50, n_sim = 1000,
                                     seed = NULL){
  
  # Set seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
    message(sprintf("  → RNG seed set to %d for reproducibility", seed))
  }
  
  # Enforce model ordering via eqnames
  eq <- attr(model, "eqnames"); if (is.null(eq)) eq <- colnames(point_forecasts)
  if (is.null(eq)) stop("Cannot infer model eqnames; set colnames(point_forecasts).")
  if (!all(eq %in% endog_vars)) stop("endog_vars missing: ", paste(setdiff(eq, endog_vars), collapse=", "))
  endog_vars <- eq
  if (!all(colnames(point_forecasts) == endog_vars))
    point_forecasts <- as.matrix(point_forecasts[, endog_vars, drop=FALSE])
  
  # Extract components
  p      <- as.integer(model$p)
  cvec   <- .num(model$intercept)
  Araw   <- model$ar.coefs
  Braw   <- if (!is.null(model$exog.coefs)) model$exog.coefs else NULL
  Sigma  <- as.matrix(model$mean.S)
  m <- length(endog_vars); H <- nrow(point_forecasts)
  if (length(cvec) != m) stop("Intercept length != number of endogenous series.")
  
  A <- .normalize_ar_cube(Araw, m, p)
  B <- if (!is.null(Braw) && length(exog_vars) > 0) .normalize_B(Braw, m) else NULL
  
  # Robust chol for Sigma with warning
  Rchol <- tryCatch({
    chol(Sigma)
  }, error = function(e) {
    warning("Sigma near-singular; adding jitter 1e-10 to diagonal", call. = FALSE)
    chol(Sigma + diag(1e-10, nrow(Sigma)))
  })
  
  # Initial history & exog FUT path (training tail; no leakage)
  Y <- as.matrix(train_df[, endog_vars, drop=FALSE])
  Yinit <- Y[(nrow(Y)-p+1):nrow(Y), , drop=FALSE]
  Zfut <- if (length(exog_vars)==0) matrix(numeric(0), H, 0) else {
    zt <- .exog_tail_from_training(train_df, exog_vars, H); colnames(zt) <- exog_vars; zt
  }
  
  # Deterministic path (ε=0) + snap-centering delta
  det <- .det_path(Yinit, A, cvec, B, Zfut)
  colnames(det) <- endog_vars
  delta <- point_forecasts - det  # H x m
  
  # Validate snap-centering (diagnostic)
  max_delta <- max(abs(delta))
  if (max_delta > 1e-3) {
    message(sprintf("  → Max snap-centering delta = %.4f (PF differs from deterministic path)", max_delta))
    message("     This is expected if PF comes from HP-tuned model with different settings")
  }
  
  # Monte Carlo with proper recursion
  sims <- array(NA_real_, dim = c(H, m, n_sim))
  dimnames(sims) <- list(NULL, endog_vars, NULL)
  for (s in 1:n_sim) {
    path <- .simulate_varx_path(Yinit, A, cvec, B, Zfut, Rchol)
    sims[,,s] <- path + delta   # snap-center to PF
  }
  
  # Build PF-anchored credible intervals after support truncation
  results <- vector("list", m); names(results) <- endog_vars
  avgw <- setNames(numeric(m), endog_vars)
  acc_avg <- setNames(numeric(m), endog_vars)
  
  for (j in seq_len(m)) {
    var <- endog_vars[j]
    b <- support_bounds[[var]]
    lb <- if (is.null(b)) NA_real_ else b[1]
    ub <- if (is.null(b)) NA_real_ else b[2]
    
    lower <- upper <- acc <- rep(NA_real_, H)
    for (h in 1:H) {
      draws <- sims[h, j, ]
      pf    <- as.numeric(point_forecasts[h, j])
      ans   <- .pf_anchored_interval(draws, pf, gamma, lb, ub, var, h)
      L <- ans$L; U <- ans$U
      if (U < L) { tmp <- U; U <- L; L <- tmp }  # logical consistency
      lower[h] <- L; upper[h] <- U; acc[h] <- ans$accept_rate
    }
    
    df <- data.frame(
      horizon        = 1:H,
      point_forecast = as.numeric(point_forecasts[, j]),
      lower_50       = lower,
      upper_50       = upper,
      interval_width = upper - lower,
      accept_rate    = acc,
      effective_coverage = gamma * acc  # Adjusted for truncation
    )
    results[[var]] <- df
    avgw[j] <- mean(df$interval_width, na.rm = TRUE)
    acc_avg[j] <- mean(df$accept_rate, na.rm = TRUE)
  }
  
  list(intervals = results,
       avg_widths = avgw,
       accept_rates = acc_avg,
       sims = sims)
}
## ======================================================================== ##


## ============================ RUN & EXPORT =============================== ##
## Enhanced with full reproducibility controls

cat("\n")
cat("================================================================================\n")
cat("  SZBVARX Forecast-Consistent Credible Intervals - PRODUCTION RUN\n")
cat(sprintf("  Country: %s\n", toupper(COUNTRY)))
cat("================================================================================\n\n")

# Capture initial RNG state
if (exists(".Random.seed")) {
  rng_state_initial <- .Random.seed
} else {
  set.seed(NULL)  # Initialize RNG
  rng_state_initial <- .Random.seed
}

# Display reproducibility info
cat("=== Reproducibility Configuration ===\n")
cat(sprintf("Country:      %s\n", toupper(COUNTRY)))
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n", gamma_level))
cat(sprintf("R version:    %s\n", R.version.string))
cat(sprintf("MASS version: %s\n", packageVersion("MASS")))
cat(sprintf("Platform:     %s\n", R.version$platform))
cat(sprintf("Timestamp:    %s\n", Sys.time()))
cat("======================================\n\n")

# Expected to exist in the environment (UK-specific):
# results_12M$model, results_12M$forecasts, var.uk.12M.full.train
# results_24M$model, results_24M$forecasts, var.uk.24M.full.train

cat("Validating input data...\n")
stopifnot(all(colnames(results_12M$forecasts) %in% endog_vars),
          all(colnames(results_24M$forecasts) %in% endog_vars))

# Align forecasts to eqnames
eq12 <- attr(results_12M$model, "eqnames"); if (is.null(eq12)) eq12 <- colnames(results_12M$forecasts)
eq24 <- attr(results_24M$model, "eqnames"); if (is.null(eq24)) eq24 <- colnames(results_24M$forecasts)
results_12M$forecasts <- as.matrix(results_12M$forecasts[, eq12, drop=FALSE])
results_24M$forecasts <- as.matrix(results_24M$forecasts[, eq24, drop=FALSE])
cat("  ✓ Input validation passed\n\n")

# Generate PF-anchored, support-respecting PPIs with explicit seeds
cat("Generating 12M prediction intervals...\n")
set.seed(SEED)  # Explicit seed for 12M
ppi_12M <- generate_ppi_pf_anchored(
  model = results_12M$model,
  train_df = var.uk.12M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_12M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED
)
cat("  ✓ 12M intervals complete\n\n")

cat("Generating 24M prediction intervals...\n")
set.seed(SEED + 1000)  # Different seed for 24M (ensures independence)
ppi_24M <- generate_ppi_pf_anchored(
  model = results_24M$model,
  train_df = var.uk.24M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_24M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED + 1000
)
cat("  ✓ 24M intervals complete\n\n")

# Consolidation helper
.consolidate <- function(ppi_obj, horizon_label){
  do.call(rbind, lapply(names(ppi_obj$intervals), function(v){
    d <- ppi_obj$intervals[[v]]
    d$variable <- v
    d$forecast_horizon <- horizon_label
    d$actual_value <- NA_real_
    d$covered <- NA_integer_
    d[, c("forecast_horizon","variable","horizon",
          "point_forecast","actual_value",
          "lower_50","upper_50","interval_width","covered","accept_rate","effective_coverage")]
  }))
}

cat("Consolidating results...\n")
cons12 <- .consolidate(ppi_12M, "12M")
cons24 <- .consolidate(ppi_24M, "24M")
consALL <- rbind(cons12, cons24)

# Write outputs (UK-specific filenames)
cat("Writing output files...\n")
write.csv(cons12, sprintf("credible_intervals_szbvarx_12M_all_variables_%s.csv", COUNTRY), row.names = FALSE)
write.csv(cons24, sprintf("credible_intervals_szbvarx_24M_all_variables_%s.csv", COUNTRY), row.names = FALSE)
write.csv(consALL, sprintf("credible_intervals_szbvarx_all_variables_%s.csv", COUNTRY), row.names = FALSE)

summary_table <- data.frame(
  variable          = names(ppi_12M$avg_widths),
  country           = COUNTRY,
  avg_width_12M     = ppi_12M$avg_widths,
  accept_12M        = ppi_12M$accept_rates,
  avg_width_24M     = ppi_24M$avg_widths,
  accept_24M        = ppi_24M$accept_rates,
  avg_width_overall = (ppi_12M$avg_widths + ppi_24M$avg_widths)/2,
  row.names = NULL
)
write.csv(summary_table, sprintf("average_interval_widths_szbvarx_%s.csv", COUNTRY), row.names = FALSE)
cat("  ✓ CSV files written\n\n")

# Capture final RNG state
rng_state_final <- .Random.seed

# Save reproducibility metadata
cat("Saving reproducibility metadata...\n")
repro_metadata <- list(
  country = COUNTRY,
  seed = SEED,
  n_sim = n_sim,
  gamma = gamma_level,
  endog_vars = endog_vars,
  exog_vars = exog_vars,
  support_bounds = support_bounds,
  rng_state_initial = rng_state_initial,
  rng_state_final = rng_state_final,
  timestamp = Sys.time(),
  session_info = sessionInfo(),
  r_version = R.version.string,
  platform = R.version$platform
)
saveRDS(repro_metadata, sprintf("reproducibility_metadata_szbvarx_ppi_%s.rds", COUNTRY))
cat(sprintf("  ✓ Metadata saved: reproducibility_metadata_szbvarx_ppi_%s.rds\n", COUNTRY))

# Compute verification hashes
if (requireNamespace("digest", quietly = TRUE)) {
  cat("Computing verification hashes...\n")
  library(digest)
  verification <- data.frame(
    object = c("ppi_12M_intervals", "ppi_24M_intervals", "cons12", "cons24", "summary_table"),
    hash_sha256 = c(
      digest(ppi_12M$intervals, algo = "sha256"),
      digest(ppi_24M$intervals, algo = "sha256"),
      digest(cons12, algo = "sha256"),
      digest(cons24, algo = "sha256"),
      digest(summary_table, algo = "sha256")
    ),
    country = COUNTRY,
    seed = SEED,
    n_sim = n_sim,
    gamma = gamma_level,
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  write.csv(verification, sprintf("verification_hashes_szbvarx_ppi_%s.csv", COUNTRY), row.names = FALSE)
  cat(sprintf("  ✓ Hashes saved: verification_hashes_szbvarx_ppi_%s.csv\n\n", COUNTRY))
  
  cat("=== Verification Hashes (SHA-256) ===\n")
  print(verification[, c("object", "hash_sha256")], row.names = FALSE)
  cat("======================================\n\n")
}

# Save session info
cat("Saving session info...\n")
sink(sprintf("session_info_szbvarx_ppi_%s.txt", COUNTRY))
cat("================================================================================\n")
cat(sprintf("  SZBVARX PPI Generation - Session Info (%s)\n", toupper(COUNTRY)))
cat("================================================================================\n\n")
cat(sprintf("Country:      %s\n", toupper(COUNTRY)))
cat(sprintf("Date:         %s\n", Sys.time()))
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n\n", gamma_level))
cat("Variables:\n")
cat(sprintf("  Endogenous: %s\n", paste(endog_vars, collapse=", ")))
cat(sprintf("  Exogenous:  %s\n\n", paste(exog_vars, collapse=", ")))
cat("Support bounds:\n")
for (v in names(support_bounds)) {
  b <- support_bounds[[v]]
  cat(sprintf("  %s: [%s, %s]\n", v, 
              ifelse(is.na(b[1]), "-Inf", b[1]),
              ifelse(is.na(b[2]), "+Inf", b[2])))
}
cat("\n")
cat("--------------------------------------------------------------------------------\n")
cat("R Session Info:\n")
cat("--------------------------------------------------------------------------------\n")
print(sessionInfo())
sink()
cat(sprintf("  ✓ Session info saved: session_info_szbvarx_ppi_%s.txt\n\n", COUNTRY))

# Summary statistics
cat("================================================================================\n")
cat("  SUMMARY STATISTICS\n")
cat("================================================================================\n\n")
cat("Average Interval Widths:\n")
print(summary_table, row.names = FALSE)
cat("\n")
cat("Average Acceptance Rates (Support Truncation):\n")
cat(sprintf("  12M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_12M$accept_rates), mean(ppi_12M$accept_rates) * gamma_level * 100))
cat(sprintf("  24M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_24M$accept_rates), mean(ppi_24M$accept_rates) * gamma_level * 100))
cat("\n")

cat("================================================================================\n")
cat("  DONE: PF-anchored, support-respecting credible intervals exported\n")
cat("================================================================================\n\n")

cat("Output files:\n")
cat(sprintf("  • credible_intervals_szbvarx_12M_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • credible_intervals_szbvarx_24M_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • credible_intervals_szbvarx_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • average_interval_widths_szbvarx_%s.csv\n", COUNTRY))
cat(sprintf("  • reproducibility_metadata_szbvarx_ppi_%s.rds\n", COUNTRY))
cat(sprintf("  • session_info_szbvarx_ppi_%s.txt\n", COUNTRY))
if (requireNamespace("digest", quietly = TRUE)) {
  cat(sprintf("  • verification_hashes_szbvarx_ppi_%s.csv\n", COUNTRY))
}
cat("\n")
cat("To verify reproducibility, re-run this script and compare hashes.\n")
cat(sprintf("To restore RNG state: .Random.seed <- readRDS('reproducibility_metadata_szbvarx_ppi_%s.rds')$rng_state_initial\n", COUNTRY))
cat("\n")
#################### End of Code ##################

#### SZBVAR Model: ITALY: 12M and 24M ahead - forecasts  ####
# Setting the working directory
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/multi_variate_forecasting_paper_G7/GitHub_Macrocasting/dataset/italy")
getwd()

########################### SZBVARx Model with HPs ##################
# Read the dataset
var.italy <- read.csv("all_mulvar_data_italy_v2.csv", header = TRUE)

# Convert Date into Datetime Value
var.italy$Date <- as.Date(var.italy$Date)
str(var.italy)


# Creation of datasets for 12M forecasts
var.italy.12M.train <- var.italy[1:327,]
var.italy.12M.val <- var.italy[328:339,]
var.italy.12M.test <- var.italy[340:351,]
var.italy.12M.full.train <- var.italy[1:339,]

# Creation of datasets for 24M forecasts
var.italy.24M.train <- var.italy[1:303,]
var.italy.24M.val <- var.italy[304:327,]
var.italy.24M.test <- var.italy[328:351,]
var.italy.24M.full.train <- var.italy[1:327,]


# Check for stationarity (using 12M full train data as an example) - Optional
for(col in c("Unemploymentrate", "RealbroadEER", "ShorttermIR", "OilpriceGlobalWTI", "CPIinflationrate", 
             "logEPU", "GPRC", "USEMV", "USMPU")) {
  print(paste("KPSS test for", col))
  print(kpss.test(var.italy.12M.full.train[[col]], null="Trend"))
}

# Function to fit SZBVAR model and generate forecasts: 12M {based on best HPs}
fit_and_forecast_szbvar_12M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 1,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.2,
                  lambda1 = 0.05,
                  lambda3 = 1,
                  lambda4 = 0.1,
                  lambda5 = 0,
                  mu5 = 1,
                  mu6 = 0,
                  prior = 0 # Normal-Wishart prior,
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}

# Function to fit SZBVAR model and generate forecasts: 24M {based on best HPs}
fit_and_forecast_szbvar_24M <- function(train_data, test_data) {
  # Fit the SZBVAR model
  model <- szbvar(Y = ts(train_data[, c("Unemploymentrate", 
                                        "RealbroadEER", 
                                        "ShorttermIR", 
                                        "OilpriceGlobalWTI", 
                                        "CPIinflationrate")]),
                  p = 4,
                  z = ts(train_data[, c("logEPU", "GPRC", "USEMV", "USMPU")]),
                  lambda0 = 0.2,
                  lambda1 = 0.05,
                  lambda3 = 1,
                  lambda4 = 0.1,
                  lambda5 = 0.5,
                  mu5 = 1,
                  mu6 = 0.5,
                  prior = 0 # Normal-Wishart prior,
  )
  
  forecasts <- forecast(model,
                        nsteps = nrow(test_data),
                        exog.fut = ts(train_data[nrow(train_data) - (nrow(test_data)-1):0, 
                                                 c("logEPU", "GPRC", "USEMV", "USMPU")]))
  
  # Extract only the forecast part
  forecasts_only <- forecasts[(nrow(train_data)+1):nrow(forecasts),]
  
  return(list(model = model, forecasts = forecasts_only))
}


# 12M Forecasts
results_12M <- fit_and_forecast_szbvar_12M(var.italy.12M.full.train, var.italy.12M.test)
results_12M
# $forecasts
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         8.082363     99.63970    2.954926          74.11944         7.771388
# [2,]         7.960396     99.77313    2.977525          73.58302         7.884509
# [3,]         7.705814     99.85402    3.044987          75.18091         8.175794
# [4,]         7.668584    100.00199    3.081483          75.15912         8.269800
# [5,]         7.803458    100.06806    3.137400          76.34208         8.290769
# [6,]         7.764743    100.17496    3.200156          77.58548         8.438841
# [7,]         7.763714    100.39336    3.258722          78.03001         8.556091
# [8,]         7.851153    100.60036    3.321997          78.65624         8.621729
# [9,]         7.929493    100.76779    3.364188          78.44445         8.635524
# [10,]         8.128341    101.19505    3.432692          78.05220         8.639414
# [11,]         8.187428    101.29857    3.479013          78.37729         8.671229
# [12,]         8.235758    101.57373    3.498041          76.71068         8.659020
# 24M Forecasts
results_24M <- fit_and_forecast_szbvar_24M(var.italy.24M.full.train, var.italy.24M.test)
results_24M
# $forecasts
# Unemploymentrate RealbroadEER ShorttermIR OilpriceGlobalWTI CPIinflationrate
# [1,]         8.494962     98.63330  -0.5389467         105.49378         6.499683
# [2,]         8.495611     98.80706  -0.5722252         102.89210         6.534880
# [3,]         8.493039     98.94848  -0.6039605          99.97395         6.553930
# [4,]         8.503037     99.00457  -0.6197376          98.51470         6.582122
# [5,]         8.505377     99.00980  -0.6304296          96.75015         6.592356
# [6,]         8.530454     99.09508  -0.6446857          96.74208         6.652601
# [7,]         8.536752     99.13949  -0.6618884          94.98279         6.669660
# [8,]         8.566001     99.27567  -0.6825942          95.18699         6.743052
# [9,]         8.579294     99.27436  -0.6869852          94.80116         6.775949
# [10,]         8.584841     99.25081  -0.6925444          93.60462         6.787784
# [11,]         8.588052     99.17987  -0.6901644          92.77488         6.796577
# [12,]         8.584435     99.13819  -0.6962524          91.07657         6.793582
# [13,]         8.584912     99.03748  -0.6913414          90.14283         6.791952
# [14,]         8.592979     98.98106  -0.6875039          90.15499         6.816795
# [15,]         8.598556     98.95454  -0.6891935          89.83979         6.841209
# [16,]         8.619131     98.96278  -0.6913347          90.56470         6.893138
# [17,]         8.630984     98.91913  -0.6892662          90.65169         6.921977
# [18,]         8.639581     98.92652  -0.6966699          90.13215         6.949030
# [19,]         8.653493     98.91081  -0.6982188          90.25666         6.983659
# [20,]         8.677550     98.88071  -0.6940278          91.37803         7.036111
# [21,]         8.706380     99.04844  -0.7159141          92.37657         7.127649
# [22,]         8.726068     99.21717  -0.7402862          92.67944         7.206153
# [23,]         8.731309     99.28872  -0.7543991          92.35987         7.253604
# [24,]         8.749455     99.52517  -0.7898943          92.36452         7.342262

######################## SZBVARx: Italy - Credible PPIs ###############################
suppressPackageStartupMessages({ 
  library(MASS)
  if (!requireNamespace("digest", quietly = TRUE)) {
    message("Note: 'digest' package not available; verification hashes will be skipped")
  }
})

## ========================= USER SETTINGS ================================= ##
endog_vars <- c("Unemploymentrate","RealbroadEER","ShorttermIR",
                "OilpriceGlobalWTI","CPIinflationrate")
exog_vars  <- c("logEPU","GPRC","USEMV","USMPU")

# Admissible supports (edit per variable if needed; NA = unbounded)
support_bounds <- list(
  Unemploymentrate   = c(0, 100),
  RealbroadEER       = c(0,  NA),
  ShorttermIR        = c(NA, NA),
  OilpriceGlobalWTI  = c(0,  NA),
  CPIinflationrate   = c(NA, NA)
)

gamma_level <- 0.50
n_sim <- 1000
SEED <- 1234
COUNTRY <- "italy"  # Country identifier
## ======================================================================== ##


## ============================ UTILITIES ================================= ##
.num <- function(x) as.numeric(x)

.normalize_ar_cube <- function(Araw, m, p){
  d <- dim(Araw)
  if (length(d)!=3) stop("model$ar.coefs must be 3D")
  if (all(d == c(m,m,p))) return(Araw)
  if (all(d == c(p,m,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[L,,]; return(out) }
  if (all(d == c(m,p,m))) { out <- array(NA_real_, c(m,m,p)); for (L in 1:p) out[,,L] <- Araw[,L,]; return(out) }
  stop("Unexpected ar.coefs dims: ", paste(d, collapse="x"))
}

.normalize_B <- function(Braw, m){
  dm <- dim(Braw); if (is.null(dm)) stop("model$exog.coefs must be a matrix when exog_vars != 0")
  if (dm[1]==m) return(Braw)     # (m x k)
  if (dm[2]==m) return(t(Braw))  # (k x m) -> (m x k)
  stop("exog.coefs incompatible dims: ", paste(dm, collapse="x"))
}

.exog_tail_from_training <- function(train_df, exog_vars, H){
  # Uses last H training observations as future exogenous path
  # Valid for: (1) known policy paths, (2) scenario analysis, (3) persistence assumption
  start <- nrow(train_df) - H + 1
  if (start < 1) stop("H exceeds training length.")
  as.matrix(train_df[start:nrow(train_df), exog_vars, drop=FALSE])
}

# One-step VARX with shock
.varx_step <- function(Yhist, A, cvec, B, z, Rchol){
  p <- dim(A)[3]; m <- length(cvec)
  y <- cvec
  for (L in 1:p) y <- y + A[,,L] %*% .num(Yhist[nrow(Yhist)-L+1, ])
  if (!is.null(B) && !is.null(z)) y <- y + B %*% .num(z)
  zstd <- rnorm(m); eps <- t(Rchol) %*% zstd  # chol returns upper triangular R
  .num(y + eps)
}

# Full H-step simulation path (proper MA accumulation)
.simulate_varx_path <- function(Yinit, A, cvec, B, Zfut, Rchol){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    z <- if (ncol(Zfut)>0) Zfut[h, ] else NULL
    y_next <- .varx_step(Yh, A, cvec, B, z, Rchol)
    out[h,] <- y_next
    Yh <- rbind(Yh, y_next)
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Deterministic path (ε≡0) for snap-centering
.det_path <- function(Yinit, A, cvec, B, Zfut){
  p <- dim(A)[3]; m <- length(cvec); H <- nrow(Zfut)
  out <- matrix(NA_real_, H, m); colnames(out) <- colnames(Yinit)
  Yh <- Yinit
  for (h in 1:H){
    y <- cvec
    for (L in 1:p) y <- y + A[,,L] %*% .num(Yh[nrow(Yh)-L+1, ])
    if (!is.null(B) && ncol(Zfut)>0) y <- y + B %*% .num(Zfut[h, ])
    out[h,] <- .num(y)
    Yh <- rbind(Yh, t(y))
    if (nrow(Yh) > p) Yh <- Yh[(nrow(Yh)-p+1):nrow(Yh), , drop=FALSE]
  }
  out
}

# Truncate simulated draws to admissible support
.truncate_draws <- function(x, lb, ub){
  keep <- rep(TRUE, length(x))
  if (!is.na(lb)) keep <- keep & (x >= lb)
  if (!is.na(ub)) keep <- keep & (x <= ub)
  x[keep]
}

# PF-anchored shortest (HPD-style) gamma-interval from draws within support
# Returns [L,U], acceptance rate, and a status flag
.pf_anchored_interval <- function(draws, pf, gamma, lb = NA_real_, ub = NA_real_, 
                                  var_name = "", horizon = 0){
  n_total <- length(draws)
  # Truncate to support
  x <- .truncate_draws(draws, lb, ub)
  acc_rate <- length(x) / max(1L, n_total)
  
  # Warn if low acceptance rate
  if (acc_rate < 0.5 && var_name != "") {
    warning(sprintf("%s horizon %d: accept_rate = %.3f (effective coverage = %.1f%%)", 
                    var_name, horizon, acc_rate, gamma * acc_rate * 100),
            call. = FALSE)
  }
  
  if (length(x) == 0L) {  # all mass out-of-support
    return(list(L = pf, U = pf, accept_rate = 0, status = "degenerate"))
  }
  
  x <- sort(x)
  n <- length(x)
  k <- max(1L, ceiling(gamma * n))
  
  # If PF outside the truncated range, anchor at PF by extending to nearest window
  if (pf <= x[1]) {
    j <- min(n, k)
    L <- pf; U <- x[j]
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="left-anchored"))
  }
  if (pf >= x[n]) {
    i <- max(1L, n - k + 1)
    L <- x[i]; U <- pf
    if (U < L) { tmp <- U; U <- L; L <- tmp }
    return(list(L=L, U=U, accept_rate=acc_rate, status="right-anchored"))
  }
  
  # Sliding-window HPD subject to "must contain PF"
  bestL <- x[1]; bestU <- x[min(n,k)]; bestW <- bestU - bestL; found <- FALSE
  j <- k
  for (i in 1:(n - k + 1)) {
    j <- i + k - 1
    L <- x[i]; U <- x[j]
    if (pf >= L && pf <= U) {
      w <- U - L
      if (!found || w < bestW) { bestW <- w; bestL <- L; bestU <- U; found <- TRUE }
    }
  }
  if (found) {
    return(list(L = bestL, U = bestU, accept_rate = acc_rate, status = "window"))
  }
  
  # Fallback: expand the closest window to include PF
  # (Preserves ≥ gamma mass, includes PF; width may be slightly larger than HPD)
  # Choose window whose center is closest to PF
  centers <- sapply(1:(n - k + 1), function(i) (x[i] + x[i+k-1]) / 2)
  idx <- which.min(abs(centers - pf))
  L <- min(x[idx], pf); U <- max(x[idx + k - 1], pf)
  list(L = L, U = U, accept_rate = acc_rate, status = "expanded")
}
## ======================================================================== ##


## ====================== MAIN: PPIs (PF-ANCHORED) ========================= ##
# Uses fitted szbvarx model, training data, and SAVED point forecasts
generate_ppi_pf_anchored <- function(model, train_df,
                                     endog_vars, exog_vars,
                                     point_forecasts,
                                     support_bounds,
                                     gamma = 0.50, n_sim = 1000,
                                     seed = NULL){
  
  # Set seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
    message(sprintf("  → RNG seed set to %d for reproducibility", seed))
  }
  
  # Enforce model ordering via eqnames
  eq <- attr(model, "eqnames"); if (is.null(eq)) eq <- colnames(point_forecasts)
  if (is.null(eq)) stop("Cannot infer model eqnames; set colnames(point_forecasts).")
  if (!all(eq %in% endog_vars)) stop("endog_vars missing: ", paste(setdiff(eq, endog_vars), collapse=", "))
  endog_vars <- eq
  if (!all(colnames(point_forecasts) == endog_vars))
    point_forecasts <- as.matrix(point_forecasts[, endog_vars, drop=FALSE])
  
  # Extract components
  p      <- as.integer(model$p)
  cvec   <- .num(model$intercept)
  Araw   <- model$ar.coefs
  Braw   <- if (!is.null(model$exog.coefs)) model$exog.coefs else NULL
  Sigma  <- as.matrix(model$mean.S)
  m <- length(endog_vars); H <- nrow(point_forecasts)
  if (length(cvec) != m) stop("Intercept length != number of endogenous series.")
  
  A <- .normalize_ar_cube(Araw, m, p)
  B <- if (!is.null(Braw) && length(exog_vars) > 0) .normalize_B(Braw, m) else NULL
  
  # Robust chol for Sigma with warning
  Rchol <- tryCatch({
    chol(Sigma)
  }, error = function(e) {
    warning("Sigma near-singular; adding jitter 1e-10 to diagonal", call. = FALSE)
    chol(Sigma + diag(1e-10, nrow(Sigma)))
  })
  
  # Initial history & exog FUT path (training tail; no leakage)
  Y <- as.matrix(train_df[, endog_vars, drop=FALSE])
  Yinit <- Y[(nrow(Y)-p+1):nrow(Y), , drop=FALSE]
  Zfut <- if (length(exog_vars)==0) matrix(numeric(0), H, 0) else {
    zt <- .exog_tail_from_training(train_df, exog_vars, H); colnames(zt) <- exog_vars; zt
  }
  
  # Deterministic path (ε=0) + snap-centering delta
  det <- .det_path(Yinit, A, cvec, B, Zfut)
  colnames(det) <- endog_vars
  delta <- point_forecasts - det  # H x m
  
  # Validate snap-centering (diagnostic)
  max_delta <- max(abs(delta))
  if (max_delta > 1e-3) {
    message(sprintf("  → Max snap-centering delta = %.4f (PF differs from deterministic path)", max_delta))
    message("     This is expected if PF comes from HP-tuned model with different settings")
  }
  
  # Monte Carlo with proper recursion
  sims <- array(NA_real_, dim = c(H, m, n_sim))
  dimnames(sims) <- list(NULL, endog_vars, NULL)
  for (s in 1:n_sim) {
    path <- .simulate_varx_path(Yinit, A, cvec, B, Zfut, Rchol)
    sims[,,s] <- path + delta   # snap-center to PF
  }
  
  # Build PF-anchored credible intervals after support truncation
  results <- vector("list", m); names(results) <- endog_vars
  avgw <- setNames(numeric(m), endog_vars)
  acc_avg <- setNames(numeric(m), endog_vars)
  
  for (j in seq_len(m)) {
    var <- endog_vars[j]
    b <- support_bounds[[var]]
    lb <- if (is.null(b)) NA_real_ else b[1]
    ub <- if (is.null(b)) NA_real_ else b[2]
    
    lower <- upper <- acc <- rep(NA_real_, H)
    for (h in 1:H) {
      draws <- sims[h, j, ]
      pf    <- as.numeric(point_forecasts[h, j])
      ans   <- .pf_anchored_interval(draws, pf, gamma, lb, ub, var, h)
      L <- ans$L; U <- ans$U
      if (U < L) { tmp <- U; U <- L; L <- tmp }  # logical consistency
      lower[h] <- L; upper[h] <- U; acc[h] <- ans$accept_rate
    }
    
    df <- data.frame(
      horizon        = 1:H,
      point_forecast = as.numeric(point_forecasts[, j]),
      lower_50       = lower,
      upper_50       = upper,
      interval_width = upper - lower,
      accept_rate    = acc,
      effective_coverage = gamma * acc  # Adjusted for truncation
    )
    results[[var]] <- df
    avgw[j] <- mean(df$interval_width, na.rm = TRUE)
    acc_avg[j] <- mean(df$accept_rate, na.rm = TRUE)
  }
  
  list(intervals = results,
       avg_widths = avgw,
       accept_rates = acc_avg,
       sims = sims)
}
## ======================================================================== ##


## ============================ RUN & EXPORT =============================== ##
## Enhanced with full reproducibility controls

cat("\n")
cat("================================================================================\n")
cat("  SZBVARX Forecast-Consistent Credible Intervals - PRODUCTION RUN\n")
cat(sprintf("  Country: %s\n", toupper(COUNTRY)))
cat("================================================================================\n\n")

# Capture initial RNG state
if (exists(".Random.seed")) {
  rng_state_initial <- .Random.seed
} else {
  set.seed(NULL)  # Initialize RNG
  rng_state_initial <- .Random.seed
}

# Display reproducibility info
cat("=== Reproducibility Configuration ===\n")
cat(sprintf("Country:      %s\n", toupper(COUNTRY)))
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n", gamma_level))
cat(sprintf("R version:    %s\n", R.version.string))
cat(sprintf("MASS version: %s\n", packageVersion("MASS")))
cat(sprintf("Platform:     %s\n", R.version$platform))
cat(sprintf("Timestamp:    %s\n", Sys.time()))
cat("======================================\n\n")

# Expected to exist in the environment (ITALY-specific):
# results_12M$model, results_12M$forecasts, var.italy.12M.full.train
# results_24M$model, results_24M$forecasts, var.italy.24M.full.train

cat("Validating input data...\n")
stopifnot(all(colnames(results_12M$forecasts) %in% endog_vars),
          all(colnames(results_24M$forecasts) %in% endog_vars))

# Align forecasts to eqnames
eq12 <- attr(results_12M$model, "eqnames"); if (is.null(eq12)) eq12 <- colnames(results_12M$forecasts)
eq24 <- attr(results_24M$model, "eqnames"); if (is.null(eq24)) eq24 <- colnames(results_24M$forecasts)
results_12M$forecasts <- as.matrix(results_12M$forecasts[, eq12, drop=FALSE])
results_24M$forecasts <- as.matrix(results_24M$forecasts[, eq24, drop=FALSE])
cat("  ✓ Input validation passed\n\n")

# Generate PF-anchored, support-respecting PPIs with explicit seeds
cat("Generating 12M prediction intervals...\n")
set.seed(SEED)  # Explicit seed for 12M
ppi_12M <- generate_ppi_pf_anchored(
  model = results_12M$model,
  train_df = var.italy.12M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_12M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED
)
cat("  ✓ 12M intervals complete\n\n")

cat("Generating 24M prediction intervals...\n")
set.seed(SEED + 1000)  # Different seed for 24M (ensures independence)
ppi_24M <- generate_ppi_pf_anchored(
  model = results_24M$model,
  train_df = var.italy.24M.full.train,
  endog_vars = endog_vars, exog_vars = exog_vars,
  point_forecasts = results_24M$forecasts,
  support_bounds = support_bounds,
  gamma = gamma_level, n_sim = n_sim,
  seed = SEED + 1000
)
cat("  ✓ 24M intervals complete\n\n")

# Consolidation helper
.consolidate <- function(ppi_obj, horizon_label){
  do.call(rbind, lapply(names(ppi_obj$intervals), function(v){
    d <- ppi_obj$intervals[[v]]
    d$variable <- v
    d$forecast_horizon <- horizon_label
    d$actual_value <- NA_real_
    d$covered <- NA_integer_
    d[, c("forecast_horizon","variable","horizon",
          "point_forecast","actual_value",
          "lower_50","upper_50","interval_width","covered","accept_rate","effective_coverage")]
  }))
}

cat("Consolidating results...\n")
cons12 <- .consolidate(ppi_12M, "12M")
cons24 <- .consolidate(ppi_24M, "24M")
consALL <- rbind(cons12, cons24)

# Write outputs (ITALY-specific filenames)
cat("Writing output files...\n")
write.csv(cons12, sprintf("credible_intervals_szbvarx_12M_all_variables_%s.csv", COUNTRY), row.names = FALSE)
write.csv(cons24, sprintf("credible_intervals_szbvarx_24M_all_variables_%s.csv", COUNTRY), row.names = FALSE)
write.csv(consALL, sprintf("credible_intervals_szbvarx_all_variables_%s.csv", COUNTRY), row.names = FALSE)

summary_table <- data.frame(
  variable          = names(ppi_12M$avg_widths),
  country           = COUNTRY,
  avg_width_12M     = ppi_12M$avg_widths,
  accept_12M        = ppi_12M$accept_rates,
  avg_width_24M     = ppi_24M$avg_widths,
  accept_24M        = ppi_24M$accept_rates,
  avg_width_overall = (ppi_12M$avg_widths + ppi_24M$avg_widths)/2,
  row.names = NULL
)
write.csv(summary_table, sprintf("average_interval_widths_szbvarx_%s.csv", COUNTRY), row.names = FALSE)
cat("  ✓ CSV files written\n\n")

# Capture final RNG state
rng_state_final <- .Random.seed

# Save reproducibility metadata
cat("Saving reproducibility metadata...\n")
repro_metadata <- list(
  country = COUNTRY,
  seed = SEED,
  n_sim = n_sim,
  gamma = gamma_level,
  endog_vars = endog_vars,
  exog_vars = exog_vars,
  support_bounds = support_bounds,
  rng_state_initial = rng_state_initial,
  rng_state_final = rng_state_final,
  timestamp = Sys.time(),
  session_info = sessionInfo(),
  r_version = R.version.string,
  platform = R.version$platform
)
saveRDS(repro_metadata, sprintf("reproducibility_metadata_szbvarx_ppi_%s.rds", COUNTRY))
cat(sprintf("  ✓ Metadata saved: reproducibility_metadata_szbvarx_ppi_%s.rds\n", COUNTRY))

# Compute verification hashes
if (requireNamespace("digest", quietly = TRUE)) {
  cat("Computing verification hashes...\n")
  library(digest)
  verification <- data.frame(
    object = c("ppi_12M_intervals", "ppi_24M_intervals", "cons12", "cons24", "summary_table"),
    hash_sha256 = c(
      digest(ppi_12M$intervals, algo = "sha256"),
      digest(ppi_24M$intervals, algo = "sha256"),
      digest(cons12, algo = "sha256"),
      digest(cons24, algo = "sha256"),
      digest(summary_table, algo = "sha256")
    ),
    country = COUNTRY,
    seed = SEED,
    n_sim = n_sim,
    gamma = gamma_level,
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  write.csv(verification, sprintf("verification_hashes_szbvarx_ppi_%s.csv", COUNTRY), row.names = FALSE)
  cat(sprintf("  ✓ Hashes saved: verification_hashes_szbvarx_ppi_%s.csv\n\n", COUNTRY))
  
  cat("=== Verification Hashes (SHA-256) ===\n")
  print(verification[, c("object", "hash_sha256")], row.names = FALSE)
  cat("======================================\n\n")
}

# Save session info
cat("Saving session info...\n")
sink(sprintf("session_info_szbvarx_ppi_%s.txt", COUNTRY))
cat("================================================================================\n")
cat(sprintf("  SZBVARX PPI Generation - Session Info (%s)\n", toupper(COUNTRY)))
cat("================================================================================\n\n")
cat(sprintf("Country:      %s\n", toupper(COUNTRY)))
cat(sprintf("Date:         %s\n", Sys.time()))
cat(sprintf("Seed:         %d\n", SEED))
cat(sprintf("Simulations:  %d\n", n_sim))
cat(sprintf("Gamma level:  %.2f\n\n", gamma_level))
cat("Variables:\n")
cat(sprintf("  Endogenous: %s\n", paste(endog_vars, collapse=", ")))
cat(sprintf("  Exogenous:  %s\n\n", paste(exog_vars, collapse=", ")))
cat("Support bounds:\n")
for (v in names(support_bounds)) {
  b <- support_bounds[[v]]
  cat(sprintf("  %s: [%s, %s]\n", v, 
              ifelse(is.na(b[1]), "-Inf", b[1]),
              ifelse(is.na(b[2]), "+Inf", b[2])))
}
cat("\n")
cat("--------------------------------------------------------------------------------\n")
cat("R Session Info:\n")
cat("--------------------------------------------------------------------------------\n")
print(sessionInfo())
sink()
cat(sprintf("  ✓ Session info saved: session_info_szbvarx_ppi_%s.txt\n\n", COUNTRY))

# Summary statistics
cat("================================================================================\n")
cat("  SUMMARY STATISTICS\n")
cat("================================================================================\n\n")
cat("Average Interval Widths:\n")
print(summary_table, row.names = FALSE)
cat("\n")
cat("Average Acceptance Rates (Support Truncation):\n")
cat(sprintf("  12M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_12M$accept_rates), mean(ppi_12M$accept_rates) * gamma_level * 100))
cat(sprintf("  24M: %.3f (effective coverage: %.1f%%)\n", 
            mean(ppi_24M$accept_rates), mean(ppi_24M$accept_rates) * gamma_level * 100))
cat("\n")

cat("================================================================================\n")
cat("  DONE: PF-anchored, support-respecting credible intervals exported\n")
cat("================================================================================\n\n")

cat("Output files:\n")
cat(sprintf("  • credible_intervals_szbvarx_12M_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • credible_intervals_szbvarx_24M_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • credible_intervals_szbvarx_all_variables_%s.csv\n", COUNTRY))
cat(sprintf("  • average_interval_widths_szbvarx_%s.csv\n", COUNTRY))
cat(sprintf("  • reproducibility_metadata_szbvarx_ppi_%s.rds\n", COUNTRY))
cat(sprintf("  • session_info_szbvarx_ppi_%s.txt\n", COUNTRY))
if (requireNamespace("digest", quietly = TRUE)) {
  cat(sprintf("  • verification_hashes_szbvarx_ppi_%s.csv\n", COUNTRY))
}
cat("\n")
cat("To verify reproducibility, re-run this script and compare hashes.\n")
cat(sprintf("To restore RNG state: .Random.seed <- readRDS('reproducibility_metadata_szbvarx_ppi_%s.rds')$rng_state_initial\n", COUNTRY))
cat("\n")
########################## End of Code ###########################