# ──────────────────────────────────────────────────────────────────────────────
# Time blender experiment — retrodiction / fill-in / prediction 
# ──────────────────────────────────────────────────────────────────────────────

# Libraries --------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(furrr)
  library(progressr)
  library(logger)
  library(fs)
  library(glue)
  library(jsonlite)
  library(withr)
  library(xgboost)
  library(workflows)
  library(recipes)
  library(parsnip)
  library(yardstick)
  library(rsample)
  library(future)
  library(purrr)
})

# Paths & IO -------------------------------------------------------------------
BASE_DIR   <- "/Users/arpm/Downloads/retrodiction"
DIR_MODELS <- file.path(BASE_DIR, "models")
DIR_METRICS<- file.path(BASE_DIR, "metrics")
DIR_PRED   <- file.path(BASE_DIR, "predictions")
DIR_DECI   <- file.path(BASE_DIR, "deciles")
DIR_CACHE  <- file.path(BASE_DIR, "cache")
DIR_FIG    <- file.path(BASE_DIR, "figures")
DIR_DIAG   <- file.path(BASE_DIR, "diagnostics")
dir_create(c(BASE_DIR, DIR_MODELS, DIR_METRICS, DIR_PRED, DIR_DECI, DIR_CACHE, DIR_FIG, DIR_DIAG))

# Logger (console + file) ------------------------------------------------------
log_appender(appender_tee(file.path(BASE_DIR, "pipeline.log")))
log_layout(layout_glue_colors)
log_threshold(logger::INFO)
info  <- logger::log_info
warn  <- logger::log_warn
debug <- logger::log_debug
error <- logger::log_error

# CONFIG -----------------------------------------------------------------------
SEED                      <- 8544L
TEST_PROP                 <- 0.30
FOLDS_V                   <- 5L
XGB_GRID_N                <- 3L
PARALLEL                  <- TRUE
N_WORKERS                 <- min(max(1L, parallel::detectCores(logical = TRUE) - 1L), 6L)
XGB_THREADS_PER_WORKER    <- 1L

options(
  future.fork.enable      = FALSE,
  future.rng.onMisuse     = "ignore",
  future.globals.maxSize  = 8 * 1024^3
)

# Predictors config (static only; NO AGE) --------------------------------------
VAR_YOB     <- "year_of_birth"
VAR_SEX     <- "sex"
VAR_RACE    <- "race"
STATIC_VARS <- c(VAR_YOB, VAR_SEX, VAR_RACE)

OUTCOMES <- c("employed", "married", "hours_actual", "numberjobs")
MONTHS   <- c("t1", "t2", "t3")
WCOL     <- c(t1 = "wt_t1", t2 = "wt_t2", t3 = "wt_t3")

set.seed(SEED); info(glue("Starting pipeline | seed={SEED}"))

# Helpers ----------------------------------------------------------------------
ybase_for <- function(outcome) if (outcome == "numberjobs") "numberjobs_num" else outcome

ensure_numberjobs_numeric <- function(df, months = c("t1","t2","t3")) {
  for (m in months) {
    num_nm <- paste0("numberjobs_num_", m)
    if (!num_nm %in% names(df) || all(is.na(df[[num_nm]]))) {
      fac_nm <- paste0("numberjobs_", m)
      if (fac_nm %in% names(df)) {
        v <- as.character(df[[fac_nm]])
        v <- dplyr::recode(v, "0"="0","1"="1","2"="2","3"="3","4+"="4", .default = NA_character_)
        df[[num_nm]] <- as.integer(v)
      } else if (all(c(paste0("MULTJOB_", m), paste0("NUMJOB_", m), paste0("employed_", m)) %in% names(df))) {
        e  <- df[[paste0("employed_", m)]]
        mj <- df[[paste0("MULTJOB_",  m)]]
        nj <- df[[paste0("NUMJOB_",   m)]]
        exact <- dplyr::case_when(
          e == 0L                                 ~ 0L,
          e == 1L & mj == 1L                      ~ 1L,
          e == 1L & mj == 2L & nj %in% c(2L,3L,4L) ~ nj,
          TRUE                                    ~ NA_integer_
        )
        df[[num_nm]] <- as.integer(pmin(exact, 4L))
      } else {
        df[[num_nm]] <- NA_integer_
      }
    }
  }
  df
}

num <- function(x) as.numeric(x)
weighted_mean <- function(x, w) {
  sw <- sum(num(w), na.rm = TRUE)
  if (!is.finite(sw) || sw == 0) return(NA_real_)
  sum(num(x) * num(w), na.rm = TRUE) / sw
}
mse_w       <- function(truth, estimate, w) weighted_mean((num(truth) - num(estimate))^2, w)
mse_uw      <- function(truth, estimate) mean((num(truth) - num(estimate))^2, na.rm = TRUE)
r2_from_mse <- function(mse_model, mse_baseline) 1 - (mse_model / mse_baseline)

outcome_range <- function(outcome) {
  if (outcome %in% c("employed", "married")) c(0, 1)
  else if (outcome == "hours_actual") c(0, 100)
  else if (outcome == "numberjobs") c(0, 4)
  else c(-Inf, Inf)
}

# Data -------------------------------------------------------------------------
triads_csv <- file.path(BASE_DIR, "triads_wide.csv")
stopifnot(file.exists(triads_csv))
triads_wide <- readr::read_csv(triads_csv, progress = TRUE, show_col_types = FALSE)

# Normalize hours: 999 -> NA; cap at 100
for (m in MONTHS) {
  nm <- paste0("hours_actual_", m)
  if (nm %in% names(triads_wide)) {
    x <- triads_wide[[nm]]
    x <- ifelse(x == 999, NA, x)
    x <- pmin(x, 100)
    triads_wide[[nm]] <- as.numeric(x)
  }
}

# numberjobs factor cleanup
for (m in MONTHS) {
  nm <- paste0("numberjobs_", m)
  if (nm %in% names(triads_wide)) {
    raw <- triads_wide[[nm]]
    if (is.character(raw) || is.factor(raw)) {
      triads_wide[[nm]] <- factor(as.character(raw), levels = c("0","1","2","3","4+"))
    } else {
      triads_wide[[nm]] <- factor(as.character(pmin(pmax(raw,0), 4)),
                                  levels = c("0","1","2","3","4"),
                                  labels = c("0","1","2","3","4+"))
    }
  }
}
triads_wide <- ensure_numberjobs_numeric(triads_wide)

# Coerce YOB numeric
if (VAR_YOB %in% names(triads_wide) && !is.numeric(triads_wide[[VAR_YOB]])) {
  suppressWarnings(triads_wide[[VAR_YOB]] <- as.numeric(triads_wide[[VAR_YOB]]))
}

# Binarize employed/married to 0/1
to_01 <- function(x) { if (is.logical(x)) as.integer(x) else as.integer(factor(x, levels=c(0,1)))-1L }
for (o in c("employed","married")) for (m in MONTHS) {
  nm <- paste0(o, "_", m)
  if (nm %in% names(triads_wide)) triads_wide[[nm]] <- to_01(triads_wide[[nm]])
}

# Ensure UPPERCASE STATEFIP_* exist (fallback to lowercase if present)
for (m in MONTHS) {
  up <- paste0("STATEFIP_", m)
  lo <- paste0("statefip_", m)
  if (!(up %in% names(triads_wide)) && (lo %in% names(triads_wide))) {
    triads_wide[[up]] <- triads_wide[[lo]]
  }
}

# Build model table:
#  - predictors: outcome lags + STATIC_VARS
#  - carry (for saving only): YEAR_* and STATEFIP_* (not used as predictors)
SAVE_YSTATE <- c(paste0("YEAR_", MONTHS), paste0("STATEFIP_", MONTHS))

keep_cols <- unique(c(
  "pid","m0","triad_uid",
  paste0(rep(OUTCOMES, each = length(MONTHS)), "_", MONTHS),
  paste0("numberjobs_num_", MONTHS),
  STATIC_VARS, WCOL,
  intersect(SAVE_YSTATE, names(triads_wide))
))
dat0 <- triads_wide %>% dplyr::select(any_of(keep_cols))
rm(triads_wide); invisible(gc())

# Split ------------------------------------------------------------------------
set.seed(SEED)
pid_all   <- unique(dat0$pid)
pid_test  <- sample(pid_all, size = floor(TEST_PROP * length(pid_all)))
test_idx  <- dat0$pid %in% pid_test
train_df0 <- dat0[!test_idx, , drop = FALSE]
test_df   <- dat0[ test_idx, , drop = FALSE]
rm(dat0); invisible(gc())

train_df0 <- ensure_numberjobs_numeric(train_df0)
test_df   <- ensure_numberjobs_numeric(test_df)

must_have <- paste0("numberjobs_num_", MONTHS)
missing_train <- setdiff(must_have, names(train_df0))
missing_test  <- setdiff(must_have, names(test_df))
if (length(missing_train) || length(missing_test)) {
  stop(glue("Missing numberjobs numeric columns. train missing: {paste(missing_train, collapse=', ')};
test missing: {paste(missing_test, collapse=', ')}"))
}

info(glue("Holdout split | train rows={nrow(train_df0)}; test rows={nrow(test_df)}; ",
          "unique pids train={length(unique(train_df0$pid))}; test={length(unique(test_df$pid))}"))

# Cache splits -----------------------------------------------------------------
train_path <- file.path(DIR_CACHE, "train_df.rds")
test_path  <- file.path(DIR_CACHE, "test_df.rds")
saveRDS(train_df0, train_path); saveRDS(test_df, test_path)

# Task spec --------------------------------------------------------------------
task_spec <- tibble::tribble(
  ~task,   ~y_month, ~x_months,      ~weight_col,  ~naive,
  "pred",  "t3",     c("t1","t2"),   WCOL[["t3"]], "carry_forward",
  "retro", "t1",     c("t2","t3"),   WCOL[["t1"]], "carry_back",
  "fill",  "t2",     c("t1","t3"),   WCOL[["t2"]], "neighbor_avg"
)

get_ycol <- function(outcome, task) {
  y_m <- task_spec$y_month[[match(task, task_spec$task)]]
  paste0(ybase_for(outcome), "_", y_m)
}

naive_predict <- function(df, outcome, task) {
  sp <- task_spec %>% dplyr::filter(task == !!task) %>% dplyr::slice(1)
  xm <- sp$x_months[[1]]
  yb <- ybase_for(outcome)
  x1 <- paste0(yb, "_", xm[1]); x2 <- paste0(yb, "_", xm[2])
  if (sp$naive[[1]] == "carry_forward") df[[x2]]
  else if (sp$naive[[1]] == "carry_back") df[[x1]]
  else rowMeans(df[, c(x1, x2)], na.rm = TRUE)
}

# Recipes & model specs --------------------------------------------------------
build_recipe <- function(df, outcome, task) {
  df <- ensure_numberjobs_numeric(df)
  sp  <- task_spec %>% dplyr::filter(task == !!task) %>% dplyr::slice(1)
  y_m <- sp$y_month[[1]]; x_m <- sp$x_months[[1]]
  ycol   <- paste0(ybase_for(outcome), "_", y_m)
  x_cols <- paste0(ybase_for(outcome), "_", x_m)   # ONLY same-outcome lags
  
  model_df <- df %>%
    dplyr::select(pid, triad_uid, dplyr::all_of(c(ycol, x_cols, STATIC_VARS)))
  
  recipes::recipe(
    stats::reformulate(termlabels = setdiff(names(model_df), c("pid","triad_uid", ycol)),
                       response = ycol),
    data = model_df
  ) %>%
    step_naomit(all_outcomes(), skip = TRUE) %>%   # drop NA y when fitting; keep test rows
    step_novel(all_nominal_predictors()) %>%
    step_unknown(all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_normalize(all_of(VAR_YOB))
}

make_spec_lin <- function() linear_reg() %>% set_engine("lm")
add_interactions <- function(rec) rec %>% step_interact(~ (all_predictors())^2)

make_spec_xgb_tuned <- function(params, n_threads_xgb) {
  boost_tree(
    mode           = "regression",
    trees          = params$trees,
    learn_rate     = params$learn_rate,
    tree_depth     = params$tree_depth,
    min_n          = params$min_n,
    loss_reduction = params$loss_reduction,
    sample_size    = params$sample_size,
    mtry           = params$mtry
  ) %>% set_engine("xgboost", nthread = as.integer(n_threads_xgb), verbosity = 0)
}
make_spec_xgb_blank <- function(n_threads_xgb) {
  boost_tree(
    mode           = "regression",
    trees          = tune(),
    learn_rate     = tune(),
    tree_depth     = tune(),
    min_n          = tune(),
    loss_reduction = tune(),
    sample_size    = tune(),
    mtry           = tune()
  ) %>% set_engine("xgboost", nthread = as.integer(n_threads_xgb), verbosity = 0)
}
xgb_param_grid <- function(pred_df, size = XGB_GRID_N) {
  set.seed(SEED)
  p <- max(1L, ncol(pred_df))
  tibble::tibble(
    mtry           = sample.int(p, size, replace = TRUE),
    trees          = sample(150:300, size, replace = TRUE),
    learn_rate     = runif(size, 0.03, 0.20),
    tree_depth     = sample(3:6, size, replace = TRUE),
    min_n          = sample(20:100, size, replace = TRUE),
    loss_reduction = runif(size, 0, 3),
    sample_size    = runif(size, 0.5, 0.8)
  )
}

build_folds <- function(train_df) {
  set.seed(SEED)
  rsample::group_vfold_cv(train_df, v = FOLDS_V, group = pid)
}

# Split evaluation -------------------------------------------------------------
eval_on_split <- function(fitted_wf, split, ycol, weight_col, outcome, task, fold_id) {
  train_dat <- rsample::analysis(split)
  test_dat  <- rsample::assessment(split)
  
  wtrain <- train_dat[[weight_col]]
  mu_tr  <- weighted_mean(train_dat[[ycol]], wtrain)
  
  preds <- predict(fitted_wf, test_dat) %>% dplyr::pull(.pred)
  rng <- outcome_range(outcome)
  if (is.finite(rng[1]) && is.finite(rng[2])) preds <- pmin(rng[2], pmax(rng[1], preds))
  
  naive <- naive_predict(test_dat, outcome, task)
  w <- test_dat[[weight_col]]
  
  mse_model_w <- mse_w(test_dat[[ycol]], preds, w)
  mse_base_w  <- mse_w(test_dat[[ycol]], mu_tr, w)
  r2_w        <- r2_from_mse(mse_model_w, mse_base_w)
  
  mse_naiv_w  <- mse_w(test_dat[[ycol]], naive, w)
  r2_naiv_w   <- r2_from_mse(mse_naiv_w,  mse_base_w)
  
  mse_model   <- mse_uw(test_dat[[ycol]], preds)
  mse_base    <- mse_uw(test_dat[[ycol]], mu_tr)
  r2_uw       <- r2_from_mse(mse_model, mse_base)
  
  mse_naiv    <- mse_uw(test_dat[[ycol]], naive)
  r2_naiv     <- r2_from_mse(mse_naiv,  mse_base)
  
  list(
    metrics = tibble::tibble(
      fold_id     = fold_id,
      mse_w       = mse_model_w,  r2_w = r2_w,
      mse         = mse_model,    r2   = r2_uw,
      mse_naive_w = mse_naiv_w,   r2_naive_w = r2_naiv_w,
      mse_naive   = mse_naiv,     r2_naive   = r2_naiv,
      n           = nrow(test_dat),
      weight_sum  = sum(num(w), na.rm = TRUE)
    ),
    preds = tibble::tibble(
      y    = test_dat[[ycol]],
      yhat = preds,
      w    = w
    )
  )
}

# % variance explained (lin_add only) ------------------------------------------
var_explained_parts <- function(train_df, outcome, task, rec) {
  y_m  <- task_spec$y_month[[match(task, task_spec$task)]]
  ycol <- paste0(ybase_for(outcome), "_", y_m)
  xm   <- task_spec$x_months[[match(task, task_spec$task)]][[1]]
  lag_cols <- paste0(ybase_for(outcome), "_", xm)
  baked <- recipes::prep(rec) %>% recipes::juice()
  
  f_static <- reformulate(termlabels = c(VAR_YOB, VAR_SEX, VAR_RACE), response = ycol)
  f_full   <- reformulate(termlabels = c(VAR_YOB, VAR_SEX, VAR_RACE, lag_cols), response = ycol)
  
  m_static <- stats::lm(f_static, data = baked)
  m_full   <- stats::lm(f_full,   data = baked)
  
  r2_static <- summary(m_static)$r.squared
  r2_full   <- summary(m_full)$r.squared
  add_temporal <- max(0, r2_full - r2_static)
  
  tibble::tibble(
    outcome = outcome, task = task, model = "lin_add",
    part    = c("Static covariates","Temporal predictors"),
    share   = pmax(0, c(r2_static, add_temporal)),
    r2_full = r2_full
  )
}

# One run (robust) -------------------------------------------------------------
run_one <- function(task, outcome, model, train_path, test_path, n_threads_xgb) {
  tryCatch({
    train_df0 <- readRDS(train_path)
    test_df   <- readRDS(test_path)
    
    train_df0 <- ensure_numberjobs_numeric(train_df0)
    test_df   <- ensure_numberjobs_numeric(test_df)
    
    sp   <- task_spec %>% dplyr::filter(task == !!task) %>% dplyr::slice(1)
    wcol <- sp$weight_col[[1]]
    ycol <- get_ycol(outcome, task)
    y_m  <- sp$y_month[[1]]
    
    run_id <- glue("{task}__{outcome}__{model}__seed{SEED}")
    info(glue("Run start: {run_id}"))
    t0 <- Sys.time()
    
    # ---------- Diagnostics scaffold ----------
    diag_row <- tibble::tibble(
      run_id   = run_id,
      task     = task,
      outcome  = outcome,
      model    = model,
      n_train_before = nrow(train_df0),
      n_train_na_y   = sum(is.na(train_df0[[ycol]])),
      n_train_after  = NA_integer_,
      y_sd_after     = NA_real_,
      unique_y_after = NA_integer_,
      n_folds_total  = FOLDS_V,
      n_folds_ok     = 0L,
      final_fit_ok   = FALSE,
      message        = NA_character_
    )
    
    # Drop NA outcomes before CV/fitting
    train_df <- train_df0 %>% dplyr::filter(!is.na(.data[[ycol]]))
    diag_row$n_train_after  <- nrow(train_df)
    diag_row$y_sd_after     <- stats::sd(train_df[[ycol]], na.rm = TRUE)
    diag_row$unique_y_after <- dplyr::n_distinct(train_df[[ycol]])
    
    # Early exits
    if (diag_row$n_train_after < 100L) {
      warn(glue("Too few training rows after dropping NA y ({diag_row$n_train_after}) for {run_id}."))
      diag_row$message <- "too_few_training_rows_after_drop_na_y"
      readr::write_csv(diag_row, file.path(DIR_DIAG, glue("DIAG__{run_id}.csv")))
      return(list(cv_metrics = tibble(), test_metrics = tibble(), diag = diag_row))
    }
    if (!is.finite(diag_row$y_sd_after) || diag_row$y_sd_after == 0) {
      warn(glue("Zero/undefined variance in y for {run_id}."))
      diag_row$message <- "zero_variance_y"
      readr::write_csv(diag_row, file.path(DIR_DIAG, glue("DIAG__{run_id}.csv")))
      return(list(cv_metrics = tibble(), test_metrics = tibble(), diag = diag_row))
    }
    
    # Folds AFTER filtering NA y
    folds <- build_folds(train_df)
    
    # Build recipe & model
    rec <- build_recipe(train_df, outcome, task)
    if (model == "lin_int") rec <- add_interactions(rec)
    
    mod_blank <- switch(model,
                        lin_add = make_spec_lin(),
                        lin_int = make_spec_lin(),
                        xgb     = make_spec_xgb_blank(n_threads_xgb),
                        stop("Unknown model: ", model))
    wf <- workflows::workflow() %>% workflows::add_model(mod_blank) %>% workflows::add_recipe(rec)
    
    params_json <- "{}"
    
    # XGB tuning (robust to failures)
    if (model == "xgb") {
      set.seed(SEED)
      baked   <- recipes::prep(rec) %>% recipes::bake(new_data = NULL) %>% tibble::as_tibble()
      pred_df <- baked %>% dplyr::select(-all_of(ycol))
      if (ncol(pred_df) == 0) {
        warn(glue("No predictors left after preprocessing for {run_id}."))
        diag_row$message <- "no_predictors_after_preprocessing"
        readr::write_csv(diag_row, file.path(DIR_DIAG, glue("DIAG__{run_id}.csv")))
        return(list(cv_metrics = tibble(), test_metrics = tibble(), diag = diag_row))
      }
      grid <- xgb_param_grid(pred_df, XGB_GRID_N)
      
      tuned_try <- try(
        tune::tune_grid(
          wf, resamples = folds, grid = grid,
          metrics = yardstick::metric_set(yardstick::rmse),
          control = tune::control_grid(save_pred = FALSE, save_workflow = FALSE, verbose = FALSE)
        ),
        silent = TRUE
      )
      
      if (inherits(tuned_try, "try-error")) {
        warn(glue("Tuning failed for {run_id}; using first grid row"))
        best_params <- grid %>% dplyr::slice(1)
      } else {
        mets <- try(tune::collect_metrics(tuned_try), silent = TRUE)
        if (inherits(mets, "try-error") || nrow(mets) == 0) {
          warn(glue("No metrics from tuning for {run_id}; using first grid row"))
          best_params <- grid %>% dplyr::slice(1)
        } else {
          best_row <- mets %>% dplyr::filter(.metric == "rmse") %>% dplyr::arrange(mean) %>% dplyr::slice(1)
          if (nrow(best_row) == 0 || !is.finite(best_row$mean[1])) {
            warn(glue("Bad tuning metrics for {run_id}; using first grid row"))
            best_params <- grid %>% dplyr::slice(1)
          } else {
            best_params <- best_row %>% dplyr::select(any_of(colnames(grid)))
          }
        }
      }
      
      mod_final <- make_spec_xgb_tuned(best_params, n_threads_xgb)
      wf <- workflows::workflow() %>% workflows::add_model(mod_final) %>% workflows::add_recipe(rec)
      params_json <- as.character(jsonlite::toJSON(best_params, auto_unbox = TRUE))
    }
    
    # --------- CV evaluation (per-fold safe fit) ---------
    fit_eval_safe <- purrr::safely(function(sp_split, fid) {
      fitted <- parsnip::fit(wf, data = rsample::analysis(sp_split))
      eval_on_split(fitted, sp_split, ycol, wcol, outcome, task, fold_id = fid)
    })
    
    fold_results_raw <- purrr::map2(folds$splits, folds$id, fit_eval_safe)
    fold_ok   <- purrr::map_lgl(fold_results_raw, ~ is.null(.x$error))
    fold_good <- fold_results_raw[fold_ok]
    diag_row$n_folds_ok <- sum(fold_ok)
    
    if (diag_row$n_folds_ok == 0L) {
      warn(glue("All folds failed for {run_id}. Example error: {fold_results_raw[[1]]$error}"))
      diag_row$message <- "all_folds_failed"
      readr::write_csv(diag_row, file.path(DIR_DIAG, glue("DIAG__{run_id}.csv")))
      return(list(cv_metrics = tibble(), test_metrics = tibble(), diag = diag_row))
    }
    
    metrics_cv <- dplyr::bind_rows(purrr::map(fold_good, ~ .x$result$metrics)) %>%
      dplyr::mutate(task = task, outcome = outcome, model = model, run_id = run_id) %>%
      dplyr::mutate(runtime_sec = as.numeric(difftime(Sys.time(), t0, units = "secs"))) %>%
      dplyr::relocate(task, outcome, model, run_id)
    readr::write_csv(metrics_cv, file.path(DIR_METRICS, glue("{run_id}__cv_metrics.csv")))
    
    preds_cv <- dplyr::bind_rows(purrr::map(fold_good, ~ .x$result$preds))
    dec_cv <- preds_cv %>%
      dplyr::mutate(decile = dplyr::ntile(yhat, 10L)) %>%
      dplyr::count(decile, name = "n") %>%
      dplyr::mutate(task = task, outcome = outcome, model = model, run_id = run_id, .before = 1L)
    readr::write_csv(dec_cv, file.path(DIR_DECI, glue("{run_id}__cv_deciles.csv")))
    
    # --------- Final fit on full train + test (safe) ---------
    final_fit_try <- try(parsnip::fit(wf, data = train_df), silent = TRUE)
    if (inherits(final_fit_try, "try-error")) {
      warn(glue("Final fit failed for {run_id}: {conditionMessage(attr(final_fit_try,'condition'))}"))
      diag_row$message <- "final_fit_failed"
      readr::write_csv(diag_row, file.path(DIR_DIAG, glue("DIAG__{run_id}.csv")))
      return(list(cv_metrics = metrics_cv, test_metrics = tibble(), diag = diag_row))
    }
    final_fit <- final_fit_try
    diag_row$final_fit_ok <- TRUE
    
    saveRDS(final_fit, file.path(DIR_MODELS, glue("{run_id}__workflow.rds")))
    
    mu_train   <- weighted_mean(train_df[[ycol]], train_df[[wcol]])
    preds_test <- predict(final_fit, test_df) %>% dplyr::pull(.pred)
    rng <- outcome_range(outcome)
    if (is.finite(rng[1]) && is.finite(rng[2])) preds_test <- pmin(rng[2], pmax(rng[1], preds_test))
    
    naive_test <- naive_predict(test_df, outcome, task)
    w_te <- test_df[[wcol]]
    
    mse_w_te  <- mse_w(test_df[[ycol]], preds_test, w_te)
    mse_w_bas <- mse_w(test_df[[ycol]], mu_train,  w_te)
    r2_w_te   <- r2_from_mse(mse_w_te, mse_w_bas)
    
    mse_w_na  <- mse_w(test_df[[ycol]], naive_test, w_te)
    r2_w_na   <- r2_from_mse(mse_w_na,  mse_w_bas)
    
    mse_te    <- mse_uw(test_df[[ycol]], preds_test)
    mse_base  <- mse_uw(test_df[[ycol]], mu_train)
    r2_te     <- r2_from_mse(mse_te, mse_base)
    
    mse_na    <- mse_uw(test_df[[ycol]], naive_test)
    r2_na     <- r2_from_mse(mse_na,  mse_base)
    
    if (model == "lin_add") {
      varexp <- var_explained_parts(train_df, outcome, task, rec)
      readr::write_csv(varexp, file.path(DIR_METRICS, glue("VAREXP__{run_id}.csv")))
    }
    
    # Save prediction-year and state for the y-month ---------------------------
    year_col  <- paste0("YEAR_",     y_m)
    state_col <- paste0("STATEFIP_", y_m)
    year_y    <- if (year_col  %in% names(test_df))  test_df[[year_col]]  else rep(NA_integer_, nrow(test_df))
    state_y   <- if (state_col %in% names(test_df))  test_df[[state_col]] else rep(NA_integer_, nrow(test_df))
    
    preds_test_df <- tibble::tibble(
      pid           = test_df$pid,
      triad_uid     = test_df$triad_uid,
      y             = test_df[[ycol]],
      yhat          = preds_test,
      w             = w_te,
      fold_id       = "TEST",
      task          = task,
      outcome       = outcome,
      model         = model,
      run_id        = run_id,
      year_of_birth = test_df[[VAR_YOB]],
      sex           = test_df[[VAR_SEX]],
      race          = test_df[[VAR_RACE]],
      year_y        = year_y,      # actual year of prediction target
      statefip_y    = state_y      # state for the target month
    )
    readr::write_csv(preds_test_df, file.path(DIR_PRED, glue("{run_id}__test_predictions.csv")))
    
    # MSE breakdown by (t2,t3) for binary outcomes (pred only) -----------------
    if (task == "pred" && outcome %in% c("employed","married")) {
      prev_col <- paste0(ybase_for(outcome), "_t2")
      cur_col  <- paste0(ybase_for(outcome), "_t3")
      bd <- test_df %>%
        transmute(prev = .data[[prev_col]], cur = .data[[cur_col]],
                  yhat = preds_test, y = .data[[cur_col]], w = w_te) %>%
        mutate(se = (y - yhat)^2) %>%
        group_by(prev, cur) %>%
        summarise(mse_w = weighted_mean(se, w), n = n(), .groups = "drop") %>%
        mutate(task = task, outcome = outcome, model = model, run_id = run_id)
      readr::write_csv(bd, file.path(DIR_METRICS, glue("{run_id}__mse_breakdown_prevcur.csv")))
    }
    
    metrics_test <- tibble::tibble(
      task, outcome, model, run_id,
      fold_id = "TEST",
      mse_w = mse_w_te, r2_w = r2_w_te,
      mse   = mse_te,   r2   = r2_te,
      mse_naive_w = mse_w_na, r2_naive_w = r2_w_na,
      mse_naive   = mse_na,   r2_naive   = r2_na,
      n = nrow(test_df), weight_sum = sum(num(w_te), na.rm = TRUE),
      params_json = params_json,
      runtime_sec = as.numeric(difftime(Sys.time(), t0, units = "secs"))
    )
    readr::write_csv(metrics_test, file.path(DIR_METRICS, glue("{run_id}__test_metrics.csv")))
    
    dec_test <- preds_test_df %>%
      dplyr::mutate(decile = dplyr::ntile(yhat, 10L)) %>%
      dplyr::count(task, outcome, model, run_id, decile, name = "n")
    readr::write_csv(dec_test, file.path(DIR_DECI, glue("{run_id}__test_deciles.csv")))
    
    # Save diagnostics row
    diag_row$message <- "ok"
    readr::write_csv(diag_row, file.path(DIR_DIAG, glue("DIAG__{run_id}.csv")))
    
    info(glue("Run done: {run_id} | {round(as.numeric(difftime(Sys.time(), t0, units = 'secs')), 2)} sec"))
    list(cv_metrics = metrics_cv, test_metrics = metrics_test, diag = diag_row)
    
  }, error = function(e) {
    warn(glue("Run failed for {task}/{outcome}/{model}: {conditionMessage(e)}"))
    diag_row <- tibble::tibble(
      run_id   = glue("{task}__{outcome}__{model}__seed{SEED}"),
      task     = task, outcome = outcome, model = model,
      n_train_before = NA_integer_, n_train_na_y = NA_integer_, n_train_after = NA_integer_,
      y_sd_after = NA_real_, unique_y_after = NA_integer_,
      n_folds_total = FOLDS_V, n_folds_ok = 0L, final_fit_ok = FALSE,
      message = paste0("top_level_error: ", conditionMessage(e))
    )
    readr::write_csv(diag_row, file.path(DIR_DIAG, glue("DIAG__{diag_row$run_id}.csv")))
    list(cv_metrics = tibble(), test_metrics = tibble(), diag = diag_row)
  })
}

# Grid of runs -----------------------------------------------------------------
run_grid <- tidyr::expand_grid(
  task    = c("pred","retro","fill"),
  outcome = OUTCOMES,
  model   = c("lin_add","lin_int","xgb")
)

# Progress handlers ------------------------------------------------------------
handlers(global = TRUE)
handlers("txtprogressbar")

# Execution plan ---------------------------------------------------------------
run_grid_safely <- function(grid) {
  if (!PARALLEL || N_WORKERS == 1L) {
    info("Running sequential (PARALLEL=FALSE or one worker).")
    return(purrr::pmap(
      grid,
      ~ withr::with_seed(SEED, run_one(..1, ..2, ..3,
                                       train_path, test_path,
                                       n_threads_xgb = XGB_THREADS_PER_WORKER))
    ))
  }
  
  info(glue("Starting multisession with {N_WORKERS} workers ..."))
  future::plan(future::multisession, workers = N_WORKERS, gc = TRUE)
  
  try_parallel <- try({
    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(grid))
      furrr::future_pmap(
        grid,
        function(task, outcome, model) {
          res <- withr::with_seed(
            SEED,
            run_one(task, outcome, model,
                    train_path, test_path,
                    n_threads_xgb = XGB_THREADS_PER_WORKER)
          )
          p(sprintf("%s/%s/%s", task, outcome, model))
          res
        },
        .options = furrr::furrr_options(seed = TRUE, stdout = FALSE, scheduling = 1)
      )
    })
  }, silent = TRUE)
  
  if (inherits(try_parallel, "try-error")) {
    warn(glue("Parallel run failed; falling back to sequential. Reason: {conditionMessage(attr(try_parallel, 'condition'))}"))
    future::plan(future::sequential)
    return(purrr::pmap(
      grid,
      ~ withr::with_seed(SEED, run_one(..1, ..2, ..3,
                                       train_path, test_path,
                                       n_threads_xgb = XGB_THREADS_PER_WORKER))
    ))
  }
  
  try_parallel
}

# Run --------------------------------------------------------------------------
set.seed(SEED)
info(glue("Submitting {nrow(run_grid)} runs ..."))
results <- run_grid_safely(run_grid)

# Combine & save summaries -----------------------------------------------------
all_cv   <- results %>% purrr::map("cv_metrics")   %>% purrr::compact() %>% dplyr::bind_rows()
all_test <- results %>% purrr::map("test_metrics") %>% purrr::compact() %>% dplyr::bind_rows()
all_diag <- results %>% purrr::map("diag")         %>% purrr::compact() %>% dplyr::bind_rows()

if (nrow(all_cv))   readr::write_csv(all_cv,   file.path(DIR_METRICS, "ALL__cv_metrics_summary.csv"))
if (nrow(all_test)) readr::write_csv(all_test, file.path(DIR_METRICS, "ALL__test_metrics_summary.csv"))
if (nrow(all_diag)) readr::write_csv(all_diag, file.path(DIR_DIAG,    "ALL__run_diagnostics.csv"))

if (nrow(all_cv)) {
  cv_means <- all_cv %>%
    dplyr::group_by(task, outcome, model) %>%
    dplyr::summarise(dplyr::across(
      c(mse_w, r2_w, mse, r2, mse_naive_w, r2_naive_w, mse_naive, r2_naive),
      ~ mean(.x, na.rm = TRUE)
    ), .groups = "drop")
  readr::write_csv(cv_means, file.path(DIR_METRICS, "ALL__cv_metrics_mean_by_run.csv"))
}

# Audit: which runs wrote predictions? -----------------------------------------
expected <- run_grid %>%
  mutate(run_id = glue("{task}__{outcome}__{model}__seed{SEED}"),
         file   = file.path(DIR_PRED, glue("{run_id}__test_predictions.csv")))

pred_exists <- expected %>%
  mutate(file_exists = fs::file_exists(file)) %>%
  left_join(all_diag %>% dplyr::select(run_id, message, n_train_before, n_train_na_y,
                                       n_train_after, y_sd_after, n_folds_ok, final_fit_ok),
            by = "run_id")

readr::write_csv(pred_exists, file.path(DIR_DIAG, "AUDIT__predictions_expected_vs_found.csv"))

missing <- pred_exists %>% filter(!file_exists)
if (nrow(missing)) {
  warn(glue("Missing predictions for {nrow(missing)} runs. See diagnostics/AUDIT__predictions_expected_vs_found.csv"))
}

info("Pipeline complete ✓")
sessionInfo()
