# ──────────────────────────────────────────────────────────────────────────────
# Grouped analyses & plots 
# ──────────────────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(glue)
  library(scales)
  library(maps)
  library(yardstick)        # ROC/AUC with weights
})

# Paths ------------------------------------------------------------------------
BASE_DIR    <- "/Users/arpm/Downloads/retrodiction"
DIR_METRICS <- file.path(BASE_DIR, "metrics")
DIR_PRED    <- file.path(BASE_DIR, "predictions")
DIR_FIG     <- file.path(BASE_DIR, "figures")
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

# Load predictions -------------------------------------------------------------
pred_files <- list.files(DIR_PRED, pattern = "__test_predictions\\.csv$", full.names = TRUE)
stopifnot(length(pred_files) > 0)
pred_all <- purrr::map_dfr(pred_files, ~ readr::read_csv(.x, show_col_types = FALSE))

# Required columns
req_cols <- c("y","yhat","w","task","outcome","model","run_id",
              "year_of_birth","sex","race","year_y","statefip_y")
missing_cols <- setdiff(req_cols, names(pred_all))
if (length(missing_cols)) {
  stop(glue("Missing required columns in saved predictions: {paste(missing_cols, collapse=', ')}"))
}

# Helpers ----------------------------------------------------------------------
num   <- function(x) as.numeric(x)
wmean <- function(x, w) {
  sw <- sum(num(w), na.rm = TRUE)
  if (!is.finite(sw) || sw == 0) return(NA_real_)
  sum(num(x) * num(w), na.rm = TRUE) / sw
}
wmse <- function(y, yhat, w) wmean((num(y) - num(yhat))^2, w)
wr2  <- function(y, yhat, w) {
  mu    <- wmean(y, w)
  denom <- wmse(y, rep(mu, length(y)), w)
  if (!is.finite(denom) || denom == 0) return(NA_real_)
  1 - wmse(y, yhat, w) / denom
}

binary_outcomes     <- c("employed","married")
continuous_outcomes <- c("hours_actual","numberjobs")

# Recover a continuous age for plots (no binning) ------------------------------
pred_all <- pred_all %>%
  mutate(
    age_raw = ifelse(!is.na(year_y) & !is.na(year_of_birth),
                     as.numeric(year_y) - as.numeric(year_of_birth), NA_real_),
    # keep a defensible plotting window (avoids sparse tails / noise)
    age = ifelse(!is.na(age_raw) & age_raw >= 15 & age_raw <= 85, age_raw, NA_real_)
  )

# Weighted R² by task × model (faceted by outcome) — remove dotted y=0 -----
overall_r2 <- pred_all %>%
  group_by(outcome, task, model) %>%
  summarise(
    n = dplyr::n(),
    weight_sum = sum(num(w), na.rm = TRUE),
    r2_w = wr2(y, yhat, w),
    .groups = "drop"
  )

p_overall <- overall_r2 %>%
  ggplot(aes(x = task, y = r2_w, color = model, group = model)) +
  geom_point(position = position_dodge(width = 0.35), alpha = 0.7) +
  geom_line(position = position_dodge(width = 0.35), alpha = 0.7) +
  facet_wrap(~ outcome, nrow = 2, scales = "free_y") +  # free y per outcome
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.03, 0.07))) +
  labs(title = "Weighted R² by task × model (faceted by outcome)",
       x = NULL, y = "R² (weighted)", color = "Model") +
  theme_bw(base_size = 11)
ggsave(file.path(DIR_FIG, "overall_r2_by_task_model.png"), p_overall, width = 10, height = 7, dpi = 200)

# --- Calibration by prediction deciles (x=y per panel; free per-panel limits) ---
# --- Calibration by prediction deciles (robust layout: pages by outcome; x=y per panel) ---
suppressPackageStartupMessages({ library(patchwork) })

# Aggregate to deciles (weighted)
cal_df <- pred_all %>%
  group_by(outcome, task, model) %>%
  group_modify(~ {
    d <- .x %>% mutate(decile = dplyr::ntile(yhat, 10L))
    d %>% group_by(decile, .add = TRUE) %>%
      summarise(
        yhat_bar = wmean(yhat, w),
        y_bar    = wmean(y, w),
        n        = dplyr::n(),
        .groups  = "drop"
      )
  }) %>% ungroup()

# Per-panel limits for continuous outcomes; binary panels will be fixed to [0,1]
cal_lims <- cal_df %>%
  group_by(outcome, task, model) %>%
  summarise(
    lim_min = min(c(yhat_bar, y_bar), na.rm = TRUE),
    lim_max = max(c(yhat_bar, y_bar), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    lim_max = ifelse(lim_max > lim_min, lim_max, lim_min + 1e-6),
    pad     = 0.04 * (lim_max - lim_min),
    xmin    = lim_min - pad,
    xmax    = lim_max + pad
  )

is_binary_outcome <- function(x) x %in% c("employed","married")

theme_cal_panel <- theme_bw(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title       = element_blank()
  )

make_panel <- function(df_one, lims_one, is_binary) {
  p <- ggplot(df_one, aes(x = yhat_bar, y = y_bar)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_point(aes(size = n), alpha = 0.85, stroke = 0) +
    theme_cal_panel +
    scale_size_continuous(
      name   = "N",
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    )
  
  if (is_binary) {
    p +
      coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  } else {
    rng <- c(lims_one$xmin[1], lims_one$xmax[1])
    p +
      coord_equal(xlim = rng, ylim = rng, expand = FALSE) +
      scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
      scale_y_continuous(labels = scales::label_number(accuracy = 1))
  }
}

# Build pages per outcome; grid = rows: tasks, cols: models
keys_all  <- cal_df %>% distinct(outcome, task, model)
outcomes  <- unique(keys_all$outcome)
tasks     <- sort(unique(keys_all$task))
models    <- sort(unique(keys_all$model))
ncol_grid <- length(models)

for (oc in outcomes) {
  keys <- keys_all %>%
    filter(outcome == oc) %>%
    arrange(match(task, tasks), match(model, models))
  
  panels <- purrr::pmap(keys, function(outcome, task, model) {
    df1 <- dplyr::filter(cal_df, outcome == !!outcome, task == !!task, model == !!model)
    lm1 <- dplyr::filter(cal_lims, outcome == !!outcome, task == !!task, model == !!model)
    make_panel(df1, lm1, is_binary_outcome(outcome)) +
      labs(title = paste0(task, " \u2022 ", model))
  })
  
  p_grid <- wrap_plots(panels, ncol = ncol_grid, guides = "collect") &
    theme(legend.position = "right",
          plot.title = element_text(size = 11, face = "bold"),
          strip.text = element_text(size = 9))
  
  p_out <- p_grid +
    plot_annotation(
      title = glue::glue("Calibration by prediction deciles (weighted) — {oc}"),
      theme = theme(plot.title = element_text(hjust = 0, face = "bold"))
    ) &
    theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5))
  
  # Add global axis labels using empty spacers
  p_out <- p_out +
    plot_annotation(
      caption = "x: Average predicted value   |   y: Observed mean"
    )
  
  ggsave(
    file.path(DIR_FIG, glue::glue("calibration_deciles_{oc}.png")),
    p_out,
    width = 12, height = 3.8 * length(tasks), dpi = 200
  )
}



# ROC curves + AUC (binary outcomes only) -----------------------------------
pred_bin <- pred_all %>% filter(outcome %in% binary_outcomes)
if (nrow(pred_bin)) {
  pred_bin_y <- pred_bin %>%
    mutate(
      y_bin   = factor(ifelse(y >= 0.5, "1", "0"), levels = c("0","1")),
      .pred_1 = as.numeric(yhat)
    ) %>%
    group_by(outcome, task, model) %>%
    filter(dplyr::n_distinct(y_bin) == 2L) %>%
    ungroup()
  
  # AUC summary
  auc_tbl <- pred_bin_y %>%
    group_by(outcome, task, model) %>%
    yardstick::roc_auc(truth = y_bin, .pred_1, event_level = "second", case_weights = w) %>%
    ungroup() %>% rename(auc = .estimate)
  readr::write_csv(auc_tbl, file.path(DIR_METRICS, "AUC__binary_outcomes_by_task_model.csv"))
  
  # ROC curves
  roc_df <- pred_bin_y %>%
    group_by(outcome, task, model) %>%
    yardstick::roc_curve(truth = y_bin, .pred_1, event_level = "second", case_weights = w) %>%
    ungroup()
  
  p_roc <- roc_df %>%
    ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
    geom_abline(linetype = "dashed") +
    geom_path(linewidth = 0.9, alpha = 0.6) +
    facet_grid(rows = vars(outcome), cols = vars(task)) +
    coord_equal() +
    labs(title = "ROC curves (weighted) — binary outcomes",
         x = "False Positive Rate", y = "True Positive Rate", color = "Model") +
    theme_bw(base_size = 11)
  ggsave(file.path(DIR_FIG, "roc_curves_binary.png"), p_roc, width = 11, height = 7, dpi = 200)
}


# Age view — continuous age on x; separate lines by task × model ------------
if ("age" %in% names(pred_all) && any(!is.na(pred_all$age))) {
  r2_age_cont <- pred_all %>%
    filter(!is.na(age)) %>%
    group_by(outcome, task, model, age) %>%
    summarise(
      n = dplyr::n(),
      r2_w = wr2(y, yhat, w),
      .groups = "drop"
    ) %>%
    filter(is.finite(r2_w))
  
  p_r2_age <- r2_age_cont %>%
    ggplot(aes(x = age, y = r2_w,
               color = task, linetype = model,
               group = interaction(task, model))) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line(alpha = 0.8, linewidth = 0.8) +
    facet_wrap(~ outcome, nrow = 2, scales = "free_y") +
    scale_x_continuous(breaks = seq(15, 85, by = 10)) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0.03, 0.07))) +
    labs(title = "Weighted R² over continuous age (separate lines per task×model)",
         x = "Age", y = "R² (weighted)", color = "Task", linetype = "Model") +
    theme_bw(base_size = 10)
  ggsave(file.path(DIR_FIG, "r2_by_age_continuous.png"), p_r2_age, width = 12, height = 7, dpi = 200)
}

# Binary prev/cur MSE heatmap — by task  ------------------------------
bd_files <- list.files(DIR_METRICS, pattern = "__mse_breakdown_prevcur\\.csv$", full.names = TRUE)
if (length(bd_files) > 0) {
  bd_all <- purrr::map_dfr(bd_files, ~ readr::read_csv(.x, show_col_types = FALSE)) %>%
    filter(outcome %in% binary_outcomes)
  
  if (nrow(bd_all)) {
    p_bd <- bd_all %>%
      mutate(prev = factor(prev, levels = c(0,1), labels = c("t2=0","t2=1")),
             cur  = factor(cur,  levels = c(0,1), labels = c("y=0","y=1"))) %>%
      ggplot(aes(x = prev, y = cur, fill = mse_w)) +
      geom_tile(color = "white") +
      facet_grid(rows = vars(outcome, task), cols = vars(model)) +
      scale_fill_distiller(palette = "RdYlGn", direction = -1) +
      labs(title = "MSE breakdown by actual states (prev, y) — binary outcomes, by task",
           x = "Previous month state", y = "Target month state", fill = "MSE (weighted)") +
      theme_bw(base_size = 11)
    ggsave(file.path(DIR_FIG, "mse_breakdown_prev_cur_binary_by_task.png"),
           p_bd, width = 12, height = 8, dpi = 200)
  }
}

# US choropleth of R² by state (kept) ---------------------------------------
if (requireNamespace("maps", quietly = TRUE)) {
  library(maps)
  
  # STATEFIP -> map region (DC excluded from base 'state' map)
  state_xwalk <- tibble::tribble(
    ~statefip_y, ~region,
    1,"alabama",2,"alaska",4,"arizona",5,"arkansas",6,"california",
    8,"colorado",9,"connecticut",10,"delaware",11,NA_character_,
    12,"florida",13,"georgia",15,"hawaii",16,"idaho",17,"illinois",
    18,"indiana",19,"iowa",20,"kansas",21,"kentucky",22,"louisiana",
    23,"maine",24,"maryland",25,"massachusetts",26,"michigan",
    27,"minnesota",28,"mississippi",29,"missouri",30,"montana",31,"nebraska",
    32,"nevada",33,"new hampshire",34,"new jersey",35,"new mexico",
    36,"new york",37,"north carolina",38,"north dakota",39,"ohio",
    40,"oklahoma",41,"oregon",42,"pennsylvania",44,"rhode island",
    45,"south carolina",46,"south dakota",47,"tennessee",48,"texas",
    49,"utah",50,"vermont",51,"virginia",53,"washington",
    54,"west virginia",55,"wisconsin",56,"wyoming"
  )
  
  us_map <- map_data("state") %>% as_tibble()
  
  # R² per state/outcome/task/model
  r2_state <- pred_all %>%
    filter(!is.na(statefip_y)) %>%
    group_by(outcome, task, model, statefip_y) %>%
    summarise(r2_w = wr2(y, yhat, w), n = dplyr::n(), .groups = "drop") %>%
    left_join(state_xwalk, by = "statefip_y") %>%
    filter(!is.na(region))
  
  # Build a COMPLETE facet grid so no NA facet appears
  facet_levels_outcome <- sort(unique(r2_state$outcome))
  facet_levels_task    <- sort(unique(r2_state$task))
  facet_levels_model   <- sort(unique(r2_state$model))
  regions_df           <- tibble(region = unique(us_map$region))
  
  facet_grid_df <- tidyr::crossing(
    regions_df,
    outcome = facet_levels_outcome,
    task    = facet_levels_task,
    model   = facet_levels_model
  )
  
  # Join R² onto complete grid, then join polygons
  plot_df <- facet_grid_df %>%
    left_join(r2_state, by = c("region","outcome","task","model")) %>%
    left_join(us_map, by = "region", relationship = "many-to-many")
  
  # Draw: rows = outcome, cols = task × model
  p_map <- ggplot(plot_df, aes(long, lat, group = group, fill = r2_w)) +
    geom_polygon(color = "white", linewidth = 0.15) +
    coord_fixed(1.3) +
    facet_grid(rows = vars(outcome), cols = vars(task, model)) +
    scale_fill_distiller(
      palette = "RdYlGn", direction = 1, na.value = "grey90",
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(title = "Weighted R² by state — all models",
         fill = "R² (weighted)", x = NULL, y = NULL) +
    theme_bw(base_size = 10) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid = element_blank())
  
  ggsave(file.path(DIR_FIG, "r2_state_map_by_model.png"),
         p_map, width = 14, height = 8.5, dpi = 200)
} else {
  message("Skipping US choropleth: package 'maps' not available.")
}

message("Saved figures to: ", DIR_FIG)
