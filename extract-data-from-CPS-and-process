# ── Packages ───────────────────────────────────────────────────────────────────
library(ipumsr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)
library(ggplot2)
library(forcats)

# desired location of final file
location <- "/Users/arpm/Downloads/retrodiction/triads_wide.csv"

# ── Discover all Basic Monthly samples ────────────────────────────────────────
samps <- get_sample_info("cps")
monthly_basic <- sort(grep("^cps\\d{4}_\\d{2}b$", samps$name, value = TRUE))

# ── Variables: labor force, hours, job chars, demographics, IDs/weights ──────
geo_vars <- c("REGION","STATEFIP","METRO","METFIPS","CBSASZ","COUNTY")
vars_needed <- c(
  # IDs / time / weights
  "YEAR","MONTH","CPSIDP","SERIAL","PERNUM","MISH","WTFINL",
  # labor force core
  "EMPSTAT","LABFORCE",
  # unemployment & layoff detail
  "WHYUNEMP","DURUNEMP","ABSENT","WHYABSNT",
  # hours worked
  "AHRSWORKT","AHRSWORK1","AHRSWORK2","UHRSWORKT","UHRSWORK1","UHRSWORK2",
  # job characteristics
  "CLASSWKR","IND","OCC","MULTJOB","NUMJOB",
  # demographics
  "AGE","SEX","RACE","HISPAN","MARST","VETSTAT","EDUC",
  # geography
  geo_vars
)


# ── Define & submit extract ───────────────────────────────────────────────────
cps_ext <- define_extract_micro(
  collection  = "cps",
  description = "CPS Basic Monthly: longitudinal person panel with core work + demo vars",
  samples     = monthly_basic,
  variables   = vars_needed
)

done     <- submit_extract(cps_ext) |> wait_for_extract()
ddi_path <- download_extract(done)
ddi      <- read_ipums_ddi(ddi_path)
cps_raw  <- read_ipums_micro(ddi, verbose = TRUE)

# ── Light prep ────────────────────────────────────────────────────────────────
as_int <- function(x) {
  if (inherits(x, "haven_labelled")) as.integer(haven::zap_labels(x)) else as.integer(x)
}

cps <- cps_raw %>%
  mutate(
    pid   = format(CPSIDP, scientific = FALSE, trim = TRUE),
    ym    = as_int(YEAR) * 100L + as_int(MONTH),          # YYYYMM
    date  = make_date(as_int(YEAR), as_int(MONTH), 1L),
    wt    = as.numeric(WTFINL)
  ) %>%
  # keep exactly one row per person-month (rare duplicates can pop up)
  arrange(pid, ym, desc(wt)) %>%
  distinct(pid, ym, .keep_all = TRUE)

# ── Select tidy set of variables for analysis ─────────────────────────────────
# keep + geo vars
keep <- c(
  "pid","YEAR","MONTH","date","ym","MISH","wt",
  "EMPSTAT","LABFORCE","WHYUNEMP","DURUNEMP","ABSENT","WHYABSNT",
  "AHRSWORKT","AHRSWORK1","AHRSWORK2","UHRSWORKT","UHRSWORK1","UHRSWORK2",
  "CLASSWKR","IND","OCC","MULTJOB","NUMJOB",
  "AGE","SEX","RACE","HISPAN","MARST","VETSTAT","EDUC",
  geo_vars                    # ← add this line
)
cps <- cps %>% select(any_of(keep))

# now build divisions (coerce STATEFIP → int to be safe)
cps <- cps %>%
  mutate(
    statefip = as_int(STATEFIP),
    division9 = dplyr::case_when(
      statefip %in% c( 9,23,25,33,44,50)       ~ 1L, # New England
      statefip %in% c(34,36,42)                ~ 2L, # Middle Atlantic
      statefip %in% c(17,18,26,39,55)          ~ 3L, # East North Central
      statefip %in% c(19,20,27,29,31,38,46)    ~ 4L, # West North Central
      statefip %in% c(10,11,12,13,24,37,45,51,54) ~ 5L, # South Atlantic
      statefip %in% c( 1,21,28,47)             ~ 6L, # East South Central
      statefip %in% c( 5,22,40,48)             ~ 7L, # West South Central
      statefip %in% c( 4, 8,16,30,32,35,49,56) ~ 8L, # Mountain
      statefip %in% c( 2, 6,15,41,53)          ~ 9L, # Pacific
      TRUE ~ NA_integer_
    ),
    division9_lbl = factor(
      division9, levels = 1:9,
      labels = c("New England","Middle Atlantic","East North Central",
                 "West North Central","South Atlantic","East South Central",
                 "West South Central","Mountain","Pacific")
    )
  )


# Convenience: build “actual hours last week” and “usual hours” across vintages
cps <- cps %>%
  mutate(
    hours_actual = coalesce(as.numeric(AHRSWORKT),
                            as.numeric(AHRSWORK1) + suppressWarnings(as.numeric(AHRSWORK2))),
    hours_usual  = coalesce(as.numeric(UHRSWORKT), as.numeric(UHRSWORK1))
  )







##############################



# 1) Compact dictionary for variables in your extract
dict <- ipums_var_info(ddi) |> 
  dplyr::select(var_name, var_label, var_desc)

# 2) Value labels for one variable
ipums_val_labels(ddi, "EMPSTAT")

# 3) Value labels for a bunch of variables (tidy)
vars <- c("EMPSTAT","LABFORCE","WHYUNEMP","ABSENT","WHYABSNT",
          "CLASSWKR","IND","OCC","MULTJOB","NUMJOB","EDUC","RACE","HISPAN","MARST","VETSTAT")
val_tbl <- purrr::map_dfr(
  vars,
  \(v) ipums_val_labels(ddi, v) |> dplyr::mutate(var_name = v)
)


# ── Person-level observation counts (max 8 in CPS 4-8-4) ─────────────────────
person_obs <- cps %>%
  group_by(pid) %>%
  summarise(n_obs = n(), .groups = "drop")

obs_counts <- person_obs %>%
  count(n_obs, name = "n_persons") %>%
  arrange(desc(n_obs))

# Print the distribution (8, 7, 6, …)
print(obs_counts)

# OPTIONAL: keep only persons with ≥2 months & their full time series
# panel_two_plus <- cps %>% semi_join(filter(person_obs, n_obs >= 2), by = "pid")

# Save tidy panel and counts if you like
# write_csv(cps, "cps_basic_monthly_panel.csv")
# write_csv(obs_counts, "cps_person_observation_counts.csv")






# months-since-epoch for clean diffs
cps_gaps <- cps %>%
  mutate(mnum = YEAR * 12L + MONTH) %>%
  arrange(pid, mnum) %>%
  group_by(pid) %>%
  mutate(gap_months = mnum - lag(mnum)) %>%
  ungroup()

# Distribution of month gaps between successive interviews
gap_counts <- cps_gaps %>%
  filter(!is.na(gap_months)) %>%
  count(gap_months, name = "n_pairs") %>%
  arrange(gap_months)

print(gap_counts)
# Expect big mass at 1 (within-wave) and 9 (between waves), with small tails elsewhere.

# Person-level total observations
person_obs <- cps %>%
  count(pid, name = "n_obs")

# --- Plot 1: by entry cohort (year of first observation) ----------------------
entry_cohort <- cps %>%
  group_by(pid) %>%
  summarise(entry_year = min(YEAR), .groups = "drop")

by_entry <- person_obs %>%
  inner_join(entry_cohort, by = "pid") %>%
  count(entry_year, n_obs, name = "n_persons") %>%
  complete(entry_year, n_obs = 1:8, fill = list(n_persons = 0L)) %>%
  mutate(n_obs = factor(n_obs, levels = 8:1))   # order legend nicely

ggplot(by_entry, aes(x = entry_year, y = n_persons, fill = n_obs)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(   x = "Entry cohort (year of first interview)", y = "Number of unique people",
       fill = "Total months\nobserved") +
  theme_minimal(base_size = 12) + theme_bw()

# --- Plot 2 (optional): by calendar year the person is seen (duplicates OK) ---
# For each calendar YEAR, count people interviewed that year by their eventual total n_obs.
by_calendar_year <- cps %>%
  distinct(YEAR, pid) %>%
  left_join(person_obs, by = "pid") %>%
  count(YEAR, n_obs, name = "n_persons") %>%
  complete(YEAR, n_obs = 1:8, fill = list(n_persons = 0L)) %>%
  mutate(n_obs = factor(n_obs, levels = 8:1))

ggplot(by_calendar_year, aes(x = YEAR, y = n_persons, fill = n_obs)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "CPS panel lengths among people interviewed in each calendar year",
       x = "Survey year", y = "People (counted if seen in that year)",
       fill = "Total months\nobserved") +
  theme_minimal(base_size = 12) + theme_bw()


head(cps)




####

# ── Build 3-month consecutive triads per person ───────────────────────────────
# Requires: `cps` already prepped (has pid, YEAR, MONTH, your `keep` vars, etc.)

library(dplyr)
library(tidyr)

# Month index for adjacency checks (handles Dec→Jan correctly)
cps <- cps %>% mutate(mnum = YEAR * 12L + MONTH)

# Identify triad starts: months with +1 and +2 consecutive months available
starts <- cps %>%
  arrange(pid, mnum) %>%
  group_by(pid) %>%
  mutate(
    m1 = lead(mnum, 1L),
    m2 = lead(mnum, 2L),
    gap_ok = (m1 - mnum == 1L) & (m2 - mnum == 2L),
    emp_ok = !is.na(EMPSTAT) & !is.na(lead(EMPSTAT, 1L)) & !is.na(lead(EMPSTAT, 2L)),
    mar_ok = !is.na(MARST)   & !is.na(lead(MARST,   1L)) & !is.na(lead(MARST,   2L)),
    triad_start = gap_ok & emp_ok & mar_ok
  ) %>%
  ungroup() %>%
  filter(triad_start) %>%
  transmute(pid, m0 = mnum, triad_uid = row_number())  # one row per triad start

# Helper to suffix all columns (keeps every covariate you selected earlier)
suffix_all <- function(df, sfx) rename_with(df, ~paste0(.x, sfx))

# Base columns to carry forward (all covariates already selected in `cps`)
# Ensure we include both IDs and mnum for joining:
base_cols <- c("pid", "mnum", names(cps)[!names(cps) %in% c("pid","mnum")])

# Month t=1 (start), t=2 (start+1), t=3 (start+2)
m_t1 <- cps %>% select(all_of(base_cols)) %>%
  rename(m0 = mnum) %>% suffix_all("_t1")

m_t2 <- cps %>% select(all_of(base_cols)) %>%
  mutate(mnum = mnum - 1L) %>%  # align current row to triad start (m0)
  rename(m0 = mnum) %>% suffix_all("_t2")

m_t3 <- cps %>% select(all_of(base_cols)) %>%
  mutate(mnum = mnum - 2L) %>%  # align current row to triad start (m0)
  rename(m0 = mnum) %>% suffix_all("_t3")

# ── WIDE triads: one row per triad, all covariates for t1/t2/t3 with suffixes ─
triads_wide <- starts %>%
  left_join(m_t1, by = c("pid" = "pid_t1", "m0" = "m0_t1")) %>%
  left_join(m_t2, by = c("pid" = "pid_t2", "m0" = "m0_t2")) %>%
  left_join(m_t3, by = c("pid" = "pid_t3", "m0" = "m0_t3")) %>%
  mutate(
    # Handy binary indicators (keep originals too)
    employed_t1 = EMPSTAT_t1 %in% c(10L, 12L),
    employed_t2 = EMPSTAT_t2 %in% c(10L, 12L),
    employed_t3 = EMPSTAT_t3 %in% c(10L, 12L),
    married_t1  = MARST_t1 %in% c(1L, 2L),
    married_t2  = MARST_t2 %in% c(1L, 2L),
    married_t3  = MARST_t3 %in% c(1L, 2L)
  )



# Quick sanity checks
nrow(triads_wide)   # number of valid triads


# ── Add time-invariant sex/race and robust year_of_birth ──────────────────────

# 1) Copy sex/race from t1 (and flag any cross-month inconsistencies)
triads_wide <- triads_wide %>%
  mutate(
    sex  = SEX_t1,
    race = RACE_t1,
    sex_consistent  = (SEX_t1 == SEX_t2) & (SEX_t1 == SEX_t3),
    race_consistent = (RACE_t1 == RACE_t2) & (RACE_t1 == RACE_t3)
  )

# 2) Robust year_of_birth = mode of {YEAR-AGE, YEAR-AGE-1} over t1..t3
y1 <- triads_wide$YEAR_t1 - triads_wide$AGE_t1
y2 <- triads_wide$YEAR_t2 - triads_wide$AGE_t2
y3 <- triads_wide$YEAR_t3 - triads_wide$AGE_t3

cand <- cbind(y1, y2, y3, y1 - 1L, y2 - 1L, y3 - 1L)

row_mode_int <- function(v) {
  v <- v[!is.na(v)]
  if (length(v) == 0L) return(NA_integer_)
  ux <- unique(v)
  ux[which.max(tabulate(match(v, ux)))]
}

triads_wide$year_of_birth <- apply(cand, 1L, row_mode_int)

# drop varying ages 
triads_wide <- triads_wide %>% select(-starts_with("AGE_"))

#  sanity check counts
sum(!triads_wide$sex_consistent,    na.rm = TRUE)
sum(!triads_wide$race_consistent,   na.rm = TRUE)
sum(is.na(triads_wide$year_of_birth))

# make vars binary
library(dplyr)

cols <- c("employed_t1","employed_t2","employed_t3",
          "married_t1","married_t2","married_t3")

# 1 = TRUE, 0 = FALSE, NA stays NA
triads_wide <- triads_wide %>%
  mutate(across(all_of(cols), ~ as.integer(.x)))

nrow(triads_wide)
length(unique(triads_wide$pid))

write.csv(triads_wide, location, row.names = FALSE)

