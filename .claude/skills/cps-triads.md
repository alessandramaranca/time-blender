# CPS Triads Construction Skill

Build 3-month consecutive triads from CPS longitudinal panel data for temporal prediction tasks.

## Quick Start

When invoked, this skill helps you:
1. Load cleaned CPS panel data
2. Identify 3-month consecutive sequences per person
3. Construct wide-format triads (one row per triad with _t1, _t2, _t3 suffixes)
4. Validate data quality and consistency
5. Log all operations for reproducibility

**Always create a detailed log file** at `logs/cps-triads-YYYYMMDD-HHMMSS.log` documenting every step.

## What Are Triads?

A **triad** is a 3-month consecutive sequence (t1, t2, t3) for a single person where:
- **Consecutive**: Months are exactly 1 month apart (no gaps)
- **Complete**: All key outcomes are non-missing across all 3 months
- **Wide format**: One row per triad with variables suffixed `_t1`, `_t2`, `_t3`

**Use cases**:
- **Prediction**: Use t1, t2 to predict t3 (future)
- **Retrodiction**: Use t2, t3 to predict t1 (past)
- **Fill-in**: Use t1, t3 to predict t2 (middle)

## Data Privacy and Governance

### ⚠️ Sensitive Data Handling
**Triads contain 3-month sequences of personal information about real people**: Employment transitions, demographic characteristics, and behavioral patterns.

**Critical Rules:**
- ✅ **Work locally**: Process data on your secure computer system
- ❌ **Never commit to git**: Triad files must not be in version control
- ❌ **Never upload to cloud**: No Dropbox, Google Drive, etc. without proper authorization
- ⚠️ **Logs contain summary statistics**: Review before sharing with collaborators

### Data Retention
- **Triads dataset**: Contains individual 3-month sequences - store securely, delete when done
- **Predictions**: Individual-level predictions must stay in secure environment
- **Aggregated metrics**: Safe to share (no individual-level data)
- **Logs**: Contain only counts/distributions, review before sharing

### Safe to Share ✅
These outputs typically contain NO individual-level data:
- Aggregated metrics from `/metrics/` directory
- Summary statistics and quality checks
- Model performance comparisons
- Plots of distributions
- This log file (counts and stability rates only)

### NEVER Share Outside Secure Environment ❌
- `triads_wide.csv` - Individual 3-month sequences
- `*_test_predictions.csv` - Individual-level predictions
- Files in `/predictions/` directory - Contains person IDs
- Any file that can identify individuals

### Best Practices
1. Add sensitive files to `.gitignore` immediately (automated in this skill)
2. Only compute and share aggregated results
3. When in doubt, aggregate to group level (e.g., by state, age group)
4. Consult IRB/data governance policies for your institution

## Skill Configuration

Customize these parameters before running:

```r
# CONFIGURATION
CONFIG <- list(
  # Input
  input_path = "cps_clean.csv",

  # Output
  output_path = "triads_wide.csv",

  # Triad requirements
  outcomes_required = c("EMPSTAT", "MARST"),  # Must be non-missing
  require_consecutive_months = TRUE,
  min_triad_length = 3,

  # Quality checks
  require_consistent_demographics = TRUE,
  flag_inconsistencies = TRUE,

  # Logging
  log_level = "INFO",  # "INFO", "WARN", "ERROR"
  verbose = TRUE
)
```

## Step 1: Clarify Requirements

Ask the user:
- **Input file**: Path to cleaned CPS data (from cps-extract skill)
- **Required outcomes**: Which variables must be non-missing? (employment, marital status, hours, etc.)
- **Optional filters**: Age restrictions? Employed only? Other criteria?
- **Output location**: Where to save triads dataset?
- **Time-invariant variables**: Which demographics to include? (sex, race, year_of_birth, etc.)

## Step 2: Set Up Logging

```r
library(dplyr)
library(tidyr)
library(readr)

# Create logs directory
if (!dir.exists("logs")) dir.create("logs", recursive = TRUE)

# Initialize log file
start_time <- Sys.time()
log_file <- sprintf("logs/cps-triads-%s.log", format(start_time, "%Y%m%d-%H%M%S"))
log_write <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- sprintf("[%s] %s: %s\n", timestamp, level, msg)
  cat(log_line, file = log_file, append = TRUE)
  cat(log_line)
}

log_write("=== CPS Triads Construction Started ===")
log_write(sprintf("Skill: cps-triads | Version: 2.0"))
log_write(sprintf("Timestamp: %s", start_time))
log_write(sprintf("User: %s", Sys.getenv("USER")))
log_write(sprintf("Hostname: %s", Sys.info()["nodename"]))
log_write(sprintf("Working directory: %s", getwd()))
log_write(sprintf("R version: %s", R.version.string))

# Log package versions for reproducibility
log_write("Package versions:")
for (pkg in c("dplyr", "tidyr", "readr")) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    log_write(sprintf("  %s: %s", pkg, packageVersion(pkg)))
  }
}
```

## Step 3: Load and Validate Input Data

```r
# Load cleaned CPS data
input_path <- "cps_clean.csv"  # User provides path

log_write(sprintf("Loading CPS data from: %s", input_path))
cps <- read_csv(input_path, show_col_types = FALSE)

log_write(sprintf("Loaded %d rows × %d columns", nrow(cps), ncol(cps)))
log_write(sprintf("Unique persons: %d", n_distinct(cps$pid)))

# Validate required columns
required_cols <- c("pid", "YEAR", "MONTH")
missing_cols <- setdiff(required_cols, names(cps))
if (length(missing_cols) > 0) {
  log_write(sprintf("ERROR: Missing required columns: %s",
            paste(missing_cols, collapse = ", ")), level = "ERROR")
  stop("Missing required columns")
}

# Create month number if not present
if (!"mnum" %in% names(cps)) {
  cps <- cps %>% mutate(mnum = YEAR * 12L + MONTH)
  log_write("Created mnum variable (YEAR*12 + MONTH)")
}

log_write(sprintf("Date range: %d-%02d to %d-%02d",
          min(cps$YEAR), min(cps$MONTH[cps$YEAR == min(cps$YEAR)]),
          max(cps$YEAR), max(cps$MONTH[cps$YEAR == max(cps$YEAR)])))
```

## Step 4: Identify Triad Starts

```r
log_write("Identifying 3-month consecutive sequences...")

# User specifies which outcomes must be non-missing
# Default: employment and marital status
outcomes_check <- c("EMPSTAT", "MARST")  # Customize based on user needs

log_write(sprintf("Requiring non-missing values for: %s",
          paste(outcomes_check, collapse = ", ")))

# Identify valid triad starts
starts <- cps %>%
  arrange(pid, mnum) %>%
  group_by(pid) %>%
  mutate(
    # Check for consecutive months
    m1 = lead(mnum, 1L),
    m2 = lead(mnum, 2L),
    gap_ok = (m1 - mnum == 1L) & (m2 - mnum == 2L)
  )

# Add outcome-specific checks
for (var in outcomes_check) {
  if (var %in% names(cps)) {
    check_col <- paste0(var, "_ok")
    starts <- starts %>%
      mutate(
        !!check_col := !is.na(.data[[var]]) &
                       !is.na(lead(.data[[var]], 1L)) &
                       !is.na(lead(.data[[var]], 2L))
      )
  } else {
    log_write(sprintf("WARNING: Outcome variable '%s' not found in data", var),
              level = "WARN")
  }
}

# Combine all checks
check_cols <- grep("_ok$", names(starts), value = TRUE)
starts <- starts %>%
  mutate(
    triad_start = if (length(check_cols) > 0) {
      rowSums(across(all_of(check_cols))) == length(check_cols)
    } else {
      gap_ok
    }
  ) %>%
  ungroup() %>%
  filter(triad_start) %>%
  transmute(pid, m0 = mnum, triad_uid = row_number())

log_write(sprintf("Found %d valid triad starts", nrow(starts)))
log_write(sprintf("Spanning %d unique persons", n_distinct(starts$pid)))
```

## Step 5: Build Wide-Format Triads

```r
log_write("Constructing wide-format triads...")

# Helper function to suffix all columns
suffix_all <- function(df, sfx) {
  rename_with(df, ~paste0(.x, sfx))
}

# Prepare base columns (everything except pid and mnum)
base_cols <- c("pid", "mnum", names(cps)[!names(cps) %in% c("pid", "mnum")])

# Create datasets for each time point
# t1 = start month
m_t1 <- cps %>%
  select(all_of(base_cols)) %>%
  rename(m0 = mnum) %>%
  suffix_all("_t1")

log_write("Created t1 dataset (start month)")

# t2 = start + 1 month
m_t2 <- cps %>%
  select(all_of(base_cols)) %>%
  mutate(mnum = mnum - 1L) %>%  # Align to triad start
  rename(m0 = mnum) %>%
  suffix_all("_t2")

log_write("Created t2 dataset (middle month)")

# t3 = start + 2 months
m_t3 <- cps %>%
  select(all_of(base_cols)) %>%
  mutate(mnum = mnum - 2L) %>%  # Align to triad start
  rename(m0 = mnum) %>%
  suffix_all("_t3")

log_write("Created t3 dataset (end month)")

# Join all time points
triads_wide <- starts %>%
  left_join(m_t1, by = c("pid" = "pid_t1", "m0" = "m0_t1")) %>%
  left_join(m_t2, by = c("pid" = "pid_t2", "m0" = "m0_t2")) %>%
  left_join(m_t3, by = c("pid" = "pid_t3", "m0" = "m0_t3"))

log_write(sprintf("Joined time points: %d rows × %d columns",
          nrow(triads_wide), ncol(triads_wide)))
```

## Step 6: Create Binary Outcomes

```r
log_write("Creating binary outcome indicators...")

# Employment indicators (EMPSTAT: 10 = at work, 12 = has job not at work)
if ("EMPSTAT_t1" %in% names(triads_wide)) {
  triads_wide <- triads_wide %>%
    mutate(
      employed_t1 = as.integer(EMPSTAT_t1 %in% c(10L, 12L)),
      employed_t2 = as.integer(EMPSTAT_t2 %in% c(10L, 12L)),
      employed_t3 = as.integer(EMPSTAT_t3 %in% c(10L, 12L))
    )

  log_write(sprintf("Employment rates: t1=%.1f%%, t2=%.1f%%, t3=%.1f%%",
            100 * mean(triads_wide$employed_t1, na.rm = TRUE),
            100 * mean(triads_wide$employed_t2, na.rm = TRUE),
            100 * mean(triads_wide$employed_t3, na.rm = TRUE)))
}

# Marriage indicators (MARST: 1 = married spouse present, 2 = married spouse absent)
if ("MARST_t1" %in% names(triads_wide)) {
  triads_wide <- triads_wide %>%
    mutate(
      married_t1 = as.integer(MARST_t1 %in% c(1L, 2L)),
      married_t2 = as.integer(MARST_t2 %in% c(1L, 2L)),
      married_t3 = as.integer(MARST_t3 %in% c(1L, 2L))
    )

  log_write(sprintf("Marriage rates: t1=%.1f%%, t2=%.1f%%, t3=%.1f%%",
            100 * mean(triads_wide$married_t1, na.rm = TRUE),
            100 * mean(triads_wide$married_t2, na.rm = TRUE),
            100 * mean(triads_wide$married_t3, na.rm = TRUE)))
}

log_write("Binary outcomes created")
```

## Step 7: Add Time-Invariant Demographics

```r
log_write("Creating time-invariant demographic variables...")

# Sex and race (should be constant, but validate)
if ("SEX_t1" %in% names(triads_wide)) {
  triads_wide <- triads_wide %>%
    mutate(
      sex = SEX_t1,
      sex_consistent = (SEX_t1 == SEX_t2) & (SEX_t1 == SEX_t3)
    )

  n_inconsistent <- sum(!triads_wide$sex_consistent, na.rm = TRUE)
  if (n_inconsistent > 0) {
    log_write(sprintf("WARNING: %d triads have inconsistent sex across months",
              n_inconsistent), level = "WARN")
  } else {
    log_write("Sex is consistent across all triads")
  }
}

if ("RACE_t1" %in% names(triads_wide)) {
  triads_wide <- triads_wide %>%
    mutate(
      race = RACE_t1,
      race_consistent = (RACE_t1 == RACE_t2) & (RACE_t1 == RACE_t3)
    )

  n_inconsistent <- sum(!triads_wide$race_consistent, na.rm = TRUE)
  if (n_inconsistent > 0) {
    log_write(sprintf("WARNING: %d triads have inconsistent race across months",
              n_inconsistent), level = "WARN")
  } else {
    log_write("Race is consistent across all triads")
  }
}

# Robust year of birth (mode across t1-t3)
if (all(c("YEAR_t1", "AGE_t1", "YEAR_t2", "AGE_t2", "YEAR_t3", "AGE_t3") %in%
        names(triads_wide))) {

  log_write("Computing robust year_of_birth (mode method)...")

  # Candidate birth years: YEAR - AGE and YEAR - AGE - 1 for each month
  y1 <- triads_wide$YEAR_t1 - triads_wide$AGE_t1
  y2 <- triads_wide$YEAR_t2 - triads_wide$AGE_t2
  y3 <- triads_wide$YEAR_t3 - triads_wide$AGE_t3

  cand <- cbind(y1, y2, y3, y1 - 1L, y2 - 1L, y3 - 1L)

  # Function to find mode
  row_mode_int <- function(v) {
    v <- v[!is.na(v)]
    if (length(v) == 0L) return(NA_integer_)
    ux <- unique(v)
    ux[which.max(tabulate(match(v, ux)))]
  }

  triads_wide$year_of_birth <- apply(cand, 1L, row_mode_int)

  n_missing <- sum(is.na(triads_wide$year_of_birth))
  if (n_missing > 0) {
    log_write(sprintf("WARNING: %d triads missing year_of_birth", n_missing),
              level = "WARN")
  }

  log_write(sprintf("Year of birth range: %d to %d",
            min(triads_wide$year_of_birth, na.rm = TRUE),
            max(triads_wide$year_of_birth, na.rm = TRUE)))

  # Drop varying age columns (keep year_of_birth)
  triads_wide <- triads_wide %>% select(-starts_with("AGE_"))
  log_write("Dropped time-varying AGE columns (kept year_of_birth)")
}

log_write("Time-invariant demographics created")
```

## Step 8: Quality Checks

```r
log_write("Running quality checks...")

# Triads per person
triads_per_person <- triads_wide %>%
  count(pid, name = "n_triads") %>%
  count(n_triads, name = "n_persons")

log_write("Triads per person distribution:")
for (i in seq_len(nrow(triads_per_person))) {
  log_write(sprintf("  %d triads: %s persons",
            triads_per_person$n_triads[i],
            format(triads_per_person$n_persons[i], big.mark = ",")))
}

# Outcome stability (what % stay in same state across 3 months?)
if ("employed_t1" %in% names(triads_wide)) {
  stable_emp <- triads_wide %>%
    filter(!is.na(employed_t1) & !is.na(employed_t2) & !is.na(employed_t3)) %>%
    mutate(emp_stable = (employed_t1 == employed_t2) & (employed_t2 == employed_t3))

  log_write(sprintf("Employment stability: %.1f%% remain in same state across 3 months",
            100 * mean(stable_emp$emp_stable, na.rm = TRUE)))
}

if ("married_t1" %in% names(triads_wide)) {
  stable_mar <- triads_wide %>%
    filter(!is.na(married_t1) & !is.na(married_t2) & !is.na(married_t3)) %>%
    mutate(mar_stable = (married_t1 == married_t2) & (married_t2 == married_t3))

  log_write(sprintf("Marital stability: %.1f%% remain in same state across 3 months",
            100 * mean(stable_mar$mar_stable, na.rm = TRUE)))
}

# Continuous outcomes (if present)
if ("hours_actual_t1" %in% names(triads_wide)) {
  log_write("Hours worked (actual) summary statistics:")
  for (t in c("t1", "t2", "t3")) {
    col <- paste0("hours_actual_", t)
    if (col %in% names(triads_wide)) {
      vals <- triads_wide[[col]]
      log_write(sprintf("  %s: mean=%.1f, median=%.1f, sd=%.1f, missing=%d",
                t,
                mean(vals, na.rm = TRUE),
                median(vals, na.rm = TRUE),
                sd(vals, na.rm = TRUE),
                sum(is.na(vals))))
    }
  }
}

# Survey weights
if ("wt_t1" %in% names(triads_wide)) {
  log_write(sprintf("Survey weights (t1): sum=%.0f, mean=%.1f",
            sum(triads_wide$wt_t1, na.rm = TRUE),
            mean(triads_wide$wt_t1, na.rm = TRUE)))
}

log_write("Quality checks completed")
```

## Step 9: Save Output

```r
log_write("Saving triads dataset...")

# User specifies output path
output_path <- "triads_wide.csv"  # Replace with user's desired path

write_csv(triads_wide, output_path)

log_write(sprintf("Triads dataset saved to: %s", output_path))
log_write(sprintf("Final dimensions: %d rows × %d columns",
          nrow(triads_wide), ncol(triads_wide)))
log_write(sprintf("File size: %.1f MB",
          file.size(output_path) / 1024^2))

# Calculate checksum for data integrity
checksum <- tools::md5sum(output_path)
log_write(sprintf("MD5 checksum: %s", checksum))
log_write("Use this checksum to verify file integrity in future analyses")

# Create/update .gitignore for data privacy
log_write("Updating .gitignore for data privacy...")
gitignore_path <- ".gitignore"
sensitive_patterns <- c(
  basename(output_path),
  "triads_wide.csv",
  "cps_clean.csv",
  "*.rds",
  "logs/",
  "predictions/",
  "cache/",
  "*_test_predictions.csv"
)

if (file.exists(gitignore_path)) {
  current_ignore <- readLines(gitignore_path, warn = FALSE)
  new_entries <- setdiff(sensitive_patterns, current_ignore)
  if (length(new_entries) > 0) {
    cat("\n# CPS triads - NEVER commit (added by cps-triads skill)\n",
        file = gitignore_path, append = TRUE)
    cat(paste(new_entries, collapse = "\n"), "\n", file = gitignore_path, append = TRUE)
    log_write(sprintf("Added %d entries to .gitignore", length(new_entries)))
  } else {
    log_write(".gitignore already contains all sensitive patterns")
  }
} else {
  cat("# CPS triads - NEVER commit (added by cps-triads skill)\n",
      file = gitignore_path)
  cat(paste(sensitive_patterns, collapse = "\n"), "\n", file = gitignore_path)
  log_write("Created .gitignore with sensitive file patterns")
}

# Calculate outcome stability rates
stability_metrics <- list()
if ("employed_t1" %in% names(triads_wide)) {
  emp_stable <- triads_wide %>%
    filter(!is.na(employed_t1) & !is.na(employed_t2) & !is.na(employed_t3)) %>%
    mutate(stable = (employed_t1 == employed_t2) & (employed_t2 == employed_t3))
  stability_metrics$employment <- mean(emp_stable$stable)
}
if ("married_t1" %in% names(triads_wide)) {
  mar_stable <- triads_wide %>%
    filter(!is.na(married_t1) & !is.na(married_t2) & !is.na(married_t3)) %>%
    mutate(stable = (married_t1 == married_t2) & (married_t2 == married_t3))
  stability_metrics$marriage <- mean(mar_stable$stable)
}

# Save run metadata for learning from past runs
log_write("Saving run metadata for future reference...")

run_metadata <- list(
  timestamp = as.character(start_time),
  skill = "cps-triads",
  version = "2.0",
  user = Sys.getenv("USER"),
  configuration = list(
    input_file = input_path,
    outcomes_required = outcomes_check
  ),
  results = list(
    n_triads = nrow(triads_wide),
    n_columns = ncol(triads_wide),
    n_persons = n_distinct(triads_wide$pid),
    triads_per_person_mean = mean(table(triads_wide$pid)),
    date_range = sprintf("%d-%02d to %d-%02d",
                         min(triads_wide$YEAR_t1), min(triads_wide$MONTH_t1),
                         max(triads_wide$YEAR_t3), max(triads_wide$MONTH_t3)),
    execution_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    file_size_mb = file.size(output_path) / 1024^2,
    md5_checksum = as.character(checksum)
  ),
  quality_metrics = list(
    stability_rates = stability_metrics,
    triads_per_person_dist = as.list(setNames(
      triads_per_person$n_persons,
      triads_per_person$n_triads
    )),
    sex_consistent = if("sex_consistent" %in% names(triads_wide)) {
      mean(triads_wide$sex_consistent, na.rm = TRUE)
    } else { NA },
    race_consistent = if("race_consistent" %in% names(triads_wide)) {
      mean(triads_wide$race_consistent, na.rm = TRUE)
    } else { NA }
  )
)

metadata_path <- sub("\\.csv$", "_run_metadata.json", output_path)
jsonlite::write_json(run_metadata, metadata_path, pretty = TRUE, auto_unbox = TRUE)
log_write(sprintf("Run metadata saved to: %s", metadata_path))

# Compare with previous runs if available
past_runs <- list.files(".", pattern = "triads_wide.*_run_metadata\\.json$", full.names = TRUE)
past_runs <- past_runs[past_runs != metadata_path]  # Exclude current run

if (length(past_runs) > 0) {
  log_write("=== COMPARISON WITH PAST RUNS ===")
  latest_past <- jsonlite::read_json(tail(sort(past_runs), 1))

  log_write(sprintf("Previous run: %s", latest_past$timestamp))
  log_write(sprintf("  Triads then: %s | now: %d | change: %+.1f%%",
            format(latest_past$results$n_triads, big.mark = ","),
            nrow(triads_wide),
            100 * (nrow(triads_wide) - latest_past$results$n_triads) / latest_past$results$n_triads))
  log_write(sprintf("  Persons then: %s | now: %s | change: %+.1f%%",
            format(latest_past$results$n_persons, big.mark = ","),
            format(n_distinct(triads_wide$pid), big.mark = ","),
            100 * (n_distinct(triads_wide$pid) - latest_past$results$n_persons) / latest_past$results$n_persons))
  log_write(sprintf("  Time then: %.1fs | now: %.1fs | change: %+.1f%%",
            latest_past$results$execution_seconds,
            run_metadata$results$execution_seconds,
            100 * (run_metadata$results$execution_seconds - latest_past$results$execution_seconds) / latest_past$results$execution_seconds))
}

# Provide recommendations based on this run
log_write("=== RECOMMENDATIONS FOR NEXT TIME ===")

triads_per_person_mean <- mean(table(triads_wide$pid))
if (triads_per_person_mean < 2) {
  log_write("Low triads per person (<2). Consider relaxing outcome requirements", level = "INFO")
}

if (exists("sex_consistent")) {
  n_sex_inconsistent <- sum(!triads_wide$sex_consistent, na.rm = TRUE)
  if (n_sex_inconsistent > nrow(triads_wide) * 0.01) {
    log_write(sprintf("%d triads (>1%%) have inconsistent sex. Review data quality",
              n_sex_inconsistent), level = "WARN")
  }
}

if (length(stability_metrics) > 0) {
  for (outcome in names(stability_metrics)) {
    if (stability_metrics[[outcome]] > 0.95) {
      log_write(sprintf("%s is very stable (%.1f%%). Prediction may be challenging",
                outcome, 100 * stability_metrics[[outcome]]), level = "INFO")
    }
  }
}

exec_seconds <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
if (exec_seconds < 1) {
  log_write("Very fast execution. Consider processing larger datasets", level = "INFO")
}

log_write("Review this log before next run to improve configuration")

# Privacy reminder
log_write("=== DATA PRIVACY REMINDER ===", level = "WARN")
log_write("Triads dataset contains individual 3-month sequences about real people", level = "WARN")
log_write("NEVER upload to cloud services or commit to git repositories", level = "WARN")
log_write("Only share aggregated results and summary statistics", level = "WARN")

# Save legacy summary for backward compatibility
summary_stats <- list(
  n_triads = nrow(triads_wide),
  n_persons = n_distinct(triads_wide$pid),
  date_range = sprintf("%d-%02d to %d-%02d",
                       min(triads_wide$YEAR_t1), min(triads_wide$MONTH_t1),
                       max(triads_wide$YEAR_t3), max(triads_wide$MONTH_t3)),
  triads_per_person = triads_per_person,
  creation_time = Sys.time()
)
summary_path <- sub("\\.csv$", "_summary.rds", output_path)
saveRDS(summary_stats, summary_path)

log_write("=== CPS Triads Construction Completed Successfully ===")
log_write(sprintf("Total execution time: %.1f seconds", exec_seconds))
log_write(sprintf("Log file: %s", log_file))
```

## Learning from Past Runs

This skill creates metadata files (`.json`) that enable continuous improvement through systematic comparison of runs.

### Analyzing Past Triad Constructions

```r
# Load and compare all past triad construction runs
log_files <- list.files("logs", pattern = "cps-triads.*\\.log", full.names = TRUE)
metadata_files <- list.files(".", pattern = "triads_wide.*_run_metadata\\.json$", full.names = TRUE)

# Parse metadata from past runs
parse_triad_metadata <- function(json_path) {
  meta <- jsonlite::read_json(json_path)
  data.frame(
    timestamp = meta$timestamp,
    n_triads = meta$results$n_triads,
    n_persons = meta$results$n_persons,
    triads_per_person = meta$results$triads_per_person_mean,
    exec_seconds = meta$results$execution_seconds,
    employment_stability = ifelse(
      !is.null(meta$quality_metrics$stability_rates$employment),
      meta$quality_metrics$stability_rates$employment,
      NA
    )
  )
}

if (length(metadata_files) > 0) {
  past_runs_df <- do.call(rbind, lapply(metadata_files, parse_triad_metadata))
  past_runs_df$timestamp <- as.POSIXct(past_runs_df$timestamp)

  # Analyze trends
  cat("Past triad construction runs:\n")
  print(past_runs_df)

  # Find run with most triads per person (best coverage)
  best_run <- past_runs_df[which.max(past_runs_df$triads_per_person), ]
  cat(sprintf("\nBest coverage: %s (%.1f triads/person)\n",
              best_run$timestamp, best_run$triads_per_person))
}
```

### Questions to Ask After Each Run

1. **Coverage**: How many triads per person? Can we improve this?
2. **Stability**: Are outcomes too stable (>95%)? Prediction will be harder
3. **Consistency**: Are demographics consistent across months?
4. **Performance**: How long did construction take? Is it reasonable?
5. **Quality**: Any warnings about data issues?

### Improving Future Runs

Based on metadata and logs:
- If low triads/person: Relax outcome requirements or check data quality
- If high stability: Expected, but note predictions may regress to mean
- If demographic inconsistencies: Investigate data cleaning at source
- If slow execution: Consider processing in chunks or filtering

## Common Issues and Solutions

### Issue: Too Few Triads
**Problem**: Only a small fraction of person-months form valid triads
**Diagnosis**: Check log for which outcome checks are failing
**Solutions**:
- Relax missingness requirements (allow more NAs)
- Use longer panels (some people have more consecutive months)
- Check if outcomes are genuinely sparse in your sample

### Issue: Inconsistent Demographics
**Problem**: Sex or race changes across months for same person
**Diagnosis**: Look for "WARNING: inconsistent" messages in log
**Solutions**:
- Data quality issue - may need to clean at source
- Use mode/majority across months
- Flag and exclude inconsistent triads if needed

### Issue: Memory Problems
**Problem**: Wide format creates huge dataset
**Solutions**:
- Select only needed columns before building triads
- Process in chunks by year or person ID ranges
- Save intermediate results and combine later

### Issue: No Time-Invariant Variables
**Problem**: Missing year_of_birth, sex, race in output
**Diagnosis**: Input data may not have AGE, SEX, RACE variables
**Solutions**:
- Re-run cps-extract skill with demo_vars included
- Manually add these variables if available elsewhere

## Understanding the Output

### Column Naming Convention
- **`_t1`**: First month (start of triad)
- **`_t2`**: Second month (middle)
- **`_t3`**: Third month (end)

### Key Columns
- **`triad_uid`**: Unique identifier for each triad
- **`pid`**: Person identifier (links triads to same person)
- **`m0`**: Month number of triad start (YEAR*12 + MONTH)
- **`year_of_birth`**: Time-invariant (robust estimate)
- **`sex`, `race`**: Time-invariant (from t1, validated)
- **`employed_t1/t2/t3`**: Binary employment (1=yes, 0=no)
- **`married_t1/t2/t3`**: Binary marriage (1=yes, 0=no)
- **`wt_t1`**: Survey weight (use this for weighted analyses)

### Temporal Tasks You Can Do

1. **Prediction** (forecast future):
   - Features: All `_t1` and `_t2` variables
   - Target: `_t3` variables
   - Research question: Can we predict t3 from past?

2. **Retrodiction** (infer past):
   - Features: All `_t2` and `_t3` variables
   - Target: `_t1` variables
   - Research question: Can we infer t1 from future?

3. **Fill-in** (interpolate middle):
   - Features: All `_t1` and `_t3` variables
   - Target: `_t2` variables
   - Research question: Can we interpolate t2 from neighbors?

## Next Steps

After running this skill:
1. Review the log file for quality issues
2. Check outcome stability rates (high stability = harder prediction)
3. Explore demographic distributions
4. Split into train/test sets for modeling
5. Use **run-experiment-pipeline.R** as template for ML tasks

## Key Points

- **Triads = 3 consecutive months** with complete data
- **Wide format** = one row per triad, variables suffixed by time
- **Validation** = check demographic consistency, outcome stability
- **Survey weights** = use wt_t1 for population-representative inference
- **Multiple triads per person** = expected (max 6 triads from 8-month panel)
- **Logging** = every decision and statistic documented

## Example Configuration

### Minimal (Employment Only)
```r
outcomes_check <- c("EMPSTAT")
```

### Standard (Employment + Marriage)
```r
outcomes_check <- c("EMPSTAT", "MARST")
```

### Extensive (Multiple Outcomes)
```r
outcomes_check <- c("EMPSTAT", "MARST", "hours_actual")
```

### No Requirements (Maximum Triads)
```r
outcomes_check <- character(0)  # Only require consecutive months
```
