# CPS Data Extraction Skill

Extract Current Population Survey (CPS) Basic Monthly microdata from IPUMS and perform initial cleaning.

## Quick Start

When invoked, this skill helps you:
1. Define and submit IPUMS CPS extracts
2. Download and read CPS microdata
3. Clean and standardize the data
4. Create derived variables
5. Log all operations for reproducibility

**Always create a detailed log file** at `logs/cps-extract-YYYYMMDD-HHMMSS.log` documenting every step.

## Data Privacy and Governance

### ⚠️ Sensitive Data Handling
**CPS microdata contains personal information about real people**: Employment status, demographics, and geographic identifiers.

**Critical Rules:**
- ✅ **Work locally**: Process data on your secure computer system
- ❌ **Never commit to git**: Raw data files must not be in version control
- ❌ **Never upload to cloud**: No Dropbox, Google Drive, etc. without proper authorization
- ⚠️ **Logs contain summary statistics**: Review before sharing with collaborators

### Data Retention
- **Raw extracts**: Delete IPUMS downloads when analysis is complete
- **Cleaned data**: Store securely, delete when project ends
- **Aggregated results**: Safe to share (no individual-level data)
- **Logs**: Contain only counts/distributions, review before sharing

### Safe to Share ✅
These outputs typically contain NO individual-level data:
- Aggregated metrics and summary statistics
- Model objects (without predictions)
- Plots and figures
- This log file (counts and distributions only)

### NEVER Share Outside Secure Environment ❌
- `cps_clean.csv` - Individual person-month records
- `*_raw.csv` - IPUMS extract files
- Any file with individual predictions
- Data files with person IDs

### Best Practices
1. Add sensitive files to `.gitignore` immediately (automated in this skill)
2. Review IPUMS terms: https://cps.ipums.org/cps/terms.shtml
3. When in doubt, only share aggregated results
4. Consult IRB/data governance policies for your institution

## Skill Configuration

Customize these parameters before running:

```r
# CONFIGURATION
CONFIG <- list(
  # Variables to extract (choose from standard sets below)
  variable_sets = c("core", "demo", "labor", "hours", "job", "geo"),

  # Time period filter (NULL = all available)
  year_range = NULL,  # e.g., 2015:2020

  # Output
  output_path = "cps_clean.csv",

  # Logging
  log_level = "INFO",  # "INFO", "WARN", "ERROR"
  verbose = TRUE
)
```

## Step 1: Clarify Requirements

Ask the user:
- **Variables needed**: Which outcomes/covariates? (employment, hours, demographics, etc.)
- **Time period**: All available years or specific range?
- **Geography**: Any specific states/regions?
- **Output location**: Where to save the final dataset?
- **Output name**: Filename for the cleaned data?

## Step 2: Set Up Logging

```r
library(ipumsr)
library(dplyr)
library(tidyr)
library(lubridate)

# Create logs directory
if (!dir.exists("logs")) dir.create("logs", recursive = TRUE)

# Initialize log file
start_time <- Sys.time()
log_file <- sprintf("logs/cps-extract-%s.log", format(start_time, "%Y%m%d-%H%M%S"))
log_write <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- sprintf("[%s] %s: %s\n", timestamp, level, msg)
  cat(log_line, file = log_file, append = TRUE)
  cat(log_line)
}

log_write("=== CPS Data Extraction Started ===")
log_write(sprintf("Skill: cps-extract | Version: 2.0"))
log_write(sprintf("Timestamp: %s", start_time))
log_write(sprintf("User: %s", Sys.getenv("USER")))
log_write(sprintf("Hostname: %s", Sys.info()["nodename"]))
log_write(sprintf("Working directory: %s", getwd()))
log_write(sprintf("R version: %s", R.version.string))

# Log package versions for reproducibility
log_write("Package versions:")
for (pkg in c("ipumsr", "dplyr", "tidyr", "readr", "lubridate")) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    log_write(sprintf("  %s: %s", pkg, packageVersion(pkg)))
  }
}
```

## Step 3: Define Standard Variable Sets

### Core Variables (Always Include)
```r
core_vars <- c(
  # IDs and timing
  "YEAR", "MONTH", "CPSIDP", "SERIAL", "PERNUM", "MISH",
  # Survey weight
  "WTFINL"
)
```

### Standard Variable Sets

**Demographics** (recommended for most extracts):
```r
demo_vars <- c("AGE", "SEX", "RACE", "HISPAN", "MARST", "EDUC", "VETSTAT")
```

**Labor Force** (for employment analysis):
```r
labor_vars <- c("EMPSTAT", "LABFORCE", "WHYUNEMP", "DURUNEMP", "ABSENT", "WHYABSNT")
```

**Hours Worked** (actual and usual across vintages):
```r
hours_vars <- c("AHRSWORKT", "AHRSWORK1", "AHRSWORK2",
                "UHRSWORKT", "UHRSWORK1", "UHRSWORK2")
```

**Job Characteristics**:
```r
job_vars <- c("CLASSWKR", "IND", "OCC", "MULTJOB", "NUMJOB")
```

**Geography**:
```r
geo_vars <- c("REGION", "STATEFIP", "METRO", "METFIPS", "CBSASZ", "COUNTY")
```

## Step 4: Define and Submit Extract

```r
log_write("Discovering available CPS samples...")

# Get all CPS Basic Monthly samples
samps <- get_sample_info("cps")
monthly_basic <- sort(grep("^cps\\d{4}_\\d{2}b$", samps$name, value = TRUE))

log_write(sprintf("Found %d CPS Basic Monthly samples", length(monthly_basic)))
log_write(sprintf("Date range: %s to %s",
          head(monthly_basic, 1), tail(monthly_basic, 1)))

# Optional: Filter to specific years
# Example: monthly_basic <- grep("^cps(2015|2016|2017)", monthly_basic, value = TRUE)

# Combine variable sets based on user needs
vars_needed <- c(
  core_vars,
  demo_vars,
  labor_vars,
  hours_vars,
  job_vars,
  geo_vars
)

log_write(sprintf("Requesting %d variables", length(vars_needed)))
log_write(sprintf("Variables: %s", paste(vars_needed, collapse = ", ")))

# Define extract
log_write("Defining IPUMS extract...")
cps_ext <- define_extract_micro(
  collection  = "cps",
  description = sprintf("CPS Basic Monthly extract - %s", Sys.Date()),
  samples     = monthly_basic,
  variables   = vars_needed
)

# Submit extract
log_write("Submitting extract to IPUMS...")
log_write("NOTE: This may take several minutes to hours depending on extract size")

done <- submit_extract(cps_ext) |> wait_for_extract()

log_write(sprintf("Extract completed: %s", done$number))
log_write(sprintf("Extract status: %s", done$status))
```

## Step 5: Download and Read Data

```r
log_write("Downloading extract...")
ddi_path <- download_extract(done)
log_write(sprintf("DDI file saved to: %s", ddi_path))

log_write("Reading DDI metadata...")
ddi <- read_ipums_ddi(ddi_path)

log_write("Reading microdata (this may take several minutes)...")
cps_raw <- read_ipums_micro(ddi, verbose = TRUE)

log_write(sprintf("Raw data dimensions: %d rows × %d columns",
          nrow(cps_raw), ncol(cps_raw)))
log_write(sprintf("Memory size: %.1f MB",
          object.size(cps_raw) / 1024^2))
```

## Step 6: Initial Cleaning

```r
log_write("Starting data cleaning...")

# Helper function for labeled variables
as_int <- function(x) {
  if (inherits(x, "haven_labelled")) {
    as.integer(haven::zap_labels(x))
  } else {
    as.integer(x)
  }
}

# Clean and create core derived variables
cps <- cps_raw %>%
  mutate(
    # Person ID (consistent across waves)
    pid = format(CPSIDP, scientific = FALSE, trim = TRUE),

    # Time variables
    ym = as_int(YEAR) * 100L + as_int(MONTH),
    date = make_date(as_int(YEAR), as_int(MONTH), 1L),

    # Survey weight
    wt = as.numeric(WTFINL)
  ) %>%
  # Handle duplicates (rare but possible)
  arrange(pid, ym, desc(wt)) %>%
  distinct(pid, ym, .keep_all = TRUE)

log_write(sprintf("After deduplication: %d rows", nrow(cps)))
log_write(sprintf("Unique persons: %d", n_distinct(cps$pid)))
log_write(sprintf("Date range: %s to %s", min(cps$date), max(cps$date)))
```

## Step 7: Create Derived Variables

```r
log_write("Creating derived variables...")

# Combined hours variables (handle vintage differences)
cps <- cps %>%
  mutate(
    hours_actual = coalesce(
      as.numeric(AHRSWORKT),
      as.numeric(AHRSWORK1) + suppressWarnings(as.numeric(AHRSWORK2))
    ),
    hours_usual = coalesce(
      as.numeric(UHRSWORKT),
      as.numeric(UHRSWORK1)
    )
  )

# Binary employment indicator (EMPSTAT: 10 = at work, 12 = has job not at work)
if ("EMPSTAT" %in% names(cps)) {
  cps <- cps %>%
    mutate(employed = as_int(EMPSTAT) %in% c(10L, 12L))

  log_write(sprintf("Employment rate: %.1f%%",
            100 * mean(cps$employed, na.rm = TRUE)))
}

# Binary marriage indicator (MARST: 1 = married spouse present, 2 = married spouse absent)
if ("MARST" %in% names(cps)) {
  cps <- cps %>%
    mutate(married = as_int(MARST) %in% c(1L, 2L))

  log_write(sprintf("Marriage rate: %.1f%%",
            100 * mean(cps$married, na.rm = TRUE)))
}

# Census divisions from state FIPS
if ("STATEFIP" %in% names(cps)) {
  cps <- cps %>%
    mutate(
      statefip = as_int(STATEFIP),
      division9 = case_when(
        statefip %in% c(9,23,25,33,44,50) ~ 1L,  # New England
        statefip %in% c(34,36,42) ~ 2L,          # Middle Atlantic
        statefip %in% c(17,18,26,39,55) ~ 3L,    # East North Central
        statefip %in% c(19,20,27,29,31,38,46) ~ 4L, # West North Central
        statefip %in% c(10,11,12,13,24,37,45,51,54) ~ 5L, # South Atlantic
        statefip %in% c(1,21,28,47) ~ 6L,        # East South Central
        statefip %in% c(5,22,40,48) ~ 7L,        # West South Central
        statefip %in% c(4,8,16,30,32,35,49,56) ~ 8L, # Mountain
        statefip %in% c(2,6,15,41,53) ~ 9L,      # Pacific
        TRUE ~ NA_integer_
      ),
      division9_lbl = factor(
        division9, levels = 1:9,
        labels = c("New England", "Middle Atlantic", "East North Central",
                   "West North Central", "South Atlantic", "East South Central",
                   "West South Central", "Mountain", "Pacific")
      )
    )

  log_write("Created Census division variables")
}

log_write("Derived variables created successfully")
```

## Step 8: Quality Checks

```r
log_write("Running quality checks...")

# Observation counts per person
person_obs <- cps %>%
  count(pid, name = "n_obs") %>%
  count(n_obs, name = "n_persons")

log_write("Person observation counts (CPS 4-8-4 design allows max 8):")
for (i in seq_len(nrow(person_obs))) {
  log_write(sprintf("  %d months: %s persons",
            person_obs$n_obs[i],
            format(person_obs$n_persons[i], big.mark = ",")))
}

# Month gaps between consecutive interviews
cps_gaps <- cps %>%
  mutate(mnum = YEAR * 12L + MONTH) %>%
  arrange(pid, mnum) %>%
  group_by(pid) %>%
  mutate(gap_months = mnum - lag(mnum)) %>%
  ungroup()

gap_summary <- cps_gaps %>%
  filter(!is.na(gap_months)) %>%
  count(gap_months, name = "n_pairs")

log_write("Month gaps between successive interviews:")
for (i in seq_len(nrow(gap_summary))) {
  log_write(sprintf("  Gap of %d months: %s pairs",
            gap_summary$gap_months[i],
            format(gap_summary$n_pairs[i], big.mark = ",")))
}

# Missing data patterns for key variables
key_vars <- c("EMPSTAT", "MARST", "AGE", "SEX", "RACE", "EDUC")
key_vars <- key_vars[key_vars %in% names(cps)]

log_write("Missing data patterns:")
for (v in key_vars) {
  n_missing <- sum(is.na(cps[[v]]))
  pct_missing <- 100 * n_missing / nrow(cps)
  log_write(sprintf("  %s: %d missing (%.2f%%)",
            v, n_missing, pct_missing))
}
```

## Step 9: Save Output

```r
log_write("Saving cleaned dataset...")

# User specifies output path
output_path <- "cps_clean.csv"  # Replace with user's desired path

write_csv(cps, output_path)

log_write(sprintf("Dataset saved to: %s", output_path))
log_write(sprintf("Final dimensions: %d rows × %d columns",
          nrow(cps), ncol(cps)))
log_write(sprintf("File size: %.1f MB",
          file.size(output_path) / 1024^2))

# Calculate checksum for data integrity
checksum <- tools::md5sum(output_path)
log_write(sprintf("MD5 checksum: %s", checksum))
log_write("Use this checksum to verify file integrity in future analyses")

# Save variable metadata
var_info <- ipums_var_info(ddi) %>%
  select(var_name, var_label, var_desc)

meta_path <- sub("\\.csv$", "_metadata.csv", output_path)
write_csv(var_info, meta_path)
log_write(sprintf("Variable metadata saved to: %s", meta_path))

# Save value labels for categorical variables
cat_vars <- c("EMPSTAT", "LABFORCE", "WHYUNEMP", "ABSENT", "WHYABSNT",
              "CLASSWKR", "IND", "OCC", "MULTJOB", "NUMJOB",
              "EDUC", "RACE", "HISPAN", "MARST", "VETSTAT", "SEX")
cat_vars <- cat_vars[cat_vars %in% names(cps_raw)]

if (length(cat_vars) > 0) {
  val_labels <- purrr::map_dfr(
    cat_vars,
    function(v) {
      ipums_val_labels(ddi, v) %>%
        mutate(var_name = v)
    }
  )

  labels_path <- sub("\\.csv$", "_value_labels.csv", output_path)
  write_csv(val_labels, labels_path)
  log_write(sprintf("Value labels saved to: %s", labels_path))
}

# Create/update .gitignore for data privacy
log_write("Updating .gitignore for data privacy...")
gitignore_path <- ".gitignore"
sensitive_patterns <- c(
  basename(output_path),
  "cps_clean.csv",
  "cps_raw.csv",
  "triads_wide.csv",
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
    cat("\n# CPS microdata - NEVER commit (added by cps-extract skill)\n",
        file = gitignore_path, append = TRUE)
    cat(paste(new_entries, collapse = "\n"), "\n", file = gitignore_path, append = TRUE)
    log_write(sprintf("Added %d entries to .gitignore", length(new_entries)))
  } else {
    log_write(".gitignore already contains all sensitive patterns")
  }
} else {
  cat("# CPS microdata - NEVER commit (added by cps-extract skill)\n",
      file = gitignore_path)
  cat(paste(sensitive_patterns, collapse = "\n"), "\n", file = gitignore_path)
  log_write("Created .gitignore with sensitive file patterns")
}

# Save run metadata for learning from past runs
log_write("Saving run metadata for future reference...")
missing_rates <- sapply(cps, function(x) mean(is.na(x)))

run_metadata <- list(
  timestamp = as.character(start_time),
  skill = "cps-extract",
  version = "2.0",
  user = Sys.getenv("USER"),
  configuration = list(
    variables = vars_needed,
    n_samples = length(monthly_basic),
    sample_range = c(head(monthly_basic, 1), tail(monthly_basic, 1))
  ),
  results = list(
    n_rows = nrow(cps),
    n_columns = ncol(cps),
    n_persons = n_distinct(cps$pid),
    date_range = c(as.character(min(cps$date)), as.character(max(cps$date))),
    execution_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    file_size_mb = file.size(output_path) / 1024^2,
    md5_checksum = as.character(checksum)
  ),
  quality_metrics = list(
    missing_rates = as.list(missing_rates[missing_rates > 0]),
    person_obs_counts = as.list(setNames(person_obs$n_persons, person_obs$n_obs))
  )
)

metadata_path <- sub("\\.csv$", "_run_metadata.json", output_path)
jsonlite::write_json(run_metadata, metadata_path, pretty = TRUE, auto_unbox = TRUE)
log_write(sprintf("Run metadata saved to: %s", metadata_path))

# Compare with previous runs if available
past_runs <- list.files(".", pattern = "cps_clean.*_run_metadata\\.json$", full.names = TRUE)
past_runs <- past_runs[past_runs != metadata_path]  # Exclude current run

if (length(past_runs) > 0) {
  log_write("=== COMPARISON WITH PAST RUNS ===")
  latest_past <- jsonlite::read_json(tail(sort(past_runs), 1))

  log_write(sprintf("Previous run: %s", latest_past$timestamp))
  log_write(sprintf("  Rows then: %s | now: %d | change: %+.1f%%",
            format(latest_past$results$n_rows, big.mark = ","),
            nrow(cps),
            100 * (nrow(cps) - latest_past$results$n_rows) / latest_past$results$n_rows))
  log_write(sprintf("  Persons then: %s | now: %s | change: %+.1f%%",
            format(latest_past$results$n_persons, big.mark = ","),
            format(n_distinct(cps$pid), big.mark = ","),
            100 * (n_distinct(cps$pid) - latest_past$results$n_persons) / latest_past$results$n_persons))
  log_write(sprintf("  Time then: %.1fs | now: %.1fs | change: %+.1f%%",
            latest_past$results$execution_seconds,
            run_metadata$results$execution_seconds,
            100 * (run_metadata$results$execution_seconds - latest_past$results$execution_seconds) / latest_past$results$execution_seconds))
}

# Provide recommendations based on this run
log_write("=== RECOMMENDATIONS FOR NEXT TIME ===")

exec_minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
if (exec_minutes > 5) {
  log_write(sprintf("Extract took %.1f minutes. Consider reducing samples or variables for faster iteration", exec_minutes), level = "INFO")
}

high_missing <- names(missing_rates[missing_rates > 0.5])
if (length(high_missing) > 0) {
  log_write(sprintf("Variables with >50%% missing: %s", paste(high_missing, collapse = ", ")), level = "INFO")
  log_write("Consider dropping these variables or investigating data quality", level = "INFO")
}

if (nrow(cps) > 5e6) {
  log_write("Large dataset (>5M rows). Consider filtering by year for faster processing", level = "INFO")
}

log_write("Review this log before next run to improve configuration")

# Privacy reminder
log_write("=== DATA PRIVACY REMINDER ===", level = "WARN")
log_write("This dataset contains individual-level CPS microdata about real people", level = "WARN")
log_write("NEVER upload to cloud services or commit to git repositories", level = "WARN")
log_write("Only share aggregated results and summary statistics", level = "WARN")
log_write("Review IPUMS terms: https://cps.ipums.org/cps/terms.shtml", level = "WARN")

log_write("=== CPS Data Extraction Completed Successfully ===")
log_write(sprintf("Total execution time: %.1f minutes", exec_minutes))
log_write(sprintf("Log file: %s", log_file))
```

## Learning from Past Runs

This skill creates metadata files (`.json`) that enable continuous improvement through systematic comparison of runs.

### Analyzing Past Extractions

```r
# Load and compare all past extraction runs
log_files <- list.files("logs", pattern = "cps-extract.*\\.log", full.names = TRUE)
metadata_files <- list.files(".", pattern = "cps_clean.*_run_metadata\\.json$", full.names = TRUE)

# Parse metadata from past runs
parse_run_metadata <- function(json_path) {
  meta <- jsonlite::read_json(json_path)
  data.frame(
    timestamp = meta$timestamp,
    n_rows = meta$results$n_rows,
    n_persons = meta$results$n_persons,
    exec_seconds = meta$results$execution_seconds,
    file_size_mb = meta$results$file_size_mb,
    n_variables = length(meta$configuration$variables)
  )
}

if (length(metadata_files) > 0) {
  past_runs_df <- do.call(rbind, lapply(metadata_files, parse_run_metadata))
  past_runs_df$timestamp <- as.POSIXct(past_runs_df$timestamp)

  # Analyze trends
  cat("Past extraction runs:\n")
  print(past_runs_df)

  # Find most efficient configuration
  past_runs_df$efficiency <- past_runs_df$n_rows / past_runs_df$exec_seconds
  best_run <- past_runs_df[which.max(past_runs_df$efficiency), ]
  cat(sprintf("\nMost efficient run: %s (%.0f rows/sec)\n",
              best_run$timestamp, best_run$efficiency))
}
```

### Questions to Ask After Each Run

1. **Data quality**: What % of variables have high missing rates? Has this changed?
2. **Performance**: How long did extraction take? Can it be faster?
3. **Coverage**: Did we get the expected number of people? Any gaps?
4. **Consistency**: Do results match previous runs on same time period?

### Improving Future Runs

Based on metadata and logs:
- If extraction is slow: Reduce year range or variable count
- If high missing rates: Consider dropping sparse variables
- If person counts are low: Check sample selection criteria
- If file size is large: Filter unnecessary variables earlier

## Common Issues and Solutions

### Authentication Error
**Issue**: `Error: IPUMS API key not found`
**Solution**:
```r
set_ipums_api_key("YOUR_KEY_HERE", save = TRUE)
```
Get your API key from: https://account.ipums.org/api_keys

### Extract Takes Too Long
**Issue**: Extract submission times out or takes hours
**Solution**: Reduce scope by filtering samples:
```r
# Example: Only 2015-2020
monthly_basic <- grep("^cps(2015|2016|2017|2018|2019|2020)",
                      monthly_basic, value = TRUE)
```

### Memory Issues
**Issue**: `Error: cannot allocate vector of size X GB`
**Solution**:
1. Increase R memory limit (if possible)
2. Extract fewer years at a time
3. Select fewer variables
4. Process in chunks

### Duplicate Records
**Issue**: Same person-month appears multiple times
**Solution**: Already handled in Step 6 with `distinct(pid, ym, .keep_all = TRUE)`

## Output Files

After successful execution, you'll have:
1. **Main dataset**: `[output_path].csv` - Cleaned CPS microdata
2. **Log file**: `logs/cps-extract-[timestamp].log` - Complete execution log
3. **Metadata**: `[output_path]_metadata.csv` - Variable descriptions
4. **Value labels**: `[output_path]_value_labels.csv` - Categorical variable codes

## Next Steps

After running this skill:
- Review the log file for any warnings or issues
- Check the quality statistics (observation counts, missing data)
- If building longitudinal panels, use the **cps-triads** skill next
- For analysis, load the cleaned dataset with `read_csv()`

## Key Points

- **CPS Design**: 4-8-4 rotation (in 4 months, out 8, in 4 more) = max 8 observations per person
- **Survey Weights**: Always use `wt` (from WTFINL) for population estimates
- **Person ID**: Use `pid` for linking across months (derived from CPSIDP)
- **Time Variables**: `ym` (YYYYMM integer) for sorting, `date` for date operations
- **Employment**: EMPSTAT 10 or 12 = employed
- **Marriage**: MARST 1 or 2 = married
- **Logging**: Every step is documented for reproducibility

## Template Variable Configurations

### Minimal (Fast Extract)
```r
vars_needed <- c(core_vars, "EMPSTAT", "AGE", "SEX", "RACE", "MARST", "EDUC")
```

### Labor Focus
```r
vars_needed <- c(core_vars, demo_vars, labor_vars, hours_vars, job_vars)
```

### Full Demographic
```r
vars_needed <- c(core_vars, demo_vars, labor_vars, geo_vars)
```

### Everything
```r
vars_needed <- c(core_vars, demo_vars, labor_vars, hours_vars, job_vars, geo_vars)
```
