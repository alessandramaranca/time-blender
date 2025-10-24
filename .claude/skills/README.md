# CPS Data Skills for Claude Code

This directory contains specialized skills for working with Current Population Survey (CPS) data from IPUMS.

## Available Skills

### 1. `cps-extract` - Basic CPS Data Extraction

Extract CPS Basic Monthly microdata from IPUMS and perform initial cleaning.

**Use when you need to:**
- Download CPS data from IPUMS
- Clean and standardize CPS microdata
- Create derived variables (employment, hours, demographics)
- Generate reproducible data pipelines

**Outputs:**
- Cleaned CPS dataset (person-month format)
- Detailed execution log
- Variable metadata
- Value labels for categorical variables

**Typical workflow:**
```r
# Invoke the skill in Claude Code
# Follow prompts to specify:
# - Variables needed (employment, demographics, geography, etc.)
# - Time period (all years or specific range)
# - Output location
```

### 2. `cps-triads` - Longitudinal Triad Construction

Build 3-month consecutive triads from CPS panel data for temporal prediction tasks.

**Use when you need to:**
- Create longitudinal sequences from CPS
- Build datasets for prediction/retrodiction/fill-in tasks
- Validate temporal consistency
- Prepare data for time-series ML models

**Outputs:**
- Wide-format triads dataset (one row per 3-month sequence)
- Detailed execution log
- Quality statistics and validation results
- Summary of triads per person

**Typical workflow:**
```r
# Invoke the skill in Claude Code after running cps-extract
# Follow prompts to specify:
# - Input file (from cps-extract)
# - Required outcomes (employment, marriage, hours, etc.)
# - Output location
```

## How to Use These Skills

### In Claude Code

1. **Invoke a skill** by asking Claude:
   ```
   Use the cps-extract skill to download CPS data
   ```

2. **Chain skills together**:
   ```
   First use cps-extract to get the data, then use cps-triads to build triads
   ```

3. **Customize parameters** during invocation:
   ```
   Use cps-extract with only labor force variables for 2015-2020
   ```

### Typical Pipeline

```
┌─────────────────┐
│  cps-extract    │  Download & clean CPS microdata
│                 │  → cps_clean.csv
└────────┬────────┘
         │
         v
┌─────────────────┐
│  cps-triads     │  Build 3-month consecutive triads
│                 │  → triads_wide.csv
└────────┬────────┘
         │
         v
┌─────────────────┐
│  Your Analysis  │  ML models, predictions, evaluation
│                 │  (e.g., run-experiment-pipeline.R)
└─────────────────┘
```

## Logging

Both skills create detailed log files in `logs/` directory:
- `logs/cps-extract-YYYYMMDD-HHMMSS.log`
- `logs/cps-triads-YYYYMMDD-HHMMSS.log`

Logs include:
- Timestamp for every operation
- Data dimensions and summary statistics
- Quality checks and validation results
- Warnings and errors
- Execution time

**Log files are human-readable and Claude-parseable** for reproducibility and debugging.

## Key Concepts

### CPS Design
- **4-8-4 rotation**: Respondents interviewed 4 months, out 8, in 4 more
- **Maximum 8 observations** per person
- **Survey weights** (WTFINL) for population estimates
- **Person ID** (CPSIDP) links across months

### Triads Structure
- **3 consecutive months** (t1, t2, t3) with no gaps
- **Wide format**: Variables suffixed with `_t1`, `_t2`, `_t3`
- **Multiple triads per person**: Expected (e.g., 6 from 8-month panel)
- **Complete data**: All required outcomes non-missing

### Temporal Tasks
1. **Prediction**: Use t1, t2 → predict t3 (future)
2. **Retrodiction**: Use t2, t3 → predict t1 (past)
3. **Fill-in**: Use t1, t3 → predict t2 (middle)

## Standard Variable Sets

### Core (Always Include)
- IDs: `YEAR`, `MONTH`, `CPSIDP`, `SERIAL`, `PERNUM`, `MISH`
- Weight: `WTFINL`

### Demographics
- `AGE`, `SEX`, `RACE`, `HISPAN`, `MARST`, `EDUC`, `VETSTAT`

### Labor Force
- `EMPSTAT`, `LABFORCE`, `WHYUNEMP`, `DURUNEMP`, `ABSENT`, `WHYABSNT`

### Hours Worked
- `AHRSWORKT`, `AHRSWORK1`, `AHRSWORK2`
- `UHRSWORKT`, `UHRSWORK1`, `UHRSWORK2`

### Job Characteristics
- `CLASSWKR`, `IND`, `OCC`, `MULTJOB`, `NUMJOB`

### Geography
- `REGION`, `STATEFIP`, `METRO`, `METFIPS`, `CBSASZ`, `COUNTY`

## Common Workflows

### Workflow 1: Basic Employment Analysis
```r
# 1. Extract minimal variables
cps-extract: core + demographics + EMPSTAT
# → cps_clean.csv

# 2. Build triads requiring employment data
cps-triads: outcomes_check = c("EMPSTAT")
# → triads_wide.csv

# 3. Analyze employment transitions
```

### Workflow 2: Full Labor Force Study
```r
# 1. Extract comprehensive labor variables
cps-extract: core + demographics + labor + hours + job
# → cps_clean.csv

# 2. Build triads with multiple outcomes
cps-triads: outcomes_check = c("EMPSTAT", "MARST", "hours_actual")
# → triads_wide.csv

# 3. Run ML experiments (see run-experiment-pipeline.R)
```

### Workflow 3: Geographic Analysis
```r
# 1. Extract with geography
cps-extract: core + demographics + labor + geography
# → cps_clean.csv

# 2. Build triads
cps-triads: standard configuration
# → triads_wide.csv

# 3. Analyze by state/region/division
```

## Prerequisites

### R Packages Required
```r
install.packages(c(
  "ipumsr",      # IPUMS data access
  "dplyr",       # Data manipulation
  "tidyr",       # Data tidying
  "readr",       # Read/write CSV
  "lubridate",   # Date handling
  "haven"        # Labeled variables
))
```

### IPUMS Account
1. Create account at https://cps.ipums.org
2. Get API key from https://account.ipums.org/api_keys
3. Set in R:
```r
ipumsr::set_ipums_api_key("YOUR_KEY_HERE", save = TRUE)
```

## Troubleshooting

### Issue: Skills not showing up
**Solution**: Skills must be in `.claude/skills/` directory with `.md` extension

### Issue: IPUMS authentication fails
**Solution**: Set API key with `ipumsr::set_ipums_api_key()`

### Issue: Extract times out
**Solution**: Reduce scope (fewer years/variables) in cps-extract

### Issue: Too few triads created
**Solution**: Relax outcome requirements in cps-triads

### Issue: Memory problems
**Solution**: Process data in chunks or reduce variable count

## File Naming Conventions

### Input/Output
- `cps_clean.csv` - Standard output from cps-extract
- `triads_wide.csv` - Standard output from cps-triads
- `cps_clean_metadata.csv` - Variable descriptions
- `cps_clean_value_labels.csv` - Categorical codes

### Logs
- `logs/cps-extract-YYYYMMDD-HHMMSS.log`
- `logs/cps-triads-YYYYMMDD-HHMMSS.log`

### Custom Naming
Both skills accept custom output paths - just specify when prompted.

## Related Files in Repository

- [extract-data-from-CPS-and-process.R](../../extract-data-from-CPS-and-process.R) - Reference implementation
- [run-experiment-pipeline.R](../../run-experiment-pipeline.R) - ML pipeline using triads
- [evaluation-and-plots.R](../../evaluation-and-plots.R) - Results analysis

## Best Practices

1. **Always review logs** after running skills
2. **Check quality statistics** (observation counts, missing data)
3. **Validate consistency** (demographics, outcome stability)
4. **Use survey weights** (wt or WTFINL) for population estimates
5. **Document your workflow** (which skill configurations used)
6. **Version your outputs** (include timestamps or version numbers)

## Customization

Skills are self-contained but flexible:
- **Variable selection**: Choose from standard sets or specify custom
- **Time periods**: All years or filtered ranges
- **Outcome requirements**: Strict (all outcomes) or relaxed (any valid sequence)
- **Quality checks**: Built-in validation with customizable thresholds

## Support

- **IPUMS CPS Documentation**: https://cps.ipums.org/cps/
- **ipumsr Package**: https://tech.popdata.org/ipumsr/
- **CPS User Notes**: https://cps.ipums.org/cps/user_notes.shtml

## Version History

- **2025-01-24**: Initial skill creation with logging support
  - cps-extract: Basic extraction + cleaning + derived variables
  - cps-triads: Triad construction + validation + quality checks
