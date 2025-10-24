# Time Blender Project - Claude Code Documentation

## Project Overview

**Time Blender** is a research project investigating **temporal predictability asymmetries** using Current Population Survey (CPS) data from IPUMS. The central research question: Is it easier to predict the future (prediction), infer the past (retrodiction), or fill in missing middle values (interpolation) for labor market and demographic outcomes?

## Guiding Principles

When working in this project, the following principles should guide your decisions:

1. **Scientific** - All work should emphasize correctness, computational reproducibility, and detailed logging. Each experiment should be logged such that it can be audited by a researcher or Claude.

2. **Modular** - This project will evolve over time and should be designed so that individual components can be added or changed with minimal impact on other components.

3. **Practical** - Work in this project is designed to do science, not win a programming contest. Don't over-engineer or do premature optimization. We don't need hundreds of lines of code to save 5 seconds.

4. **Privacy respecting** - Much of the data in this project is about people. All data should be treated with care, and some should never leave the user's computer system. Tasks should be designed with clear data governance.

5. **Self improving** - Always look for ways to learn from earlier experiments to design new experiments, improve skills, and improve analysis. The more work we do, the easier things should be because we have more designs, results, and logs from which to learn.

## Research Question

Given a 3-month sequence (t1, t2, t3) for a person, which task is most accurate?
1. **Prediction**: Use t1, t2 → predict t3 (future)
2. **Retrodiction**: Use t2, t3 → predict t1 (past)
3. **Fill-in**: Use t1, t3 → predict t2 (middle)

## Key Outcomes Analyzed

### Binary Outcomes
- **Employment status** (employed vs not employed)
- **Marital status** (married vs not married)

### Continuous Outcomes
- **Hours worked per week** (actual hours, 0-100)
- **Number of jobs** (0-4, treated as numeric)

## Project Structure

### Core Pipeline Scripts (in order)

1. **[extract-data-from-CPS-and-process.R](../extract-data-from-CPS-and-process.R)** (340 lines)
   - Downloads CPS Basic Monthly microdata via IPUMS API
   - Extracts longitudinal person panels (4-8-4 rotation scheme)
   - Constructs 3-month consecutive triads (t1, t2, t3)
   - Output: `triads_wide.csv`

2. **[run-experiment-pipeline.R](../run-experiment-pipeline.R)** (753 lines)
   - Main ML experiment orchestration
   - 36 model configurations: 3 tasks × 4 outcomes × 3 models
   - 5-fold cross-validation with survey weights
   - Parallel execution with comprehensive error handling
   - Outputs: models, metrics, predictions, diagnostics

3. **[evaluation-and-plots.R](../evaluation-and-plots.R)** (351 lines)
   - Analyzes results across tasks and models
   - Creates calibration plots and performance comparisons
   - Examines heterogeneous effects by demographics
   - Generates publication-quality figures

### Data Flow

```
IPUMS CPS Extract
    ↓
extract-data-from-CPS-and-process.R
    ↓
triads_wide.csv (1 row = 3-month sequence)
    ↓
run-experiment-pipeline.R
    ↓
├─ /models/ (fitted workflows)
├─ /metrics/ (CV & test performance)
├─ /predictions/ (test set predictions)
├─ /deciles/ (prediction distributions)
└─ /diagnostics/ (run logs & audits)
    ↓
evaluation-and-plots.R
    ↓
Figures & analysis tables
```

## Experiment Configuration

### Model Types
- **lin_add**: Linear regression (main effects only)
- **lin_int**: Linear regression with interactions (quadratic terms)
- **xgb**: XGBoost with hyperparameter tuning (3-point grid search)

### Cross-Validation Setup
- Train/test split: 70/30
- CV folds: 5-fold grouped by person ID
- Metrics: Weighted using CPS survey weights

### Naive Baselines
- **Prediction**: Carry forward (use t2 value)
- **Retrodiction**: Carry back (use t2 value)
- **Fill-in**: Neighbor average (mean of t1 and t3)

## CPS Data Details

### Survey Design
- **4-8-4 rotation**: In sample 4 months, out 8, in 4 more
- **Maximum observations**: 8 per person
- **Month gaps**: Mostly 1 (consecutive) or 9 (between waves)

### Key Variables
- **IDs**: CPSIDP (person), SERIAL (household), PERNUM
- **Timing**: YEAR, MONTH, MISH (month in sample)
- **Weight**: WTFINL (survey weight for population inference)
- **Labor**: EMPSTAT, LABFORCE, hours worked, job characteristics
- **Demographics**: AGE, SEX, RACE, HISPAN, MARST, EDUC, VETSTAT
- **Geography**: REGION, STATEFIP, METRO, COUNTY

### Derived Variables in Triads
- `employed_t1/t2/t3`: Binary (1=employed, 0=not)
- `married_t1/t2/t3`: Binary (1=married, 0=not)
- `hours_actual_t1/t2/t3`: Continuous (0-100)
- `numberjobs_t1/t2/t3`: Ordinal as numeric (0-4)
- `year_of_birth`: Time-invariant (mode across measurements)
- `sex`, `race`: Time-invariant (validated for consistency)
- `division9`: Census division from STATEFIP

## Claude Code Skills

This project includes two specialized skills for CPS data extraction:

### 1. **cps-extract** ([.claude/skills/cps-extract.md](.claude/skills/cps-extract.md))
Automates downloading and cleaning CPS data from IPUMS.

**Use when:**
- Starting a new extract
- Updating data with recent CPS releases
- Building variations with different variables/years

**Outputs:**
- `cps_clean.csv` - Person-month panel
- `logs/cps-extract-*.log` - Detailed execution log
- `*_metadata.csv` - Variable documentation
- `*_value_labels.csv` - Categorical codes

### 2. **cps-triads** ([.claude/skills/cps-triads.md](.claude/skills/cps-triads.md))
Constructs 3-month consecutive triads from CPS panel data.

**Use when:**
- Building temporal prediction datasets
- Creating wide-format longitudinal sequences
- Validating temporal consistency

**Outputs:**
- `triads_wide.csv` - Wide format triads
- `logs/cps-triads-*.log` - Detailed execution log
- `*_summary.rds` - Quality statistics

**See [.claude/skills/README.md](.claude/skills/README.md) for detailed skill documentation.**

## Key Technical Patterns

### Triad Construction Logic
```r
# Identify 3-month consecutive sequences
starts <- cps %>%
  arrange(pid, mnum) %>%
  group_by(pid) %>%
  mutate(
    m1 = lead(mnum, 1L),
    m2 = lead(mnum, 2L),
    gap_ok = (m1 - mnum == 1L) & (m2 - mnum == 2L),
    emp_ok = !is.na(EMPSTAT) & !is.na(lead(EMPSTAT, 1L)) & !is.na(lead(EMPSTAT, 2L)),
    triad_start = gap_ok & emp_ok
  )
```

### Weighted Metrics
```r
# All evaluation uses survey weights for population-representative results
weighted_r2 <- function(truth, estimate, weights) {
  yardstick::rsq_trad_vec(truth, estimate, case_weights = weights)
}
```

### Parallel Execution with Fallback
```r
# Safely runs models in parallel, falls back to sequential if parallel fails
if (PARALLEL) {
  plan(multisession, workers = N_WORKERS)
} else {
  plan(sequential)
}
results <- future_map(tasks, ~fit_model(.x), .options = furrr_options(seed = TRUE))
```

### Task Specification Table
```r
task_specs <- tribble(
  ~task,    ~target_month, ~predictor_months, ~naive_fn,
  "pred",   "t3",          c("t1", "t2"),     naive_forward,
  "retro",  "t1",          c("t2", "t3"),     naive_back,
  "fill",   "t2",          c("t1", "t3"),     naive_avg
)
```

## Directory Structure

```
time-blender/
├── README.md
├── extract-data-from-CPS-and-process.R
├── run-experiment-pipeline.R
├── evaluation-and-plots.R
├── triads_wide.csv                      # Main dataset (gitignored if large)
├── .claude/
│   ├── claude.md                        # This file
│   └── skills/
│       ├── README.md                    # Skills overview
│       ├── cps-extract.md              # Data extraction skill
│       └── cps-triads.md               # Triad construction skill
├── logs/                                # Execution logs (created by skills)
├── cache/                               # Train/test splits
├── models/                              # Fitted workflow objects
├── metrics/                             # CV and test metrics
├── predictions/                         # Test set predictions
├── deciles/                             # Prediction distributions
└── diagnostics/                         # Run logs and audits
```

## How to Work with This Project

### Initial Setup

1. **Install R packages**:
```r
install.packages(c(
  "ipumsr", "tidymodels", "xgboost", "tidyverse",
  "furrr", "future", "logger", "fs", "glue", "jsonlite"
))
```

2. **Set IPUMS API key**:
```r
ipumsr::set_ipums_api_key("YOUR_KEY_HERE", save = TRUE)
```

3. **Extract data** (or use existing `triads_wide.csv`):
```r
# Option A: Run extraction script directly
source("extract-data-from-CPS-and-process.R")

# Option B: Use Claude skill
# Ask Claude: "Use the cps-extract skill to download CPS data"
```

### Running Experiments

```r
# Run full ML pipeline (36 models)
source("run-experiment-pipeline.R")

# Or customize configuration:
SEED <- 8544
TEST_PROP <- 0.30
FOLDS_V <- 5
XGB_GRID_N <- 3
PARALLEL <- TRUE
```

### Analyzing Results

```r
# Generate plots and summaries
source("evaluation-and-plots.R")
```

### Building Variations

Ask Claude to help with:
- "Extract CPS data for 2015-2020 only"
- "Build triads requiring both employment and hours data"
- "Run experiments with only XGBoost models"
- "Analyze results by age groups"
- "Create calibration plots for marriage outcomes"

## Common Tasks

### Task: Update with Recent CPS Data
```
Use the cps-extract skill to download the latest CPS data,
then rebuild triads with cps-triads
```

### Task: Add New Variables
```
Modify extract-data-from-CPS-and-process.R to include [NEW_VARS],
then re-run the extraction and triad construction
```

### Task: Test Different Models
```
Edit run-experiment-pipeline.R to add [NEW_MODEL_TYPE],
update the model grid, and re-run experiments
```

### Task: Analyze Subgroups
```
Load predictions from predictions/ directory and create
stratified analyses by [DEMOGRAPHIC_VAR]
```

### Task: Debug Failed Model Run
```
Check diagnostics/run_diagnostics.csv for error messages,
review logs in logger output, and re-run specific configurations
```

## Important Notes

### Survey Weights
- **Always use** `wt` or `WTFINL` for population-representative estimates
- All metrics in pipeline are weighted
- Naive baselines also weighted

### Missing Data
- Triads require non-missing outcomes across all 3 months
- IPUMS uses specific codes for missing (99, 999, etc.)
- The `ipumsr` package handles label conversion automatically

### Employment Definition
- **Employed**: EMPSTAT = 10 (at work) or 12 (has job, not at work)
- **Not employed**: Unemployed or not in labor force

### Marriage Definition
- **Married**: MARST = 1 (spouse present) or 2 (spouse absent)
- **Not married**: Never married, widowed, divorced, separated

### Reproducibility
- Set `SEED = 8544` for consistent results
- All randomization uses this seed
- Cross-validation folds are grouped by person ID to prevent data leakage

### Computational Considerations
- Full pipeline (36 models) takes ~30-60 minutes on modern hardware
- XGBoost tuning is most time-intensive
- Parallel execution recommended (`PARALLEL = TRUE`)
- Can run subsets by filtering task/outcome/model combinations

## Expected Results

### Typical Performance Patterns
- **High stability outcomes** (e.g., employment) → harder to predict changes
- **Naive baselines** often competitive for stable outcomes
- **XGBoost** typically outperforms linear models
- **Prediction vs retrodiction** differences are research focus

### Output Files Interpretation
- `metrics/*_cv_metrics.csv`: Cross-validation performance
- `metrics/*_test_metrics.csv`: Final test set performance
- `predictions/*_test_predictions.csv`: Individual predictions + demographics
- `deciles/*_test_deciles.csv`: Prediction distributions by decile

## Troubleshooting

### Issue: IPUMS extract fails
- Check API key with `ipumsr::get_ipums_api_key()`
- Verify internet connection
- Check IPUMS system status

### Issue: Memory errors
- Reduce number of samples/variables in extract
- Process experiments in batches
- Increase R memory limit

### Issue: Parallel execution fails
- Set `PARALLEL = FALSE` to run sequentially
- Reduce `N_WORKERS`
- Check for port conflicts

### Issue: Model fitting errors
- Check `diagnostics/run_diagnostics.csv` for specific errors
- Verify data has sufficient variation
- Check for collinearity issues

### Issue: No triads created
- Verify input data has consecutive months
- Check outcome missingness patterns
- Relax requirements in triad construction

## Git Repository

- **Current branch**: main
- **Remote**: (check with `git remote -v`)
- **Recent commits**: Pipeline implementation, grouped analyses, plotting

### .gitignore Recommendations
```
# Large data files
triads_wide.csv
cps_clean.csv
*.rds

# Model outputs
models/
predictions/
cache/

# Logs
logs/

# R
.RData
.Rhistory
```

## References

- **IPUMS CPS**: https://cps.ipums.org/
- **CPS Documentation**: https://cps.ipums.org/cps/user_notes.shtml
- **ipumsr Package**: https://tech.popdata.org/ipumsr/
- **tidymodels**: https://www.tidymodels.org/

## Research Context

This project explores **temporal asymmetries in predictability** - a fundamental question in time-series analysis and causal inference. Understanding whether future states are more or less predictable than past states (given the same information) has implications for:
- Forecasting labor market outcomes
- Understanding economic mobility
- Survey design and imputation strategies
- Causal inference with longitudinal data

## Contact and Collaboration

When working with Claude on this project:
- Reference file paths relative to project root
- Use skills for standard CPS operations
- Check logs for troubleshooting
- Cite specific line numbers when discussing code (e.g., `run-experiment-pipeline.R:245`)

## Version History

- **Initial commit (2025-01-24)**: Project setup
- **Pipeline implementation**: Full ML experiment framework
- **Grouped analyses**: Demographic stratification
- **Skills created**: Automated CPS extraction and triad construction
