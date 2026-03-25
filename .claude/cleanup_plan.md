# Plan: Full Codebase Cleanup — UpdatingMisinformation-Analysis

## Context

The codebase produces correct results but has accumulated friction: stargazer timestamps cause spurious git diffs, variable names are cryptic/inconsistent, two monolithic files hold everything, O(n²) loops slow down data prep, and plot styling is inconsistent. This cleanup restructures the repo into a proper modular pipeline while fixing all of these issues.

**User preferences:** Medium-length variable names (e.g., `obs_log_post_ratio`), include performance fixes, unify plot themes, descriptive section names, rename intermediate dataframes, add formula documentation.

---

## Phase 0: Baseline Snapshot

Before any changes:
1. Commit current state (clean working tree)
2. Run full pipeline once to ensure current output is up to date
3. Copy `output/` to `output_baseline/` (gitignored) for diffing later
4. Copy `data/processed/` to `data/processed_baseline/` (gitignored) for data verification

Add to `.gitignore`:
```
output_baseline/
data/processed_baseline/
```

---

## Phase 1: Stargazer Timestamps (quick win)

**Problem:** Line 3 of every `.tex` file: `% Date and time: Wed, Mar 25, 2026 - 6:55:53 PM`. Causes git diff on every run.

**Fix:** Add wrapper in `Figures and Tables.R` (~line 113):
```r
write_stargazer <- function(..., out) {
  lines <- capture.output(stargazer::stargazer(...))
  lines <- lines[!grepl("^% Date and time:|^<!-- Date and time:", lines)]
  writeLines(lines, out)
}
```

Replace all 23 `stargazer(...)` calls that have `out =` → `write_stargazer(...)`, moving `out` from inside stargazer to the wrapper parameter.

**Files:** `code/Figures and Tables.R` (23 replacements at lines 231, 264, 338, 473, 522, 852, 908, 947, 1007, 1049, 1077, 1112, 1354, 1389, 1419, 1453, 1642, 1899, 1946, 1995, 2032, 2062, 2099)

**Verify:** Re-run → diff `.tex` files vs baseline → only timestamp line removed. Second re-run → zero git changes.

**Commit:** "Remove stargazer timestamps from .tex output for clean git diffs"

---

## Phase 2: Performance Fixes (in `Preparing Data.R`)

### 2a. Replace 8 sapply O(n²) loops with cumsum (lines 352-376)

**Before:**
```r
df_long$prev_verified <- sapply(1:nrow(df_long), function(r)
  sum(df_long$verify_round & df_long$id==df_long$id[r] & df_long$round < df_long$round[r], na.rm=TRUE))
```

**After:**
```r
df_long <- df_long %>%
  arrange(id, round) %>%
  group_by(id) %>%
  mutate(
    prev_verified = cumsum(lag(replace_na(verify_round, 0), default = 0)),
    prev_balls_red = cumsum(lag(replace_na(ball_red, 0), default = 0)),
    # ... same for all 8 variables
  ) %>%
  ungroup()
```

**NA handling note:** The sapply loops use `na.rm = TRUE` on boolean columns. `replace_na(..., 0)` before `cumsum` achieves the same — NAs treated as 0 (not counted). Must verify this matches for verify_round, aggregate_informative, etc. where NA means "not this type of round".

### 2b. Replace `colSums(rbind(a, b))` with `a + b` (lines 360-371)

```r
# Before:
red_balls = colSums(rbind(ball_red, prev_balls_red), na.rm = TRUE)
# After:
red_balls = replace_na(ball_red, 0) + prev_balls_red
```

### 2c. Replace nested `ifelse` chains with `case_when()` (lines 100-102, 297-316, 587-591)

Treatment recoding, signal classification, response categorization — all cleaner with `case_when()`.

**Verify:** `all.equal(old_data, new_data)` on all 8 exported datasets vs `data/processed_baseline/`. Must be exact match including column names.

**Commit:** "Replace O(n²) sapply loops with cumsum and clean up ifelse chains"

---

## Phase 3: Modular File Restructure

### New structure:
```
code/
  00_prepare_data.R            # renamed from "Preparing Data.R"
  01_analysis_regular.R        # Section 1 (~lines 220-632)
  02_analysis_retract.R        # Section 2 (~lines 635-1658)
  03_analysis_confirm.R        # Section 3 (~lines 1661-2243)
  utils/
    packages.R                 # library() calls + stargazer patch
    constants.R                # dimensions, prior bins, output_type, paths, magic numbers
    helpers.R                  # write_stargazer, fig_path, tab_path, model_clusters, clip_belief, add_prior_bins
    run_control.R              # run_sections, .deps, .should_run, .tick, .fail
    derived_variables.R        # data loading + shared derived columns + df_coef creation
    plot_theme.R               # unified ggplot theme
  run_all.R                    # orchestrator
```

### File header template (every new file starts with):
```r
# ──────────────────────────────────────────────────
# [Descriptive Title]
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Purpose: [one-line description]
# Inputs:  [what data/files it reads]
# Outputs: [what it produces]
# ──────────────────────────────────────────────────
```

### Key details:
- Each analysis file can run standalone (checks `if (!exists(".utils_loaded"))`)
- `run_all.R` sets `.utils_loaded <- TRUE` after sourcing utils, then sources analysis files in order
- Cross-file dependency fix: move `df_coef` creation (mixed-effects model + coefficient extraction from fig12, lines 260-290) into `derived_variables.R`. Remove `tab7 = "fig12"` from `.deps` list.
- `run_sections` / `.should_run()` / `.deps` preserved — `.deps` updated for removed dependencies
- Old files (`Preparing Data.R`, `Figures and Tables.R`) deleted after verification

### `constants.R` consolidates magic numbers:
```r
BELIEF_FLOOR <- 0.001      # prevents log(0) in log-odds calculations
BELIEF_CEIL  <- 0.999
REPORT_TOL   <- 0.01       # threshold for correct/over/under classification
gamma <- 0.75              # signal precision (P(red|Red urn))
prob_fake <- 0.6           # probability of fake signal in retraction
prior_R <- 0.5             # prior probability of Red urn
```

### `helpers.R` includes extracted helper functions:
```r
clip_belief <- function(x) pmin(pmax(x, BELIEF_FLOOR), BELIEF_CEIL)

add_prior_bins <- function(df, ball_col, belief_col) {
  # Creates prior_aligned and belief_bin columns using shared constants
  ...
}

create_lag <- function(df, col, n, min_round = n) {
  # Creates lag with proper NA for rounds < min_round
  ...
}

plot_histogram <- function(df, x_col, title, ...) {
  # Standardized histogram for Fig 10-style variance comparison plots
  ...
}
```

### `plot_theme.R` — unified theme:
```r
theme_paper <- function(base_size = 11) {
  theme_classic(base_size = base_size) +
    theme(...)
}
```
Applied to all ~20 figures. `theme_classic()` as base (majority of figures already use it). Figures currently on `theme_minimal()` switch to `theme_classic()`.

**Verify:** `source("code/run_all.R")` → diff all outputs vs `output_baseline/`. `.tex` files should be byte-identical. `.jpg` files should be identical (or near-identical if only theme changed — visual spot-check).

**Commit:** "Restructure into modular pipeline with utils directory"

---

## Phase 4: Variable Renaming

**Convention:** snake_case, medium length. Expand cryptic abbreviations. Keep established short forms where clear (`ret`, `conf`, `lag1`).

### Column renames (applied in `00_prepare_data.R` before save, updated in all analysis files):

| Old | New | Rationale |
|-----|-----|-----------|
| `c` | `confirms_prior` | Single-letter, unclear meaning |
| `obslnpost` | `obs_log_post_ratio` | Cryptic abbreviation |
| `truelnpost` | `true_log_post_ratio` | Cryptic abbreviation |
| `comp_hist` | `compressed_hist` | Abbreviation unclear |
| `agg_hist` | `aggregate_hist` | Abbreviation unclear |
| `sign_hist` | `sign_adjusted_hist` | "sign" ambiguous (signature? signed?) |
| `hist` | `signal_hist` | "hist" alone is ambiguous (histogram?) |
| `sig` | `signal_code` | Single-letter-ish, internal only |
| `over_report_ret` | `over_report_ret` | **Keep** — `_ret` is established shorthand |
| `post_induced` | `posterior_induced` | Inconsistent `post_` prefix |
| `post_subj_adj` | `posterior_subj_adj` | Same prefix inconsistency |
| `post_induced_adj` | `posterior_induced_adj` | Same |
| `post_lag1` / `post_lag2` | `posterior_lag1` / `posterior_lag2` | Same |
| `ver_retract` | `is_retraction` | Clearer as boolean indicator |
| `ret_same` / `ret_other` | `ret_same_color` / `ret_diff_color` | "same/other" vague — same as what? |
| `conf_same` / `conf_other` | `conf_same_color` / `conf_diff_color` | Same issue |
| `ver_same` / `ver_other` | `ver_same_color` / `ver_diff_color` | Same issue |
| `prev_ret` / `prev_conf` | `cum_ret` / `cum_conf` | "prev" suggests previous round, but it's cumulative |
| `belief_diff_priorinduced` | `belief_dev_induced` | Too long |
| `belief_diff_priorinduced_adj` | `belief_dev_induced_adj` | Same |
| `signal_confirm` | **DELETE** | Never used, produces NaN |
| `signal_confirm_ratio` | **DELETE** | Never used, produces NaN |
| `signal_adj` | `signal_aligned` | "adj" doesn't convey alignment |
| `prior_adj` | `prior_aligned` | Same (already used in derived_variables — unify) |

### `is_retraction` rename safety check:
`ver_retract` appears in model formulas and may show up as raw coefficient labels in stargazer output. **Before renaming**, verify every stargazer call referencing it has explicit `covariate.labels` or `keep` patterns. If any call exposes raw column names, update the labels.

### Columns to KEEP as-is:
`belief`, `belief_lag1-3`, `posterior_subj`, `posterior_agg`, `over_report`, `signal_ratio`, `signal_ratio_obj`, `prior_ratio`, `ball_red`, `ball_red_lag1`, `verify_round`, `aggregate_round`, `type`, `treat`, `id`, `round`, `correct`, `over`, `under`, `wrong`, `no_change`, `initial_reaction`, `red_retract`, `blue_retract`, `hist_ret`, `hist_conf`

### Intermediate dataframe renames (internal to analysis files):

| Old | New | Location |
|-----|-----|----------|
| `df_retract_sum_all` | `df_ret_response_summary` | 02_analysis_retract.R |
| `df_retract_sum` | `df_ret_response_labeled` | 02_analysis_retract.R |
| `df_retract_type_initial` | `df_ret_by_initial` | 02_analysis_retract.R |
| `df_retract_type` | `df_ret_by_reaction` | 02_analysis_retract.R |
| `df_retract_no_outliers` | `df_ret_robust` | 02_analysis_retract.R |
| `df_regular_type` | `df_reg_subject_stats` | derived_variables.R |
| `df_regular_type_number` | `df_reg_type_counts` | 02_analysis_retract.R |
| `df_regular_mixedballs` | `df_reg_opposite_signals` | 02_analysis_retract.R |
| `df_retract_opposite_all` | `df_ret_vs_opposite` | 02_analysis_retract.R |
| `df_confirm_temp` | `df_conf_nonceiling` | 03_analysis_confirm.R |
| `df_confirm_type` | `df_conf_by_reaction` | 03_analysis_confirm.R |
| `df_confirm_type_restricted` | `df_conf_by_reaction_clean` | 03_analysis_confirm.R |
| `df_uninformative_id` | `df_uninf_subject_stats` | 02_analysis_retract.R |
| `df_uninformative_sum` | `df_uninf_summary` | 02_analysis_retract.R |
| `df_ret_logodds` / `df_conf_logodds` | `df_ret_prior_bins` / `df_conf_prior_bins` | analysis files |

### Model object renames (internal to analysis files):

| Old | New |
|-----|-----|
| `ols_retract_5_main` | `ols_ret_persistence` |
| `ols_retract_5_expl1/2` | `ols_ret_lag2` / `ols_ret_treat` |
| `ols_retract_6a/6b` | `ols_ret_comp_hist` / `ols_ret_comp_hist_round` |
| `ols_retract_5_types1-4` | `ols_ret_by_avg` / `_by_cd` / `_by_type` / `_by_subject_fe` |
| `ols_retract_9` | `ols_ret_induced_prior` |
| `ols_retract_10a` | `ols_ret_vs_opposite` |
| `me_regular_expl1-4` | `me_reg_by_prev_ver` / `_by_prev_ret_conf` / `_by_same_other` / `_by_same_other_split` |

### Implementation approach:
1. Add `dplyr::rename()` calls in `00_prepare_data.R` right before each dataset save
2. Update all references in analysis files — one variable at a time, grep to find ALL occurrences
3. Delete dead code (`signal_confirm`, `signal_confirm_ratio`, bare `var()` prints, `signal_adj` flip on line 832)

**Verify:** Load old/new `.rda` files → `all.equal(old, new, check.names = FALSE)` for all 8 datasets. Diff `.tex` → numbers unchanged. Any coefficient label changes are intentional and cosmetic only.

**Commit:** "Rename variables, model objects, and intermediate dataframes for clarity"

---

## Phase 5: Section Name Standardization

Switch all `.should_run()` names from paper-numbered to descriptive. This decouples the code from paper numbering (which will change during revision and can be mapped later).

### Section name map:

**Regular signals:**
| Old | New |
|-----|-----|
| `tab1` | `tab_reg_belief_vs_posterior` |
| `fig12` | `fig_reg_inference` (also produces tab2) |
| `tab3` | `tab_reg_treatment` |
| `fig13` | `fig_reg_overreport` |
| `fig17` | `fig_reg_time` |
| `tab14` | `tab_reg_llr` |
| `tab15` | `tab_reg_belief_change` |
| `fig_reg_prior` | `fig_reg_prior` (already descriptive) |

**Retractions:**
| Old | New |
|-----|-----|
| `fig5` | `fig_ret_response_by_initial` |
| `fig6` | `fig_ret_vs_opposite_ball` |
| `fig14` | `fig_ret_scatter` |
| `fig15` | `fig_ret_individual` |
| `tab4` | `tab_ret_main` |
| `tab5` | `tab_ret_compressed_hist` |
| `tab6` | `tab_ret_obj_cd` |
| `tab7` | `tab_ret_types` |
| `tab8` | `tab_ret_robustness` |
| `tab9` | `tab_ret_induced_prior` |
| `tab10` | `tab_ret_vs_ball` |
| `fig9` | `fig_ret_vs_uninformative` |
| `fig10` | `fig_ret_variance_vs_uninf` |
| `tab12` | `tab_ret_vs_uninformative` |
| (revision sections already descriptive — keep as-is) |

**Confirmations:**
| Old | New |
|-----|-----|
| `fig7` | `fig_conf_all_reactions` |
| `fig8` | `fig_conf_belief_change` |
| `fig11` | `fig_conf_by_initial` |
| `fig16` | `fig_conf_vs_informative` |
| `tab11` | `tab_conf_categories` |
| `tab11b` | `tab_conf_types` |
| `tab13` | `tab_conf_vs_informative` |
| (revision sections already descriptive — keep as-is) |

Update `.deps` list to use new names. Update `run_sections` documentation comment.

**Verify:** Test selective execution with new names: `run_sections <- c("tab_ret_main", "fig_ret_response_by_initial")`.

**Commit:** "Switch section names from paper-numbered to descriptive"

---

## Phase 6: Documentation & Cleanup

- **Delete `Rplots.pdf`** from repo
- **Add inline formula comments** in `00_prepare_data.R`:
  - Posterior calculation (lines ~461-474): explain Bayes' rule application with gamma/prob_fake
  - Induced urn distribution (lines ~668-690): explain what "rationalizing" means and the algebra
  - Log-odds decomposition (lines ~509-514): explain `obs_log_post_ratio = prior_ratio + signal_ratio`
  - Signal memory values (lines ~497-505): explain where 0.4/0.6 come from (gamma parameter)
- **Remove dead code:**
  - `signal_confirm` + `signal_confirm_ratio` creation (produces NaN, never used)
  - `signal_adj` flip on `df_confirm` (never used downstream)
  - Bare `var()` console prints (lines ~1317-1324)
- **Consolidate column dropping** in `00_prepare_data.R` (11 `grepl` calls → 1 regex)
- **Apply deduplication** using helpers created in Phase 3:
  - Prior binning → `add_prior_bins()` (used 3 times)
  - Boundary clipping → `clip_belief()` (used 8 times)
  - Histogram generation → `plot_histogram()` (used 6 times in Fig 10)
  - Lag-with-NA → `create_lag()` (used 4+ times)
- **Update `CLAUDE.md`:**
  - New file structure and run instructions
  - Updated variable reference with new names
  - Updated section name list
  - Remove references to old line numbers
- **Update `README.md`** with new run instructions
- **Update `Tables_and_Figures_Reference.md`** with new variable names and file paths

**Commit:** "Add formula docs, remove dead code, deduplicate helpers, update documentation"

---

## Execution Order Summary

| Step | What | Risk | Verification |
|------|------|------|-------------|
| 0 | Baseline snapshot | None | Pipeline runs, outputs captured |
| 1 | Stargazer timestamps | Low | Diff .tex: only timestamp removed |
| 2 | Performance fixes | Medium | `all.equal()` on all 8 datasets |
| 3 | File restructure + helper extraction | Medium | Full re-run, diff all outputs |
| 4 | Variable renaming (columns + models + dataframes) | High | `all.equal(..., check.names=FALSE)`, diff .tex for numbers |
| 5 | Section name standardization | Low | Test selective execution with new names |
| 6 | Docs, dead code, dedup, formula comments | Low | Full re-run |

Each step gets its own commit for easy bisection.

---

## Critical Files

- `code/Figures and Tables.R` — monolith to split (23 stargazer calls, all variable references, all section names)
- `code/Preparing Data.R` — performance fixes, column renames, formula comments; rename to `00_prepare_data.R`
- `CLAUDE.md` — must stay comprehensive and current after restructure
- `Tables_and_Figures_Reference.md` — update variable names and file paths
- `README.md` — update run instructions
- All `output/**/*.tex` — verify no numerical changes
- All `data/processed/*.rda` — verify data integrity

## Future (deferred until revision complete)

- Align figure/table numbers with final paper (single pass: add paper-number comments or rename output files)
- Trim unused analyses based on what made it into the paper
- The descriptive section names chosen now will make this remapping straightforward
