# Revision Plan — Code/Analysis Tasks

**Status: All analysis tasks complete.** Code restructured 2026-03-21 (topic-based organization, derived variables extraction, output folder reorganization).

## Context
Paper "Belief Updating with Misinformation" received revision request. Lars handles code/analysis; Elias handles writing. Goal: make targeted code changes to produce new/revised figures and tables that address reviewer concerns.

All changes in `Figures and Tables.R`. No changes to `Preparing Data.R` needed.

---

## Task 1: Replot Figures 7 & 11 — Show over-report instead of raw belief change ✓ DONE
**Why:** Reviewer 1 says raw belief change is "hard to interpret" and suggests plotting belief difference relative to Bayesian (adjusted for signal direction), which directly shows under-/over-reaction.

### Figure 7 — DONE
- Bars now show `over_report` (belief - Bayesian posterior, sign-adjusted) instead of `belief_change_adj`
- Bar 1 ("Initial Signal"): weighted mean of `over_report_lag1` per subject
- Bar 2 ("After Confirmation"): weighted mean of `over_report` per subject (standalone, not cumulative)
- Removed Bayesian benchmark dashed lines — zero line IS the benchmark
- Y-axis: `"Belief higher than Bayesian (%pts)"`, limits `c(-5, 5)`

### Figure 11 — DONE
- Now shows only 2 bars (removed separate "Initial Signal" bar): "Initial Signal + Confirmation" and "Informative Signal"
- Both bars use `over_report`
- Removed hardcoded `yintercept=19.25` and Bayesian benchmark line
- Y-axis: `"Belief higher than Bayesian (%pts)"`, limits `c(-5, 5)`

### Output: `02_fig_confirm_change.jpg`, `02_fig_confirm_vs_informative_change.jpg` (overwrites existing)

---

## Task 2: Add treatment heterogeneity analysis for confirmations (new table after Table 11)
**Why:** Reviewer 2 (comment 1) wants heterogeneity analysis. Table 8 already does treatment interactions for retractions — need the equivalent for confirmations.

### New code block after line 1109
- Copy Table 8 pattern (lines 1010-1029)
- Regression: `over_report ~ over_report_lag1*treat_no_anchor + over_report_lag1*treat_no_history` on `df_confirm`
- Use `starprep(..., clusters = df_confirm$id)` for clustered SEs
- Also add clustered SEs to existing Table 11 (currently missing)

### Files: Figues and Tables.R insert after line 1109
### Output: `03_tab_confirm_beliefdiff_treat.{tex/html}` (NEW file)

---

## Task 3: Expand confirmation analysis (Editor comment 2 — new section)
**Why:** Editor says "it would be important to add to your analysis of confirmations." Reviewer 2 (comment 3) says confirmations are underrepresented (~1.5 pages in 5.3 vs ~7 pages in 5.2 on retractions). The natural approach is to mirror the retraction robustness checks for confirmations.

### 3a. Add t-2 over-report control to confirmation regression (mirrors Table 8 col 1 for retractions)
- Tests whether subjects are "correcting" a previous mistake when they respond to a confirmation
- Regression: `over_report ~ over_report_lag1 + over_report_lag2` on `df_confirm`
- Simple 1-line addition, add as extra column in Table 11
- This parallels how Table 8 col 1 adds `over_report_lag2` for retractions

### 3b. Confirmations vs informative signals — direct over_report comparison
- Current Figure 11 uses raw belief change → weak evidence (R1: "not convincing")
- After Task 1 replots Figure 11 with `over_report`, also run a simple pooled regression:
  `over_report ~ confirmation_dummy` where we pool confirmation and informative signal observations
- This gives a formal test of whether the over_report difference is statistically significant
- Much cleaner than Table 13's history-based approach, directly addresses R1's concern

### 3c. Confirmation belief dispersion analysis (mirrors Section 5.4.2 for retractions)
- Section 5.4.1 shows belief dispersion for retractions vs uninformative signals (Figure 10)
- Can do the analogous comparison: belief dispersion for confirmations vs informative signals
- Shows whether the variance of beliefs after confirmations is higher than after informative signals
- Simple histogram or variance comparison, reinforces the under-reaction finding

### Priority: 3a first (easiest, most valuable), then 3b, then 3c if needed. Share results with Elias.
### Files: Figues and Tables.R new code blocks after Table 11

---

## Task 5: Signal history & prior belief heterogeneity (Reviewer 2 comment 1) ✓ DONE
**Why:** Reviewer 2 says heterogeneity in retraction/confirmation responses might be driven by how many signals/checks subjects have previously seen (`prev_verified`), not by updating strength (`over_report_lag1`). Also asked (by Elias) whether prior belief level matters.

### 4 new tables + 2 new figures in Figures and Tables.R (after Table 11b)

#### Signal history robustness (tab_ret_hist, tab_conf_hist)
- 3 columns: baseline, + prev_verified control, + prev_verified × over_report_lag1 interaction
- **Retractions:** interactions insignificant (−0.063, +0.081) → signal history does NOT drive heterogeneity ✓
- **Confirmations:** interactions marginally significant (−0.25*, −0.25**) → effect weakens ~30% with experience, plateaus after first check

#### Prior belief heterogeneity (tab_ret_prior, tab_conf_prior)
- 10 even bins of belief_lag2 (0–10%, 10–20%, ..., 90–100%), reference = 40–50%
- Regression: `over_report_ret ~ over_report_lag1 * belief_lag2_bin` with clustered SEs
- **Retractions:** U-shaped pattern — subjects with extreme priors (both tails) show stronger retraction persistence; weakest at 40–60%
- **Confirmations:** asymmetric — high-prior subjects (80–100%) show strongest confirmation persistence; 50–70% near zero

#### U-shape figures (fig_ret_prior, fig_conf_prior)
- Bar charts showing total over_report_lag1 coefficient per prior bin with 95% CIs (via lm_robust CR2)
- n per bin shown at bottom

### Output files:
- `03_tab_retract_beliefdiff_history.tex`, `03_tab_confirm_beliefdiff_history.tex`
- `03_tab_retract_beliefdiff_prior.tex`, `03_tab_confirm_beliefdiff_prior.tex`
- `02_fig_retract_prior_ushape.jpg`, `02_fig_confirm_prior_ushape.jpg`

---

## Task 4: Update GitHub replication package
**Why:** Reviewer requirement. Push final code after all changes verified.
- Commit all changes, push to https://github.com/lwittrock/UpdatingMisinformation-Analysis
- **Note:** Output folder structure changed — old `output/figures/` and `output/tables/` replaced by `output/{regular,retract,confirm}/{figures,tables}/`

---

## Implementation Order
1. **Task 2** (new treatment table + clustered SEs fix) — simplest, self-contained, no risk to existing code
2. **Task 1** (replot Figures 7 & 11) — modifies existing code, has dependency chain
3. **Task 3** (expanded analysis) — exploratory, depends on results from 1 & 2
4. **Task 4** (GitHub push) — after everything verified

## Verification
- After each task, source the relevant code block in radian
- Check output JPGs/tex files visually
- Full regression test: source entire `Figues and Tables.R` to confirm nothing broke
- Verify `df_confirm$treat_no_anchor` is populated: `table(df_confirm$treat_no_anchor)`
