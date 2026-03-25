# Tables and Figures Reference

Reference document for all tables and figures in "Belief Updating with Misinformation" (Wittrock, Strobel, Tsakas).

---

## Experimental Design Overview

### Two Experiments

**Experiment 1** (`raw_data.csv`, `treat_aggregate_signal == 0`): Conducted January 2022. Subjects see 9 uncertain ball draws + 3 ex-post verification rounds = **12 rounds** total. In the verification rounds, subjects learn whether the previous ball was informative or uninformative (retraction/confirmation). The 3 verification rounds always occur for 3 consecutive ball draws, but the starting position varies across subjects.

**Experiment 2** (`raw_data_extra.csv`, `treat_aggregate_signal == 1`): Conducted January 2023. Same basic design but with **ex-ante** verification: subjects see 6 uncertain signals + 3 signals where informativeness is revealed immediately (combined signal) = **9 rounds** total. This allows comparison of ex-post checks (retractions/confirmations) with ex-ante labeling (informative/uninformative signals).

### Information Display Treatments (within Experiment 1)

Within Experiment 1, three sub-treatments vary what subjects see on screen (`treat` variable):

| `treat` | Code value | Description |
|---------|------------|-------------|
| 1 | `belief_survey.1.player.treat==3` | **Baseline**: shows history of signals AND previously reported belief |
| 2 | `belief_survey.1.player.treat==1` | **No anchor**: shows history but NOT the previously reported belief |
| 3 | `belief_survey.1.player.treat==2` | **No history**: shows only the most recent signal (and in verification rounds also the uncertain initial signal), plus the previously reported belief |

Derived dummies:
- `treat_no_anchor` = 1 if `treat == 2`
- `treat_no_history` = 1 if `treat == 3`

### Signal Types

| Type | `type` label | Source | Description |
|------|-------------|--------|-------------|
| Regular signal | "Regular signal" | Both experiments | Ball color shown, informativeness unknown |
| Retraction | "Retraction" | Experiment 1 | Ex-post: previous ball revealed as uninformative |
| Confirmation | "Confirmation" | Experiment 1 | Ex-post: previous ball revealed as informative |
| Uninformative | "Uninformative" | Experiment 2 | Ex-ante: ball color + uninformative label shown simultaneously |
| Informative | "Informative" | Experiment 2 | Ex-ante: ball color + informative label shown simultaneously |

### Key Parameters

- Prior: p(R) = p(B) = 0.5
- Urn distribution (gamma): 0.75 (3 same-color, 1 opposite-color ball)
- Probability signal is informative (alpha): 0.4
- Uninformative signal bias (beta): 0.5
- Informative signal accuracy (1-epsilon): 0.75
- Resulting uncertain signal strength: pi(r|R) = pi(b|B) = 0.6

### Sample

849 total subjects completed. 96 removed as outliers based on pre-registered criteria:
- Experiment 1: belief50 >= 8, wrong > 6, no_change >= 8
- Experiment 2: belief50 > 6, wrong >= 5, no_change > 6
- Both: duration < 360 seconds

This yields 753 subjects and 8,397 total observations.

### Processed Datasets

| Dataset | N | Content |
|---------|---|---------|
| `data_main` | 8,397 | All observations (outliers removed) |
| `data_subject` | 849 | One row per subject (includes outliers) |
| `data_regular` | 6,777 | Regular signal rounds only (`verify_round==0` AND `aggregate_round==0`) |
| `data_retract` | 985 | Retraction rounds only (`ver_retract==1`) |
| `data_confirm` | 635 | Confirmation rounds only (`ver_retract==0` when `verify_round==1`) |
| `data_informative` | 269 | Ex-ante informative signals (`aggregate_informative==1`) |
| `data_uninformative` | 370 | Ex-ante uninformative signals (`aggregate_informative==0`) |
| `data_time` | ~19,485 | Page-by-page timing data |

---

## Tables

### Table 1: OLS Regression — Reported Belief on Bayesian Posterior (p.30)

- **Equation:** `belief = alpha + beta * posterior_subj + epsilon`
- **Sample:** `data_main`, all 8,397 observations (both experiments, all signal types)
- **DV:** Reported belief (0-1 scale)
- **IV:** Bayesian posterior based on sequential updating (subject's previous belief as prior)
- **SE:** Clustered by subject
- **Intention:** Validate the experimental design. A high correlation (R^2 = 0.495) shows subjects understood the task. Coefficient < 1 indicates systematic deviation from full rationality, as expected.

### Table 2: Updating with Regular Signals (p.31)

- **Equation 5:** `ln(b(R|s)/b(B|s)) = alpha + beta1 * ln(p(s|R)/p(s|B)) + beta2 * ln(p(R)/p(B)) + epsilon`
- **Sample:** `data_regular`, 6,777 observations (only regular uncertain signals, no verification or aggregate rounds)
- **DV:** Observed log-posterior-ratio (`obslnpost`)
- **IVs:** Signal log-likelihood ratio (`signal_ratio`, measures inference *c*), prior log-ratio (`prior_ratio`, measures base-rate use *d*)
- **Models:** (1) OLS with clustered SE, (2) linear mixed-effects, (3) mixed-effects with "Signal Confirms Prior" interaction
- **Intention:** Estimate inference and base-rate use following Grether (1980)/Benjamin (2019). **Result 1:** Subjects over-infer (c ~ 1.53 > 1) and exhibit base-rate neglect (d ~ 0.70 < 1). The over-inference is notable because it contrasts with most of the literature; the paper suggests this may be due to information uncertainty or weak signals (Thaler 2021).

### Table 3: Updating with Regular Signals — Effect of Varying Information Display (p.33)

- **Equation:** Same as Equation 5, with treatment dummies and interactions
- **Sample:** 6,093 observations from the 3 information display sub-treatments in Experiment 1 only (excludes Experiment 2 since it has no sub-treatments)
- **DV:** Observed log-posterior-ratio
- **IVs:** Signal, prior, treatment dummies (`treat_no_anchor`, `treat_no_history`), and their interactions with signal and prior
- **Intention:** Robustness check that the visual display doesn't drive results. Finding: hiding history slightly lowers base-rate use (Prior * No history: -0.174**), but overall effects are minimal. No effect from hiding the previous belief.

### Table 4: Impact of Retractions on Beliefs — Main Result (p.34)

- **Equation 6:** `b_t - p_{t-1} = alpha + beta1 * (b_{t-1} - B_{t-1}) + epsilon`
- **Sample:** `data_retract`, 985 retraction observations
- **DV:** Belief after retraction minus initial prior before the uncertain signal (`over_report_ret`, i.e., the residual bias from the retraction, adjusted for signal direction)
- **IV:** Initial belief over-report relative to Bayesian posterior (`over_report_lag1`, i.e., how much the subject over/under-reacted to the initial uncertain signal)
- **SE:** Clustered by subject
- **Intention:** **Core result (Result 3).** The constant alpha ~ 0 means no average bias from retractions. But beta1 = 0.614*** means the initial reaction strongly predicts the retraction response. Since 0 < beta1 < 1, subjects partially correct their initial mistake (~40%), but not fully. Over-reactors keep a residual bias in the direction of the retracted signal; under-reactors end up biased in the opposite direction.

### Table 5: Impact of Retractions — Compressed History Analysis (p.36)

- **Equation 7:** `b_t(R|s1,...,st) = alpha + beta * F_{R(Ht)} + gamma * F_{C(Ht)} + epsilon`
- **Sample:** 6,480 observations (pooled across subjects, comparing those who saw retracted signals with those who saw equivalent compressed histories without retractions)
- **DV:** Reported belief
- **IVs:** Factor variable for retraction history (14 levels: R, B, RR, BB, RB, BR, RRR, BBB, RRB, BBR, RBB, BRR, RBR, BRB) + compressed history fixed effects. Column 2 adds round fixed effects.
- **Intention:** Replicates the Goncalves et al. (2022) methodology. Tests whether beliefs of people who saw retracted signals differ from beliefs of people with the same compressed history who never saw the retraction. Finding: mostly beta ~ 0 (no significant continued influence for single retractions), contrary to Goncalves et al. Only 3 same-color retractions (RRR/BBB) show qualitative continued influence. The paper argues this is because over- and under-reactors cancel out on average.

### Table 6: Updating with Retraction Signals — Log-Likelihood Approach (p.37)

- **Equation 8:** `ln(b_t(R|st)/b_t(B|st)) = alpha + beta1 * ln(x/(1-x)) + beta2 * ln(p_t(R)/p_t(B)) + epsilon`
- **Sample:** Column 1: all 985 retractions. Columns 2-4: split by initial reaction — previously correct (+-1%pt, n=166), previously under-inferred (n=190), previously over-inferred (n=377)
- **DV:** Observed log-posterior-ratio after retraction
- **IVs:** Hypothetical retraction signal *x* in log-odds (computed from the subject's initial update, representing the signal needed for a Bayesian to return to the initial prior), and prior log-ratio
- **Intention:** Alternative test of Result 3 using the inference/base-rate framework. The hypothetical signal *x* is different for each subject because each person updated differently from the initial uncertain signal. Finding: previously over-inferring subjects under-use the retraction (c = 0.293 < 1 in column 4), while previously under-inferring subjects over-infer from it (c = 1.415 in column 3). Previously correct subjects show c ~ 1.228 (close to rational).

### Table 7: Different Categorizations of Types (p.38)

- **Equation:** `b_t - p_{t-1} = alpha + beta1 * (b_{t-1} - B_{t-1}) + beta2 * [type controls] + epsilon`
- **Sample:** `data_retract`, 985 retraction observations
- **DV:** Belief biased toward initial signal (same as Table 4 DV)
- **IVs across columns:**
  - (1) Initial over-report + average belief over-report across all regular rounds
  - (2) Initial over-report + average inference (c-1) + average base-rate use (d-1) per subject
  - (3) Initial over-report + majority-reaction type dummies (Correct, Over-reported, Under-reported, Wrong, Not categorized)
  - (4) Initial over-report + subject fixed effects
- **Intention:** Tests whether the retraction bias is just a subject "type" effect (some people always over-report). Finding: initial over-report remains the dominant predictor in all columns (beta1 = 0.64-0.74***). Subject types add little. In fact, consistent over-reporters are *better* at reacting to retractions (beta2 negative in column 1). This rules out the explanation that certain subjects are simply always biased.

### Table 8: Impact of Retractions — Robustness Checks (p.39)

- **Column 1 — Equation 9:** `b_t - p_{t-1} = alpha + beta1 * (b_{t-1} - B_{t-1}) + beta2 * (b_{t-2} - B_{t-2}) + epsilon`
  - Adds the lagged over-report from round t-2 (before the uncertain signal) to control for backward correction.
  - Finding: beta1 remains significant (0.637***). Beta2 is significantly negative (-0.163***), meaning subjects are NOT misreporting to correct a previous mistake — if anything, the opposite.

- **Column 2 — Treatment interactions:**
  - Adds `treat_no_anchor`, `treat_no_history` dummies and their interactions with initial over-report.
  - Finding: no significant treatment effects. The result is not driven by anchoring on the previously displayed belief.

- **Sample:** 985 retractions in both columns
- **Intention:** Two robustness checks addressing (1) strategic correction of past mistakes and (2) anchoring effects from information display.

### Table 9: Retraction Signals — Induced Prior Analysis (p.40)

- **Equation:** `b_t - induced_prior = alpha + beta * over_report_in_previous_round + epsilon`
- **Sample:** 985 retraction observations
- **DV:** Belief after retraction minus the "induced prior" (the prior that would rationalize the subject's initial update if they had used Bayes' rule with a different prior)
- **IV:** Belief over-report in previous round relative to the Bayesian posterior
- **Intention:** Uses the induced prior as an alternative benchmark. The coefficient (-0.389***) confirms subjects try to correct their initial mistake but only partially. Corroborates Table 4 from a different angle.

### Table 10: Retractions vs Opposite Colored Ball (p.40)

- **Equation 10:** `b_t(R|s1,...,st) = alpha + beta1 * r_t + beta2 * s_t + beta3 * r_t * s_t + gamma * F_{O(Ht)} + epsilon`
- **Sample:** 6,912 observations (985 retractions pooled with cases of two consecutive opposite-colored uncertain signals, with sign history fixed effects equating the two)
- **DV:** Reported belief
- **IVs:** Retraction dummy, signal direction, retraction x direction interaction, sign history fixed effects
- **Key coefficient:** beta3 (interaction) = 0.056*** — positive means retractions produce *less* belief revision than informationally equivalent opposite new balls
- **Intention:** **Result 4.** Retractions and opposite-colored new information are informationally equivalent but processed differently. People over-react to opposite new information but not to retractions.

### Table 11: Impact of Confirmations on Beliefs (p.41)

- **Equation 11:** `b_t - B_t(confirm) = alpha + beta1 * (b_{t-1} - B_{t-1}) * I(s_{t-1}) + epsilon`
- **Sample:** 635 confirmation observations (column 3: 591, excluding cases where induced prior is outside [0,1])
- **DV:** Belief after confirmation minus the Bayesian posterior for the confirmed signal
- **IVs:** Initial belief over-report * signal direction indicator
- **Columns:** (1) Standard Bayesian benchmark, (2) adds "initial update wrong" control, (3) uses alternative Bayesian benchmark from Equation 4 (accounts for misperception of epsilon)
- **Intention:** **Result 5.** Alpha < 0 (around -0.026 to -0.039***) shows subjects systematically under-react to confirmations. Beta > 0 (0.39-0.69***) shows the initial update predicts the confirmation response, similar to retractions.

### Table 12: Uninformative Signals vs Retractions (p.42)

- **Equation 12:** `b_t(R|s1,...,st) = alpha + beta * F_{U(Ht)} + gamma * F_{A(Ht)} + epsilon`
- **Sample:** `data_main`, 8,397 observations (full dataset, comparing retraction and uninformative signal histories via aggregate history fixed effects)
- **DV:** Reported belief
- **IVs:** Factor for uninformative signal history (14 levels) + aggregated history fixed effects (retracted signals coded same as uninformative)
- **Intention:** Tests whether beliefs differ between retracted and ex-ante uninformative signals for equivalent histories. For single signals: no significant difference. Multiple signals: some differences emerge but based on small samples. Combined with Figure 10, shows that repeated retractions increase belief dispersion more than equivalent uninformative signals.

### Table 13: Informative Signals vs Confirmations (p.43)

- **Equation 13:** `b_t(R|s1,...,st) = alpha + beta * F_{I(Ht)} + gamma * F_{A(Ht)} + epsilon`
- **Sample:** Column 1: 8,397. Column 2: 7,319 (excluding histories with uninformative signals)
- **DV:** Reported belief
- **IVs:** Factor for informative signal history + aggregated history fixed effects (confirmed signals coded same as informative)
- **Intention:** Tests whether confirmations produce different beliefs than ex-ante informative signals. Coefficients qualitatively suggest people react more to informative signals, but most are insignificant due to small comparable samples.

### Table 14: Impact of Previous Information Checks — Log-Likelihood (p.44)

- **Equation:** Extended version of Equation 5 with interactions for past verification counts
- **Sample:** 4,860 regular signal observations (subset of `data_regular` where subjects have experienced some prior verifications)
- **DV:** Observed log-posterior-ratio
- **IVs:** Signal, prior, round interactions, plus interactions with counts of previous checks:
  - (1) Total # previously checked signals
  - (2) # previous retractions + # previous confirmations
  - (3) # previous same-color checks + # previous other-color checks
  - (4) Full decomposition: # previous same/other retractions and same/other confirmations
- **Intention:** Tests **Result 6** — do past verifications affect future inference? Some coefficients appear significant (e.g., other-color checks lower inference) but findings are not robust across specifications and lack intuitive explanation.

### Table 15: Impact of Previous Information Checks — Belief Change (p.45)

- **Equation:** `(b_t - b_{t-1}) * I(s_t) = alpha + beta * round + [verification count variables] + epsilon`
- **Sample:** 4,860 regular signal observations (same as Table 14)
- **DV:** Absolute belief change adjusted for signal direction (simpler than log-likelihood)
- **IVs:** Round + same verification count decompositions as Table 14
- **Intention:** Complement to Table 14 using a more intuitive DV. R^2 near zero in all columns. **Result 6:** No consistent effect of past information checks on future updating.

---

## Figures

### Figure 1: Overview of Signal Terminology (p.8)
- **Type:** Schematic diagram (not data-driven)
- **Intention:** Illustrates the belief number line showing how uncertain, informative, uninformative, retraction, and confirmation signals relate to each other. Shows that after an uncertain red signal the prior p(R) moves to p(R|r), and then either back (retraction) or further (confirmation) to p(R|r_inf).

### Figure 2: Rational Retraction Example (p.9)
- **Type:** Schematic diagram
- **Intention:** Shows that regardless of the initial belief update (whether b(R|r) or b'(R|r)), the rational response to a retraction is always to return to the original prior p(R).

### Figure 3: Rational Confirmation Example (p.10)
- **Type:** Schematic diagram
- **Intention:** Illustrates the two possible Bayesian benchmarks for confirmations: p(R|r,c) assumes the initial mistake came from misperceiving alpha; p'(R|r,c) assumes the mistake came from misperceiving epsilon. The two benchmarks diverge for subjects who initially mis-updated.

### Figure 4: Experimental Design (p.12)
- **Type:** Visual depiction of urns and black box (shown to participants)
- **Intention:** Shows how 4 informative balls from the selected urn are mixed with 6 uninformative balls (3 red U, 3 blue U) in the black box.

### Figure 5: Initial Belief Update Predicts Retraction Response (p.16)
- **Code:** `02_fig_retract_diff_group.jpg`
- **Sample:** 985 retraction observations, split into 3 groups + pooled:
  - Correctly reacted (+-1%pt): n=133
  - Over-reacted (>1%pt): n=377
  - Under-reacted (<1%pt, excluding wrong direction): n=353
  - All retractions: n=985
- **Y-axis:** Belief biased toward initial signal (`over_report_ret`, i.e., b_t - p_{t-1} adjusted for signal direction). Positive = continued influence, negative = reverse influence.
- **Intention:** Visual summary of **Result 3**. Over-reactors show continued influence (positive), under-reactors show reverse influence (negative), correct updaters show no bias (~0). The "All Retractions" column shows near-zero average, explaining why aggregate tests miss the heterogeneous pattern.

### Figure 6: Retractions vs Opposite Signals (p.20)
- **Code:** `02_fig_retract_diff_vs_opposite_ball_all.jpg`
- **Sample:** 2,134 opposite new information observations (two consecutive opposite-colored uncertain balls) + 985 retraction observations
- **Y-axis:** Belief difference to initial prior, adjusted for signal direction
- **Intention:** Visual representation of **Result 4**. Retracted signals produce near-zero average bias, but opposite-colored new balls produce significant negative bias (subjects over-react to the second signal, overshooting the prior). Demonstrates retractions are processed differently from informationally equivalent new information.

### Figure 7: Reaction to Confirmation Signals (p.21) — REVISED
- **Code:** `02_fig_confirm_change.jpg`
- **Sample:** 590 confirmation observations (excluding ceiling/floor where `belief_change_rational_lag1 == 0`)
- **Bars:** Bar 1 ("Initial Signal"): weighted mean of `over_report_lag1` per subject. Bar 2 ("After Confirmation"): weighted mean of `over_report` per subject. Each bar is standalone (not cumulative).
- **Y-axis:** Belief higher than Bayesian (percentage points). Zero = Bayesian benchmark. Limits: -5 to 5.
- **Intention:** Directly shows over-/under-reaction relative to Bayesian. Positive bars = over-reaction, negative = under-reaction. Replaces the old cumulative belief change plot with benchmark lines, per Reviewer 1's request for a more interpretable visualization.

### Figure 8: Influence of Initial Reaction on Beliefs after Confirmation (p.22)
- **Code:** `02_fig_confirm_diff_restricted.jpg`
- **Sample:** Confirmations split by initial reaction:
  - Correctly reacted (+-1%pt): n=89
  - Over-reacted (>1%pt, excluding those beyond confirmed signal): n=93
  - Under-reacted (<1%pt, excluding wrong direction/no update): n=133
- **Y-axis:** Belief higher than Bayesian (percentage points)
- **Intention:** Shows all groups under-react to confirmations. Even correct initial updaters end up below Bayesian. Only over-reactors approach the Bayesian benchmark. Mirrors the retraction result for confirmations.

### Figure 9: Uninformative vs Retracted Signals — Average Belief Difference (p.23)
- **Code:** `02_fig_retract_vs_uninformative.jpg`
- **Sample:** 985 retraction observations (Experiment 1) + 370 uninformative signal observations (Experiment 2)
- **Y-axis:** Average difference between prior and posterior, adjusted for signal direction. Positive = bias toward the signal color.
- **Intention:** Compares the bias from retracted signals (near zero, some spread) with uninformative signals (slightly positive — some subjects update toward the ball color despite knowing it's uninformative). The two are not significantly different at 5% level for single signals.
- **Note (known issue):** Annotation indices may be reversed in the code (n[1] and n[2] swapped).

### Figure 10: Belief Dispersion after Multiple Retractions vs Uninformative Signals (p.24)
- **Code:** `02_fig_variance_retract_uninf.jpg`
- **Sample:** All subjects with 1, 2, or 3 consecutive retracted or uninformative signals (varying sample sizes per panel)
- **X-axis:** Belief minus initial prior (before any of the consecutive signals)
- **Y-axis:** Percentage of subjects
- **Layout:** 6-panel histogram (3 rows x 2 columns: retracted left, uninformative right; 1/2/3 signals top to bottom)
- **Intention:** Key visualization showing belief dispersion grows with consecutive retractions far more than with uninformative signals. After 3 retractions: variance = 7.1%pts vs 4.4%pts for uninformative. Fewer people correctly return to prior with each additional retraction. Demonstrates the real-world implication: repeated misinformation, even if corrected, leads to dispersed beliefs.

### Figure 11: Informative vs Confirmed Signals — Over-report Comparison (p.25) — REVISED
- **Code:** `02_fig_confirm_vs_informative_change.jpg`
- **Sample:** 590 confirmed signal observations (Experiment 1) + 269 informative signal observations (Experiment 2)
- **Bars:** 2 bars: "Initial Signal + Confirmation" (`over_report` from `df_confirm`) and "Informative Signal" (`over_report` from `df_informative`). Weighted means grouped by subject.
- **Y-axis:** Belief higher than Bayesian (percentage points). Zero = Bayesian benchmark. Limits: -5 to 5.
- **Intention:** Directly compares over-report after confirmations vs informative signals. Replaces the old 3-bar belief change plot with benchmark lines, per Reviewer 1's request. Clearer visualization of under-reaction to confirmations relative to both Bayesian and informative signal benchmarks.

### Figure 12: Distribution of Inference and Base-Rate Use per Subject (p.32)
- **Code:** `02_fig_regular_updating_inference_baserate.jpg`
- **Sample:** Subject-level estimates of *c* and *d* from regular signal rounds (computed per subject by running subject-level regressions)
- **Left panel:** Histogram of estimated inference (*c*) per subject. Most cluster around c ~ 1.5.
- **Right panel:** Histogram of estimated base-rate use (*d*) per subject. Most cluster around d ~ 0.3-0.5.
- **Intention:** Shows the distribution of subject "types" in terms of inference and base-rate use. Supports Table 7 by showing that while type variation exists, it is less predictive of retraction responses than the specific initial reaction. A value of 1 for both would be Bayesian.

### Figure 13: Average Bias per Subject (p.32)
- **Code:** `02_fig_regular_overreport.jpg`
- **Sample:** Subject-level average over-report from regular signal rounds
- **X-axis:** Average over-reported belief compared to Bayesian posterior (adjusted for signal direction)
- **Y-axis:** Count of subjects
- **Intention:** Complement to Figure 12. Most subjects slightly over-report on average, with substantial individual variation. Used alongside Table 7 to argue that individual-level type classification is less predictive than the specific initial reaction.

### Figure 14: Scatter Plot of Retraction Responses (p.34)
- **Code:** `02_fig_retract_change_lm.jpg`
- **Sample:** 985 retraction observations (zoomed, ~5% of data points omitted for visibility)
- **X-axis:** Belief change from initial signal (adjusted for signal direction)
- **Y-axis:** Belief change from retraction (adjusted for signal direction)
- **Diagonal (dashed):** Perfect retraction response (undo exactly what was done, i.e., return to prior)
- **Blue line:** OLS regression fit
- **Intention:** Raw data visualization. Points on the diagonal = rational retraction. Points above = under-reaction to retraction (didn't undo enough). Points below = over-reaction (undid too much or reversed). The blue regression line shows the partial correction pattern from Table 4 (slope < 1, intercept ~ 0).

### Figure 15: Individual Belief Reports Following Retractions (p.35)
- **Code:** `02_fig_retract_diff_cont_noout.jpg`
- **Sample:** 985 retraction observations (zoomed, 3% omitted)
- **X-axis:** Reaction to initial signal compared to Bayesian (over-report, in percentage points)
- **Y-axis:** Belief biased toward initial signal after retraction (in percentage points)
- **Horizontal dashed line at 0:** Rational retraction response (regardless of initial update)
- **Vertical dashed line at 0:** Rational initial response (Bayesian updating)
- **Blue line:** OLS regression fit (corresponds to Table 4 regression)
- **Intention:** Alternative visualization to Figure 14. The positive slope confirms over-reactors remain biased upward and under-reactors become biased downward after retraction. The intercept near zero confirms no average bias.

### Figure 16: Complete Confirmation Effects — All Reaction Types (p.41)
- **Code:** `02_fig_confirm_diff.jpg`
- **Sample:** 590 confirmation observations split into 6 groups:
  - Correctly reacted (+-1%pt): n=89
  - Over-reacted (>1%pt): n=93
  - Over-reacted a lot (beyond confirmed signal range): n=126
  - Under-reacted (<1%pt): n=133
  - No update: n=78
  - Wrong direction: n=71
- **Y-axis:** Belief higher than Bayesian (percentage points)
- **Intention:** Extended version of Figure 8 showing all subject groups. Confirms that under-reaction to confirmations is pervasive. Only strong over-reactors come close to Bayesian.
- **Note (known issue):** Hardcoded annotation indices may not match factor ordering.

### Figure 17: Time per Type of Belief Updating Problem (p.46)
- **Code:** `02_fig_time_belief_type.jpg`
- **Sample:** Timing data across all rounds (excluding round 1, which took much longer)
- **X-axis:** Signal type (Regular, Retraction, Confirmation, Uninformative, Informative)
- **Y-axis:** Seconds spent on belief report
- **Intention:** Shows confirmations and informative signals take longest. No evidence retractions are slower than regular updating. This addresses the Goncalves et al. (2022) hypothesis that retractions are harder to process (more cognitive effort). In this data, retractions do not appear more effortful than regular signals.

### Figure 18: Example Screen (p.52)
- **Type:** Screenshot from the oTree experiment (baseline treatment)
- **Intention:** Shows what participants actually saw: round number, history table (Ball 1 through Ball 9), previous belief reminder, new information description, and slider for belief input (0-100%). Note: no default value on the slider; subjects must click to select.

---

## Inconsistencies and Notes

### Confirmed Issues (documented in CLAUDE.md)

1. **`lag()` without grouping by id** (Preparing Data.R): All `lag()` operations on `df_long` cross subject boundaries since there's no `group_by(id)`. This means the first round of each subject incorrectly picks up the last round of the previous subject for lagged variables. Pre-existing, not fixed to avoid changing published results.

2. **`belief_lag3` uses `lag(..., 2)` instead of `lag(..., 3)`** (line 456): Off-by-one error. This variable is used in uninformative signal analysis.

3. **`post_induced` clipped to [-1, 1] instead of [0, 1]** (line 689-690): Probabilities should be bounded to [0, 1].

4. ~~**Figure 16 annotation indices**~~: Visually verified correct. n[1,3,4,5,2,6] correctly maps alphabetical row order to factor level order. `factor(type)` on line 735 drops the unused "All" level.

5. ~~**Figure 9 annotation indices**~~: Visually verified correct. Swapped n[1]/n[2] correctly compensates for alphabetical factor ordering vs rbind row ordering.

### Investigated and Cleared

The following were investigated and found to be intentional or inconsequential:

6. **Experiment 1 has 12 rounds, Experiment 2 has 9 rounds** — By design. Outlier thresholds differ accordingly (e.g., `belief50 >= 8` vs `> 6`).

7. **`df_confirm` correctly captures only Experiment 1 confirmations** — `ver_retract` is set to NA for all non-verification rounds (line 284), so the filter `ver_retract == 0` on line 811 only matches `verify_round == 1 & verification != "fake"`. Experiment 2 rows have `ver_retract = NA` and are excluded. The `!is.na()` filter on line 812 is redundant but harmless.

8. **`signal_adj` flip on line 814 is inconsequential** — `signal_adj` is never referenced in Figures and Tables.R (confirmed via search). The variable exists in saved datasets but does not affect any output.

9. **Table 3 uses 6,093 observations** — Intentional filter `aggregate_round == 0` excludes Experiment 2 aggregate signal rounds. Table 3 analyzes display treatment effects which only vary in Experiment 1.

10. **Table 14/15 use 4,860 observations** — Intentional filter `treat_aggregate_signal == 0` excludes Experiment 2 subjects. These tables analyze how prior verification experience affects updating.

11. **n=590 (figures) vs n=635 (Table 11) for confirmations** — Figures filter with `belief_change_rational_lag1 != 0` (line 199 of Figures and Tables.R), excluding 45 observations at ceiling/floor where Bayesian belief change was zero. Documented in footnote 12 of the paper. Table 11 uses the full sample.
