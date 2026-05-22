# Methodology — experimental design and belief updating

Reference document for the experimental design and the Bayesian framework
behind Wittrock, Strobel & Tsakas, "Belief updating with misinformation."
Section 1 describes the design and the processed datasets; Sections 2 onward
derive Bayes' rule for each signal type, the log-odds regression framework,
and the two ways of measuring signal strength (person-specific vs objective).
For the mapping from paper tables/figures to output files, see `README.md`.

---

## 1. Experimental Design

### 1.1 Two experiments

**Experiment 1** (`raw_data.csv`, `treat_aggregate_signal == 0`) — January 2022.
Subjects see 9 uncertain ball draws + 3 ex-post verification rounds = 12 rounds.
In a verification round subjects learn whether a previous ball was informative
(confirmation) or uninformative (retraction). The 3 verification rounds are 3
consecutive draws; the starting position varies across subjects.

**Experiment 2** (`raw_data_extra.csv`, `treat_aggregate_signal == 1`) — January
2023. Same design but with ex-ante verification: 6 uncertain signals + 3 signals
whose informativeness is revealed immediately = 9 rounds. This lets us compare
ex-post checks with ex-ante labelling.

### 1.2 Information-display treatments (Experiment 1)

Three sub-treatments vary what is shown on screen (`treat` variable):

| `treat` | Description |
|---|---|
| 1 | Baseline: history of signals AND previously reported belief |
| 2 | No anchor: history shown, previously reported belief hidden |
| 3 | No history: only the most recent signal + previous belief |

### 1.3 The urn and the signals

Two equally likely urns (prior = 0.5):

- **Red urn**: 3 red balls + 1 blue ball (gamma = 0.75 red)
- **Blue urn**: 1 red ball + 3 blue balls (gamma = 0.75 blue)

The participant estimates the probability the urn is Red after a sequence of
ball draws. Each draw is **uncertain**: with probability `prob_fake = 0.6` the
ball comes from a 50/50 urn (uninformative); with probability `1 - prob_fake =
0.4` it comes from the actual urn (informative). The participant sees the
colour but not the informativeness.

| Signal type | Source | Description |
|---|---|---|
| Regular signal | Both experiments | Colour shown, informativeness unknown |
| Retraction (`ver_retract = 1`) | Experiment 1 | Ex-post: a previous ball is revealed uninformative — the update should be undone |
| Confirmation (`ver_retract = 0`) | Experiment 1 | Ex-post: a previous ball is revealed informative — it came from the real urn |
| Uninformative | Experiment 2 | Ex-ante: colour + "uninformative" shown together |
| Informative | Experiment 2 | Ex-ante: colour + "informative" shown together |

Section 16 lists every numeric parameter.

### 1.4 Sample

849 subjects completed; 96 were removed as outliers (pre-registered criteria),
leaving **753 subjects and 8,397 observations**. By experiment: Exp 1 — 606
completed, 66 removed; Exp 2 — 243 completed, 30 removed.

### 1.5 Processed datasets

`00_prepare_data.R` builds these into `data/processed/`:

| Dataset | N | Content |
|---|---|---|
| `data_main`          | 8,397 | All observations (outliers removed) |
| `data_regular`       | 6,777 | Regular signal rounds |
| `data_retract`       |   985 | Retraction rounds |
| `data_confirm`       |   635 | Confirmation rounds |
| `data_informative`   |   269 | Ex-ante informative signals (Exp 2) |
| `data_uninformative` |   370 | Ex-ante uninformative signals (Exp 2) |

`00_prepare_data.R` also writes `data_subject` and `data_time`, which the
analysis pipeline does not use.

---

## 2. Signal Likelihoods

### 2.1 Uncertain Signal (Regular Round)

Because each ball draw is informative with probability 0.4 and uninformative with probability 0.6:

```
P(red | Red urn) = (1 - prob_fake) * gamma + prob_fake * 0.5
                 = 0.4 * 0.75 + 0.6 * 0.5
                 = 0.30 + 0.30
                 = 0.60

P(blue | Red urn) = (1 - prob_fake) * (1 - gamma) + prob_fake * 0.5
                  = 0.4 * 0.25 + 0.6 * 0.5
                  = 0.10 + 0.30
                  = 0.40
```

By symmetry:

```
P(red | Blue urn) = 0.40
P(blue | Blue urn) = 0.60
```

In the code these are `prob_r_R = 0.6`, `prob_b_R = 0.4`, `prob_r_B = 0.4`, `prob_b_B = 0.6`.

**Likelihood ratio** of a red signal:

```
LR_red = P(red | Red) / P(red | Blue) = 0.6 / 0.4 = 1.5
```

And for blue:

```
LR_blue = P(blue | Red) / P(blue | Blue) = 0.4 / 0.6 = 2/3 ≈ 0.667
```

Note: `LR_blue = 1 / LR_red`. Red signals are evidence for Red urn, blue signals are evidence for Blue urn.

### 2.2 Confirmed Signal (Known Informative)

If the signal is **known to be informative**, it came from the actual urn, so:

```
P(red | Red urn, informative) = gamma = 0.75
P(red | Blue urn, informative) = 1 - gamma = 0.25
```

**Likelihood ratio** of a confirmed red signal:

```
LR_confirmed_red = gamma / (1 - gamma) = 0.75 / 0.25 = 3
```

A confirmed signal is much more informative (LR = 3) than an uncertain signal (LR = 1.5).

### 2.3 Retracted Signal (Known Uninformative)

A retracted signal carries **no information** about the urn. Its likelihood ratio is 1.

```
LR_retracted = 1
```

---

## 3. Bayes' Rule in Odds Form

### 3.1 Basic Bayes' Rule

For a single signal with likelihood ratio LR, starting from prior P(Red):

```
posterior odds = prior odds * LR

P(Red|signal)     P(Red)
------------ = ---------- * LR
P(Blue|signal)   P(Blue)
```

Multiple independent signals multiply:

```
posterior odds = prior odds * LR_1 * LR_2 * ... * LR_n
```

### 3.2 In Log-Odds (the Regression Framework)

Taking logs, multiplication becomes addition:

```
log(posterior odds) = log(prior odds) + log(LR_1) + log(LR_2) + ... + log(LR_n)
```

Define:

- `obslnpost = log(belief / (1 - belief))` — observed log-posterior-ratio
- `prior_ratio = log(belief_lag1 / (1 - belief_lag1))` — log-prior-ratio
- `signal_ratio = log(signal / (1 - signal))` — log-likelihood-ratio of the signal

For a **perfect Bayesian** processing a single signal from a prior:

```
obslnpost = 1 * signal_ratio + 1 * prior_ratio
```

The regression model relaxes this:

```
obslnpost = c * signal_ratio + d * prior_ratio
```

- **c (inference / signal use)**: weight on new information. Bayesian benchmark = 1. If c > 1, the person over-weights the signal; if c < 1, under-weights.
- **d (base-rate use)**: weight on prior belief. Bayesian benchmark = 1. If d < 1, the person under-weights their prior (beliefs regress toward 0.5); if d > 1, they are too anchored.

---

## 4. Applying Bayes' Rule to Each Signal Type

### 4.1 Regular Uncertain Signal

Person is at `belief_lag1`, sees a red ball (uncertain).

**Bayesian posterior:**
```
posterior = post(belief_lag1, 1, 0, 0, 0, 0, 0)
         = (belief_lag1 * prob_r_R) / (belief_lag1 * prob_r_R + (1 - belief_lag1) * prob_r_B)
         = (belief_lag1 * 0.6) / (belief_lag1 * 0.6 + (1 - belief_lag1) * 0.4)
```

**Log-odds increment:** `log(LR_red) = log(0.6 / 0.4) = log(1.5) ≈ 0.405`

**Objective signal:** 0.6 for red, 0.4 for blue.

### 4.2 Retraction

Person is at `belief_lag1`. They learn that the **previous** ball (say, red) was uninformative.

**What should happen:** The previous red signal had LR = 1.5. Undoing it means dividing the current odds by 1.5. This is equivalent to multiplying by 1/1.5 = LR_blue = 2/3.

In other words: **a retraction of a red signal is equivalent to receiving a blue signal.**

**Bayesian posterior:**
```
posterior = belief_lag2
```

(Return to the belief before the retracted signal. In `00_prepare_data.R`: `posterior_subj = belief_lag2` for retractions.)

**But why is the objective signal "opposite ball" and not "return to belief_lag2"?**

In the log-odds regression framework, we decompose the update into:
- prior = belief_lag1 (where the person currently is)
- signal = the informational content of the retraction

The retraction's informational content IS equivalent to an opposite ball:
```
log(LR_retraction_of_red) = -log(LR_red) = -log(1.5) = log(2/3) ≈ -0.405
log(LR_blue) = log(2/3) ≈ -0.405    (same!)
```

**Objective signal:** 0.4 for retraction of red (= blue ball), 0.6 for retraction of blue (= red ball).

**Numerical example with flat prior (belief_lag2 = 0.5):**

Person saw red ball in round t-1 and correctly updated to 0.6. Now in round t, that red ball is retracted.
- belief_lag1 = 0.6, prior_ratio = log(0.6/0.4) = 0.405
- signal_ratio_obj = log(0.4/0.6) = -0.405
- Bayesian: obslnpost = 0.405 + (-0.405) = 0 → belief = 0.5 ✓ (returns to 0.5)

### 4.3 Confirmation

Person is at `belief_lag1`. They learn that the **previous** ball (say, red) was informative.

**What should happen:** The previous signal was processed as uncertain (LR = 1.5). It should now be treated as confirmed (LR = 3). The incremental information is the ratio:

```
LR_confirmation = LR_confirmed / LR_uncertain = 3 / 1.5 = 2
```

So a confirmation of a red signal provides additional evidence FOR the Red urn with LR = 2.

**In log-odds:**
```
log(LR_confirmation_of_red) = log(3) - log(1.5) = log(2) ≈ 0.693
```

**Objective signal:** 2/3 for confirmation of red, 1/3 for confirmation of blue.

To verify: `log((2/3) / (1/3)) = log(2) ≈ 0.693` ✓

**Bayesian posterior (in the code):**
```
posterior = post(belief_lag2, 0, 0, 0, 0, 1, 0)   [for confirmed red]
```

This computes the posterior starting from belief_lag2 (before the initial signal) and applying a confirmed red signal (using gamma directly). Note this uses belief_lag2, NOT belief_lag1, because the confirmation replaces the uncertain processing entirely.

**But in the regression, we use belief_lag1 as the prior.** This is intentional: we want to measure how people update FROM where they currently are (belief_lag1), given the new information (the confirmation). The objective signal (LR = 2) captures the incremental informational content of the confirmation.

**Numerical example with flat prior (belief_lag2 = 0.5):**

Person saw red ball and correctly updated to 0.6. Now that ball is confirmed.
- belief_lag1 = 0.6, prior_ratio = log(0.6/0.4) = 0.405
- signal_ratio_obj = log(2) = 0.693
- Bayesian: obslnpost = 0.405 + 0.693 = 1.099 → belief = 0.75

Check: post(0.5, 0, 0, 0, 0, 1, 0) = (0.5 * 0.75) / (0.5 * 0.25 + 0.5 * 0.75) = 0.75 ✓

**Numerical example where person over-reacted initially (belief_lag2 = 0.5):**

Person saw red ball but over-reacted, updating to 0.75 instead of 0.6.
- belief_lag1 = 0.75, prior_ratio = log(0.75/0.25) = 1.099
- signal_ratio_obj = log(2) = 0.693
- Bayesian obslnpost = 1.099 + 0.693 = 1.792 → belief = 0.857
- Correct posterior: post(0.5, 0, 0, 0, 0, 1, 0) = 0.75

So Bayes from belief_lag1 (using the incremental signal) predicts 0.857, but the "true" correct answer (from belief_lag2) is 0.75. The person is already at 0.75. If they don't react at all to the confirmation, they're AT the correct posterior by coincidence (because their over-reaction to the initial signal = the confirmation's increment).

**This is why c and d capture different things than raw over/under-reaction:** The regression measures updating from the person's current state (belief_lag1), regardless of whether that state is "correct." A person who over-reacted to the initial signal has an inflated prior, and the regression separates how much of their new belief comes from the signal vs the prior.

---

## 5. Summary of Objective Signals

| Signal type | Event | Objective signal | LR | log(LR) |
|---|---|---|---|---|
| Regular red | See uncertain red ball | 0.6 | 1.5 | 0.405 |
| Regular blue | See uncertain blue ball | 0.4 | 2/3 | -0.405 |
| Retraction of red | Learn red was uninformative | 0.4 | 2/3 | -0.405 |
| Retraction of blue | Learn blue was uninformative | 0.6 | 1.5 | 0.405 |
| Confirmation of red | Learn red was informative | 2/3 | 2 | 0.693 |
| Confirmation of blue | Learn blue was informative | 1/3 | 1/2 | -0.693 |

Key observations:
- A retraction has the **same LR as an opposite-colored ball** (LR = 2/3 for retraction of red = LR of a blue ball).
- A confirmation has a **stronger signal** (LR = 2) than a regular ball (LR = 1.5) or a retraction (LR = 2/3), because it resolves more uncertainty.
- Retraction and regular signals have |log(LR)| = 0.405. Confirmations have |log(LR)| = 0.693.

---

## 6. The `signal_memory` Variable

The variable `signal_memory` in `00_prepare_data.R` stores these objective signal values. It is defined as the probability P(signal indicates Red urn) — i.e., the signal expressed as a probability rather than a likelihood ratio.

Conversion to log-likelihood-ratio:
```
signal_ratio_obj = log(signal_memory / (1 - signal_memory))
```

This is the variable used in the regression as the objective signal.

---

## 7. Person-Specific vs Objective Signals

### 7.1 Person-Specific Signal (`signal_subj`)

Computed via `added_info(belief_lag_adj, post_subj_adj)`:

```
signal_subj = (posterior - prior * posterior) / (posterior + prior - 2 * prior * posterior)
```

This backs out: "what signal value s would produce the Bayesian posterior, given this person's actual prior?" It is person-specific because `posterior_subj` depends on the person's own belief history.

For regular signals: `posterior_subj = post(belief_lag1, 1, 0, ...)`, so signal_subj varies by person because belief_lag1 varies.

For retractions: `posterior_subj = belief_lag2`, so signal_subj captures the gap between belief_lag1 and belief_lag2.

For confirmations: `posterior_subj = post(belief_lag2, 0, 0, 0, 0, 1, 0)`, so signal_subj captures the gap between belief_lag1 and the correct confirmed-signal posterior.

### 7.2 Objective Signal (`signal_memory`)

Fixed values based on the informational content of each signal type (see Section 5). Same for all people because it depends only on the experimental parameters (gamma, prob_fake), not on individual beliefs.

### 7.3 When They Coincide

For a **perfect Bayesian**, signal_subj = signal_memory. The person-specific signal equals the objective signal because their beliefs are always correct, so the backed-out signal matches the true signal.

For real participants, signal_subj ≠ signal_memory because their beliefs deviate from Bayesian predictions.

### 7.4 Trade-offs

**Person-specific signal (signal_subj):** Captures the exact informational content of the signal given where the person actually is. The regression `obslnpost = c * signal_ratio + d * prior_ratio` is a tautological decomposition (c = 1, d = 1 by construction for a Bayesian). Deviations of c and d from 1 reflect how the person's actual update differs from the Bayesian update, conditional on their own prior.

**Objective signal (signal_memory):** The same for everyone, so variation in signal_ratio_obj comes only from signal direction (red vs blue) and type (regular/retraction/confirmation). This is a structural test: given that everyone faces the same objective informational content, how much weight do they place on it (c) vs their prior (d)?

---

## 8. The `post()` Function

The function in `00_prepare_data.R` computes the Bayesian posterior in two stages:

```r
post(prior, num_red, num_blue, num_red_ret, num_blue_ret, num_red_conf, num_blue_conf)
```

**Stage 1 — Uncertain signals (non-retracted):**
```
num_red_nonretracted = num_red - num_red_ret
num_blue_nonretracted = num_blue - num_blue_ret

posterior_temp = (prior * prob_r_R^num_red_nr * prob_b_R^num_blue_nr) /
                (prior * prob_r_R^num_red_nr * prob_b_R^num_blue_nr +
                 (1-prior) * prob_r_B^num_red_nr * prob_b_B^num_blue_nr)
```

This applies Bayes' rule with uncertain signal likelihoods (prob_r_R = 0.6 etc.) for all non-retracted signals. Retracted signals are excluded (their LR = 1, so they cancel out).

**Stage 2 — Confirmed signals:**
```
posterior = (posterior_temp * gamma^num_red_conf * (1-gamma)^num_blue_conf) /
           (posterior_temp * gamma^num_red_conf * (1-gamma)^num_blue_conf +
            (1-posterior_temp) * (1-gamma)^num_red_conf * gamma^num_blue_conf)
```

This applies Bayes' rule with confirmed signal likelihoods (gamma = 0.75 for same-color-as-urn). Confirmed signals use gamma directly because we know they came from the real urn.

**Why two stages?** Non-retracted uncertain signals and confirmed signals have different likelihood ratios. Uncertain signals use `prob_r_R / prob_r_B = 1.5`. Confirmed signals use `gamma / (1-gamma) = 3`. The function separates them to apply the correct LR to each group.

---

## 9. Deriving the `added_info()` Formula

The function `added_info(prior, post)` computes what signal value s would make the posterior equal to `post` given the prior.

Starting from Bayes' rule with a single signal of strength s (where s = P(signal | Red urn)):

```
post = (prior * s) / (prior * s + (1 - prior) * (1 - s))
```

Solving for s:

```
post * (prior * s + (1 - prior)(1 - s)) = prior * s
post * prior * s + post * (1 - prior) - post * (1 - prior) * s = prior * s
post * (1 - prior) = prior * s - post * prior * s + post * (1 - prior) * s
post * (1 - prior) = s * (prior - post * prior + post * (1 - prior))
post * (1 - prior) = s * (prior - post * prior + post - post * prior)
post * (1 - prior) = s * (prior + post - 2 * post * prior)

s = (post - prior * post) / (post + prior - 2 * post * prior)
```

This is exactly the `added_info()` function. It inverts Bayes' rule to extract the implied signal.

---

## 10. Interpreting c and d Across Signal Types

These are the estimated coefficients from `obslnpost = c * signal_ratio_obj + d * prior_ratio`. The Bayesian benchmark is c = 1, d = 1 in all cases. These numbers describe the decomposition; causal interpretations require care and should be cross-checked against other results (e.g., belief change analyses, over-report regressions).

### Regular signals (benchmark)
- c ≈ 1.53, d ≈ 0.70
- Both effects (signal over-weighting, prior under-weighting) are well-documented in the belief updating literature

### Retractions (objective signal)
- c ≈ 0.82, d ≈ 0.71
- c < 1 suggests the retraction signal receives less weight than Bayes prescribes, while d is similar to regular signals
- **Caveat:** the c < 1 finding may partly reflect properties of the log-odds decomposition rather than a clear-cut "retractions are less effective" story. The relationship between the log-odds c/d decomposition and probability-space belief change deserves care when interpreting (see the retraction-vs-opposite-signal comparison, paper Section 6 / Figure 7).

### Confirmations (objective signal)
- c ≈ 1.45, d ≈ 0.58
- c > 1 and d < 1 follow a qualitatively similar pattern to regular signals (over-weigh signal, under-weigh prior), but d is notably lower (0.58 vs 0.70)
- **Caveat:** over-reporting tends to persist through confirmations. How this persistence relates to c > 1 and d < 1 in the log-odds decomposition is not immediately obvious. The low d could partly explain persistence (prior biases carry through at reduced but nonzero weight), but the interplay between signal over-weighting and prior under-weighting in producing the net effect needs care.
- **Open question:** Why is d lower for confirmations (0.58) than for regular signals (0.70) and retractions (0.71)? Possible explanations include: the verification event disrupting confidence in the current belief, collinearity between signal and prior (both push in the same direction for confirmations), or a genuine behavioral difference. Disentangling these requires further work.

---

## 11. Relationship Between Log-Odds Decomposition and Over-Report

The variable `over_report = belief - posterior_subj` measures the raw deviation from the Bayesian benchmark. This is related to but distinct from the c/d decomposition:

- `over_report` is in **probability space** and measures total deviation from the Bayesian posterior.
- The c/d regression is in **log-odds space** and decomposes the observed belief into signal-weighting (c) and prior-weighting (d) channels.

These two perspectives can yield results that seem superficially contradictory (e.g., c > 1 alongside persistent over-reporting). This is because the c/d decomposition separates two channels that jointly determine the total belief, while over-report captures the net outcome. A full reconciliation requires computing the implied over-report from the c/d estimates for specific prior values and comparing against the observed patterns.

**Key mapping:** For a person with prior_ratio = p and signal_ratio_obj = s:
- Bayesian belief (log-odds): s + p
- Estimated belief (log-odds): c * s + d * p
- Deviation (log-odds): (c - 1) * s + (d - 1) * p

The deviation is positive (over-report toward signal) when the signal over-weighting `(c - 1) * s` exceeds the prior under-weighting `(1 - d) * p`, and negative otherwise. Since both s and p vary across observations, the average over-report depends on their joint distribution in the data.

---

## 12. Two Posterior Benchmarks

The code computes two distinct Bayesian posteriors:

### 12.1 Aggregate posterior (`posterior_agg`)
```
posterior_agg = post(prior_R, red_balls, blue_balls, red_balls_ret, blue_balls_ret, red_balls_conf, blue_balls_conf)
```
Starts from the objective prior (0.5) and processes ALL signals seen so far. This is the "fully rational from scratch" benchmark. Used for computing aggregate-level deviations.

### 12.2 Subject-conditional posterior (`posterior_subj`)
```
posterior_subj = post(belief_lag1, ...)   [for regular and aggregate rounds]
posterior_subj = belief_lag2              [for retractions]
posterior_subj = post(belief_lag2, 0, 0, 0, 0, 1, 0)  [for confirmations]
```
Starts from the person's own reported belief and applies only the current round's signal. This is the "one-step rational" benchmark: given where you actually are, what should you believe after this signal? Used for computing `over_report` and `signal_subj`.

**Why posterior_subj uses belief_lag2 for verifications:** For retractions, the correct action is to return to before the retracted signal (belief_lag2). For confirmations, the correct action is to start from before the initial signal (belief_lag2) and apply a confirmed signal — because the confirmation replaces the uncertain processing entirely.

---

## 13. Boundary Adjustments

Beliefs of exactly 0 or 1 produce undefined log-odds (log(0) or log(∞)). Following Benjamin (2019), extreme values are clipped:

```
belief_adj:     0 → 0.001,  1 → 0.999
belief_lag_adj: 0 → 0.001,  1 → 0.999
post_subj_adj:  0 → 0.001,  1 → 0.999
```

All log-odds variables (`obslnpost`, `prior_ratio`, `signal_ratio`, `truelnpost`) are computed from these adjusted values.

---

## 14. Log-Odds Additivity — Why This Framework Works

The central property that makes the regression framework valid is that **Bayes' rule is additive in log-odds**:

```
log-posterior-odds = log-prior-odds + log-likelihood-ratio
```

This holds exactly, regardless of the prior value. A signal with LR = 1.5 always adds log(1.5) ≈ 0.405 to the log-odds, whether the prior is 0.3, 0.5, or 0.9. This is why we can meaningfully regress `obslnpost` on `signal_ratio + prior_ratio` and interpret the coefficients as weights.

In probability space, this additivity does NOT hold: the same signal produces different absolute belief changes depending on the prior (larger changes near 0.5, smaller near extremes). This is why the analysis is conducted in log-odds, not in raw probabilities.

**Implication for objective signals:** Because log-odds are additive, the objective signal log(LR) is the correct measure of informational content regardless of the person's prior. This justifies using the same `signal_ratio_obj` for everyone.

---

## 15. Sign Conventions and Direction Alignment

### 15.1 Signal direction

All signals are expressed as evidence for/against the **Red urn**:
- Positive signal_ratio → evidence for Red
- Negative signal_ratio → evidence for Blue (= against Red)

A retraction of a red ball has negative signal_ratio (evidence against Red), which makes sense: removing evidence for Red is equivalent to adding evidence for Blue.

### 15.2 Over-report sign convention

`over_report = belief - posterior_subj` can be positive or negative depending on the direction of deviation.

For **retractions**, the analysis uses `over_report_ret = -over_report * sign(ball_red_lag1)` or similar direction-aligned transformations, so that positive values always mean "belief biased toward the retracted signal." Check the specific variable definitions in context — the sign convention differs across tables.

For **confirmations**, `over_report` is used directly in some tables, and in direction-aligned form in others (e.g., `belief_dist_signal_subj`).

---

## 16. Parameter Summary

| Symbol | Value | Meaning |
|---|---|---|
| gamma | 0.75 | Fraction of same-colored balls in urn |
| prob_fake | 0.60 | Probability a signal is uninformative |
| prior_R | 0.50 | Prior probability urn is Red |
| prob_r_R | 0.60 | P(red signal \| Red urn) — uncertain signal |
| prob_b_R | 0.40 | P(blue signal \| Red urn) — uncertain signal |
| prob_r_B | 0.40 | P(red signal \| Blue urn) — uncertain signal |
| prob_b_B | 0.60 | P(blue signal \| Blue urn) — uncertain signal |
| LR_uncertain | 1.5 | Likelihood ratio of uncertain red signal |
| LR_confirmed | 3.0 | Likelihood ratio of confirmed red signal |
| LR_retracted | 1.0 | Likelihood ratio of retracted signal (no info) |
| LR_retraction_step | 2/3 | Incremental LR when retracting a red signal |
| LR_confirmation_step | 2.0 | Incremental LR when confirming a red signal |
