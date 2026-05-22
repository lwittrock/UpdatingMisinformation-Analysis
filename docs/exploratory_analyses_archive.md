# Archive — exploratory analyses removed during cleanup

During the final cleanup the analysis pipeline was reduced to exactly the
outputs that appear in the published paper (Table 1, B1, B2 and Figures 5–7,
B1, B2, B4, B6–B11). A large body of exploratory and robustness analysis
produced during the revision was removed.

This file is a condensed record of *what* was explored and *why* it was cut.
The full code for every item below remains recoverable from git history
(the pre-cleanup commit and the files `01_analysis_regular.R`,
`02_analysis_retract.R`, `03_analysis_confirm.R`, `04_additional_analysis.R`).

## Regular-signal background analyses (former `01_analysis_regular.R`)

Belief-vs-posterior calibration, per-subject inference/base-rate histograms
from a mixed-effects model, treatment heterogeneity of regular updating,
over-report distributions, response-time by input type, log-likelihood-ratio
regressions, and belief-change regressions. **Cut:** the final paper presents
regular signals only as the "Initial" column of Table 1; none of these
stand-alone background results are referenced.

## Retraction robustness analyses (former `02_analysis_retract.R`)

Reaction-by-initial-response figures, retraction-vs-opposite-ball and
retraction-vs-uninformative comparisons, belief-change scatter, compressed-
history regressions, induced-prior tests, subject-type heterogeneity,
persistence-by-prior-bin regressions, and over-report-by-prior /
over-report-by-log-odds figures. **Cut:** superseded by the bias-by-prior
figures (Fig 5, 7, B2) and the period/profile splits (Fig B4, B7), which
present the same mechanism more directly.

## Confirmation analyses (former `03_analysis_confirm.R`)

All-reactions and by-initial-reaction figures, confirmation-vs-informative
comparisons, treatment heterogeneity, signal-history and prior-heterogeneity
regressions, and over-report-by-prior / by-log-odds figures. **Cut:** the
paper presents confirmations via Table 1 and the bias-by-prior figures
(Fig 6, B4, B8).

## Exploratory c/d cuts (former `04_additional_analysis.R`)

`04_additional_analysis.R` was a temporary scratch file for the revision. Its
paper-relevant groups were folded into `01`–`03`; the following exploratory
groups were dropped:

- **Frequency table** of rounds × prior bins (descriptive only).
- **c and d by individual round** — too granular; superseded by the
  period-block split (Table B1).
- **c and d by prior bin** — exploratory; the paper uses bias-by-prior
  figures instead of per-bin c/d estimates.
- **c and d by period block × prior group** — a 2-D heat-map disentangling
  period and prior; did not make the paper.
- **c and d / bias bars by number of previous verifications (0/1/2)** —
  superseded by the by-verification-*profile* split (Table B2, Fig B6–B8).
- **Markdown overview generator** — an internal VS Code preview aid.
