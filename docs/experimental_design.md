# Experimental design reference

Reference for the experimental design and processed datasets behind *"Belief
updating with misinformation"* (Wittrock, Strobel, Tsakas). For the mapping
from paper tables/figures to output files, see the main `README.md`.

## Two experiments

**Experiment 1** (`raw_data.csv`, `treat_aggregate_signal == 0`) — January 2022.
Subjects see 9 uncertain ball draws + 3 ex-post verification rounds = **12
rounds**. In a verification round subjects learn whether the previous ball was
informative or uninformative (confirmation / retraction). The 3 verification
rounds are 3 consecutive draws; the starting position varies across subjects.

**Experiment 2** (`raw_data_extra.csv`, `treat_aggregate_signal == 1`) —
January 2023. Same design but with **ex-ante** verification: 6 uncertain
signals + 3 signals whose informativeness is revealed immediately = **9
rounds**. Lets us compare ex-post checks with ex-ante labelling.

## Information-display treatments (Experiment 1)

Three sub-treatments vary what is shown on screen (`treat` variable):

| `treat` | Description |
|---|---|
| 1 | **Baseline**: history of signals AND previously reported belief |
| 2 | **No anchor**: history shown, previously reported belief hidden |
| 3 | **No history**: only the most recent signal + previous belief |

## Signal types

| Type | Source | Description |
|---|---|---|
| Regular signal | Both experiments | Ball colour shown, informativeness unknown |
| Retraction     | Experiment 1 | Ex-post: previous ball revealed uninformative |
| Confirmation   | Experiment 1 | Ex-post: previous ball revealed informative |
| Uninformative  | Experiment 2 | Ex-ante: colour + "uninformative" shown together |
| Informative    | Experiment 2 | Ex-ante: colour + "informative" shown together |

## Key parameters

- Prior: p(R) = p(B) = 0.5
- Urn distribution: 3 same-colour + 1 opposite-colour ball
- Probability a signal is informative (α): 0.4
- Informative-signal accuracy (1 − ε): 0.75
- Resulting uncertain-signal strength: π(r|R) = π(b|B) = 0.6

## Sample

849 subjects completed; 96 removed as outliers (pre-registered criteria) →
**753 subjects, 8,397 observations**. By experiment: Exp 1 — 606 completed,
66 removed; Exp 2 — 243 completed, 30 removed.

## Processed datasets (`data/processed/`, built by `00_prepare_data.R`)

| Dataset | N | Content |
|---|---|---|
| `data_main`          | 8,397 | All observations (outliers removed) |
| `data_regular`       | 6,777 | Regular signal rounds |
| `data_retract`       |   985 | Retraction rounds |
| `data_confirm`       |   635 | Confirmation rounds |
| `data_informative`   |   269 | Ex-ante informative signals (Exp 2) |
| `data_uninformative` |   370 | Ex-ante uninformative signals (Exp 2) |

`00_prepare_data.R` also writes `data_subject` and `data_time`; these are not
used by the analysis pipeline.
