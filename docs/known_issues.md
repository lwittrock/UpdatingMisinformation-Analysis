# Known issues

## Code vs. paper — intentional discrepancies

The three points below are places where the replication code intentionally
differs from the numbers as printed in the paper. In every case the code is
internally consistent with the paper's stated methodology (Section 4.2:
Grether regressions estimated **without an intercept**); the printed paper has
small inconsistencies introduced during revisions, and its appendix should be
updated to match.

### Table B1 — estimated without an intercept

The paper's printed Table B1 (c and d by period block) was generated **with**
an intercept, whereas Table 1, Table B2, and the paper's Section 4.2 all use
**no intercept**. This package estimates Table B1 with no intercept, for
consistency.

Effect: most cells move by ≤ 0.01; the early-retraction cell moves most
(c 0.635 → 0.674, d 1.136 → 1.205). No sign or qualitative conclusion changes.
Restoring an intercept in `run_cd_by()` reproduces the printed paper exactly.

### Table B2 — H(0,0) baseline definition

Profile H(0,0) is "regular signals with no verification of any type seen so
far". This package defines it as `cum_ret == 0 & cum_conf == 0`, the
consistent extension of the other five profiles (which match the paper
exactly).

The paper's printed H(0,0) (c = 0.601, d = 1.540) was produced by a separate
code path that filtered on retractions only (`cum_ret == 0`, ignoring prior
confirmations). The methodologically correct value is c = 0.436, d = 1.660.
The other five profiles (H(1,0) … H(1,1)) are unaffected and match the paper.

### Table 1 — Initial column standard errors

The paper's printed Table 1 has the Prior and Signal standard errors
**transposed in the Initial column**: it prints Prior (0.051) / Signal
(0.027), whereas the correct, code-generated values are Prior (0.027) /
Signal (0.052). Point estimates and the other two columns are correct. This
package outputs the correct values.

## Implementation notes

### Theory-prediction curves are not generated

The bias-by-prior figures (Figures 5, 6, 7, B2, B4, …) contain the observed
**data bars only**. The red model-prediction curves shown in the paper are
added by hand in the paper's LaTeX. Each generated TikZ figure carries a
`% ADD THEORY CURVE BELOW` placeholder marking where the curve is inserted.

### stargazer 5.2.3 compatibility patch

`code/utils/bootstrap.R` patches stargazer 5.2.3 for R ≥ 4.2 (the released
stargazer fails on newer R). This is applied automatically when the pipeline
loads.
