# UpdatingMisinformation-Analysis

R analysis code for the paper **"Belief Updating with Misinformation"** by Lars Wittrock, Martin Strobel, and Elias Tsakas.

Paper: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4352516

## Project Structure

```
.
├── Preparing Data.R       # Step 1: Load raw CSVs and produce .rda data files
├── Figues and Tables.R    # Step 2: Generate all figures and tables from .rda files
├── raw_data.csv           # Treatment 1 raw data
├── raw_data_extra.csv     # Treatment 2 raw data
├── raw_times.csv          # Treatment 1 timing data
└── raw_times_extra.csv    # Treatment 2 timing data
```

## Running the Code

**Always run in order:**

1. `Preparing Data.R` — set the `path` variable to the repo root, then source the file. This produces `.rda` files used by step 2.
2. `Figues and Tables.R` — set `inpath` (where `.rda` files are) and `outpath` (where to write figures/tables), then source the file.

## R Packages Required

```r
install.packages(c(
  "tidyr", "dplyr", "plotrix", "stringr", "anytime",
  "ggplot2", "ggsci", "gridExtra", "ggpubr", "ggforce",
  "stargazer", "lme4", "estimatr"
))
```

## VS Code Setup (instead of RStudio)

Install the recommended extensions when prompted (see `.vscode/extensions.json`):
- **R** (`REditorSupport.r`) — syntax highlighting, autocompletion, inline output
- **R LSP** (`REditorSupport.r-lsp`) — language server (requires `languageserver` R package)
- **R Debugger** (`rdebugger.r-debugger`) — breakpoints and debugging

Install the `languageserver` package in R once:
```r
install.packages("languageserver")
```

To run code interactively like RStudio:
- `Ctrl+Enter` — send current line/selection to R terminal
- `Ctrl+Shift+S` — source entire file
- Open an R terminal via **Terminal > New Terminal**, then type `R`

## Notes

- The scripts use Windows-style backslash paths (`\\`) in `Figues and Tables.R` for `load()` calls. On Linux/Mac change these to forward slashes `/`.
- Figure DPI is set via `set_dpi <- 400` at the top of `Figues and Tables.R`.
- Table output format (`latex` or `html`) is set via `output_type` at the top of `Figues and Tables.R`.
