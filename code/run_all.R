# ──────────────────────────────────────────────────
# Pipeline Orchestrator
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Purpose: Run the full analysis pipeline (figures and tables)
# Usage:   source("code/run_all.R")
#
# PREREQUISITES: Run 00_prepare_data.R first to create processed datasets.
# ──────────────────────────────────────────────────


######################################################
# CONFIGURATION
######################################################

# SELECTIVE EXECUTION:
# Set run_sections to choose what to run. Options:
#   "all"      -- run everything (default)
#   "figures"  -- only figures
#   "tables"   -- only tables
#   c("fig_5", "tab_1", ...) -- specific items
# Section names: tab_1, tab_B1, tab_B2, fig_5, fig_6, fig_7, fig_B1, fig_B2,
#                fig_B4, fig_B6, fig_B7, fig_B8, fig_B9, fig_B10, fig_B11
run_sections <- "all"  # Reset to "all" before committing.

# File locations relative to project root
inpath <- "data/processed"

# Table output format
output_type <- "latex"  # can be set to 'html' or 'latex'

# Figure quality
set_dpi <- 400


######################################################
# LOAD UTILITIES
######################################################
source("code/utils/packages.R")
source("code/utils/constants.R")
source("code/utils/helpers.R")
source("code/utils/plot_theme.R")
source("code/utils/figure_helpers.R")
source("code/utils/run_control.R")
.utils_loaded <- TRUE

cat(">> Pipeline started\n")


######################################################
# LOAD DATA & DERIVED VARIABLES
######################################################
source("code/utils/derived_variables.R")


######################################################
# ANALYSIS
######################################################
source("code/01_main_results.R")
source("code/02_dynamics.R")
source("code/03_treatments.R")


######################################################
# SUMMARY
######################################################
.print_summary()
