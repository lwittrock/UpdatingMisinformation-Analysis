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
#   c("fig5", "fig6", "tab1", ...) -- specific items
# Dependencies are handled automatically (e.g. fig16 needs fig8 needs fig7).
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
source("code/01_analysis_regular.R")
source("code/02_analysis_retract.R")
source("code/03_analysis_confirm.R")


######################################################
# SUMMARY
######################################################
.print_summary()
