# ──────────────────────────────────────────────────
# Data Loading & Derived Variables
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Purpose: Load the processed datasets and create the shared derived
#          variables needed by the analysis files.
# Inputs:  data/processed/*.rda
# Outputs: Loaded dataframes (df_main, df_regular, df_retract, df_confirm,
#          df_uninformative, df_informative) with additional derived columns:
#          - df_retract: prior_aligned, prior_aligned_bin, belief_lag2_bin
#          - df_confirm: prior_aligned, belief_lag2_bin
# ──────────────────────────────────────────────────


######################################################
# Reading data
######################################################
cat(">> Loading processed data...\n")

load(file = paste0(inpath, "/data_main.rda"))
load(file = paste0(inpath, "/data_regular.rda"))
load(file = paste0(inpath, "/data_retract.rda"))
load(file = paste0(inpath, "/data_confirm.rda"))
rownames(df_confirm) <- NULL  # reset non-sequential row names from subsetting
load(file = paste0(inpath, "/data_uninformative.rda"))
load(file = paste0(inpath, "/data_informative.rda"))


######################################################
# DERIVED VARIABLES
# Signal-aligned prior and prior bins. Computed here so the bin definition
# lives in one place (see prior_bin_breaks / prior_bin_labels in helpers.R).
######################################################

# Prior aligned with signal direction on df_retract
df_retract$prior_aligned <- ifelse(df_retract$ball_red_lag1 == 1,
                                   df_retract$belief_lag2,
                                   1 - df_retract$belief_lag2)
df_retract$prior_aligned_bin <- cut(df_retract$prior_aligned,
    breaks = prior_bin_breaks, include.lowest = TRUE,
    labels = prior_bin_labels)
df_retract$belief_lag2_bin <- relevel(
  factor(cut(df_retract$prior_aligned,
             breaks = prior_bin_breaks, include.lowest = TRUE,
             labels = prior_bin_labels)),
  ref = prior_bin_ref)

# Prior aligned with signal direction on df_confirm
df_confirm$prior_aligned <- ifelse(df_confirm$ball_red_lag1 == 1,
                                   df_confirm$belief_lag2,
                                   1 - df_confirm$belief_lag2)
df_confirm$belief_lag2_bin <- relevel(
  factor(cut(df_confirm$prior_aligned,
             breaks = prior_bin_breaks, include.lowest = TRUE,
             labels = prior_bin_labels)),
  ref = prior_bin_ref)

cat(">> Data loading and derived variables complete\n")
