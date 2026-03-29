# ──────────────────────────────────────────────────
# Data Loading & Derived Variables
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Purpose: Load processed datasets and create shared derived variables
#          that are needed across multiple analysis files. This eliminates
#          hidden cross-section dependencies.
# Inputs:  data/processed/*.rda
# Outputs: Loaded dataframes with additional derived columns:
#          - df_regular: overreport_avg_id, overreport_variance_id, confirms_prior
#          - df_retract: overreport_avg_id, overreport_variance_id,
#                        prior_aligned, prior_aligned_bin, belief_lag2_bin
#          - df_confirm: prior_aligned, belief_lag2_bin
#          - df_coef: per-subject inference and base_rate_use coefficients
# ──────────────────────────────────────────────────


######################################################
# Reading data
######################################################
cat(">> Loading processed data...\n")

load(file = paste0(inpath, "/data_main.rda"))
load(file = paste0(inpath, "/data_subject.rda"))
load(file = paste0(inpath, "/data_regular.rda"))
load(file = paste0(inpath, "/data_time.rda"))
load(file = paste0(inpath, "/data_retract.rda"))
load(file = paste0(inpath, "/data_confirm.rda"))
rownames(df_confirm) <- NULL  # reset non-sequential row names from subsetting, needed for model_clusters()
load(file = paste0(inpath, "/data_uninformative.rda"))
load(file = paste0(inpath, "/data_informative.rda"))


######################################################
# DERIVED VARIABLES
# Shared data objects extracted from individual sections
# to eliminate hidden cross-section dependencies.
######################################################

# Subject-level over-report avg and variance (originally in fig13)
# Used by: fig13, tab15, tab7
df_reg_subject_stats <- df_regular %>%
  group_by(id) %>%
  summarise(overreport_avg_id = mean(over_report, na.rm = TRUE),
            overreport_variance_id = var(over_report, na.rm = TRUE),
            id = mean(id))
df_regular <- merge(df_regular, df_reg_subject_stats, by = "id")
df_retract <- merge(df_retract, df_reg_subject_stats, by = "id")

# Confirmation bias check variable (originally in fig12)
# Used by: fig12 (me_regular_c regression)
df_regular <- mutate(df_regular, confirms_prior = ifelse(sign(prior_ratio) == sign(signal_ratio), 1, 0))

# Prior aligned with signal direction on df_retract (originally in fig_ret_overreact_prior / tab_ret_prior)
# Used by: tab_ret_prior, fig_ret_prior, fig_ret_overreact_prior, fig_ret_response_prior, tab_ret_persist_prior
df_retract$prior_aligned <- ifelse(df_retract$ball_red_lag1 == 1, df_retract$belief_lag2, 1 - df_retract$belief_lag2)
df_retract$prior_aligned_bin <- cut(df_retract$prior_aligned,
    breaks = prior_bin_breaks, include.lowest = TRUE,
    labels = prior_bin_labels)
df_retract$belief_lag2_bin <- cut(df_retract$prior_aligned,
    breaks = prior_bin_breaks, include.lowest = TRUE,
    labels = prior_bin_labels)
df_retract$belief_lag2_bin <- relevel(factor(df_retract$belief_lag2_bin), ref = prior_bin_ref)

# Prior aligned with signal direction on df_confirm (originally in tab_conf_prior)
# Used by: tab_conf_prior, fig_conf_prior, fig_conf_overreact_prior
df_confirm$prior_aligned <- ifelse(df_confirm$ball_red_lag1 == 1, df_confirm$belief_lag2, 1 - df_confirm$belief_lag2)
df_confirm$belief_lag2_bin <- cut(df_confirm$prior_aligned,
    breaks = prior_bin_breaks, include.lowest = TRUE,
    labels = prior_bin_labels)
df_confirm$belief_lag2_bin <- relevel(factor(df_confirm$belief_lag2_bin), ref = prior_bin_ref)


######################################################
# Per-subject coefficient estimates (originally in fig12)
# Fits the mixed-effects model and extracts per-subject
# inference (d) and base-rate use (c) coefficients.
# Used by: tab7 (retraction types analysis)
######################################################
cat(">> Fitting mixed-effects model for per-subject coefficients...\n")

me_regular <- lmer(obs_log_post_ratio ~ signal_ratio + prior_ratio + (1 + signal_ratio + prior_ratio|id), df_regular)

coef_me <- coef(me_regular)
df_coef <- as.data.frame(coef_me$id)
df_coef$id <- as.numeric(rownames(df_coef))

names(df_coef)[names(df_coef) == "prior_ratio"] <- "base_rate_use"
names(df_coef)[names(df_coef) == "signal_ratio"] <- "inference"
names(df_coef)[names(df_coef) == "(Intercept)"] <- "intercept"

df_coef <- subset(df_coef, select = -c(intercept))

cat(">> Data loading and derived variables complete\n")
