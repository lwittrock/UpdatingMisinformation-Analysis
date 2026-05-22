# ──────────────────────────────────────────────────
# 03 — Treatments
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Paper outputs (Section 7):
#   Figure B9  — Observed bias by information-display treatment (Exp 1)
#   Figure B10 — Retraction bias: ex-ante vs ex-post verification
#   Figure B11 — Confirmation bias: ex-ante vs ex-post verification
#
# Ex-ante data comes from Experiment 2 (uninformative / informative signals
# shown immediately); ex-post data from Experiment 1 (retractions /
# confirmations).
# ──────────────────────────────────────────────────


######################################################
# SETUP (auto-loads utilities if run standalone)
######################################################
if (!exists(".utils_loaded")) {
  inpath      <- "data/processed"
  output_type <- "latex"
  set_dpi     <- 400
  source("code/utils/packages.R")
  source("code/utils/constants.R")
  source("code/utils/helpers.R")
  source("code/utils/plot_theme.R")
  source("code/utils/figure_helpers.R")
  source("code/utils/run_control.R")
  .utils_loaded <- TRUE
  source("code/utils/derived_variables.R")
}

.tick("Section 3: Treatments")


######################################################
# SHARED: ex-post bias-by-prior summaries (Experiment 1)
######################################################
ret_bias <- df_retract %>%
  group_by(prior_bin = prior_aligned_bin) %>%
  summarise(Mean = mean(over_report_ret, na.rm = TRUE),
            SD   = sd(over_report_ret,   na.rm = TRUE),
            N    = n(), .groups = "drop")

conf_bias <- df_confirm %>%
  group_by(prior_bin = belief_lag2_bin) %>%
  summarise(Mean = mean(over_report, na.rm = TRUE),
            SD   = sd(over_report,   na.rm = TRUE),
            N    = n(), .groups = "drop")


######################################################
# FIGURE B9 — Observed bias by information-display treatment
#   treat 1: history + anchor   treat 2: history only   treat 3: anchor only
######################################################
if (.should_run("fig_B9")) {
.tick("Figure B9 -- bias by treatment")
tryCatch({

treat_colors <- c("Treatment 1" = "black", "Treatment 2" = "blue", "Treatment 3" = "red")

ret_treats  <- list()
conf_treats <- list()
for (tr in 1:3) {
  nm <- paste0("Treatment ", tr)
  ret_treats[[nm]] <- df_retract[df_retract$treat == tr, ] %>%
    group_by(prior_bin = prior_aligned_bin) %>%
    summarise(Mean = mean(over_report_ret, na.rm = TRUE),
              SD   = sd(over_report_ret,   na.rm = TRUE),
              N    = n(), .groups = "drop")
  conf_treats[[nm]] <- df_confirm[df_confirm$treat == tr, ] %>%
    group_by(prior_bin = belief_lag2_bin) %>%
    summarise(Mean = mean(over_report, na.rm = TRUE),
              SD   = sd(over_report,   na.rm = TRUE),
              N    = n(), .groups = "drop")
}

write_tikz_bias_grouped(ret_treats, unname(treat_colors),
  tikz_path("treatments", "figure_B9_retraction_bias_by_treatment"),
  ylabel = LBL_TIKZ_RET, positive_label = POS_RET)
write_jpg_bias_grouped(ret_treats, unname(treat_colors),
  fig_path("treatments", "figure_B9_retraction_bias_by_treatment"),
  ylabel_text = LBL_JPG_RET, positive_label = POS_RET,
  title = "Retractions — by treatment")

write_tikz_bias_grouped(conf_treats, unname(treat_colors),
  tikz_path("treatments", "figure_B9_confirmation_bias_by_treatment"),
  ylabel = LBL_TIKZ_REG, positive_label = POS_REG)
write_jpg_bias_grouped(conf_treats, unname(treat_colors),
  fig_path("treatments", "figure_B9_confirmation_bias_by_treatment"),
  ylabel_text = LBL_JPG_REG, positive_label = POS_REG,
  title = "Confirmations — by treatment")

}, error = .fail)
}


######################################################
# EX-ANTE bias-by-prior summaries (Experiment 2)
######################################################
df_uninformative$prior_aligned_k <- ifelse(
  df_uninformative$ball_red == 1,
  df_uninformative$belief_lag1,
  1 - df_uninformative$belief_lag1
)
df_uninformative$prior_bin_k <- factor(
  cut(df_uninformative$prior_aligned_k,
      breaks = prior_bin_breaks, include.lowest = TRUE,
      labels = prior_bin_labels),
  levels = prior_bin_labels
)
df_informative$prior_aligned_k <- ifelse(
  df_informative$ball_red == 1,
  df_informative$belief_lag1,
  1 - df_informative$belief_lag1
)
df_informative$prior_bin_k <- factor(
  cut(df_informative$prior_aligned_k,
      breaks = prior_bin_breaks, include.lowest = TRUE,
      labels = prior_bin_labels),
  levels = prior_bin_labels
)

uninf_bias <- df_uninformative %>%
  group_by(prior_bin = prior_bin_k) %>%
  summarise(Mean = mean(over_report, na.rm = TRUE),
            SD   = sd(over_report,   na.rm = TRUE),
            N    = n(), .groups = "drop")
inf_bias <- df_informative %>%
  group_by(prior_bin = prior_bin_k) %>%
  summarise(Mean = mean(over_report, na.rm = TRUE),
            SD   = sd(over_report,   na.rm = TRUE),
            N    = n(), .groups = "drop")


######################################################
# FIGURE B10 — Retraction bias: ex-ante vs ex-post
######################################################
if (.should_run("fig_B10")) {
.tick("Figure B10 -- ex-ante vs ex-post retraction")
tryCatch({

figB10_groups <- list(`Ex ante` = uninf_bias, `Ex post` = ret_bias)
write_tikz_bias_grouped(figB10_groups, c("blue", "black"),
  tikz_path("treatments", "figure_B10_retraction_exante_vs_expost"),
  ylabel = LBL_TIKZ_RET, positive_label = POS_RET)
write_jpg_bias_grouped(figB10_groups, c("blue", "black"),
  fig_path("treatments", "figure_B10_retraction_exante_vs_expost"),
  ylabel_text = LBL_JPG_RET, positive_label = POS_RET,
  title = "Retraction bias — ex ante vs ex post")

}, error = .fail)
}


######################################################
# FIGURE B11 — Confirmation bias: ex-ante vs ex-post
######################################################
if (.should_run("fig_B11")) {
.tick("Figure B11 -- ex-ante vs ex-post confirmation")
tryCatch({

figB11_groups <- list(`Ex ante` = inf_bias, `Ex post` = conf_bias)
write_tikz_bias_grouped(figB11_groups, c("blue", "black"),
  tikz_path("treatments", "figure_B11_confirmation_exante_vs_expost"),
  ylabel = LBL_TIKZ_REG, positive_label = POS_REG)
write_jpg_bias_grouped(figB11_groups, c("blue", "black"),
  fig_path("treatments", "figure_B11_confirmation_exante_vs_expost"),
  ylabel_text = LBL_JPG_REG, positive_label = POS_REG,
  title = "Confirmation bias — ex ante vs ex post")

}, error = .fail)
}
