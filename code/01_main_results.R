# ──────────────────────────────────────────────────
# 01 — Main Results
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Paper outputs (Section 5.1-5.3, Section 6):
#   Table 1  — Grether regression for the three signal types
#   Figure 5 — Bias when processing retractions, by prior
#   Figure 6 — Bias when processing confirmations, by prior
#   Figure 7 — Bias when processing a signal of the opposite colour
#   Figure B1 — Initial-signal bias vs retraction bias (scatter)
#   Figure B2 — Initial-signal bias vs retraction bias, by prior
#
# Bias TikZ figures contain the observed data bars only; the red model
# prediction curves are added by hand in the paper (see % ADD THEORY CURVE).
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

.tick("Section 1: Main Results")


######################################################
# SHARED: bias-by-prior summaries (cheap; computed once)
######################################################

# Retraction bias: over_report_ret > 0 => belief still biased toward the
# initial signal (underreaction / continued influence effect).
ret_bias <- df_retract %>%
  group_by(prior_bin = prior_aligned_bin) %>%
  summarise(Mean = mean(over_report_ret, na.rm = TRUE),
            SD   = sd(over_report_ret,   na.rm = TRUE),
            N    = n(), .groups = "drop")

# Confirmation bias: over_report > 0 => overreaction to the confirmation.
conf_bias <- df_confirm %>%
  group_by(prior_bin = belief_lag2_bin) %>%
  summarise(Mean = mean(over_report, na.rm = TRUE),
            SD   = sd(over_report,   na.rm = TRUE),
            N    = n(), .groups = "drop")

# Initial-signal bias measured on the retraction sample (the bias to the
# signal that is later retracted): over_report_lag1.
init_bias <- df_retract %>%
  group_by(prior_bin = prior_aligned_bin) %>%
  summarise(Mean = mean(over_report_lag1, na.rm = TRUE),
            SD   = sd(over_report_lag1,   na.rm = TRUE),
            N    = n(), .groups = "drop")

# Opposite-signal bias: second ball opposite colour to the first.
# Prior aligned with the FIRST signal direction (belief_lag2), mirroring the
# retraction analysis. -over_report2 > 0 => belief biased toward initial signal.
df_main_ver <- df_main[!is.na(df_main$two_balls), ]
df_opp      <- df_main_ver[df_main_ver$two_balls %in% c("BR", "RB"), ]
df_opp$prior_aligned_opp <- ifelse(
  df_opp$prev_ball == 1,
  df_opp$belief_lag2,
  1 - df_opp$belief_lag2
)
df_opp$prior_bin_opp <- factor(
  cut(df_opp$prior_aligned_opp,
      breaks = prior_bin_breaks, include.lowest = TRUE,
      labels = prior_bin_labels),
  levels = prior_bin_labels
)
df_opp$bias_opp <- -df_opp$over_report2
opp_bias <- df_opp %>%
  group_by(prior_bin = prior_bin_opp) %>%
  summarise(Mean = mean(bias_opp, na.rm = TRUE),
            SD   = sd(bias_opp,   na.rm = TRUE),
            N    = n(), .groups = "drop")


######################################################
# TABLE 1 — Grether regression for the three signal types
# Initial signals use signal_ratio; retractions and confirmations use
# signal_ratio_obj (the objective LLR fixed by design). For regular signals
# the two are identical. No intercept, per paper Section 4.2.
######################################################
if (.should_run("tab_1")) {
.tick("Table 1 -- Grether regressions")
tryCatch({

df_regular$signal <- df_regular$signal_ratio
df_retract$signal <- df_retract$signal_ratio_obj
df_confirm$signal <- df_confirm$signal_ratio_obj

ols_initial <- lm(obs_log_post_ratio ~ 0 + prior_ratio + signal, df_regular)
ols_retract <- lm(obs_log_post_ratio ~ 0 + prior_ratio + signal, df_retract)
ols_confirm <- lm(obs_log_post_ratio ~ 0 + prior_ratio + signal, df_confirm)

write_stargazer(
  ols_initial, ols_retract, ols_confirm,
  se = list(starprep(ols_initial, clusters = df_regular$id)[[1]],
            starprep(ols_retract, clusters = df_retract$id)[[1]],
            starprep(ols_confirm, clusters = df_confirm$id)[[1]]),
  type = output_type,
  dep.var.labels = "Observed Log-Posterior-Ratio",
  column.labels  = c("Initial", "Retractions", "Confirmations"),
  covariate.labels = c("Prior", "Signal"),
  model.numbers = TRUE,
  no.space = TRUE,
  notes = "SEs clustered by subject.",
  out = tab_path("main", "table_1_grether_regressions"))

}, error = .fail)
}


######################################################
# FIGURE 5 — Bias when processing retractions, by prior
######################################################
if (.should_run("fig_5")) {
.tick("Figure 5 -- Retraction bias by prior")
tryCatch({

write_tikz_bias(ret_bias, tikz_path("main", "figure_5_retraction_bias"),
  ylabel = LBL_TIKZ_RET, positive_label = POS_RET)
write_jpg_bias(ret_bias, fig_path("main", "figure_5_retraction_bias"),
  ylabel_text = LBL_JPG_RET, positive_label = POS_RET)

}, error = .fail)
}


######################################################
# FIGURE 6 — Bias when processing confirmations, by prior
######################################################
if (.should_run("fig_6")) {
.tick("Figure 6 -- Confirmation bias by prior")
tryCatch({

write_tikz_bias(conf_bias, tikz_path("main", "figure_6_confirmation_bias"),
  ylabel = LBL_TIKZ_REG, positive_label = POS_REG)
write_jpg_bias(conf_bias, fig_path("main", "figure_6_confirmation_bias"),
  ylabel_text = LBL_JPG_REG, positive_label = POS_REG)

}, error = .fail)
}


######################################################
# FIGURE 7 — Bias when processing a signal of the opposite colour
# Two-series overlay: retraction bias vs opposite-signal bias.
######################################################
if (.should_run("fig_7")) {
.tick("Figure 7 -- Opposite-signal bias by prior")
tryCatch({

fig7_groups <- list(Retraction = ret_bias, `Opposite signal` = opp_bias)
write_tikz_bias_grouped(fig7_groups, c("red", "blue"),
  tikz_path("main", "figure_7_opposite_signal"),
  ylabel = LBL_TIKZ_RET, positive_label = POS_RET)
write_jpg_bias_grouped(fig7_groups, c("red", "blue"),
  fig_path("main", "figure_7_opposite_signal"),
  ylabel_text = LBL_JPG_RET, positive_label = POS_RET,
  title = "Retraction vs opposite-colour signal")

}, error = .fail)
}


######################################################
# FIGURE B1 — Initial-signal bias vs retraction bias (scatter)
######################################################
if (.should_run("fig_B1")) {
.tick("Figure B1 -- Initial vs retraction bias scatter")
tryCatch({

# Restrict to make graph clearer (~43 obs omitted)
df_ret_robust <- df_retract[df_retract$over_report <= 0.5
                                     & df_retract$over_report >= -0.5
                                     & df_retract$over_report_lag1 <= 0.5
                                     & df_retract$over_report_lag1 >= -0.5, ]

df_ret_robust$over_report_lag1_pts <- df_ret_robust$over_report_lag1 * 100
df_ret_robust$under_report_pts     <- -df_ret_robust$over_report * 100

fig_b1 <- ggplot(df_ret_robust, aes(x = over_report_lag1_pts, y = under_report_pts)) +
  geom_jitter(alpha = 0.3) +
  geom_abline(slope = 0, intercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_smooth(method = 'lm', formula = y ~ x, se = TRUE) +
  xlab("Reaction to initial signal (comp. to Bayesian)") +
  ylab("Belief biased towards initial signal (%pts)") +
  theme_classic()

ggsave(fig_path("main", "figure_B1_initial_vs_retraction_bias"),
       plot = fig_b1, width = fig_width, height = fig_height,
       units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# FIGURE B2 — Initial-signal bias vs retraction bias, by prior
# Two-series overlay (initial signal vs retraction).
######################################################
if (.should_run("fig_B2")) {
.tick("Figure B2 -- Initial-signal vs retraction bias by prior")
tryCatch({

figB2_groups <- list(`Initial signal` = init_bias, Retraction = ret_bias)
write_tikz_bias_grouped(figB2_groups, c("blue", "red"),
  tikz_path("main", "figure_B2_initial_signal_bias"),
  ylabel = LBL_TIKZ_RET, positive_label = POS_RET)
write_jpg_bias_grouped(figB2_groups, c("blue", "red"),
  fig_path("main", "figure_B2_initial_signal_bias"),
  ylabel_text = LBL_JPG_RET, positive_label = POS_RET,
  title = "Initial-signal bias vs retraction bias")

}, error = .fail)
}
