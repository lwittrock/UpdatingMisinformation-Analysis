# ──────────────────────────────────────────────────
# Regular Signals / Background Analysis
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Purpose: Generate figures and tables for regular (non-verification) signals
# Inputs:  Processed datasets (loaded by derived_variables.R)
# Outputs: output/regular/figures/*.jpg, output/regular/tables/*.tex
# ──────────────────────────────────────────────────

# Standalone support: load utils if not already loaded by run_all.R
if (!exists(".utils_loaded")) {
  source("code/utils/packages.R")
  source("code/utils/constants.R")
  output_type <- "latex"
  set_dpi <- 400
  source("code/utils/helpers.R")
  source("code/utils/plot_theme.R")
  source("code/utils/run_control.R")
  if (!exists("run_sections")) run_sections <- "all"
  inpath <- "data/processed"
  source("code/utils/derived_variables.R")
}

cat(">> Section 1: Regular Signals\n")


# --- Paper ---

######################################################
# TABLE 1
######################################################
if (.should_run("tab1")) {
.tick("Table 1")
tryCatch({

# Regression
ols_belief_post_subj <- lm(belief ~ posterior_subj, df_main)

# Table with overview
write_stargazer(ols_belief_post_subj,
          se = starprep(ols_belief_post_subj, clusters = df_main$id),
          type = output_type,
          style = "default",
          dep.var.labels = c("Reported Belief"),
          covariate.labels = c("Constant", "Bayesian Posterior", "Aggregate Posterior"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "OLS Regression Output",
          notes = "SEs clustered by subject.",
          out = tab_path("regular", "tab_belief_vs_posterior"))

}, error = .fail)
}


######################################################
# FIGURE 12 AND TABLE 2
######################################################
if (.should_run("fig12")) {
.tick("Figure 12 & Table 2")
tryCatch({

# Estimating Inference and Base Rate Use
######################################################

# Regressions
# me_regular is already fitted in derived_variables.R
ols_regular <- lm(obs_log_post_ratio ~ signal_ratio + prior_ratio, df_regular)
me_regular_c <- lmer(obs_log_post_ratio ~ signal_ratio + prior_ratio + signal_ratio:confirms_prior + (1 + signal_ratio + prior_ratio|id), df_regular)

# Table with overview
write_stargazer(ols_regular, me_regular, me_regular_c,
          se = starprep(ols_regular, clusters = df_regular$id),
          type = output_type,
          style = "default",
          dep.var.labels = c("Observed Log-Posterior-Ratio"),
          covariate.labels = c("Constant", "Signal", "Prior", "Signal Confirms Prior"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Updating with Regular Signals",
          notes = "SEs clustered by subject.",
          out = tab_path("regular", "tab_updating"))


# Distribution of coefficients estimated per subject separately
######################################################

# Plotting (df_coef created in derived_variables.R)
fig_regular_inference <- ggplot(df_coef, aes(x = inference)) +
  geom_histogram(binwidth = 0.1, fill = "white", color = "black") +
  xlab("Estimated Inference (d) per Subject") +
  ylab("Count") +
  ggtitle("Inference Bias") +
  theme_classic()

fig_regular_baserate <- ggplot(df_coef, aes(x = base_rate_use)) +
  geom_histogram(binwidth = 0.05, fill = "white", color = "black") +
  xlab("Estimated Base-Rate Use (c) per Subject") +
  ylab("Count") +
  ggtitle("Base-Rate Use") +
  theme_classic()

fig_regular_updating <- ggarrange(fig_regular_inference, fig_regular_baserate, ncol = 2, nrow = 1)
ggsave(fig_path("regular", "fig_inference_baserate"), plot = fig_regular_updating, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# TABLE 2
######################################################
if (.should_run("tab2")) {
.tick("Table 2")
tryCatch({

# above

}, error = .fail)
}


######################################################
# TABLE 3
######################################################
if (.should_run("tab3")) {
.tick("Table 3")
tryCatch({

# Regression
ols_regular_treat <- lm(obs_log_post_ratio ~ signal_ratio*factor(treat) + prior_ratio*factor(treat), df_regular[df_regular$aggregate_round==0,])

# Table with overview
write_stargazer(ols_regular_treat,
          se = starprep(ols_regular_treat, clusters = df_regular[df_regular$aggregate_round==0,]$id),
          type = output_type,
          style = "default",
          dep.var.labels = c("Observed Log-Posterior-Ratio"),
          column.labels = c("Benchmark", "No prev. belief", "No history"),
          covariate.labels = c("Constant", "Treat: No prev. belief", "Treat: No history", "Signal", "Signal * Treat: No prev. belief", "Signal * Treat: No history", "Prior", "Prior * Treat: No prev. belief", "Prior * Treat: No history"),
          intercept.bottom = FALSE,
          order = c(1,3,4,2,6,7,5,8,9),
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Updating with Regular Signals - Effect of Varying Information Display",
          notes = "SEs clustered by subject.",
          out = tab_path("regular", "tab_updating_treatment"))

}, error = .fail)
}


######################################################
# FIGURE 13 - EXPANDED
######################################################
if (.should_run("fig13")) {
.tick("Figure 13 (expanded)")
tryCatch({

# Overview average and median
df_regular_sum <- df_regular %>%
  summarise(overreport_avg = mean(over_report, na.rm = TRUE),
            SE_over_report = std.error(over_report, na.rm = TRUE),
            overreport_med = median(over_report, na.rm = TRUE))

# Graph types - Avg and Var
fig_belief_overreport_type <- ggplot(df_reg_subject_stats, aes(x = overreport_avg_id)) +
  geom_histogram(binwidth = 0.025, fill = "white", colour = "black") +
  scale_x_continuous(name = "Average of Over-Reported Belief") +
  scale_y_continuous(name = "Count") +
  theme_minimal()

fig_belief_overreporting_type_var <- ggplot(df_reg_subject_stats, aes(x = overreport_variance_id)) +
  geom_histogram(binwidth = 0.01, fill = "white", colour = "black") +
  scale_x_continuous(name = "Variance of Over-Reported Beliefs") +
  scale_y_continuous(name = "Count") +
  theme_minimal()

# Combine
fig_overreport_regular <- ggarrange(fig_belief_overreport_type, fig_belief_overreporting_type_var,
                                    ncol = 2, nrow = 1,
                                    labels = c("Average per Subject", "Variance per Subject"))

ggsave(fig_path("regular", "fig_overreport"), plot = fig_overreport_regular, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# FIGURE 17
######################################################
if (.should_run("fig17")) {
.tick("Figure 17")
tryCatch({

# Summarizing data by belief input type
df_time_belief_type <- df_main[df_main$round>1,] %>%
  group_by(type) %>%
  summarise(sec_avg = mean(seconds_belief, na.rm = TRUE), SE = std.error(seconds_belief, na.rm = TRUE), sec_med = mean(seconds_belief, na.rm = TRUE))

# Plot Mean
fig_time_belief_type <- ggplot(df_time_belief_type, aes(x = factor(type), y = sec_avg)) +
  geom_bar(stat="identity", width=0.9, position = position_dodge(), fill = "white", colour = "black") +
  geom_errorbar(aes(x = factor(type), ymin = sec_avg - 1.96*SE, ymax = sec_avg + 1.96*SE), position = position_dodge(0.7), width = 0.2) +
  labs(x = "Type", y = "Seconds",
       title = "Average Time per Type of Belief Updating Problem",
       caption = "Excluding round 1.") +
  theme_minimal()

ggsave(fig_path("regular", "fig_time_by_type"), plot = fig_time_belief_type, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# TABLE 14
######################################################
if (.should_run("tab14")) {
.tick("Table 14")
tryCatch({

# Preparation
df_regular$belief_lag_extreme <- abs(df_regular$belief_lag1-0.5)

df_regular$conf_diff_color <- df_regular$conf_total - df_regular$conf_same_color
df_regular$ret_diff_color <- df_regular$ret_total - df_regular$ret_same_color

df_regular$ver_diff_color <- df_regular$ret_diff_color + df_regular$conf_diff_color
df_regular$ver_same_color <- df_regular$ret_same_color + df_regular$conf_same_color

df_regular$cum_ret <- df_regular$ret_diff_color + df_regular$ret_same_color
df_regular$cum_conf <- df_regular$conf_diff_color + df_regular$conf_same_color


# Regressions
me_reg_by_prev_ver <- lmer(obs_log_post_ratio ~ signal_ratio + prior_ratio
                         + prior_ratio:round
                         + signal_ratio:round
                         + signal_ratio:prev_verified
                         + (1 + signal_ratio + prior_ratio|id), df_regular[df_regular$treat_aggregate_signal==0,])

me_reg_by_prev_ret_conf <- lmer(obs_log_post_ratio ~ signal_ratio + prior_ratio
                         + prior_ratio:round
                         + signal_ratio:round
                         + signal_ratio:cum_ret
                         + signal_ratio:cum_conf
                         + (1 + signal_ratio + prior_ratio|id), df_regular[df_regular$treat_aggregate_signal==0,])

me_reg_by_same_other <- lmer(obs_log_post_ratio ~ signal_ratio + prior_ratio
                         + prior_ratio:round
                         + signal_ratio:round
                         + signal_ratio:ver_same_color
                         + signal_ratio:ver_diff_color
                         + (1 + signal_ratio + prior_ratio|id), df_regular[df_regular$treat_aggregate_signal==0,])


me_reg_by_same_other_split <- lmer(obs_log_post_ratio ~ signal_ratio + prior_ratio
                         + prior_ratio:round
                         + signal_ratio:round
                         + signal_ratio:ret_same_color
                         + signal_ratio:conf_same_color
                         + signal_ratio:ret_diff_color
                         + signal_ratio:conf_diff_color
                         + (1 + signal_ratio + prior_ratio|id), df_regular[df_regular$treat_aggregate_signal==0,])

# Table with overview
write_stargazer(me_reg_by_prev_ver, me_reg_by_prev_ret_conf, me_reg_by_same_other, me_reg_by_same_other_split,
          type = output_type,
          style = "default",
          dep.var.labels = c("Observed Log-Posterior-Ratio"),
          covariate.labels = c("Constant", "Signal", "Prior",
                               "Signal * Round", "Prior * Round",
                               "Signal * # Previously Checked Signals",
                               "Signal * # Previous Retractions", "Signal * # Previous Confirmations",
                               "Signal * # Previous Same Checks", "Signal * # Previous Other Checks",
                               "Signal * # Previous Same Retractions", "Signal * # Previous Same Confirmations",
                               "Signal * # Previous Other Retractions", "Signal * # Previous Other Confirmations"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          order = c(1,2,3,5,4),
          omit.stat = c("ll","bic"),
          title = "Updating with Regular Signals",
          out = tab_path("regular", "tab_verification_llr"))

}, error = .fail)
}


######################################################
# TABLE 15
######################################################
if (.should_run("tab15")) {
.tick("Table 15")
tryCatch({

# Regressions
ols_regular_belief_change1 <- lm(belief_change_adj ~ round
                                 + prev_verified, df_regular[df_regular$treat_aggregate_signal==0,])

ols_regular_belief_change2 <- lm(belief_change_adj ~ round
                                 + cum_ret
                                 + cum_conf, df_regular[df_regular$treat_aggregate_signal==0,])

ols_regular_belief_change3 <- lm(belief_change_adj ~ round
                                 + ver_same_color
                                 + ver_diff_color, df_regular[df_regular$treat_aggregate_signal==0,])

ols_regular_belief_change4 <- lm(belief_change_adj ~ round
                                 + ret_same_color
                                 + conf_same_color
                                 + ret_diff_color
                                 + conf_diff_color, df_regular[df_regular$treat_aggregate_signal==0,])


# Table with overview
write_stargazer(ols_regular_belief_change1, ols_regular_belief_change2, ols_regular_belief_change3, ols_regular_belief_change4,
          type = output_type,
          style = "default",
          dep.var.labels = c("$(b_t - b_{t-1}) \\cdot I(s)$"),
          covariate.labels = c("Constant", "Round",
                               "# Previously Verified Signals",
                               "# Previous Retractions", "# Previous Confirmations",
                               "# Previous Same Checks", "Previous Other Checks",
                               "# Previous Same Retractions", "# Previous Same Confirmations",
                               "# Previous Other Retractions", "# Previous Other Confirmations"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Updating with Regular Signals",
          out = tab_path("regular", "tab_verification_belief_change"))

}, error = .fail)
}


# --- Revision additions ---

######################################################
# FIGURE -- Inference (c) and Base-Rate Use (d) by Prior Belief Bin
######################################################
if (.should_run("fig_reg_prior")) {
.tick("Figure -- Regular c & d by prior bin")
tryCatch({

# Align prior with signal direction: "how much does my prior agree with the signal?"
# For red signals, use belief_lag1 (prob of red). For blue signals, flip to 1 - belief_lag1.
df_regular$prior_aligned <- ifelse(df_regular$ball_red == 1, df_regular$belief_lag1, 1 - df_regular$belief_lag1)

# Bin aligned prior into 10pp bins
df_regular$belief_lag1_bin <- cut(df_regular$prior_aligned,
    breaks = prior_bin_breaks, include.lowest = TRUE,
    labels = prior_bin_labels)

bin_labels <- levels(df_regular$belief_lag1_bin)
reg_prior_effects <- data.frame(bin = bin_labels, d_coef = NA, c_coef = NA,
                                 d_se = NA, c_se = NA, n = NA)

for (i in seq_along(bin_labels)) {
  bl <- bin_labels[i]
  df_sub <- df_regular[df_regular$belief_lag1_bin == bl, ]
  reg_prior_effects$n[i] <- nrow(df_sub)

  # Try mixed-effects; fall back to OLS if convergence fails
  fit <- tryCatch(
    lmer(obs_log_post_ratio ~ signal_ratio + prior_ratio + (1 + signal_ratio + prior_ratio | id), df_sub),
    error = function(e) NULL, warning = function(w) NULL
  )
  if (is.null(fit)) {
    fit <- lm(obs_log_post_ratio ~ signal_ratio + prior_ratio, df_sub)
    beta <- coef(fit)
    se <- sqrt(diag(vcov(fit)))
  } else {
    beta <- fixef(fit)
    se <- sqrt(diag(vcov(fit)))
  }

  reg_prior_effects$d_coef[i] <- beta["signal_ratio"]
  reg_prior_effects$c_coef[i] <- beta["prior_ratio"]
  reg_prior_effects$d_se[i] <- se["signal_ratio"]
  reg_prior_effects$c_se[i] <- se["prior_ratio"]
}

reg_prior_effects$bin <- factor(reg_prior_effects$bin, levels = bin_labels)

# Shared y-axis limits across both panels
y_lo <- min(c(reg_prior_effects$d_coef - 1.96 * reg_prior_effects$d_se,
              reg_prior_effects$c_coef - 1.96 * reg_prior_effects$c_se), na.rm = TRUE)
y_hi <- max(c(reg_prior_effects$d_coef + 1.96 * reg_prior_effects$d_se,
              reg_prior_effects$c_coef + 1.96 * reg_prior_effects$c_se), na.rm = TRUE)
y_pad <- (y_hi - y_lo) * 0.1
y_lim <- c(y_lo - y_pad, y_hi + y_pad)
n_y <- y_lo - y_pad * 0.5  # position for n-labels

# Plot inference (d) by prior bin — d = signal weight per Grether (1980)
fig_reg_d <- ggplot(reg_prior_effects, aes(x = bin, y = d_coef)) +
  geom_bar(stat = "identity", width = 0.8, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = d_coef - 1.96 * d_se, ymax = d_coef + 1.96 * d_se), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_text(aes(label = paste0("n=", n)), y = n_y, size = 2.5) +
  scale_x_discrete(name = "Prior aligned with signal (t-1)") +
  coord_cartesian(ylim = y_lim) +
  ylab("Inference (d)") +
  ggtitle("Signal Weight by Prior Belief") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot base-rate use (c) by prior bin — c = prior weight per Grether (1980)
fig_reg_c <- ggplot(reg_prior_effects, aes(x = bin, y = c_coef)) +
  geom_bar(stat = "identity", width = 0.8, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = c_coef - 1.96 * c_se, ymax = c_coef + 1.96 * c_se), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_text(aes(label = paste0("n=", n)), y = n_y, size = 2.5) +
  scale_x_discrete(name = "Prior aligned with signal (t-1)") +
  coord_cartesian(ylim = y_lim) +
  ylab("Base-Rate Use (c)") +
  ggtitle("Prior Weight by Prior Belief") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig_reg_cd_prior <- annotate_figure(
  ggarrange(fig_reg_d, fig_reg_c, ncol = 2, nrow = 1),
  top = text_grob("Regular Signals", face = "bold", size = 14))
ggsave(fig_path("regular", "fig_cd_by_prior"), plot = fig_reg_cd_prior, width = 10, height = 5.5, units = "in", dpi = set_dpi)

}, error = .fail)
}
