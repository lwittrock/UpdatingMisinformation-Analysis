# Figures and Tables for 'Belief Updating with Misinformation'
# Code by Lars Wittrock
# Date: 12/12/2023


# NOTES
######################################################
# Please run file 'Preparing Data' first.
# Then check that paths below are set correctly.
# Also check that all packages are installed.
# Some tables take about a minute to compile, only run the entire script if needed.
#
# SELECTIVE EXECUTION:
# Set run_sections to choose what to run. Options:
#   "all"      -- run everything (default)
#   "figures"  -- only figures
#   "tables"   -- only tables
#   c("fig5", "fig6", "tab1", ...) -- specific items (fig5-fig17, tab1-tab15)
# Dependencies are handled automatically (e.g. tab7 needs fig12, fig16 needs fig8).
run_sections <- "all"  # Options: "all", "figures", "tables", or c("fig5", "tab1", ...). Reset to "all" before committing.


######################################################
# SETUP
######################################################

# File locations relative to project root -- run scripts with setwd() at project root
inpath <- "data/processed"

# Setting file type for tables
output_type <- "latex" # can be set to 'html' or 'latex'

# Change figure quality
set_dpi <- 400


# Package check
required_packages <- c("ggplot2", "ggsci", "gridExtra", "tidyr", "dplyr",
                        "plotrix", "ggpubr", "ggforce", "stargazer", "lme4",
                        "stringr", "estimatr")
missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nInstall with: install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))")
}

# Packages
library(ggplot2)
library(ggsci)
library(gridExtra)
library(tidyr)
library(dplyr)
library(plotrix)
library(ggpubr)
library(ggforce)

library(stargazer)
# Required for stargazer 5.2.3 with R >= 4.2. Can be removed when stargazer is updated.
# Patch stargazer 5.2.3 is.na() bug for R >= 4.2
# Bug: if(is.na(s)) called on a vector at line ~2104 of .stargazer.wrap
# Fix: replace is.na(s) with anyNA(s) in the closure
local({
  sg_env <- environment(stargazer::stargazer)
  sw <- get(".stargazer.wrap", envir = sg_env)
  sw_body <- deparse(body(sw))
  # The .inside.bracket function checks is.na(s) and s=="" before length(s)>1,
  # which crashes when s is a vector. Fix: add length check before scalar ops.
  sw_body <- gsub(
    "if (is.na(s)) {",
    "if (length(s) > 1) { return(\"\") }\n        if (is.na(s)) {",
    sw_body, fixed = TRUE)
  body(sw) <- parse(text = paste(sw_body, collapse = "\n"))[[1]]
  unlockBinding(".stargazer.wrap", sg_env)
  assign(".stargazer.wrap", sw, envir = sg_env)
  lockBinding(".stargazer.wrap", sg_env)
})
library(lme4)
library(stringr)
library(estimatr)


######################################################
# Shared constants
######################################################

# Figure dimensions (in inches)
fig_width <- 7.5
fig_height <- 5

# For binning priors in fig_reg_prior and tab_conf_prior
prior_bin_breaks <- seq(0, 1, 0.1)
prior_bin_labels <- c("0-10", "11-20", "21-30", "31-40", "41-50",
                       "51-60", "61-70", "71-80", "81-90", "91-100")
prior_bin_ref <- "41-50"


######################################################
# Output path helpers
######################################################
outpath <- "output"
fig_path <- function(topic, name) paste0(outpath, "/", topic, "/figures/", name, ".jpg")
tab_path <- function(topic, name) paste0(outpath, "/", topic, "/tables/", name, ".", output_extension)

# Create output directories
for (topic in c("regular", "retract", "confirm")) {
  dir.create(paste0(outpath, "/", topic, "/figures"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(outpath, "/", topic, "/tables"), recursive = TRUE, showWarnings = FALSE)
}

# Helper: get cluster vector matching rows actually used by lm (after NA removal)
model_clusters <- function(model, cluster_vec) {
  cluster_vec[as.integer(names(model$residuals))]
}

# Helper: write stargazer output without the auto-generated timestamp comment
# (prevents spurious git diffs when re-running the pipeline with unchanged results)
write_stargazer <- function(..., out) {
  lines <- capture.output(stargazer::stargazer(...))
  lines <- lines[!grepl("^% Date and time:|^<!-- Date and time:", lines)]
  writeLines(lines, out)
}


######################################################
# Reading data
######################################################
load(file = paste0(inpath, "/data_main.rda"))
load(file = paste0(inpath, "/data_subject.rda"))
load(file = paste0(inpath, "/data_regular.rda"))
load(file = paste0(inpath, "/data_time.rda"))
load(file = paste0(inpath, "/data_retract.rda"))
load(file = paste0(inpath, "/data_confirm.rda"))
rownames(df_confirm) <- NULL  # reset non-sequential row names from subsetting, needed for model_clusters()
load(file = paste0(inpath, "/data_uninformative.rda"))
load(file = paste0(inpath, "/data_informative.rda"))

# Changing output type based on selection above
output_extension <- ifelse(output_type=="html", "html", "tex")


######################################################
# DERIVED VARIABLES
# Shared data objects extracted from individual sections
# to eliminate hidden cross-section dependencies.
######################################################

# Subject-level over-report avg and variance (originally in fig13)
df_regular_type <- df_regular %>%
  group_by(id) %>%
  summarise(overreport_avg_id = mean(over_report, na.rm = TRUE),
            overreport_variance_id = var(over_report, na.rm = TRUE),
            id = mean(id))
df_regular <- merge(df_regular, df_regular_type, by = "id")
df_retract <- merge(df_retract, df_regular_type, by = "id")

# Confirmation bias check variable (originally in fig12)
df_regular <- mutate(df_regular, c = ifelse(sign(prior_ratio) == sign(signal_ratio), 1, 0))

# Prior aligned with signal direction on df_retract (originally in fig_ret_overreact_prior / tab_ret_prior)
df_retract$prior_aligned <- ifelse(df_retract$ball_red_lag1 == 1, df_retract$belief_lag2, 1 - df_retract$belief_lag2)
df_retract$prior_aligned_bin <- cut(df_retract$prior_aligned,
    breaks = prior_bin_breaks, include.lowest = TRUE,
    labels = prior_bin_labels)
df_retract$belief_lag2_bin <- cut(df_retract$prior_aligned,
    breaks = prior_bin_breaks, include.lowest = TRUE,
    labels = prior_bin_labels)
df_retract$belief_lag2_bin <- relevel(factor(df_retract$belief_lag2_bin), ref = prior_bin_ref)

# Prior aligned with signal direction on df_confirm (originally in tab_conf_prior)
df_confirm$prior_aligned <- ifelse(df_confirm$ball_red_lag1 == 1, df_confirm$belief_lag2, 1 - df_confirm$belief_lag2)
df_confirm$belief_lag2_bin <- cut(df_confirm$prior_aligned,
    breaks = prior_bin_breaks, include.lowest = TRUE,
    labels = prior_bin_labels)
df_confirm$belief_lag2_bin <- relevel(factor(df_confirm$belief_lag2_bin), ref = prior_bin_ref)


######################################################
# Section selector helper
######################################################
.deps <- list(
  # Paper: ordering dependencies
  fig6 = "fig5",   # df_retract_sum_all
  fig9 = "fig5",   # df_retract_sum_all
  fig8 = "fig7",   # df_confirm_temp
  fig11 = "fig7",  # df_confirm_change_sum
  fig16 = "fig8",  # df_confirm_type
  tab7 = "fig12"   # df_coef
)
.should_run <- function(section) {
  if (identical(run_sections, "all")) return(TRUE)
  if (identical(run_sections, "figures")) return(grepl("^fig", section))
  if (identical(run_sections, "tables")) return(grepl("^tab", section) || section %in% unlist(.deps[run_sections[grepl("^tab", run_sections)]]))
  section %in% run_sections || section %in% unlist(.deps[run_sections])
}

.timer_start <- proc.time()["elapsed"]
.section_start <- .timer_start
.last_section <- NULL
.section_times <- list()
.section_status <- list()
.tick <- function(label) {
  now <- proc.time()["elapsed"]
  if (!is.null(.last_section)) {
    elapsed <- now - .section_start
    .section_times[[.last_section]] <<- elapsed
    if (is.null(.section_status[[.last_section]])) .section_status[[.last_section]] <<- "OK"
    cat(sprintf("   done (%.1fs)\n", elapsed))
  }
  cat(paste0(">> ", label, "...\n"))
  .section_start <<- now
  .last_section <<- label
}
.fail <- function(e) {
  .section_status[[.last_section]] <<- conditionMessage(e)
  cat(sprintf("   !! FAILED: %s\n", conditionMessage(e)))
}
cat(">> Figures and Tables.R started\n")


######################################################
######################################################
# SECTION 1: REGULAR SIGNALS / BACKGROUND
######################################################
######################################################

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
ols_regular <- lm(obslnpost ~ signal_ratio + prior_ratio, df_regular)
me_regular <- lmer(obslnpost ~ signal_ratio + prior_ratio + (1 + signal_ratio + prior_ratio|id), df_regular)
me_regular_c <- lmer(obslnpost ~ signal_ratio + prior_ratio + signal_ratio:c + (1 + signal_ratio + prior_ratio|id), df_regular)

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

# Creating data frame
coef <- coef(me_regular)
df_coef <- as.data.frame(coef$id)
df_coef$id <- as.numeric(rownames(df_coef))

names(df_coef)[names(df_coef) == "prior_ratio"] <- "base_rate_use"
names(df_coef)[names(df_coef) == "signal_ratio"] <- "inference"
names(df_coef)[names(df_coef) == "(Intercept)"] <- "intercept"

df_coef <- subset(df_coef,select=-c(intercept))

# Plotting
fig_regular_inference <- ggplot(df_coef, aes(x = inference)) +
  geom_histogram(binwidth = 0.1, fill = "white", color = "black") +
  xlab("Estimated Inference (c) per Subject") +
  ylab("Count") +
  ggtitle("Inference Bias") +
  theme_classic()

fig_regular_baserate <- ggplot(df_coef, aes(x = base_rate_use)) +
  geom_histogram(binwidth = 0.05, fill = "white", color = "black") +
  xlab("Estimated Base-Rate Use (d) per Subject") +
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
ols_regular_treat <- lm(obslnpost ~ signal_ratio*factor(treat) + prior_ratio*factor(treat), df_regular[df_regular$aggregate_round==0,])

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
fig_belief_overreport_type <- ggplot(df_regular_type, aes(x = overreport_avg_id)) +
  geom_histogram(binwidth = 0.025, fill = "white", colour = "black") +
  scale_x_continuous(name = "Average of Over-Reported Belief") +
  scale_y_continuous(name = "Count") +
  theme_minimal()

fig_belief_overreporting_type_var <- ggplot(df_regular_type, aes(x = overreport_variance_id)) +
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

df_regular$conf_other <- df_regular$conf_total - df_regular$conf_same
df_regular$ret_other <- df_regular$ret_total - df_regular$ret_same

df_regular$ver_other <- df_regular$ret_other + df_regular$conf_other
df_regular$ver_same <- df_regular$ret_same + df_regular$conf_same

df_regular$prev_ret <- df_regular$ret_other + df_regular$ret_same
df_regular$prev_conf <- df_regular$conf_other + df_regular$conf_same


# Regressions
me_regular_expl1 <- lmer(obslnpost ~ signal_ratio + prior_ratio
                         + prior_ratio:round
                         + signal_ratio:round
                         + signal_ratio:prev_verified
                         + (1 + signal_ratio + prior_ratio|id), df_regular[df_regular$treat_aggregate_signal==0,])

me_regular_expl2 <- lmer(obslnpost ~ signal_ratio + prior_ratio
                         + prior_ratio:round
                         + signal_ratio:round
                         + signal_ratio:prev_ret
                         + signal_ratio:prev_conf
                         + (1 + signal_ratio + prior_ratio|id), df_regular[df_regular$treat_aggregate_signal==0,])

me_regular_expl3 <- lmer(obslnpost ~ signal_ratio + prior_ratio
                         + prior_ratio:round
                         + signal_ratio:round
                         + signal_ratio:ver_same
                         + signal_ratio:ver_other
                         + (1 + signal_ratio + prior_ratio|id), df_regular[df_regular$treat_aggregate_signal==0,])


me_regular_expl4 <- lmer(obslnpost ~ signal_ratio + prior_ratio
                         + prior_ratio:round
                         + signal_ratio:round
                         + signal_ratio:ret_same
                         + signal_ratio:conf_same
                         + signal_ratio:ret_other
                         + signal_ratio:conf_other
                         + (1 + signal_ratio + prior_ratio|id), df_regular[df_regular$treat_aggregate_signal==0,])

# Table with overview
write_stargazer(me_regular_expl1, me_regular_expl2, me_regular_expl3, me_regular_expl4,
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
                                 + prev_ret
                                 + prev_conf, df_regular[df_regular$treat_aggregate_signal==0,])

ols_regular_belief_change3 <- lm(belief_change_adj ~ round
                                 + ver_same
                                 + ver_other, df_regular[df_regular$treat_aggregate_signal==0,])

ols_regular_belief_change4 <- lm(belief_change_adj ~ round
                                 + ret_same
                                 + conf_same
                                 + ret_other
                                 + conf_other, df_regular[df_regular$treat_aggregate_signal==0,])


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
reg_prior_effects <- data.frame(bin = bin_labels, c_coef = NA, d_coef = NA,
                                 c_se = NA, d_se = NA, n = NA)

for (i in seq_along(bin_labels)) {
  bl <- bin_labels[i]
  df_sub <- df_regular[df_regular$belief_lag1_bin == bl, ]
  reg_prior_effects$n[i] <- nrow(df_sub)

  # Try mixed-effects; fall back to OLS if convergence fails
  fit <- tryCatch(
    lmer(obslnpost ~ signal_ratio + prior_ratio + (1 + signal_ratio + prior_ratio | id), df_sub),
    error = function(e) NULL, warning = function(w) NULL
  )
  if (is.null(fit)) {
    fit <- lm(obslnpost ~ signal_ratio + prior_ratio, df_sub)
    beta <- coef(fit)
    se <- sqrt(diag(vcov(fit)))
  } else {
    beta <- fixef(fit)
    se <- sqrt(diag(vcov(fit)))
  }

  reg_prior_effects$c_coef[i] <- beta["signal_ratio"]
  reg_prior_effects$d_coef[i] <- beta["prior_ratio"]
  reg_prior_effects$c_se[i] <- se["signal_ratio"]
  reg_prior_effects$d_se[i] <- se["prior_ratio"]
}

reg_prior_effects$bin <- factor(reg_prior_effects$bin, levels = bin_labels)

# Shared y-axis limits across both panels
y_lo <- min(c(reg_prior_effects$c_coef - 1.96 * reg_prior_effects$c_se,
              reg_prior_effects$d_coef - 1.96 * reg_prior_effects$d_se), na.rm = TRUE)
y_hi <- max(c(reg_prior_effects$c_coef + 1.96 * reg_prior_effects$c_se,
              reg_prior_effects$d_coef + 1.96 * reg_prior_effects$d_se), na.rm = TRUE)
y_pad <- (y_hi - y_lo) * 0.1
y_lim <- c(y_lo - y_pad, y_hi + y_pad)
n_y <- y_lo - y_pad * 0.5  # position for n-labels

# Plot inference (c) by prior bin
fig_reg_c <- ggplot(reg_prior_effects, aes(x = bin, y = c_coef)) +
  geom_bar(stat = "identity", width = 0.8, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = c_coef - 1.96 * c_se, ymax = c_coef + 1.96 * c_se), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_text(aes(label = paste0("n=", n)), y = n_y, size = 2.5) +
  scale_x_discrete(name = "Prior aligned with signal (t-1)") +
  coord_cartesian(ylim = y_lim) +
  ylab("Inference (c)") +
  ggtitle("Signal Weight by Prior Belief") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot base-rate use (d) by prior bin
fig_reg_d <- ggplot(reg_prior_effects, aes(x = bin, y = d_coef)) +
  geom_bar(stat = "identity", width = 0.8, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = d_coef - 1.96 * d_se, ymax = d_coef + 1.96 * d_se), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_text(aes(label = paste0("n=", n)), y = n_y, size = 2.5) +
  scale_x_discrete(name = "Prior aligned with signal (t-1)") +
  coord_cartesian(ylim = y_lim) +
  ylab("Base-Rate Use (d)") +
  ggtitle("Prior Weight by Prior Belief") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig_reg_cd_prior <- annotate_figure(
  ggarrange(fig_reg_c, fig_reg_d, ncol = 2, nrow = 1),
  top = text_grob("Regular Signals", face = "bold", size = 14))
ggsave(fig_path("regular", "fig_cd_by_prior"), plot = fig_reg_cd_prior, width = 10, height = 5.5, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
######################################################
# SECTION 2: RETRACTIONS
######################################################
######################################################

# --- Paper ---

######################################################
# FIGURE 5
######################################################
if (.should_run("fig5")) {
.tick("Figure 5")
tryCatch({

# Summary per type - SE grouped by subject
df_retract_type_initial <- df_retract %>%
  group_by(initial_reaction_alt, id, treat) %>%
  summarise(belief_diff = mean(-over_report, na.rm = TRUE),
            n = length(id))

df_retract_type <- df_retract_type_initial %>%
  group_by(initial_reaction_alt) %>%
  summarise(belief_diff_sum = weighted.mean(belief_diff, n, na.rm = TRUE),
            SE = std.error(belief_diff, na.rm = TRUE),
            n = sum(n))

# Adjusting names for graph
names(df_retract_type)[names(df_retract_type) == "initial_reaction_alt"] <- "type"
df_retract_type <- mutate(df_retract_type, type = ifelse(type=="correct", "Correctly reacted (+- 1%pt)", type))
df_retract_type <- mutate(df_retract_type, type = ifelse(type=="under", "Under-reacted (<1%pt)*", type))
df_retract_type <- mutate(df_retract_type, type = ifelse(type=="over", "Over-reacted (>1%pt)", type))
df_retract_type <- mutate(df_retract_type, type = ifelse(type=="wrong", "Wrong direction", type))

# Summary data all
df_retract_sum_all <- df_retract_type_initial %>%
  group_by() %>%
  summarise(belief_diff_sum = weighted.mean(belief_diff, n, na.rm = TRUE),
            SE = std.error(belief_diff, na.rm = TRUE),
            n = sum(n),
            type = "All Retractions")

# Merging all and per type
df_retract_sum <- rbind(df_retract_sum_all, df_retract_type)

# Remove wrong for graph
df_retract_sum <- df_retract_sum[df_retract_sum$type != "Wrong direction", ]

# Express beliefs in %
df_retract_sum$belief_diff_pts <- df_retract_sum$belief_diff_sum*100
df_retract_sum$SE_pts <- df_retract_sum$SE*100
df_retract_sum$type <- factor(df_retract_sum$type, levels = c("Correctly reacted (+- 1%pt)", "Over-reacted (>1%pt)", "Under-reacted (<1%pt)*", "All Retractions"))

# Influence of retraced signals - belief difference
fig_retract_diff_group <- ggplot(df_retract_sum, aes(x = type, y = belief_diff_pts)) +
  geom_bar(stat="identity", width=0.9, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = belief_diff_pts - 1.96*SE_pts, ymax = belief_diff_pts + 1.96*SE_pts), position = position_dodge(0.9), width = 0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_discrete(name = "Reaction to initial signal (relative to Bayesian)", drop = FALSE) +
  scale_y_continuous(limits = c(-5.5, 13)) +
  annotate("text", y=-5.5, x=1, label=paste0("n = ", df_retract_sum$n[2]), size=3) +
  annotate("text", y=-5.5, x=2, label=paste0("n = ", df_retract_sum$n[3]), size=3) +
  annotate("text", y=-5.5, x=3, label=paste0("n = ", df_retract_sum$n[4]), size=3) +
  annotate("text", y=-5.5, x=4, label=paste0("n = ", df_retract_sum$n[1]), size=3) +
  ylab("Belief biased towards initial signal (%pts)") +
  #labs(title = "Influence of Retractions",
  #     subtitle = "Mean belief before vs. after retracted signal & 95% CI",
  #     caption = "* not including observations with initial update in wrong direction. SEs grouped by subject.") +
  theme_classic()

ggsave(fig_path("retract", "fig_response_by_initial"), plot = fig_retract_diff_group, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# FIGURE 6
######################################################
if (.should_run("fig6")) {
.tick("Figure 6")
tryCatch({

# Preparation
df_main_ver <- df_main[!is.na(df_main$two_balls),]
df_regular_mixedballs <- df_main_ver[df_main_ver$two_balls=="BR" | df_main_ver$two_balls=="RB",]

# Summary data all
df_regular_sum_all <- df_regular_mixedballs %>%
  summarise(belief_diff_sum = mean(-over_report2, na.rm = TRUE),
            SE = std.error(-over_report2, na.rm = TRUE),
            n = length(id),
            type = "All Retractions/ Opposite Signals")

# Merging relevant data
df_retract_opposite_all <- rbind(df_regular_sum_all, df_retract_sum_all)
df_retract_opposite_all <- mutate(df_retract_opposite_all, type = ifelse(type=="All Retractions/ Opposite Signals", "Opposite New Information", type))
df_retract_opposite_all <- mutate(df_retract_opposite_all, type = ifelse(type=="All Retractions", "Retraction", type))

df_retract_opposite_all$belief_diff_pts <- df_retract_opposite_all$belief_diff_sum*100
df_retract_opposite_all$SE_pts <- df_retract_opposite_all$SE*100

# Plot
fig_regular_diff_group_all <- ggplot(NULL, ) +
  geom_col(aes(x = type, y = belief_diff_pts),
           data = df_retract_opposite_all, width=0.9,
           fill = "white", color = "black", alpha = 0.5) +
  geom_errorbar(aes(x = type, y = belief_diff_pts,
                    ymin = belief_diff_pts - 1.96*SE_pts,
                    ymax = belief_diff_pts + 1.96*SE_pts),
                data = df_retract_opposite_all, position = position_dodge(0.2),
                width = 0.2, color = "black") +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_discrete(name = "", drop = FALSE) +
  scale_y_continuous(limits = c(-6, 2.5)) +
  annotate("text", y=-6, x=1, label=paste0("n = ", df_retract_opposite_all$n[1]), size=3, color="black") +
  annotate("text", y=-6, x=2, label=paste0("n = ", df_retract_opposite_all$n[2]), size=3, color="black") +
  ylab("Belief biased towards initial signal (%pts)") +
  #labs(title = "Retractions vs Opposite Signals") +
  theme_classic()

ggsave(fig_path("retract", "fig_vs_opposite_ball"), plot = fig_regular_diff_group_all, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# FIGURE 14 AND ADDITIONAL GRAPHS FOR PRESENTATION
######################################################
if (.should_run("fig14")) {
.tick("Figure 14 & Additional Graphs")
tryCatch({

# Restricting data to make graph clearer - 55obs less.
df_retract_no_outliers <- df_retract[df_retract$belief_change_adj <= 0.75
                                     & df_retract$belief_change_adj >= -0.25
                                     & df_retract$belief_change_adj_lag1 <= 0.75
                                     & df_retract$belief_change_adj_lag1 >= -0.25, ]

# Plot formatting
fig_retract_change_basic <- ggplot(df_retract_no_outliers, aes(x = belief_change_adj_lag1, y = -belief_change_adj)) +
  geom_abline(slope = -1, intercept = 0, linetype = "dashed", size = 1, alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.75) +
  xlab("Belief Change: Initial Signal") +
  ylab("Belief Change: Retraction") +
  scale_x_continuous(limits = c(-0.25, 0.75)) +
  scale_y_continuous(limits = c(-0.75, 0.25)) +
  labs(#title = "Belief Change with Retractions",
    #subtitle = "All initial signals converted to 'red'",
    caption = "Zoomed in for better visibility (~5% of data points omitted)") +
  theme_classic()

# Main plot
fig_retract_change <- fig_retract_change_basic +
  geom_point(alpha = 0.15, size = 3)

# Main plot with regression line
fig_retract_change_lm <- fig_retract_change +
  geom_smooth(method = 'lm', formula = y ~ x, se = TRUE)

# Exporting graph
ggsave(fig_path("retract", "fig_scatter"), plot = fig_retract_change_lm, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}



######################################################
# FIGURE 15
######################################################
if (.should_run("fig15")) {
.tick("Figure 15")
tryCatch({

# Restricting data to make graph clearer - 43 obs less.
df_retract_no_outliers <- df_retract[df_retract$over_report <= 0.5
                                     & df_retract$over_report >= -0.5
                                     & df_retract$over_report_lag1 <= 0.5
                                     & df_retract$over_report_lag1 >= -0.5, ]

# Beliefs in % for graph
df_retract_no_outliers$over_report_lag1_pts <- df_retract_no_outliers$over_report_lag1*100
df_retract_no_outliers$under_report_pts <- -df_retract_no_outliers$over_report*100

# Plot
fig_retract_diff_cont <- ggplot(df_retract_no_outliers, aes(x = over_report_lag1_pts, y = under_report_pts)) +
  geom_jitter(alpha = 0.3) +
  geom_abline(slope = 0, intercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_smooth(method = 'lm', formula = y ~ x, se = TRUE) +
  xlab("Reaction to initial signal (comp. to Bayesian)") +
  ylab("Belief biased towards initial signal (%pts)") +
  #labs(title = "Influence of Retractions",
  #     subtitle = "Belief before vs. after retracted signal",
  #     caption = "Zoomed in for better visibility (3% of data points omitted)") +
  theme_classic()

ggsave(fig_path("retract", "fig_individual_beliefs"), plot = fig_retract_diff_cont, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# TABLE 4
######################################################
if (.should_run("tab4")) {
.tick("Table 4")
tryCatch({

# Regression
ols_retract_5_main <- lm(over_report_ret ~ over_report_lag1, df_retract)

# Table with overview
write_stargazer(ols_retract_5_main,
          se = starprep(ols_retract_5_main, clusters = df_retract$id),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief minus Bayesian Posterior"),
          covariate.labels = c("Constant", "Belief minus Bayesian Posterior Previously"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Impact of Retractions on Beliefs",
          notes = "SEs clustered by subject.",
          out = tab_path("retract", "tab_main"))

}, error = .fail)
}



######################################################
# TABLE 5
######################################################
if (.should_run("tab5")) {
.tick("Table 5")
tryCatch({

# variable indicating retraction
df_main$ver_retract_all <- ifelse(df_main$ver_retract==1 & !is.na(df_main$ver_retract), 1, 0)

# create variable with all combinations of red and blue retractions. Otherwise potential confounds.
df_main$ret_hist <- apply(str_extract_all(df_main$hist, pattern = "[a-z]_ret", simplify = TRUE),1,paste,collapse=" ")

df_main$ret_hist <- gsub("_ret", "", df_main$ret_hist)
df_main$ret_hist <- gsub(" ", "", df_main$ret_hist)

df_main$ret_hist <- ifelse(is.na(df_main$ret_hist), "", df_main$ret_hist)

ord <- c("", "r", "b", "rr", "bb", "rb", "br",
         "rrr", "bbb", "rrb", "bbr",
         "rbb", "brr", "rbr", "brb")

df_main$ret_hist <- factor(df_main$ret_hist,levels=ord)


# Restricting data to include only compressed histories without any confirmations
df_main_noconf <- df_main[df_main$hist_conf==0,]
df_main_t1 <- df_main[df_main$treat_aggregate_signal==0,]

# Regressions
ols_retract_6a <-lm(belief ~ ver_retract_all + factor(ret_hist)
                    + factor(comp_hist), df_main_t1)

ols_retract_6b <-lm(belief ~ ver_retract_all + factor(ret_hist)
                    + factor(comp_hist) + factor(round), df_main_t1)


# Table with overview
write_stargazer(ols_retract_6a, ols_retract_6b,
          #se = starprep(ols_retract_6a, ols_retract_6b, clusters = df_main$id), # horribly slow.
          type = output_type,
          style = "default",
          dep.var.labels = c("Reported Belief"),
          keep = c("ver_retract", "ret_hist"),
          covariate.labels = c("Retraction", "Retraction History: R", "Retraction History: B",
                               "Retraction History: RR", "Retraction History: BB", "Retraction History: RB", "Retraction History: BR",
                               "Retraction History: RRR", "Retraction History: BBB", "Retraction History: RRB", "Retraction History: BBR",
                               "Retraction History: RBB", "Retraction History: BRR", "Retraction History: RBR", "Retraction History: BRB"),
          add.lines = list(c("Compressed History FEs?", "Yes", "Yes"), c("Round FEs?", "No", "Yes")),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Impact of Retractions",
          #table.layout = "=ldc-ta-s-n",
          out = tab_path("retract", "tab_compressed_history"))

}, error = .fail)
}



######################################################
# TABLE 6 — Signal Use and Base-Rate Use (Objective Signals)
######################################################
if (.should_run("tab6")) {
.tick("Table 6")
tryCatch({

# Combined retraction + confirmation data
df_retcon <- df_main[df_main$verify_round == 1,]

# Regressions with objective signal ratio
ols_retract_obj <- lm(obslnpost ~ 0 + signal_ratio_obj + prior_ratio, df_retract)
ols_confirm_obj <- lm(obslnpost ~ 0 + signal_ratio_obj + prior_ratio, df_confirm)
ols_retcon_obj  <- lm(obslnpost ~ 0 + signal_ratio_obj + prior_ratio, df_retcon)

# Table with overview
write_stargazer(ols_retract_obj, ols_confirm_obj, ols_retcon_obj,
          se = list(starprep(ols_retract_obj, clusters = model_clusters(ols_retract_obj, df_retract$id))[[1]],
                    starprep(ols_confirm_obj, clusters = model_clusters(ols_confirm_obj, df_confirm$id))[[1]],
                    starprep(ols_retcon_obj, clusters = model_clusters(ols_retcon_obj, df_retcon$id))[[1]]),
          type = output_type,
          style = "default",
          dep.var.labels = c("Observed Log-Posterior-Ratio"),
          column.labels = c("Retractions", "Confirmations", "Combined"),
          covariate.labels = c("Signal (objective)", "Prior"),
          no.space = TRUE,
          omit.stat = c("rsq", "ser", "f"),
          title = "Signal Use and Base-Rate Use with Objective Signals",
          notes = "SEs clustered by subject.",
          out = tab_path("retract", "tab_obj_cd"))

}, error = .fail)
}




######################################################
# TABLE 7
######################################################
if (.should_run("tab7")) {
.tick("Table 7")
tryCatch({

# Preparation 1
df_overreport <- df_regular %>%
  group_by(id) %>%
  summarise(overreport_avg = mean(over_report))
df_retract <- merge(df_retract, df_overreport, by = "id")

# Preparation 2
df_retract <- merge(df_retract, df_coef, by = "id")
df_retract$inference_adj <- df_retract$inference - 1
df_retract$base_rate_use_adj <- df_retract$base_rate_use - 1

# Preparation 3
df_regular_type_number <- df_regular %>%
  group_by(id, reaction_alt) %>%
  summarise(id = mean(id), n = length(belief), type_subject = first(reaction_alt))
df_regular_type_id <- df_regular_type_number %>%
  group_by(id) %>%
  filter(n == max(n))
df_regular_type_id <- mutate(df_regular_type_id, type_subject = ifelse(n<5, "Not categorized", type_subject))
df_regular_type_id <- df_regular_type_id %>% select(id, type_subject)
df_regular_type_id <- unique(df_regular_type_id)

df_retract <- merge(df_retract, df_regular_type_id, by="id")

# Regressions
ols_retract_5_types1 <- lm(over_report_ret ~ over_report_lag1 + overreport_avg, df_retract)
ols_retract_5_types2 <- lm(over_report_ret ~ over_report_lag1 + inference_adj + base_rate_use_adj, df_retract)
ols_retract_5_types3 <- lm(over_report_ret ~ over_report_lag1 + type_subject, df_retract)
ols_retract_5_types4 <- lm(over_report_ret ~ over_report_lag1 + factor(id), df_retract)

# Table with overview
write_stargazer(ols_retract_5_types1, ols_retract_5_types2, ols_retract_5_types3, ols_retract_5_types4,
          se = starprep(ols_retract_5_types1, ols_retract_5_types2, ols_retract_5_types3, clusters = df_retract$id),
          keep = c("Constant", "over_report_lag1", "type_subject", "inference", "base_rate", "overreport_avg"),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief biased towards initial signal"),
          covariate.labels = c("Constant",
                               "Initial belief over-report",
                               "Average belief over-report",
                               "Average inference (c-1)",
                               "Average base-rate use (d-1)",
                               "Type: Not categorized",
                               "Type: Majority Over-reported",
                               "Type: Majority Under-reported",
                               "Type: Majority Wrong"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          add.lines = list(c("Subject FEs?", "No", "No", "No", "Yes")),
          title = "Impact of Retractions on Beliefs",
          notes = "SEs clustered by subject.",
          out = tab_path("retract", "tab_types"))

}, error = .fail)
}


######################################################
# TABLE 8
######################################################
if (.should_run("tab8")) {
.tick("Table 8")
tryCatch({

# Regressions
ols_retract_5_expl1 <- lm(over_report_ret ~ over_report_lag1 + over_report_lag2, df_retract)

ols_retract_5_expl2 <- lm(over_report_ret ~ over_report_lag1*treat_no_anchor
                          + over_report_lag1*treat_no_history, df_retract)


# Table with overview
write_stargazer(ols_retract_5_expl1, ols_retract_5_expl2,
          se = starprep(ols_retract_5_expl1, ols_retract_5_expl2, clusters = df_retract$id),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief biased towards initial signal"),
          covariate.labels = c("Constant", "Initial belief over-report (t-1)", "Belief over-report before (t-2)", "No anchor treatment", "No history treatment", "No anchor treat * initial belief over-report (t-1)", "No history treat * initial belief over-report (t-1)"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Impact of Retractions on Beliefs",
          notes = "SEs clustered by subject.",
          out = tab_path("retract", "tab_robustness"))

}, error = .fail)
}


######################################################
# TABLE 9
######################################################
if (.should_run("tab9")) {
.tick("Table 9")
tryCatch({

# Regression
ols_retract_9 <- lm(belief_diff_priorinduced_adj ~ over_report_lag1, df_retract)

# Table with overview
write_stargazer(ols_retract_9,
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief higher than induced Prior after Retraction"),
          covariate.labels = c("Constant", "Belief Over-Report in Previous Round"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Updating with Retraction Signals - All Signals converted to Red",
          out = tab_path("retract", "tab_induced_prior"))

}, error = .fail)
}


######################################################
# TABLE 10
######################################################
if (.should_run("tab10")) {
.tick("Table 10")
tryCatch({

# Preparation
df_main$signal_direction <- ifelse(df_main$ball_red_lag1==1, 1, -1)
df_main <- mutate(df_main, signal_direction = ifelse(two_balls == "BR", -1, signal_direction))
df_main <- mutate(df_main, signal_direction = ifelse(two_balls == "RB", 1, signal_direction))
df_main <- mutate(df_main, signal_direction = ifelse(is.na(signal_direction), 0, signal_direction))

df_main$ver_retract_all <- ifelse(df_main$ver_retract==1 & !is.na(df_main$ver_retract), 1, 0)

# Regression
ols_retract_10a <-lm(belief ~ ver_retract_all*signal_direction
                     + factor(sign_hist), df_main[df_main$aggregate_round==0,])

# Output
write_stargazer(ols_retract_10a,
          type = output_type,
          style = "default",
          dep.var.labels = c("Reported Belief", "Belief higher than Bayesian"),
          keep = c("ver_retract", "signal_direction", "over_report_lag1"),
          covariate.labels = c("Retraction", "Signal direction", "Retraction * Direction of retracted ball"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          add.lines = list(c("Sign History FEs?", "Yes")),
          omit.stat = c("rsq", "f", "ser"),
          title = "Retractions vs Opposite Colored Ball",
          #table.layout = "=ldc-ta-s-n",
          out = tab_path("retract", "tab_vs_ball"))

}, error = .fail)
}


######################################################
# FIGURE 9
######################################################
if (.should_run("fig9")) {
.tick("Figure 9")
tryCatch({

# Summary per type - SE grouped by subject
df_uninformative_id <- df_uninformative %>%
  group_by(id) %>%
  summarise(belief_diff = mean(over_report, na.rm = TRUE),
            n = length(id))

df_uninformative_sum <- df_uninformative_id %>%
  group_by() %>%
  summarise(belief_diff_sum = weighted.mean(belief_diff, n, na.rm = TRUE),
            SE = std.error(belief_diff, na.rm = TRUE),
            n = sum(n),
            type = "Uninformative Signal")

# Merging with summary data on retractions
df_retract_uninformative <- rbind(df_uninformative_sum, df_retract_sum_all)
df_retract_uninformative$type <- ifelse(df_retract_uninformative$type == "All Retractions", "Signal + Retraction", df_retract_uninformative$type)

# Express beliefs in %
df_retract_uninformative$belief_diff_pts <- df_retract_uninformative$belief_diff_sum*100
df_retract_uninformative$SE_pts <- df_retract_uninformative$SE*100

# Influence of Uninformative signals
fig_retract_uninf <- ggplot(df_retract_uninformative, aes(x = factor(type), y = belief_diff_pts)) +
  geom_bar(stat="identity", width=0.9, fill="white", col="black") +
  geom_errorbar(aes(ymin = belief_diff_pts - 1.96*SE_pts, ymax = belief_diff_pts + 1.96*SE_pts), position = position_dodge(0.9), width = 0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_discrete(name = "Type", drop = FALSE) +
  scale_y_continuous(limits = c(-1, 7.5)) +
  annotate("text", y=-1, x=1, label=paste0("n = ", df_retract_uninformative$n[2]), size=3) +
  annotate("text", y=-1, x=2, label=paste0("n = ", df_retract_uninformative$n[1]), size=3) +
  ylab("Belief biased towards initial signal (%pts)") +
  #labs(title = "Retractions vs Uninformative Signals",
  #     subtitle = "Mean belief & 95% CI") +
  theme_classic()

ggsave(fig_path("retract", "fig_vs_uninformative"), plot = fig_retract_uninf, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}



######################################################
# FIGURE 10
######################################################
if (.should_run("fig10")) {
.tick("Figure 10")
tryCatch({

# 1 retraction / uninformative signal
df_uninformative1 <- df_uninformative[df_uninformative$prev_verified==0,]
df_retract1 <- df_retract[df_retract$prev_verified==0,]

df_retract1$belief_diff1 <- df_retract1$belief - df_retract1$belief_lag2


x <- length(df_uninformative1$belief_diff)/10
y <- 5*x

fig_variance_uninformative1 <- ggplot(df_uninformative1, aes(x = belief_diff)) +
  geom_histogram(fill = "white", col = "black", binwidth = 0.02)+
  scale_x_continuous(name = "",
                     limits = c(-1.01,1.01)) +
  scale_y_continuous(breaks = seq(0,y,x),
                     labels = paste(seq(0, 50, by = 10), "%", sep = ""),
                     limits = c(0,y),
                     position = "right")+
  ylab("") +
  theme_minimal()

x <- length(df_retract1$belief_diff1)/10
y <- 5*x

fig_variance_retract1 <- ggplot(df_retract1, aes(x = belief_diff1)) +
  geom_histogram(fill = "white", col = "black", binwidth = 0.02)+
  scale_x_continuous(name = "",
                     limits = c(-1.01,1.01)) +
  scale_y_continuous(breaks = seq(0,y,x),
                     labels = paste(seq(0, 50, by = 10), "%", sep = ""),
                     limits = c(0,y))+
  ylab("") +
  theme_minimal()


# 2 retractions / uninformative signals
df_retract2 <- df_retract[df_retract$prev_verified==1 &
                            (df_retract$blue_balls_conf==0 &
                               df_retract$red_balls_conf==0),]
df_retract2$belief_diff2 <- df_retract2$belief - df_retract2$belief_lag4

df_uninformative2 <- df_uninformative[df_uninformative$prev_uninformative==1 &
                                        df_uninformative$prev_informative==0,]
df_uninformative2$belief_diff2 <- df_uninformative2$belief - df_uninformative2$belief_lag2


x <- length(df_uninformative2$belief_diff2)/10
y <- 5*x

fig_variance_uninformative2 <- ggplot(df_uninformative2, aes(x = belief_diff2)) +
  geom_histogram(fill = "white", col = "black", binwidth = 0.02)+
  scale_x_continuous(name = "",
                     limits = c(-1.01,1.01)) +
  scale_y_continuous(breaks = seq(0,y,x),
                     labels = paste(seq(0, 50, by = 10), "%", sep = ""),
                     limits = c(0,y),
                     position = "right")+
  ylab("") +
  theme_minimal()


x <- length(df_retract2$belief_diff2)/10
y <- 5*x

fig_variance_retract2 <- ggplot(df_retract2, aes(x = belief_diff2)) +
  geom_histogram(fill = "white", col = "black", binwidth = 0.02)+
  scale_x_continuous(name = "",
                     limits = c(-1.01,1.01)) +
  scale_y_continuous(breaks = seq(0,y,x),
                     labels = paste(seq(0, 50, by = 10), "%", sep = ""),
                     limits = c(0,y))+
  ylab("") +
  theme_minimal()


# 3 retractions / uninformative signals
df_retract3 <- df_retract[df_retract$prev_verified==2,]
df_retract3$belief_diff3 <- df_retract3$belief - df_retract3$belief_lag6

df_uninformative3 <- df_uninformative[df_uninformative$prev_uninformative==2,]
df_uninformative3$belief_diff3 <- df_uninformative3$belief - df_uninformative3$belief_lag3


x <- length(df_uninformative3$belief_diff3)/10
y <- 5*x

fig_variance_uninformative3 <- ggplot(df_uninformative3, aes(x = belief_diff3)) +
  geom_histogram(fill = "white", col = "black", binwidth = 0.02)+
  scale_x_continuous(name = "Belief - Prior",
                     limits = c(-1.01,1.01)) +
  scale_y_continuous(breaks = seq(0,y,x),
                     labels = paste(seq(0, 50, by = 10), "%", sep = ""),
                     limits = c(0,y),
                     position = "right")+
  ylab("") +
  theme_minimal()


x <- length(df_retract3$belief_diff3)/10
y <- 5*x

fig_variance_retract3 <- ggplot(df_retract3, aes(x = belief_diff3)) +
  geom_histogram(fill = "white", col = "black", binwidth = 0.02)+
  scale_x_continuous(name = "Belief - Prior",
                     limits = c(-1.01,1.01)) +
  scale_y_continuous(breaks = seq(0,y,x),
                     labels = paste(seq(0, 50, by = 10), "%", sep = ""),
                     limits = c(0,y))+
  ylab("") +
  theme_minimal()

# Plot all
graphs <- list(fig_variance_retract1, fig_variance_retract2, fig_variance_retract3,
               fig_variance_uninformative1, fig_variance_uninformative2, fig_variance_uninformative3)

# Create row and column titles
col.titles = c("Retracted Signals", "Uninformative Signals")
row.titles = c("1 signal", "2 signals", "3 signals")

# Add row titles
graphs[1:3] = lapply(1:3, function(i) arrangeGrob(graphs[[i]], left=row.titles[i]))

# Add column titles and lay out plots
fig_variance_retract_uninf_all <- arrangeGrob(grobs=lapply(c(1,4), function(i) {
  arrangeGrob(grobs=graphs[i:(i+2)], top=col.titles[i/3 + 1], ncol=1)
}), ncol=2)

# Final output
ggsave(fig_path("retract", "fig_variance_vs_uninf"), plot = fig_variance_retract_uninf_all, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

# Calculate variances
var(df_uninformative1$belief_diff)
var(df_retract1$belief_diff1)

var(df_uninformative2$belief_diff2)
var(df_retract2$belief_diff2)

var(df_uninformative3$belief_diff3)
var(df_retract3$belief_diff3)

}, error = .fail)
}


######################################################
# TABLE 12
######################################################
if (.should_run("tab12")) {
.tick("Table 12")
tryCatch({

# Preparation: create variable with all combinations of red and blue uninformative signals.
df_main$uninf_hist <- apply(str_extract_all(df_main$hist, pattern = "[a-z]_uninf", simplify = TRUE),1,paste,collapse=" ")
df_main$uninf_hist <- gsub("_uninf", "", df_main$uninf_hist)
df_main$uninf_hist <- gsub(" ", "", df_main$uninf_hist)
df_main$uninf_hist <- ifelse(is.na(df_main$uninf_hist), "", df_main$uninf_hist)

ord <- c("", "r", "b", "rr", "bb", "rb", "br",
         "rrr", "bbb", "rrb", "bbr",
         "rbb", "brr", "rbr", "brb")
df_main$uninf_hist <- factor(df_main$uninf_hist,levels=ord)


# Regression
ols_uninf_1a <-lm(belief ~ factor(uninf_hist)
                  + factor(agg_hist), df_main)

# Table with overview
write_stargazer(ols_uninf_1a, #ols_uninf_1b,
          type = output_type,
          style = "default",
          dep.var.labels = c("Reported Belief"),
          keep = c("uninf_hist"),
          covariate.labels = c("Uninformative Signal: R", "Uninformative Signal: B",
                               "Uninformative Signals: RR", "Uninformative Signals: BB", "Uninformative Signals: RB", "Uninformative Signals: BR",
                               "Uninformative Signals: RRR", "Uninformative Signals: BBB", "Uninformative Signals: RRB", "Uninformative Signals: BBR",
                               "Uninformative Signals: RBB", "Uninformative Signals: BRR", "Uninformative Signals: RBR", "Uninformative Signals: BRB"),
          add.lines = list(c("Aggregate History FEs?", "Yes", "Yes")),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Uninformative Signals vs Retractions",
          column.labels = c("All histories", "Excluding Inf. Signal Histories"),
          out = tab_path("retract", "tab_vs_uninformative"))

}, error = .fail)
}


# --- Revision additions ---

######################################################
# TABLE -- Retraction Inference (c) and Base-Rate Use (d)
######################################################
if (.should_run("tab_ret_cd")) {
.tick("Table -- Retraction c & d")
tryCatch({

# Regressions
ols_ret_cd <- lm(obslnpost ~ 0 + signal_ratio + prior_ratio, df_retract)
me_ret_cd <- lmer(obslnpost ~ 0 + signal_ratio + prior_ratio + (1 | id), df_retract)

# Table
write_stargazer(ols_ret_cd, me_ret_cd,
          se = list(starprep(ols_ret_cd, clusters = df_retract$id)[[1]], NULL),
          type = output_type,
          style = "default",
          dep.var.labels = c("Observed Log-Posterior-Ratio"),
          covariate.labels = c("Signal (c)", "Prior (d)"),
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Inference and Base-Rate Use: Retractions",
          notes = "OLS SEs clustered by subject. Signal is person-specific (accounts for initial reaction).",
          out = tab_path("retract", "tab_cd"))

}, error = .fail)
}


######################################################
# TABLE -- Retraction Signal History Robustness (new for revision)
######################################################
if (.should_run("tab_ret_hist")) {
.tick("Table -- Retraction Signal History Robustness")
tryCatch({

# Regressions
ols_ret_hist1 <- lm(over_report_ret ~ over_report_lag1, df_retract)
ols_ret_hist2 <- lm(over_report_ret ~ over_report_lag1 + factor(prev_verified), df_retract)
ols_ret_hist3 <- lm(over_report_ret ~ over_report_lag1 * factor(prev_verified), df_retract)

# Table
write_stargazer(ols_ret_hist1, ols_ret_hist2, ols_ret_hist3,
          se = starprep(ols_ret_hist1, ols_ret_hist2, ols_ret_hist3,
                        clusters = df_retract$id),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief biased towards initial signal"),
          covariate.labels = c("Constant",
                               "Over-report (t-1)",
                               "1 prev. verification",
                               "2 prev. verifications",
                               "Over-report (t-1) $\\times$ 1 prev. verif.",
                               "Over-report (t-1) $\\times$ 2 prev. verif."),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Impact of Retractions on Beliefs -- Signal History Robustness",
          notes = "SEs clustered by subject.",
          out = tab_path("retract", "tab_signal_history"))

}, error = .fail)
}


######################################################
# TABLE -- Retraction Prior Belief Heterogeneity (new for revision)
######################################################
if (.should_run("tab_ret_prior")) {
.tick("Table -- Retraction Prior Belief Heterogeneity")
tryCatch({

# Regression
ols_ret_prior <- lm(over_report_ret ~ over_report_lag1 * belief_lag2_bin, df_retract)

# Table
write_stargazer(ols_ret_prior,
          se = starprep(ols_ret_prior, clusters = df_retract$id),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief biased towards initial signal"),
          covariate.labels = c("Constant",
                               "Over-report (t-1)",
                               "Prior 0--10\\%",
                               "Prior 11--20\\%",
                               "Prior 21--30\\%",
                               "Prior 31--40\\%",
                               "Prior 51--60\\%",
                               "Prior 61--70\\%",
                               "Prior 71--80\\%",
                               "Prior 81--90\\%",
                               "Prior 91--100\\%",
                               "Over-report (t-1) $\\times$ Prior 0--10\\%",
                               "Over-report (t-1) $\\times$ Prior 11--20\\%",
                               "Over-report (t-1) $\\times$ Prior 21--30\\%",
                               "Over-report (t-1) $\\times$ Prior 31--40\\%",
                               "Over-report (t-1) $\\times$ Prior 51--60\\%",
                               "Over-report (t-1) $\\times$ Prior 61--70\\%",
                               "Over-report (t-1) $\\times$ Prior 71--80\\%",
                               "Over-report (t-1) $\\times$ Prior 81--90\\%",
                               "Over-report (t-1) $\\times$ Prior 91--100\\%"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Impact of Retractions on Beliefs -- Prior Belief Heterogeneity",
          notes = "SEs clustered by subject. Reference group: prior 41--50\\%. Prior aligned with initial signal direction (t-2).",
          out = tab_path("retract", "tab_prior_heterogeneity"))

}, error = .fail)
}


######################################################
# FIGURE -- Retraction Prior Belief U-shape (new for revision)
######################################################
if (.should_run("fig_ret_prior")) {
.tick("Figure -- Retraction Prior Belief U-shape")
tryCatch({

rob_ret_prior <- lm_robust(over_report_ret ~ over_report_lag1 * belief_lag2_bin,
                            data = df_retract, clusters = id, se_type = "CR2")

# Extract total over_report_lag1 effect per bin
beta <- coef(rob_ret_prior)
V <- vcov(rob_ret_prior)
base_idx <- which(names(beta) == "over_report_lag1")
base_coef <- beta[base_idx]

bin_labels <- prior_bin_labels

total_effects <- data.frame(bin = bin_labels, effect = NA, se = NA, n = NA)

for (i in seq_along(bin_labels)) {
  bl <- bin_labels[i]
  total_effects$n[i] <- sum(df_retract$belief_lag2_bin == bl, na.rm = TRUE)

  if (bl == prior_bin_ref) {
    total_effects$effect[i] <- base_coef
    total_effects$se[i] <- sqrt(V[base_idx, base_idx])
  } else {
    int_name <- paste0("over_report_lag1:belief_lag2_bin", bl)
    int_idx <- which(names(beta) == int_name)
    total_effects$effect[i] <- base_coef + beta[int_idx]
    total_effects$se[i] <- sqrt(V[base_idx, base_idx] + V[int_idx, int_idx] + 2 * V[base_idx, int_idx])
  }
}

total_effects$effect_pct <- total_effects$effect
total_effects$se_pct <- total_effects$se
total_effects$bin <- factor(total_effects$bin, levels = bin_labels)

fig_ret_prior <- ggplot(total_effects, aes(x = bin, y = effect_pct)) +
  geom_bar(stat = "identity", width = 0.8, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = effect_pct - 1.96 * se_pct, ymax = effect_pct + 1.96 * se_pct),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = paste0("n=", n), y = -0.08), size = 2.5) +
  scale_x_discrete(name = "Prior aligned with initial signal (t-2)") +
  ylab("Over-report (t-1) coefficient") +
  theme_classic()

ggsave(fig_path("retract", "fig_prior_ushape"), plot = fig_ret_prior, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# FIGURE -- Initial Over-Reaction by Prior (Step 1)
######################################################
if (.should_run("fig_ret_overreact_prior")) {
.tick("Figure -- Initial over-reaction by prior")
tryCatch({

# Average over_report_lag1 by prior bin
or_by_prior <- df_retract %>%
  group_by(prior_aligned_bin) %>%
  summarise(mean_or = mean(over_report_lag1, na.rm = TRUE),
            se_or = std.error(over_report_lag1, na.rm = TRUE),
            n = n())

fig_ret_overreact <- ggplot(or_by_prior, aes(x = prior_aligned_bin, y = mean_or)) +
  geom_bar(stat = "identity", width = 0.8, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = mean_or - 1.96 * se_or, ymax = mean_or + 1.96 * se_or), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = paste0("n=", n), y = min(mean_or - 1.96 * se_or) - 0.01), size = 2.5) +
  scale_x_discrete(name = "Prior aligned with initial signal (t-2)") +
  ylab("Over-report at initial signal (t-1)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(fig_path("retract", "fig_overreact_by_prior"), plot = fig_ret_overreact, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# FIGURE -- Initial Over-Reaction by Log Prior Odds (Step 1, scatter)
######################################################
if (.should_run("fig_ret_overreact_logodds")) {
.tick("Figure -- Initial over-reaction by log prior odds")
tryCatch({

df_ret_logodds <- df_retract %>%
  filter(prior_aligned > 0 & prior_aligned < 1) %>%
  mutate(log_prior_odds = log(prior_aligned / (1 - prior_aligned)))

fig_ret_overreact_lo <- ggplot(df_ret_logodds, aes(x = log_prior_odds, y = over_report_lag1)) +
  geom_point(alpha = 0.1, size = 1) +
  geom_smooth(method = "loess", colour = "black", fill = "grey70") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Log prior odds (aligned with initial signal, t-2)") +
  ylab("Over-report at initial signal (t-1)") +
  theme_classic()

ggsave(fig_path("retract", "fig_overreact_by_logodds"), plot = fig_ret_overreact_lo, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# FIGURE -- Retraction Response by Prior
######################################################
if (.should_run("fig_ret_response_prior")) {
.tick("Figure -- Retraction response by prior")
tryCatch({

# Average over_report_ret by prior bin
ret_by_prior <- df_retract %>%
  group_by(prior_aligned_bin) %>%
  summarise(mean_or = mean(over_report_ret, na.rm = TRUE),
            se_or = std.error(over_report_ret, na.rm = TRUE),
            n = n())

fig_ret_response <- ggplot(ret_by_prior, aes(x = prior_aligned_bin, y = mean_or)) +
  geom_bar(stat = "identity", width = 0.8, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = mean_or - 1.96 * se_or, ymax = mean_or + 1.96 * se_or), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = paste0("n=", n), y = min(mean_or - 1.96 * se_or) - 0.005), size = 2.5) +
  scale_x_discrete(name = "Prior aligned with initial signal (t-2)") +
  ylab("Belief biased towards initial signal (t)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(fig_path("retract", "fig_response_by_prior"), plot = fig_ret_response, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# TABLE -- Retraction Persistence by Prior (Step 2)
######################################################
if (.should_run("tab_ret_persist_prior")) {
.tick("Table -- Retraction persistence by prior")
tryCatch({

# Regressions
ols_persist1 <- lm(over_report_ret ~ over_report_lag1, df_retract)
ols_persist2 <- lm(over_report_ret ~ over_report_lag1 + prior_aligned_bin, df_retract)
ols_persist3 <- lm(over_report_ret ~ over_report_lag1 * prior_aligned_bin, df_retract)

# Table
write_stargazer(ols_persist1, ols_persist2, ols_persist3,
          se = list(starprep(ols_persist1, clusters = df_retract$id)[[1]],
                    starprep(ols_persist2, clusters = df_retract$id)[[1]],
                    starprep(ols_persist3, clusters = df_retract$id)[[1]]),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief biased towards initial signal"),
          column.labels = c("Baseline", "+ Prior", "Interaction"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Retraction Persistence by Prior Belief",
          notes = "SEs clustered by subject. Reference group: prior 41--50\\%. Prior aligned with initial signal direction (t-2).",
          out = tab_path("retract", "tab_persistence_by_prior"))

}, error = .fail)
}


######################################################
######################################################
# SECTION 3: CONFIRMATIONS
######################################################
######################################################

# --- Paper ---

######################################################
# FIGURE 7
# * REVISED IN REVISION: bars now show over_report instead of belief_change_adj
######################################################
if (.should_run("fig7")) {
.tick("Figure 7")
tryCatch({

df_confirm_temp <- df_confirm[df_confirm$belief_change_rational_lag1!=0,]

# Summary per type
df_confirm_id1 <- df_confirm_temp %>%
  group_by(id) %>%
  summarise(belief_change1 = mean(over_report_lag1, na.rm = TRUE),
            n = length(id))

df_confirm_id2 <- df_confirm_temp %>%
  group_by(id) %>%
  summarise(belief_change2 = mean(over_report, na.rm = TRUE),
            n = length(id))

df_confirm_change_sum1 <- df_confirm_id1 %>%
  summarise(belief_change = weighted.mean(belief_change1, n, na.rm = TRUE),
            SE = std.error(belief_change1, na.rm = TRUE),
            n = sum(n),
            type = "Initial Signal")

df_confirm_change_sum2 <- df_confirm_id2 %>%
  summarise(belief_change = weighted.mean(belief_change2, n, na.rm = TRUE),
            SE = std.error(belief_change2, na.rm = TRUE),
            n = sum(n),
            type = "After Confirmation")

df_confirm_change_sum <- rbind(df_confirm_change_sum1, df_confirm_change_sum2)

# Express beliefs in %
df_confirm_change_sum$belief_change_pts <- df_confirm_change_sum$belief_change*100
df_confirm_change_sum$SE_pts <- df_confirm_change_sum$SE*100
df_confirm_change_sum$type <- factor(df_confirm_change_sum$type, levels = c("Initial Signal", "After Confirmation"))

# Influence of Confirmations
fig_confirm_change <- ggplot(df_confirm_change_sum, aes(x = factor(type), y = belief_change_pts)) +
  geom_bar(stat="identity", width=0.9, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = belief_change_pts - 1.96*SE_pts, ymax = belief_change_pts + 1.96*SE_pts), position = position_dodge(0.9), width = 0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_discrete(name = "") +
  scale_y_continuous(limits = c(-5, 5)) +
  ylab("Belief higher than Bayesian (%pts)") +
  #labs(title = "Reaction to Confirmations",
  #     subtitle = "Mean belief change after initial signal and confirmation & 95% CI") +
  theme_classic()

ggsave(fig_path("confirm", "fig_belief_change"), plot = fig_confirm_change, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}



######################################################
# FIGURE 8
######################################################
if (.should_run("fig8")) {
.tick("Figure 8")
tryCatch({

# Summary per type
df_confirm_type <- df_confirm_temp %>%
  group_by(initial_reaction_conf) %>%
  summarise(belief_diff_sum = mean(over_report, na.rm = TRUE),
            SE = std.error(over_report, na.rm = TRUE),
            n = length(id))

# Adjusting names for graph
names(df_confirm_type)[names(df_confirm_type) == "initial_reaction_conf"] <- "type"
df_confirm_type <- mutate(df_confirm_type, type = ifelse(type=="correct", "Correctly reacted (+- 1%pt)", type))
df_confirm_type <- mutate(df_confirm_type, type = ifelse(type=="under", "Under-reacted (<1%pt)", type))
df_confirm_type <- mutate(df_confirm_type, type = ifelse(type=="over", "Over-reacted (>1%pt)", type))
df_confirm_type <- mutate(df_confirm_type, type = ifelse(type=="over much", "Over-reacted a lot", type))
df_confirm_type <- mutate(df_confirm_type, type = ifelse(type=="wrong", "Wrong direction", type))
df_confirm_type <- mutate(df_confirm_type, type = ifelse(type=="no change", "No update", type))

# Express beliefs in %
df_confirm_type$belief_diff_pts <- df_confirm_type$belief_diff_sum*100
df_confirm_type$SE_pts <- df_confirm_type$SE*100

# Restricted graph
df_confirm_type_restricted <- df_confirm_type

# Rearranging complete
df_confirm_type$type <- factor(df_confirm_type$type, levels = c("All", "Correctly reacted (+- 1%pt)", "Over-reacted (>1%pt)", "Over-reacted a lot", "Under-reacted (<1%pt)", "No update", "Wrong direction"))


# Restricted graph
df_confirm_type_restricted <- df_confirm_type_restricted[df_confirm_type_restricted$type != "Wrong direction", ]
df_confirm_type_restricted <- df_confirm_type_restricted[df_confirm_type_restricted$type != "No update", ]
df_confirm_type_restricted <- df_confirm_type_restricted[df_confirm_type_restricted$type != "Over-reacted a lot", ]

# Renaming
df_confirm_type_restricted <- mutate(df_confirm_type_restricted, type = ifelse(type=="Over-reacted (>1%pt)", "Over-reacted (>1%pt)*", type))
df_confirm_type_restricted <- mutate(df_confirm_type_restricted, type = ifelse(type=="Under-reacted (<1%pt)", "Under-reacted (<1%pt)**", type))

df_confirm_type_restricted$type <- factor(df_confirm_type_restricted$type, levels = c("Correctly reacted (+- 1%pt)", "Over-reacted (>1%pt)*", "Under-reacted (<1%pt)**"))


# Influence of Confirmations- restricted
fig_confirm_diff_restricted <- ggplot(df_confirm_type_restricted, aes(x = factor(type), y = belief_diff_pts)) +
  geom_bar(stat="identity", width=0.9, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = belief_diff_pts - 1.96*SE_pts, ymax = belief_diff_pts + 1.96*SE_pts), position = position_dodge(0.9), width = 0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_discrete(name = "Reaction to initial signal (comp. to Bayesian)", drop = FALSE) +
  scale_y_continuous(limits = c(-11, 5)) +
  annotate("text", y=-11, x=1, label=paste0("n = ", df_confirm_type_restricted$n[1]), size=3) +
  annotate("text", y=-11, x=2, label=paste0("n = ", df_confirm_type_restricted$n[2]), size=3) +
  annotate("text", y=-11, x=3, label=paste0("n = ", df_confirm_type_restricted$n[3]), size=3) +
  ylab("Belief higher than Bayesian (%pts)") +
  labs(#title = "Influence of Confirmations",
    #subtitle = "Mean belief after confirmation of initial signal & 95% CI",
    caption = "* not including observations with initial update further than confirmed signal,\n ** not including observations with initial update in wrong direction or no update.") +
  theme_classic()

ggsave(fig_path("confirm", "fig_by_initial_reaction"), plot = fig_confirm_diff_restricted, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}



######################################################
# FIGURE 11
# * REVISED IN REVISION: reduced to 2 bars, shows over_report
######################################################
if (.should_run("fig11")) {
.tick("Figure 11")
tryCatch({

# Summary per type - SE grouped by subject
df_informative_id <- df_informative %>%
  group_by(id) %>%
  summarise(belief_diff = mean(over_report, na.rm = TRUE),
            n = length(id))

df_informative_change <- df_informative_id %>%
  group_by() %>%
  summarise(belief_change = weighted.mean(belief_diff, n, na.rm = TRUE),
            SE = std.error(belief_diff, na.rm = TRUE),
            n = sum(n),
            type = "Informative Signal")

# Express beliefs in %
df_informative_change$belief_change_pts <- df_informative_change$belief_change*100
df_informative_change$SE_pts <- df_informative_change$SE*100


# Merging -- only "After Confirmation" bar (not initial signal separately)
df_confirm_change_sum2_renamed <- df_confirm_change_sum[df_confirm_change_sum$type == "After Confirmation",]
df_confirm_change_sum2_renamed$type <- "Initial Signal + Confirmation"
df_confirm_informative_change_sum <- rbind(df_confirm_change_sum2_renamed, df_informative_change)
df_confirm_informative_change_sum$type <- factor(df_confirm_informative_change_sum$type, levels = c("Initial Signal + Confirmation", "Informative Signal"))


# Plot
fig_confirm_inf_change <- ggplot(df_confirm_informative_change_sum, aes(x = type, y = belief_change_pts)) +
  geom_bar(stat="identity", width=0.9, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = belief_change_pts - 1.96*SE_pts, ymax = belief_change_pts + 1.96*SE_pts), position = position_dodge(0.9), width = 0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_discrete(name = "", drop = FALSE) +
  scale_y_continuous(limits = c(-5, 5)) +
  annotate("text", y=-5, x=1, label=paste0("n = ", df_confirm_informative_change_sum$n[1]), size=3) +
  annotate("text", y=-5, x=2, label=paste0("n = ", df_confirm_informative_change_sum$n[2]), size=3) +
  ylab("Belief higher than Bayesian (%pts)") +
  #labs(title = "Confirmations vs Informative Signals",
  #     subtitle = "Mean belief change & 95% CI") +
  theme_classic()

ggsave(fig_path("confirm", "fig_vs_informative"), plot = fig_confirm_inf_change, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# FIGURE 16
######################################################
if (.should_run("fig16")) {
.tick("Figure 16")
tryCatch({

# Influence of Confirmations - complete
fig_confirm_diff <- ggplot(df_confirm_type, aes(x = factor(type), y = belief_diff_pts)) +
  geom_bar(stat="identity", width=0.9, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = belief_diff_pts - 1.96*SE_pts, ymax = belief_diff_pts + 1.96*SE_pts), position = position_dodge(0.9), width = 0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_discrete(name = "Reaction to initial signal (comp. to Bayesian)", drop = FALSE) +
  scale_y_continuous(limits = c(-21, 15)) +
  annotate("text", y=-21, x=1, label=paste0("n = ", df_confirm_type$n[1]), size=3) +
  annotate("text", y=-21, x=2, label=paste0("n = ", df_confirm_type$n[3]), size=3) +
  annotate("text", y=-21, x=3, label=paste0("n = ", df_confirm_type$n[4]), size=3) +
  annotate("text", y=-21, x=4, label=paste0("n = ", df_confirm_type$n[5]), size=3) +
  annotate("text", y=-21, x=5, label=paste0("n = ", df_confirm_type$n[2]), size=3) +
  annotate("text", y=-21, x=6, label=paste0("n = ", df_confirm_type$n[6]), size=3) +
  ylab("Belief higher than Bayesian (%pts)") +
  #labs(title = "Influence of Confirmations",
  #     subtitle = "Mean belief after confirmation of initial signal & 95% CI") +
  theme_classic()

ggsave(fig_path("confirm", "fig_all_reactions"), plot = fig_confirm_diff, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# TABLE 11 — replaced by tab6 (objective signal c & d estimation)
######################################################
# Confirmation c & d estimation now in Table 6 alongside retractions.


######################################################
# TABLE -- Confirmation Treatment Heterogeneity (new for revision)
######################################################
if (.should_run("tab11b")) {
.tick("Table -- Confirmation Treatment Heterogeneity")
tryCatch({

# Regressions
ols_confirm_treat <- lm(over_report ~ over_report_lag1*treat_no_anchor
                        + over_report_lag1*treat_no_history, df_confirm)

# Table with overview
write_stargazer(ols_confirm_treat,
          se = starprep(ols_confirm_treat, clusters = model_clusters(ols_confirm_treat, df_confirm$id)),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief higher than Bayesian"),
          covariate.labels = c("Constant", "Initial belief over-report (t-1)", "No anchor treatment", "No history treatment", "No anchor treat * initial belief over-report (t-1)", "No history treat * initial belief over-report (t-1)"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Impact of Confirmations on Beliefs -- Treatment Heterogeneity",
          notes = "SEs clustered by subject.",
          out = tab_path("confirm", "tab_treatment"))

}, error = .fail)
}


######################################################
# TABLE 13
######################################################
if (.should_run("tab13")) {
.tick("Table 13")
tryCatch({

# create variable with all combinations of red and blue informative signals.
df_main$inf_hist <- apply(str_extract_all(df_main$hist, pattern = "[a-z]_inf", simplify = TRUE),1,paste,collapse=" ")
df_main$inf_hist <- gsub("_inf", "", df_main$inf_hist)
df_main$inf_hist <- gsub(" ", "", df_main$inf_hist)
df_main$inf_hist <- ifelse(is.na(df_main$inf_hist), "", df_main$inf_hist)

ord <- c("", "r", "b", "rr", "bb", "rb", "br",
         "rrr", "bbb", "rrb", "bbr",
         "rbb", "brr", "rbr", "brb")
df_main$inf_hist <- factor(df_main$inf_hist,levels=ord)


df_main_nouninf <- df_main[df_main$hist_uninf==0,]


# Regression
ols_inf_1a <-lm(belief ~ factor(inf_hist)
                + factor(agg_hist), df_main)

ols_inf_1b <-lm(belief ~ factor(inf_hist)
                + factor(agg_hist), df_main_nouninf)

# Table with overview
write_stargazer(ols_inf_1a,ols_inf_1b,
          type = output_type,
          style = "default",
          dep.var.labels = c("Reported Belief"),
          keep = c("inf_hist"),
          covariate.labels = c("Informative Signal: R", "Informative Signal: B",
                               "Informative Signals: RR", "Informative Signals: BB", "Informative Signals: RB", "Informative Signals: BR",
                               "Informative Signals: RRR", "Informative Signals: BBB", "Informative Signals: RRB", "Informative Signals: BBR",
                               "Informative Signals: RBB", "Informative Signals: BRR", "Informative Signals: RBR", "Informative Signals: BRB"),
          add.lines = list(c("Aggregate History FEs?", "Yes", "Yes"), c("Excl. Uninformative Signals", "No", "Yes")),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Informative Signals vs Confirmations",
          column.labels = c("All histories", "Excl. Uninformative Sig."),
          out = tab_path("confirm", "tab_vs_informative_hist"))

}, error = .fail)
}


# --- Revision additions ---

######################################################
# TABLE -- Confirmations vs Informative Signals (new for revision)
######################################################
if (.should_run("tab_conf_inf")) {
.tick("Table -- Confirmations vs Informative Signals")
tryCatch({

# Pool confirmation and informative signal observations
# Exclude ceiling/floor confirmations (belief_change_rational_lag1 == 0), matching Figure 11
df_confirm_filt <- df_confirm[df_confirm$belief_change_rational_lag1 != 0, ]
df_conf_inf <- rbind(
  data.frame(over_report = df_confirm_filt$over_report, id = df_confirm_filt$id,
             is_confirmation = 1, prior = df_confirm_filt$belief_lag2),
  data.frame(over_report = df_informative$over_report, id = df_informative$id,
             is_confirmation = 0, prior = df_informative$belief_lag1)
)
df_conf_inf$prior_bin <- cut(df_conf_inf$prior,
    breaks = seq(0, 1, 0.1), include.lowest = TRUE,
    labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))
df_conf_inf$prior_bin <- relevel(factor(df_conf_inf$prior_bin), ref = "40-50")

# Regressions
ols_conf_inf1 <- lm(over_report ~ is_confirmation, df_conf_inf)
ols_conf_inf2 <- lm(over_report ~ is_confirmation * prior_bin, df_conf_inf)

# Table
write_stargazer(ols_conf_inf1, ols_conf_inf2,
          se = starprep(ols_conf_inf1, ols_conf_inf2,
                        clusters = model_clusters(ols_conf_inf1, df_conf_inf$id)),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief higher than Bayesian"),
          covariate.labels = c("Constant", "Confirmation (vs Informative Signal)",
                               "Prior 0--10\\%", "Prior 10--20\\%", "Prior 20--30\\%", "Prior 30--40\\%",
                               "Prior 50--60\\%", "Prior 60--70\\%", "Prior 70--80\\%", "Prior 80--90\\%", "Prior 90--100\\%",
                               "Confirm $\\times$ Prior 0--10\\%", "Confirm $\\times$ Prior 10--20\\%",
                               "Confirm $\\times$ Prior 20--30\\%", "Confirm $\\times$ Prior 30--40\\%",
                               "Confirm $\\times$ Prior 50--60\\%", "Confirm $\\times$ Prior 60--70\\%",
                               "Confirm $\\times$ Prior 70--80\\%", "Confirm $\\times$ Prior 80--90\\%",
                               "Confirm $\\times$ Prior 90--100\\%"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Confirmations vs Informative Signals",
          notes = "SEs clustered by subject. Confirmations from Experiment 1 (excl. ceiling/floor), informative signals from Experiment 2. Reference group: prior 40--50\\%.",
          out = tab_path("confirm", "tab_vs_informative"))

}, error = .fail)
}


######################################################
# TABLE -- Confirmation Inference (c) and Base-Rate Use (d)
######################################################
if (.should_run("tab_conf_cd")) {
.tick("Table -- Confirmation c & d")
tryCatch({

# Regressions
ols_conf_cd <- lm(obslnpost ~ 0 + signal_ratio + prior_ratio, df_confirm)
me_conf_cd <- lmer(obslnpost ~ 0 + signal_ratio + prior_ratio + (1 | id), df_confirm)

# Table
write_stargazer(ols_conf_cd, me_conf_cd,
          se = list(starprep(ols_conf_cd, clusters = model_clusters(ols_conf_cd, df_confirm$id))[[1]], NULL),
          type = output_type,
          style = "default",
          dep.var.labels = c("Observed Log-Posterior-Ratio"),
          covariate.labels = c("Signal (c)", "Prior (d)"),
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Inference and Base-Rate Use: Confirmations",
          notes = "OLS SEs clustered by subject. Signal is person-specific (accounts for initial reaction).",
          out = tab_path("confirm", "tab_cd"))

}, error = .fail)
}


######################################################
# TABLE -- Confirmation Signal History Robustness (new for revision)
######################################################
if (.should_run("tab_conf_hist")) {
.tick("Table -- Confirmation Signal History Robustness")
tryCatch({

# Regressions
ols_conf_hist1 <- lm(over_report ~ over_report_lag1, df_confirm)
ols_conf_hist2 <- lm(over_report ~ over_report_lag1 + factor(prev_verified), df_confirm)
ols_conf_hist3 <- lm(over_report ~ over_report_lag1 * factor(prev_verified), df_confirm)

# Table
write_stargazer(ols_conf_hist1, ols_conf_hist2, ols_conf_hist3,
          se = list(
            starprep(ols_conf_hist1, clusters = model_clusters(ols_conf_hist1, df_confirm$id))[[1]],
            starprep(ols_conf_hist2, clusters = model_clusters(ols_conf_hist2, df_confirm$id))[[1]],
            starprep(ols_conf_hist3, clusters = model_clusters(ols_conf_hist3, df_confirm$id))[[1]]
          ),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief higher than Bayesian"),
          covariate.labels = c("Constant",
                               "Over-report (t-1)",
                               "1 prev. verification",
                               "2 prev. verifications",
                               "Over-report (t-1) $\\times$ 1 prev. verif.",
                               "Over-report (t-1) $\\times$ 2 prev. verif."),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Impact of Confirmations on Beliefs -- Signal History Robustness",
          notes = "SEs clustered by subject.",
          out = tab_path("confirm", "tab_signal_history"))

}, error = .fail)
}


######################################################
# TABLE -- Confirmation Prior Belief Heterogeneity (new for revision)
######################################################
if (.should_run("tab_conf_prior")) {
.tick("Table -- Confirmation Prior Belief Heterogeneity")
tryCatch({

# Regression
ols_conf_prior <- lm(over_report ~ over_report_lag1 * belief_lag2_bin, df_confirm)

# Table
write_stargazer(ols_conf_prior,
          se = list(starprep(ols_conf_prior, clusters = model_clusters(ols_conf_prior, df_confirm$id))[[1]]),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief higher than Bayesian"),
          covariate.labels = c("Constant",
                               "Over-report (t-1)",
                               "Prior 0--10\\%",
                               "Prior 11--20\\%",
                               "Prior 21--30\\%",
                               "Prior 31--40\\%",
                               "Prior 51--60\\%",
                               "Prior 61--70\\%",
                               "Prior 71--80\\%",
                               "Prior 81--90\\%",
                               "Prior 91--100\\%",
                               "Over-report (t-1) $\\times$ Prior 0--10\\%",
                               "Over-report (t-1) $\\times$ Prior 11--20\\%",
                               "Over-report (t-1) $\\times$ Prior 21--30\\%",
                               "Over-report (t-1) $\\times$ Prior 31--40\\%",
                               "Over-report (t-1) $\\times$ Prior 51--60\\%",
                               "Over-report (t-1) $\\times$ Prior 61--70\\%",
                               "Over-report (t-1) $\\times$ Prior 71--80\\%",
                               "Over-report (t-1) $\\times$ Prior 81--90\\%",
                               "Over-report (t-1) $\\times$ Prior 91--100\\%"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Impact of Confirmations on Beliefs -- Prior Belief Heterogeneity",
          notes = "SEs clustered by subject. Reference group: prior 41--50\\%. Prior aligned with initial signal direction (t-2).",
          out = tab_path("confirm", "tab_prior_heterogeneity"))

}, error = .fail)
}


######################################################
# FIGURE -- Confirmation Prior Belief U-shape (new for revision)
######################################################
if (.should_run("fig_conf_prior")) {
.tick("Figure -- Confirmation Prior Belief U-shape")
tryCatch({

rob_conf_prior <- lm_robust(over_report ~ over_report_lag1 * belief_lag2_bin,
                             data = df_confirm, clusters = id, se_type = "CR2")

# Extract total over_report_lag1 effect per bin
beta <- coef(rob_conf_prior)
V <- vcov(rob_conf_prior)
base_idx <- which(names(beta) == "over_report_lag1")
base_coef <- beta[base_idx]

bin_labels <- prior_bin_labels

total_effects <- data.frame(bin = bin_labels, effect = NA, se = NA, n = NA)

for (i in seq_along(bin_labels)) {
  bl <- bin_labels[i]
  total_effects$n[i] <- sum(df_confirm$belief_lag2_bin == bl, na.rm = TRUE)

  if (bl == prior_bin_ref) {
    total_effects$effect[i] <- base_coef
    total_effects$se[i] <- sqrt(V[base_idx, base_idx])
  } else {
    int_name <- paste0("over_report_lag1:belief_lag2_bin", bl)
    int_idx <- which(names(beta) == int_name)
    total_effects$effect[i] <- base_coef + beta[int_idx]
    total_effects$se[i] <- sqrt(V[base_idx, base_idx] + V[int_idx, int_idx] + 2 * V[base_idx, int_idx])
  }
}

total_effects$effect_pct <- total_effects$effect
total_effects$se_pct <- total_effects$se
total_effects$bin <- factor(total_effects$bin, levels = bin_labels)

fig_conf_prior <- ggplot(total_effects, aes(x = bin, y = effect_pct)) +
  geom_bar(stat = "identity", width = 0.8, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = effect_pct - 1.96 * se_pct, ymax = effect_pct + 1.96 * se_pct),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = paste0("n=", n), y = -1.2), size = 2.5) +
  scale_x_discrete(name = "Prior aligned with initial signal (t-2)") +
  ylab("Over-report (t-1) coefficient") +
  theme_classic()

ggsave(fig_path("confirm", "fig_prior_ushape"), plot = fig_conf_prior, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# FIGURE -- Initial Over-Reaction by Prior (Confirmations)
######################################################
if (.should_run("fig_conf_overreact_prior")) {
.tick("Figure -- Initial over-reaction by prior (confirmations)")
tryCatch({

# Average over_report at confirmation by prior bin
or_by_prior_conf <- df_confirm %>%
  mutate(prior_bin = factor(belief_lag2_bin, levels = prior_bin_labels)) %>%
  group_by(prior_bin) %>%
  summarise(mean_or = mean(over_report, na.rm = TRUE),
            se_or = std.error(over_report, na.rm = TRUE),
            n = n())

fig_conf_overreact <- ggplot(or_by_prior_conf, aes(x = prior_bin, y = mean_or)) +
  geom_bar(stat = "identity", width = 0.8, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = mean_or - 1.96 * se_or, ymax = mean_or + 1.96 * se_or), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = paste0("n=", n), y = min(mean_or - 1.96 * se_or) - 0.01), size = 2.5) +
  scale_x_discrete(name = "Prior aligned with initial signal (t-2)") +
  ylab("Over-report at confirmation (t)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(fig_path("confirm", "fig_overreact_by_prior"), plot = fig_conf_overreact, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# FIGURE -- Confirmation Over-Report by Log Prior Odds (scatter)
######################################################
if (.should_run("fig_conf_overreact_logodds")) {
.tick("Figure -- Confirmation over-report by log prior odds")
tryCatch({

df_conf_logodds <- df_confirm %>%
  filter(prior_aligned > 0 & prior_aligned < 1) %>%
  mutate(log_prior_odds = log(prior_aligned / (1 - prior_aligned)))

fig_conf_overreact_lo <- ggplot(df_conf_logodds, aes(x = log_prior_odds, y = over_report)) +
  geom_point(alpha = 0.1, size = 1) +
  geom_smooth(method = "loess", colour = "black", fill = "grey70") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Log prior odds (aligned with initial signal, t-2)") +
  ylab("Over-report at confirmation (t)") +
  theme_classic()

ggsave(fig_path("confirm", "fig_overreact_by_logodds"), plot = fig_conf_overreact_lo, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# RUN SUMMARY
######################################################
# Final section timing
if (!is.null(.last_section)) {
  elapsed <- proc.time()["elapsed"] - .section_start
  .section_times[[.last_section]] <- elapsed
  if (is.null(.section_status[[.last_section]])) .section_status[[.last_section]] <- "OK"
  cat(sprintf("   done (%.1fs)\n", elapsed))
}
total <- proc.time()["elapsed"] - .timer_start

n_ok <- sum(unlist(.section_status) == "OK")
n_fail <- sum(unlist(.section_status) != "OK")
cat(sprintf("\n========================================\n"))
cat(sprintf("  Figures and Tables.R complete\n"))
cat(sprintf("  %d sections run: %d OK, %d failed\n", n_ok + n_fail, n_ok, n_fail))
cat(sprintf("  Total: %.0fs (%.1f min)\n", total, total / 60))
cat(sprintf("========================================\n"))

# Show failures first
if (n_fail > 0) {
  cat("\n  FAILURES:\n")
  for (nm in names(.section_status)) {
    if (.section_status[[nm]] != "OK") {
      cat(sprintf("    !! %s: %s\n", nm, .section_status[[nm]]))
    }
  }
}

# Timing sorted slowest first
cat("\n  Section times (slowest first):\n")
times_vec <- unlist(.section_times)
times_sorted <- sort(times_vec, decreasing = TRUE)
for (nm in names(times_sorted)) {
  st <- .section_status[[nm]]
  status <- if (is.null(st) || st == "OK") " " else "!"
  cat(sprintf("  %s %5.1fs  %s\n", status, times_sorted[nm], nm))
}
cat("\n")
