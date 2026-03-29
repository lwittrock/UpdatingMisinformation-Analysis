# ──────────────────────────────────────────────────
# Confirmation Analysis
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Purpose: Generate figures and tables for confirmation signals
# Inputs:  Processed datasets (loaded by derived_variables.R)
# Outputs: output/confirm/figures/*.jpg, output/confirm/tables/*.tex
# ──────────────────────────────────────────────────

# Standalone support: load utils if not already loaded by run_all.R
if (!exists(".utils_loaded")) {
  source("code/utils/packages.R")
  source("code/utils/constants.R")
  output_type <- "latex"
  output_extension <- "tex"
  source("code/utils/helpers.R")
  source("code/utils/plot_theme.R")
  source("code/utils/run_control.R")
  if (!exists("run_sections")) run_sections <- "all"
  inpath <- "data/processed"
  set_dpi <- 400
  source("code/utils/derived_variables.R")
}

cat(">> Section 3: Confirmations\n")


# --- Paper ---

######################################################
# FIGURE 7
# * REVISED IN REVISION: bars now show over_report instead of belief_change_adj
######################################################
if (.should_run("fig_conf_all_reactions")) {
.tick("Figure -- All Confirmation Reactions")
tryCatch({

df_conf_nonceiling <- df_confirm[df_confirm$belief_change_rational_lag1!=0,]

# Summary per type
df_confirm_id1 <- df_conf_nonceiling %>%
  group_by(id) %>%
  summarise(belief_change1 = mean(over_report_lag1, na.rm = TRUE),
            n = length(id))

df_confirm_id2 <- df_conf_nonceiling %>%
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
  theme_classic()

ggsave(fig_path("confirm", "fig_belief_change"), plot = fig_confirm_change, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}



######################################################
# FIGURE 8
######################################################
if (.should_run("fig_conf_belief_change")) {
.tick("Figure -- Confirmation Belief Change")
tryCatch({

# Summary per type
df_conf_by_reaction <- df_conf_nonceiling %>%
  group_by(initial_reaction_conf) %>%
  summarise(belief_diff_sum = mean(over_report, na.rm = TRUE),
            SE = std.error(over_report, na.rm = TRUE),
            n = length(id))

# Adjusting names for graph
names(df_conf_by_reaction)[names(df_conf_by_reaction) == "initial_reaction_conf"] <- "type"
df_conf_by_reaction <- mutate(df_conf_by_reaction, type = ifelse(type=="correct", "Correctly reacted (+- 1%pt)", type))
df_conf_by_reaction <- mutate(df_conf_by_reaction, type = ifelse(type=="under", "Under-reacted (<1%pt)", type))
df_conf_by_reaction <- mutate(df_conf_by_reaction, type = ifelse(type=="over", "Over-reacted (>1%pt)", type))
df_conf_by_reaction <- mutate(df_conf_by_reaction, type = ifelse(type=="over much", "Over-reacted a lot", type))
df_conf_by_reaction <- mutate(df_conf_by_reaction, type = ifelse(type=="wrong", "Wrong direction", type))
df_conf_by_reaction <- mutate(df_conf_by_reaction, type = ifelse(type=="no change", "No update", type))

# Express beliefs in %
df_conf_by_reaction$belief_diff_pts <- df_conf_by_reaction$belief_diff_sum*100
df_conf_by_reaction$SE_pts <- df_conf_by_reaction$SE*100

# Restricted graph
df_conf_by_reaction_clean <- df_conf_by_reaction

# Rearranging complete
df_conf_by_reaction$type <- factor(df_conf_by_reaction$type, levels = c("All", "Correctly reacted (+- 1%pt)", "Over-reacted (>1%pt)", "Over-reacted a lot", "Under-reacted (<1%pt)", "No update", "Wrong direction"))


# Restricted graph
df_conf_by_reaction_clean <- df_conf_by_reaction_clean[df_conf_by_reaction_clean$type != "Wrong direction", ]
df_conf_by_reaction_clean <- df_conf_by_reaction_clean[df_conf_by_reaction_clean$type != "No update", ]
df_conf_by_reaction_clean <- df_conf_by_reaction_clean[df_conf_by_reaction_clean$type != "Over-reacted a lot", ]

# Renaming
df_conf_by_reaction_clean <- mutate(df_conf_by_reaction_clean, type = ifelse(type=="Over-reacted (>1%pt)", "Over-reacted (>1%pt)*", type))
df_conf_by_reaction_clean <- mutate(df_conf_by_reaction_clean, type = ifelse(type=="Under-reacted (<1%pt)", "Under-reacted (<1%pt)**", type))

df_conf_by_reaction_clean$type <- factor(df_conf_by_reaction_clean$type, levels = c("Correctly reacted (+- 1%pt)", "Over-reacted (>1%pt)*", "Under-reacted (<1%pt)**"))


# Influence of Confirmations- restricted
fig_confirm_diff_restricted <- ggplot(df_conf_by_reaction_clean, aes(x = factor(type), y = belief_diff_pts)) +
  geom_bar(stat="identity", width=0.9, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = belief_diff_pts - 1.96*SE_pts, ymax = belief_diff_pts + 1.96*SE_pts), position = position_dodge(0.9), width = 0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_discrete(name = "Reaction to initial signal (comp. to Bayesian)", drop = FALSE) +
  scale_y_continuous(limits = c(-11, 5)) +
  annotate("text", y=-11, x=1, label=paste0("n = ", df_conf_by_reaction_clean$n[1]), size=3) +
  annotate("text", y=-11, x=2, label=paste0("n = ", df_conf_by_reaction_clean$n[2]), size=3) +
  annotate("text", y=-11, x=3, label=paste0("n = ", df_conf_by_reaction_clean$n[3]), size=3) +
  ylab("Belief higher than Bayesian (%pts)") +
  labs(caption = "* not including observations with initial update further than confirmed signal,\n ** not including observations with initial update in wrong direction or no update.") +
  theme_classic()

ggsave(fig_path("confirm", "fig_by_initial_reaction"), plot = fig_confirm_diff_restricted, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}



######################################################
# FIGURE 11
# * REVISED IN REVISION: reduced to 2 bars, shows over_report
######################################################
if (.should_run("fig_conf_by_initial")) {
.tick("Figure -- Confirmation by Initial Reaction")
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
  theme_classic()

ggsave(fig_path("confirm", "fig_vs_informative"), plot = fig_confirm_inf_change, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}


######################################################
# FIGURE 16
######################################################
if (.should_run("fig_conf_vs_informative")) {
.tick("Figure -- Confirmation vs Informative")
tryCatch({

# Influence of Confirmations - complete
fig_confirm_diff <- ggplot(df_conf_by_reaction, aes(x = factor(type), y = belief_diff_pts)) +
  geom_bar(stat="identity", width=0.9, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = belief_diff_pts - 1.96*SE_pts, ymax = belief_diff_pts + 1.96*SE_pts), position = position_dodge(0.9), width = 0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_discrete(name = "Reaction to initial signal (comp. to Bayesian)", drop = FALSE) +
  scale_y_continuous(limits = c(-21, 15)) +
  annotate("text", y=-21, x=1, label=paste0("n = ", df_conf_by_reaction$n[1]), size=3) +
  annotate("text", y=-21, x=2, label=paste0("n = ", df_conf_by_reaction$n[3]), size=3) +
  annotate("text", y=-21, x=3, label=paste0("n = ", df_conf_by_reaction$n[4]), size=3) +
  annotate("text", y=-21, x=4, label=paste0("n = ", df_conf_by_reaction$n[5]), size=3) +
  annotate("text", y=-21, x=5, label=paste0("n = ", df_conf_by_reaction$n[2]), size=3) +
  annotate("text", y=-21, x=6, label=paste0("n = ", df_conf_by_reaction$n[6]), size=3) +
  ylab("Belief higher than Bayesian (%pts)") +
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
if (.should_run("tab_conf_types")) {
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
if (.should_run("tab_conf_vs_informative")) {
.tick("Table -- Confirmation vs Informative")
tryCatch({

# create variable with all combinations of red and blue informative signals.
df_main$inf_hist <- apply(str_extract_all(df_main$signal_hist, pattern = "[a-z]_inf", simplify = TRUE),1,paste,collapse=" ")
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
                + factor(aggregate_hist), df_main)

ols_inf_1b <-lm(belief ~ factor(inf_hist)
                + factor(aggregate_hist), df_main_nouninf)

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
ols_conf_cd <- lm(obs_log_post_ratio ~ 0 + signal_ratio + prior_ratio, df_confirm)
me_conf_cd <- lmer(obs_log_post_ratio ~ 0 + signal_ratio + prior_ratio + (1 | id), df_confirm)

# Table
write_stargazer(ols_conf_cd, me_conf_cd,
          se = list(starprep(ols_conf_cd, clusters = model_clusters(ols_conf_cd, df_confirm$id))[[1]], NULL),
          type = output_type,
          style = "default",
          dep.var.labels = c("Observed Log-Posterior-Ratio"),
          covariate.labels = c("Signal (d)", "Prior (c)"),
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

df_conf_prior_bins <- df_confirm %>%
  filter(prior_aligned > 0 & prior_aligned < 1) %>%
  mutate(log_prior_odds = log(prior_aligned / (1 - prior_aligned)))

fig_conf_overreact_lo <- ggplot(df_conf_prior_bins, aes(x = log_prior_odds, y = over_report)) +
  geom_point(alpha = 0.1, size = 1) +
  geom_smooth(method = "loess", colour = "black", fill = "grey70") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Log prior odds (aligned with initial signal, t-2)") +
  ylab("Over-report at confirmation (t)") +
  theme_classic()

ggsave(fig_path("confirm", "fig_overreact_by_logodds"), plot = fig_conf_overreact_lo, width = fig_width, height = fig_height, units = "in", dpi = set_dpi)

}, error = .fail)
}
