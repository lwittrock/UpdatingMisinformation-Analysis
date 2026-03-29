# ──────────────────────────────────────────────────
# Retraction Analysis
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Purpose: Generate figures and tables for retraction signals
# Inputs:  Processed datasets (loaded by derived_variables.R)
# Outputs: output/retract/figures/*.jpg, output/retract/tables/*.tex
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

cat(">> Section 2: Retractions\n")


# --- Paper ---

######################################################
# FIGURE 5
######################################################
if (.should_run("fig5")) {
.tick("Figure 5")
tryCatch({

# Summary per type - SE grouped by subject
df_ret_by_initial <- df_retract %>%
  group_by(initial_reaction_alt, id, treat) %>%
  summarise(belief_diff = mean(-over_report, na.rm = TRUE),
            n = length(id))

df_ret_by_reaction <- df_ret_by_initial %>%
  group_by(initial_reaction_alt) %>%
  summarise(belief_diff_sum = weighted.mean(belief_diff, n, na.rm = TRUE),
            SE = std.error(belief_diff, na.rm = TRUE),
            n = sum(n))

# Adjusting names for graph
names(df_ret_by_reaction)[names(df_ret_by_reaction) == "initial_reaction_alt"] <- "type"
df_ret_by_reaction <- mutate(df_ret_by_reaction, type = ifelse(type=="correct", "Correctly reacted (+- 1%pt)", type))
df_ret_by_reaction <- mutate(df_ret_by_reaction, type = ifelse(type=="under", "Under-reacted (<1%pt)*", type))
df_ret_by_reaction <- mutate(df_ret_by_reaction, type = ifelse(type=="over", "Over-reacted (>1%pt)", type))
df_ret_by_reaction <- mutate(df_ret_by_reaction, type = ifelse(type=="wrong", "Wrong direction", type))

# Summary data all
df_ret_response_summary <- df_ret_by_initial %>%
  group_by() %>%
  summarise(belief_diff_sum = weighted.mean(belief_diff, n, na.rm = TRUE),
            SE = std.error(belief_diff, na.rm = TRUE),
            n = sum(n),
            type = "All Retractions")

# Merging all and per type
df_ret_response_labeled <- rbind(df_ret_response_summary, df_ret_by_reaction)

# Remove wrong for graph
df_ret_response_labeled <- df_ret_response_labeled[df_ret_response_labeled$type != "Wrong direction", ]

# Express beliefs in %
df_ret_response_labeled$belief_diff_pts <- df_ret_response_labeled$belief_diff_sum*100
df_ret_response_labeled$SE_pts <- df_ret_response_labeled$SE*100
df_ret_response_labeled$type <- factor(df_ret_response_labeled$type, levels = c("Correctly reacted (+- 1%pt)", "Over-reacted (>1%pt)", "Under-reacted (<1%pt)*", "All Retractions"))

# Influence of retraced signals - belief difference
fig_retract_diff_group <- ggplot(df_ret_response_labeled, aes(x = type, y = belief_diff_pts)) +
  geom_bar(stat="identity", width=0.9, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = belief_diff_pts - 1.96*SE_pts, ymax = belief_diff_pts + 1.96*SE_pts), position = position_dodge(0.9), width = 0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_discrete(name = "Reaction to initial signal (relative to Bayesian)", drop = FALSE) +
  scale_y_continuous(limits = c(-5.5, 13)) +
  annotate("text", y=-5.5, x=1, label=paste0("n = ", df_ret_response_labeled$n[2]), size=3) +
  annotate("text", y=-5.5, x=2, label=paste0("n = ", df_ret_response_labeled$n[3]), size=3) +
  annotate("text", y=-5.5, x=3, label=paste0("n = ", df_ret_response_labeled$n[4]), size=3) +
  annotate("text", y=-5.5, x=4, label=paste0("n = ", df_ret_response_labeled$n[1]), size=3) +
  ylab("Belief biased towards initial signal (%pts)") +
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
df_reg_opposite_signals <- df_main_ver[df_main_ver$two_balls=="BR" | df_main_ver$two_balls=="RB",]

# Summary data all
df_regular_sum_all <- df_reg_opposite_signals %>%
  summarise(belief_diff_sum = mean(-over_report2, na.rm = TRUE),
            SE = std.error(-over_report2, na.rm = TRUE),
            n = length(id),
            type = "All Retractions/ Opposite Signals")

# Merging relevant data
df_ret_vs_opposite <- rbind(df_regular_sum_all, df_ret_response_summary)
df_ret_vs_opposite <- mutate(df_ret_vs_opposite, type = ifelse(type=="All Retractions/ Opposite Signals", "Opposite New Information", type))
df_ret_vs_opposite <- mutate(df_ret_vs_opposite, type = ifelse(type=="All Retractions", "Retraction", type))

df_ret_vs_opposite$belief_diff_pts <- df_ret_vs_opposite$belief_diff_sum*100
df_ret_vs_opposite$SE_pts <- df_ret_vs_opposite$SE*100

# Plot
fig_regular_diff_group_all <- ggplot(NULL, ) +
  geom_col(aes(x = type, y = belief_diff_pts),
           data = df_ret_vs_opposite, width=0.9,
           fill = "white", color = "black", alpha = 0.5) +
  geom_errorbar(aes(x = type, y = belief_diff_pts,
                    ymin = belief_diff_pts - 1.96*SE_pts,
                    ymax = belief_diff_pts + 1.96*SE_pts),
                data = df_ret_vs_opposite, position = position_dodge(0.2),
                width = 0.2, color = "black") +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_discrete(name = "", drop = FALSE) +
  scale_y_continuous(limits = c(-6, 2.5)) +
  annotate("text", y=-6, x=1, label=paste0("n = ", df_ret_vs_opposite$n[1]), size=3, color="black") +
  annotate("text", y=-6, x=2, label=paste0("n = ", df_ret_vs_opposite$n[2]), size=3, color="black") +
  ylab("Belief biased towards initial signal (%pts)") +
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
df_ret_robust <- df_retract[df_retract$belief_change_adj <= 0.75
                                     & df_retract$belief_change_adj >= -0.25
                                     & df_retract$belief_change_adj_lag1 <= 0.75
                                     & df_retract$belief_change_adj_lag1 >= -0.25, ]

# Plot formatting
fig_retract_change_basic <- ggplot(df_ret_robust, aes(x = belief_change_adj_lag1, y = -belief_change_adj)) +
  geom_abline(slope = -1, intercept = 0, linetype = "dashed", size = 1, alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.75) +
  xlab("Belief Change: Initial Signal") +
  ylab("Belief Change: Retraction") +
  scale_x_continuous(limits = c(-0.25, 0.75)) +
  scale_y_continuous(limits = c(-0.75, 0.25)) +
  labs(caption = "Zoomed in for better visibility (~5% of data points omitted)") +
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
df_ret_robust <- df_retract[df_retract$over_report <= 0.5
                                     & df_retract$over_report >= -0.5
                                     & df_retract$over_report_lag1 <= 0.5
                                     & df_retract$over_report_lag1 >= -0.5, ]

# Beliefs in % for graph
df_ret_robust$over_report_lag1_pts <- df_ret_robust$over_report_lag1*100
df_ret_robust$under_report_pts <- -df_ret_robust$over_report*100

# Plot
fig_retract_diff_cont <- ggplot(df_ret_robust, aes(x = over_report_lag1_pts, y = under_report_pts)) +
  geom_jitter(alpha = 0.3) +
  geom_abline(slope = 0, intercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_smooth(method = 'lm', formula = y ~ x, se = TRUE) +
  xlab("Reaction to initial signal (comp. to Bayesian)") +
  ylab("Belief biased towards initial signal (%pts)") +
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
ols_ret_persistence <- lm(over_report_ret ~ over_report_lag1, df_retract)

# Table with overview
write_stargazer(ols_ret_persistence,
          se = starprep(ols_ret_persistence, clusters = df_retract$id),
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
df_main$ver_retract_all <- ifelse(df_main$is_retraction==1 & !is.na(df_main$is_retraction), 1, 0)

# create variable with all combinations of red and blue retractions. Otherwise potential confounds.
df_main$ret_hist <- apply(str_extract_all(df_main$signal_hist, pattern = "[a-z]_ret", simplify = TRUE),1,paste,collapse=" ")

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
ols_ret_comp_hist <-lm(belief ~ ver_retract_all + factor(ret_hist)
                    + factor(compressed_hist), df_main_t1)

ols_ret_comp_hist_round <-lm(belief ~ ver_retract_all + factor(ret_hist)
                    + factor(compressed_hist) + factor(round), df_main_t1)


# Table with overview
write_stargazer(ols_ret_comp_hist, ols_ret_comp_hist_round,
          #se = starprep(ols_ret_comp_hist, ols_ret_comp_hist_round, clusters = df_main$id), # horribly slow.
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
ols_retract_obj <- lm(obs_log_post_ratio ~ 0 + signal_ratio_obj + prior_ratio, df_retract)
ols_confirm_obj <- lm(obs_log_post_ratio ~ 0 + signal_ratio_obj + prior_ratio, df_confirm)
ols_retcon_obj  <- lm(obs_log_post_ratio ~ 0 + signal_ratio_obj + prior_ratio, df_retcon)

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

# Preparation 2 (df_coef created in derived_variables.R)
df_retract <- merge(df_retract, df_coef, by = "id")
df_retract$inference_adj <- df_retract$inference - 1
df_retract$base_rate_use_adj <- df_retract$base_rate_use - 1

# Preparation 3
df_reg_type_counts <- df_regular %>%
  group_by(id, reaction_alt) %>%
  summarise(id = mean(id), n = length(belief), type_subject = first(reaction_alt))
df_regular_type_id <- df_reg_type_counts %>%
  group_by(id) %>%
  filter(n == max(n))
df_regular_type_id <- mutate(df_regular_type_id, type_subject = ifelse(n<5, "Not categorized", type_subject))
df_regular_type_id <- df_regular_type_id %>% select(id, type_subject)
df_regular_type_id <- unique(df_regular_type_id)

df_retract <- merge(df_retract, df_regular_type_id, by="id")

# Regressions
ols_ret_by_avg <- lm(over_report_ret ~ over_report_lag1 + overreport_avg, df_retract)
ols_ret_by_cd <- lm(over_report_ret ~ over_report_lag1 + inference_adj + base_rate_use_adj, df_retract)
ols_ret_by_type <- lm(over_report_ret ~ over_report_lag1 + type_subject, df_retract)
ols_ret_by_subject_fe <- lm(over_report_ret ~ over_report_lag1 + factor(id), df_retract)

# Table with overview
write_stargazer(ols_ret_by_avg, ols_ret_by_cd, ols_ret_by_type, ols_ret_by_subject_fe,
          se = starprep(ols_ret_by_avg, ols_ret_by_cd, ols_ret_by_type, clusters = df_retract$id),
          keep = c("Constant", "over_report_lag1", "type_subject", "inference", "base_rate", "overreport_avg"),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief biased towards initial signal"),
          covariate.labels = c("Constant",
                               "Initial belief over-report",
                               "Average belief over-report",
                               "Average inference (d-1)",
                               "Average base-rate use (c-1)",
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
ols_ret_lag2 <- lm(over_report_ret ~ over_report_lag1 + over_report_lag2, df_retract)

ols_ret_treat <- lm(over_report_ret ~ over_report_lag1*treat_no_anchor
                          + over_report_lag1*treat_no_history, df_retract)


# Table with overview
write_stargazer(ols_ret_lag2, ols_ret_treat,
          se = starprep(ols_ret_lag2, ols_ret_treat, clusters = df_retract$id),
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
ols_ret_induced_prior <- lm(belief_dev_induced_adj ~ over_report_lag1, df_retract)

# Table with overview
write_stargazer(ols_ret_induced_prior,
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

df_main$ver_retract_all <- ifelse(df_main$is_retraction==1 & !is.na(df_main$is_retraction), 1, 0)

# Regression
ols_ret_vs_opposite <-lm(belief ~ ver_retract_all*signal_direction
                     + factor(sign_adjusted_hist), df_main[df_main$aggregate_round==0,])

# Output
write_stargazer(ols_ret_vs_opposite,
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
df_uninf_subject_stats <- df_uninformative %>%
  group_by(id) %>%
  summarise(belief_diff = mean(over_report, na.rm = TRUE),
            n = length(id))

df_uninf_summary <- df_uninf_subject_stats %>%
  group_by() %>%
  summarise(belief_diff_sum = weighted.mean(belief_diff, n, na.rm = TRUE),
            SE = std.error(belief_diff, na.rm = TRUE),
            n = sum(n),
            type = "Uninformative Signal")

# Merging with summary data on retractions
df_retract_uninformative <- rbind(df_uninf_summary, df_ret_response_summary)
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

}, error = .fail)
}


######################################################
# TABLE 12
######################################################
if (.should_run("tab12")) {
.tick("Table 12")
tryCatch({

# Preparation: create variable with all combinations of red and blue uninformative signals.
df_main$uninf_hist <- apply(str_extract_all(df_main$signal_hist, pattern = "[a-z]_uninf", simplify = TRUE),1,paste,collapse=" ")
df_main$uninf_hist <- gsub("_uninf", "", df_main$uninf_hist)
df_main$uninf_hist <- gsub(" ", "", df_main$uninf_hist)
df_main$uninf_hist <- ifelse(is.na(df_main$uninf_hist), "", df_main$uninf_hist)

ord <- c("", "r", "b", "rr", "bb", "rb", "br",
         "rrr", "bbb", "rrb", "bbr",
         "rbb", "brr", "rbr", "brb")
df_main$uninf_hist <- factor(df_main$uninf_hist,levels=ord)


# Regression
ols_uninf_1a <-lm(belief ~ factor(uninf_hist)
                  + factor(aggregate_hist), df_main)

# Table with overview
write_stargazer(ols_uninf_1a,
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
ols_ret_cd <- lm(obs_log_post_ratio ~ 0 + signal_ratio + prior_ratio, df_retract)
me_ret_cd <- lmer(obs_log_post_ratio ~ 0 + signal_ratio + prior_ratio + (1 | id), df_retract)

# Table
write_stargazer(ols_ret_cd, me_ret_cd,
          se = list(starprep(ols_ret_cd, clusters = df_retract$id)[[1]], NULL),
          type = output_type,
          style = "default",
          dep.var.labels = c("Observed Log-Posterior-Ratio"),
          covariate.labels = c("Signal (d)", "Prior (c)"),
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
