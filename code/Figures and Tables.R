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
#   "all"      — run everything (default)
#   "figures"  — only figures
#   "tables"   — only tables
#   c("fig5", "fig6", "tab1", ...) — specific items (fig5-fig17, tab1-tab15)
# Dependencies are handled automatically (e.g. tab7 needs fig12, fig16 needs fig8).
run_sections <- c("tab_conf_inf")


######################################################
# TO ADJUST
######################################################

# File locations relative to project root -- run scripts with setwd() at project root
inpath <- "data/processed"
fig_outpath <- "output/figures"
tab_outpath <- "output/tables"

# Setting file type for tables
output_type <- "latex" # can be set to 'html' or 'latex'

# Change figure quality
set_dpi <- 400



######################################################
# SETUP
######################################################

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

# Helper: get cluster vector matching rows actually used by lm (after NA removal)
model_clusters <- function(model, cluster_vec) {
  cluster_vec[as.integer(names(model$residuals))]
}

# Reading data
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

# Section selector helper
# Dependencies: fig8 -> fig16, fig7 -> fig11, fig13 -> tab6+, fig12 -> tab7
.deps <- list(fig16 = "fig8", fig11 = "fig7", tab6 = "fig13", tab7 = c("fig12", "fig13"),
              tab8 = "fig13", tab9 = "fig13", tab10 = "fig13", tab11 = "fig13",
              tab11b = "fig13", tab12 = "fig13", tab13 = "fig13", tab14 = "fig13", tab15 = "fig13",
              tab_ret_hist = "fig13", tab_ret_prior = "fig13", fig_ret_prior = "fig13",
              tab_conf_hist = "fig13", tab_conf_prior = "fig13", fig_conf_prior = "fig13")
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
.tick <- function(label) {
  now <- proc.time()["elapsed"]
  if (!is.null(.last_section)) {
    elapsed <- now - .section_start
    .section_times[[.last_section]] <<- elapsed
    cat(sprintf("   done (%.1fs)\n", elapsed))
  }
  cat(paste0(">> ", label, "...\n"))
  .section_start <<- now
  .last_section <<- label
}
cat(">> Figures and Tables.R started\n")

######################################################
# FIGURE 5
######################################################
if (.should_run("fig5")) {
.tick("Figure 5")

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

ggsave(paste0(fig_outpath, "/02_fig_retract_diff_group.jpg"), plot = fig_retract_diff_group, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}


######################################################
# FIGURE 6
######################################################
if (.should_run("fig6")) {
.tick("Figure 6")

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

ggsave(paste0(fig_outpath, "/02_fig_retract_diff_vs_opposite_ball_all.jpg"), plot = fig_regular_diff_group_all, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}


######################################################
# FIGURE 7
######################################################
if (.should_run("fig7")) {
.tick("Figure 7")

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

ggsave(paste0(fig_outpath, "/02_fig_confirm_change.jpg"), plot = fig_confirm_change, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}



######################################################
# FIGURE 8
######################################################
if (.should_run("fig8")) {
.tick("Figure 8")

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

ggsave(paste0(fig_outpath, "/02_fig_confirm_diff_restricted.jpg"), plot = fig_confirm_diff_restricted, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}



######################################################
# FIGURE 9
######################################################
if (.should_run("fig9")) {
.tick("Figure 9")

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

ggsave(paste0(fig_outpath, "/02_fig_retract_vs_uninformative.jpg"), plot = fig_retract_uninf, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}



######################################################
# FIGURE 10
######################################################
if (.should_run("fig10")) {
.tick("Figure 10")

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
fig_variance_retract_uninf_all <- grid.arrange(grobs=lapply(c(1,4), function(i) {
  arrangeGrob(grobs=graphs[i:(i+2)], top=col.titles[i/3 + 1], ncol=1)
}), ncol=2)

# Final output
ggsave(paste0(fig_outpath, "/02_fig_variance_retract_uninf.jpg"), plot = fig_variance_retract_uninf_all, width = 7.5, height = 5, units = "in", dpi = set_dpi)

# Calculate variances
var(df_uninformative1$belief_diff)
var(df_retract1$belief_diff1)

var(df_uninformative2$belief_diff2)
var(df_retract2$belief_diff2)

var(df_uninformative3$belief_diff3)
var(df_retract3$belief_diff3)
}


######################################################
# FIGURE 11
######################################################
if (.should_run("fig11")) {
.tick("Figure 11")

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


# Merging — only "After Confirmation" bar (not initial signal separately)
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

ggsave(paste0(fig_outpath, "/02_fig_confirm_vs_informative_change.jpg"), plot = fig_confirm_inf_change, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}


######################################################
# TABLE — Confirmations vs Informative Signals (new for revision)
######################################################
if (.should_run("tab_conf_inf")) {
.tick("Table — Confirmations vs Informative Signals")

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
ols_conf_inf2 <- lm(over_report ~ is_confirmation + prior_bin, df_conf_inf)
ols_conf_inf3 <- lm(over_report ~ is_confirmation * prior_bin, df_conf_inf)

# Table
stargazer(ols_conf_inf1, ols_conf_inf2, ols_conf_inf3,
          se = starprep(ols_conf_inf1, ols_conf_inf2, ols_conf_inf3,
                        clusters = model_clusters(ols_conf_inf1, df_conf_inf$id)),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief higher than Bayesian"),
          covariate.labels = c("Constant", "Confirmation (vs Informative Signal)",
                               "Prior 0--10\\%", "Prior 10--20\\%", "Prior 20--30\\%", "Prior 30--40\\%",
                               "Prior 50--60\\%", "Prior 60--70\\%", "Prior 70--80\\%", "Prior 80--90\\%", "Prior 90--100\\%"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Confirmations vs Informative Signals",
          notes = "SEs clustered by subject. Confirmations from Experiment 1 (excl. ceiling/floor), informative signals from Experiment 2. Reference group: prior 40--50\\%.",
          out = paste0(tab_outpath, "/03_tab_confirm_vs_informative.", output_extension))
}


######################################################
# FIGURE 12 AND TABLE 2
######################################################
if (.should_run("fig12")) {
.tick("Figure 12 & Table 2")

# Estimating Inference and Base Rate Use
######################################################

# Creating variable for confirmation bias check
df_regular <- mutate(df_regular, c = ifelse(sign(prior_ratio)==sign(signal_ratio), 1, 0))

# Regressions
ols_regular <- lm(obslnpost ~ signal_ratio + prior_ratio, df_regular)
me_regular <- lmer(obslnpost ~ signal_ratio + prior_ratio + (1 + signal_ratio + prior_ratio|id), df_regular)
me_regular_c <- lmer(obslnpost ~ signal_ratio + prior_ratio + signal_ratio:c + (1 + signal_ratio + prior_ratio|id), df_regular)

# Table with overview
stargazer(ols_regular, me_regular, me_regular_c,
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
          out = paste0(tab_outpath, "/03_tab_regular_updating.", output_extension))


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
ggsave(paste0(fig_outpath, "/02_fig_regular_updating_inference_baserate.jpg"), plot = fig_regular_updating, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}




######################################################
# FIGURE 13 - EXPANDED
######################################################
if (.should_run("fig13")) {
.tick("Figure 13 (expanded)")

# Overview average and median
df_regular_sum <- df_regular %>%
  summarise(overreport_avg = mean(over_report, na.rm = TRUE), 
            SE_over_report = std.error(over_report, na.rm = TRUE),
            overreport_med = median(over_report, na.rm = TRUE))

# Finding types 
df_regular_type <- df_regular %>%
  group_by(id) %>%
  summarise(overreport_avg_id = mean(over_report, na.rm = TRUE),
            overreport_variance_id = var(over_report, na.rm = TRUE),
            id = mean(id))

# Merging with main data frames
df_regular <- merge(df_regular, df_regular_type, by="id")
df_retract <- merge(df_retract, df_regular_type, by="id")


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

ggsave(paste0(fig_outpath, "/02_fig_regular_overreport.jpg"), plot = fig_overreport_regular, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}



######################################################
# FIGURE 14 AND ADDITIONAL GRAPHS FOR PRESENTATION
######################################################
if (.should_run("fig14")) {
.tick("Figure 14 & Additional Graphs")

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
ggsave(paste0(fig_outpath, "/02_fig_retract_change_lm.jpg"), plot = fig_retract_change_lm, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}



######################################################
# FIGURE 15
######################################################
if (.should_run("fig15")) {
.tick("Figure 15")

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

ggsave(paste0(fig_outpath, "/02_fig_retract_diff_cont_noout.jpg"), plot = fig_retract_diff_cont, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}


######################################################
# FIGURE 16
######################################################
if (.should_run("fig16")) {
.tick("Figure 16")

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

ggsave(paste0(fig_outpath, "/02_fig_confirm_diff.jpg"), plot = fig_confirm_diff, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}


######################################################
# FIGURE 17
######################################################
if (.should_run("fig17")) {
.tick("Figure 17")

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

ggsave(paste0(fig_outpath, "/02_fig_time_belief_type.jpg"), plot = fig_time_belief_type, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}



######################################################
# TABLE 1
######################################################
if (.should_run("tab1")) {
.tick("Table 1")

# Regression
ols_belief_post_subj <- lm(belief ~ posterior_subj, df_main)

# Table with overview
stargazer(ols_belief_post_subj,
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
          out = paste0(tab_outpath, "/03_tab_regression_belief.", output_extension))
}


######################################################
# TABLE 2
######################################################
if (.should_run("tab2")) {
.tick("Table 2")

# above
}


######################################################
# TABLE 3
######################################################
if (.should_run("tab3")) {
.tick("Table 3")

# Regression
ols_regular_treat <- lm(obslnpost ~ signal_ratio*factor(treat) + prior_ratio*factor(treat), df_regular[df_regular$aggregate_round==0,])

# Table with overview
stargazer(ols_regular_treat,
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
          out = paste0(tab_outpath, "/03_tab_regular_updating_treat.", output_extension))
}


######################################################
# TABLE 4
######################################################
if (.should_run("tab4")) {
.tick("Table 4")

# Regression
ols_retract_5_main <- lm(over_report_ret ~ over_report_lag1, df_retract)

# Table with overview
stargazer(ols_retract_5_main,
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
          out = paste0(tab_outpath, "/03_tab_retract_beliefdiff_main.", output_extension))
}



######################################################
# TABLE 5
######################################################
if (.should_run("tab5")) {
.tick("Table 5")

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
stargazer(ols_retract_6a, ols_retract_6b,
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
          out = paste0(tab_outpath, "/03_tab_retract_comphist.", output_extension))
}



######################################################
# TABLE 6
######################################################
if (.should_run("tab6")) {
.tick("Table 6")

# Regressions
ols_retract_2main <- lm(obslnpost ~ signal_ratio + signal_ratio:over_report_lag1 + prior_ratio + prior_ratio:over_report_lag1, df_retract)
ols_retract_2 <- lm(obslnpost ~ signal_ratio + prior_ratio, df_retract)
ols_retract_2a <- lm(obslnpost ~ signal_ratio + prior_ratio, df_retract[df_retract$correct_lag==1,])
ols_retract_2b <- lm(obslnpost ~ signal_ratio + prior_ratio, df_retract[df_retract$under_lag==1,])
ols_retract_2c <- lm(obslnpost ~ signal_ratio + prior_ratio, df_retract[df_retract$over_lag==1,])

# Table with overview
stargazer(ols_retract_2, ols_retract_2a, ols_retract_2b, ols_retract_2c,
          type = output_type,
          se = starprep(ols_retract_2main, clusters = df_retract$id),
          style = "default",
          dep.var.labels = c("Observed Log-Posterior-Ratio"),
          column.labels = c("All Retractions", 
                            "Prev. Correct (+- 1%pt)", 
                            "Prev. under-infered (<1%pt)", 
                            "Prev. over-infered (>1%pt)"),
          covariate.labels = c("Constant", "Retraction", "Prior"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "ser", "f"),
          title = "Updating with Retraction Signals",
          out = paste0(tab_outpath, "/03_tab_retract_categories.", output_extension))
}




######################################################
# TABLE 7
######################################################
if (.should_run("tab7")) {
.tick("Table 7")

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
stargazer(ols_retract_5_types1, ols_retract_5_types2, ols_retract_5_types3, ols_retract_5_types4,
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
          out = paste0(tab_outpath, "/03_tab_retract_beliefdiff_types.", output_extension))
}


######################################################
# TABLE 8
######################################################
if (.should_run("tab8")) {
.tick("Table 8")

# Regressions
ols_retract_5_expl1 <- lm(over_report_ret ~ over_report_lag1 + over_report_lag2, df_retract)

ols_retract_5_expl2 <- lm(over_report_ret ~ over_report_lag1*treat_no_anchor
                          + over_report_lag1*treat_no_history, df_retract)


# Table with overview
stargazer(ols_retract_5_expl1, ols_retract_5_expl2,
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
          out = paste0(tab_outpath, "/03_tab_retract_beliefdiff_expl.", output_extension))
}


######################################################
# TABLE 9
######################################################
if (.should_run("tab9")) {
.tick("Table 9")

# Regression
ols_retract_9 <- lm(belief_diff_priorinduced_adj ~ over_report_lag1, df_retract)

# Table with overview
stargazer(ols_retract_9,
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief higher than induced Prior after Retraction"),
          covariate.labels = c("Constant", "Belief Over-Report in Previous Round"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Updating with Retraction Signals - All Signals converted to Red",
          out = paste0(tab_outpath, "/03_tab_retract_beliefdiff_priorinduced.", output_extension))
}


######################################################
# TABLE 10
######################################################
if (.should_run("tab10")) {
.tick("Table 10")

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
stargazer(ols_retract_10a, 
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
          out = paste0(tab_outpath, "/03_tab_retract_vs_ball.", output_extension))
}


######################################################
# TABLE 11
######################################################
if (.should_run("tab11")) {
.tick("Table 11")

# Preparation
df_confirm$belief_diff_subj <- df_confirm$belief - df_confirm$post_induced
df_confirm <- mutate(df_confirm, belief_dist_signal_subj = ifelse(ball_red_lag1==1, belief_diff_subj, -belief_diff_subj))
df_confirm_nNA <- df_confirm[df_confirm$belief_dist_signal_subj<=1 & df_confirm$belief_dist_signal_subj>=-1,]
rownames(df_confirm_nNA) <- NULL  # reset row names for model_clusters()

# regressions
ols_confirm_simple_1 <- lm(over_report ~ over_report_lag1, df_confirm)
ols_confirm_simple_2 <- lm(over_report ~ over_report_lag1 + wrong_lag, df_confirm)
ols_confirm_simple_3 <- lm(belief_dist_signal_subj ~ over_report_lag1 + wrong_lag, df_confirm_nNA)

# Table with overview (clustered SEs via model_clusters helper)
stargazer(ols_confirm_simple_1, ols_confirm_simple_2, ols_confirm_simple_3,
          se = list(starprep(ols_confirm_simple_1, clusters = model_clusters(ols_confirm_simple_1, df_confirm$id))[[1]],
                    starprep(ols_confirm_simple_2, clusters = model_clusters(ols_confirm_simple_2, df_confirm$id))[[1]],
                    starprep(ols_confirm_simple_3, clusters = model_clusters(ols_confirm_simple_3, df_confirm_nNA$id))[[1]]),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief higher than Bayesian"),
          column.labels = c("Standard Bayesian","Standard Bayesian" , "Alternative Bayesian"),
          covariate.labels = c("Constant", "Belief Over-Report Previously * Signal", "Initial Update wrong"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Impact of Confirmations on Beliefs",
          notes = "SEs clustered by subject.",
          out = paste0(tab_outpath, "/03_tab_confirm_beliefdiff_main.", output_extension))
}


######################################################
# TABLE — Confirmation Treatment Heterogeneity (new for revision)
######################################################
if (.should_run("tab11b")) {
.tick("Table — Confirmation Treatment Heterogeneity")

# Regressions
ols_confirm_treat <- lm(over_report ~ over_report_lag1*treat_no_anchor
                        + over_report_lag1*treat_no_history, df_confirm)

# Table with overview
stargazer(ols_confirm_treat,
          se = starprep(ols_confirm_treat, clusters = model_clusters(ols_confirm_treat, df_confirm$id)),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief higher than Bayesian"),
          covariate.labels = c("Constant", "Initial belief over-report (t-1)", "No anchor treatment", "No history treatment", "No anchor treat * initial belief over-report (t-1)", "No history treat * initial belief over-report (t-1)"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Impact of Confirmations on Beliefs — Treatment Heterogeneity",
          notes = "SEs clustered by subject.",
          out = paste0(tab_outpath, "/03_tab_confirm_beliefdiff_treat.", output_extension))
}


######################################################
# TABLE — Retraction Signal History Robustness (new for revision)
######################################################
if (.should_run("tab_ret_hist")) {
.tick("Table — Retraction Signal History Robustness")

# Regressions
ols_ret_hist1 <- lm(over_report_ret ~ over_report_lag1, df_retract)
ols_ret_hist2 <- lm(over_report_ret ~ over_report_lag1 + factor(prev_verified), df_retract)
ols_ret_hist3 <- lm(over_report_ret ~ over_report_lag1 * factor(prev_verified), df_retract)

# Table
stargazer(ols_ret_hist1, ols_ret_hist2, ols_ret_hist3,
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
          out = paste0(tab_outpath, "/03_tab_retract_beliefdiff_history.", output_extension))
}


######################################################
# TABLE — Retraction Prior Belief Heterogeneity (new for revision)
######################################################
if (.should_run("tab_ret_prior")) {
.tick("Table — Retraction Prior Belief Heterogeneity")

# Preparation: belief_lag2 bins with 10pp breakpoints (reference = 40-50%)
df_retract$belief_lag2_bin <- cut(df_retract$belief_lag2,
    breaks = seq(0, 1, 0.1),
    include.lowest = TRUE,
    labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))
df_retract$belief_lag2_bin <- relevel(factor(df_retract$belief_lag2_bin), ref = "40-50")

# Regression
ols_ret_prior <- lm(over_report_ret ~ over_report_lag1 * belief_lag2_bin, df_retract)

# Table
stargazer(ols_ret_prior,
          se = starprep(ols_ret_prior, clusters = df_retract$id),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief biased towards initial signal"),
          covariate.labels = c("Constant",
                               "Over-report (t-1)",
                               "Prior 0--10\\%",
                               "Prior 10--20\\%",
                               "Prior 20--30\\%",
                               "Prior 30--40\\%",
                               "Prior 50--60\\%",
                               "Prior 60--70\\%",
                               "Prior 70--80\\%",
                               "Prior 80--90\\%",
                               "Prior 90--100\\%",
                               "Over-report (t-1) $\\times$ Prior 0--10\\%",
                               "Over-report (t-1) $\\times$ Prior 10--20\\%",
                               "Over-report (t-1) $\\times$ Prior 20--30\\%",
                               "Over-report (t-1) $\\times$ Prior 30--40\\%",
                               "Over-report (t-1) $\\times$ Prior 50--60\\%",
                               "Over-report (t-1) $\\times$ Prior 60--70\\%",
                               "Over-report (t-1) $\\times$ Prior 70--80\\%",
                               "Over-report (t-1) $\\times$ Prior 80--90\\%",
                               "Over-report (t-1) $\\times$ Prior 90--100\\%"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Impact of Retractions on Beliefs -- Prior Belief Heterogeneity",
          notes = "SEs clustered by subject. Reference group: prior 40--50\\%. Prior = belief before initial signal (t-2).",
          out = paste0(tab_outpath, "/03_tab_retract_beliefdiff_prior.", output_extension))
}


######################################################
# FIGURE — Retraction Prior Belief U-shape (new for revision)
######################################################
if (.should_run("fig_ret_prior")) {
.tick("Figure — Retraction Prior Belief U-shape")

# Re-run with lm_robust for clustered vcov (if tab_ret_prior didn't run first)
if (!exists("ols_ret_prior")) {
  df_retract$belief_lag2_bin <- cut(df_retract$belief_lag2,
      breaks = seq(0, 1, 0.1), include.lowest = TRUE,
      labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))
  df_retract$belief_lag2_bin <- relevel(factor(df_retract$belief_lag2_bin), ref = "40-50")
}
rob_ret_prior <- lm_robust(over_report_ret ~ over_report_lag1 * belief_lag2_bin,
                            data = df_retract, clusters = id, se_type = "CR2")

# Extract total over_report_lag1 effect per bin
beta <- coef(rob_ret_prior)
V <- vcov(rob_ret_prior)
base_idx <- which(names(beta) == "over_report_lag1")
base_coef <- beta[base_idx]

bin_labels <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100")

total_effects <- data.frame(bin = bin_labels, effect = NA, se = NA, n = NA)

for (i in seq_along(bin_labels)) {
  bl <- bin_labels[i]
  total_effects$n[i] <- sum(df_retract$belief_lag2_bin == bl, na.rm = TRUE)

  if (bl == "40-50") {
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
  scale_x_discrete(name = "Prior belief before initial signal (t-2)") +
  ylab("Over-report (t-1) coefficient") +
  theme_classic()

ggsave(paste0(fig_outpath, "/02_fig_retract_prior_ushape.jpg"), plot = fig_ret_prior, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}


######################################################
# TABLE — Confirmation Signal History Robustness (new for revision)
######################################################
if (.should_run("tab_conf_hist")) {
.tick("Table — Confirmation Signal History Robustness")

# Regressions
ols_conf_hist1 <- lm(over_report ~ over_report_lag1, df_confirm)
ols_conf_hist2 <- lm(over_report ~ over_report_lag1 + factor(prev_verified), df_confirm)
ols_conf_hist3 <- lm(over_report ~ over_report_lag1 * factor(prev_verified), df_confirm)

# Table
stargazer(ols_conf_hist1, ols_conf_hist2, ols_conf_hist3,
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
          out = paste0(tab_outpath, "/03_tab_confirm_beliefdiff_history.", output_extension))
}


######################################################
# TABLE — Confirmation Prior Belief Heterogeneity (new for revision)
######################################################
if (.should_run("tab_conf_prior")) {
.tick("Table — Confirmation Prior Belief Heterogeneity")

# Preparation: belief_lag2 bins with 10pp breakpoints (reference = 40-50%)
df_confirm$belief_lag2_bin <- cut(df_confirm$belief_lag2,
    breaks = seq(0, 1, 0.1),
    include.lowest = TRUE,
    labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))
df_confirm$belief_lag2_bin <- relevel(factor(df_confirm$belief_lag2_bin), ref = "40-50")

# Regression
ols_conf_prior <- lm(over_report ~ over_report_lag1 * belief_lag2_bin, df_confirm)

# Table
stargazer(ols_conf_prior,
          se = list(starprep(ols_conf_prior, clusters = model_clusters(ols_conf_prior, df_confirm$id))[[1]]),
          type = output_type,
          style = "default",
          dep.var.labels = c("Belief higher than Bayesian"),
          covariate.labels = c("Constant",
                               "Over-report (t-1)",
                               "Prior 0--10\\%",
                               "Prior 10--20\\%",
                               "Prior 20--30\\%",
                               "Prior 30--40\\%",
                               "Prior 50--60\\%",
                               "Prior 60--70\\%",
                               "Prior 70--80\\%",
                               "Prior 80--90\\%",
                               "Prior 90--100\\%",
                               "Over-report (t-1) $\\times$ Prior 0--10\\%",
                               "Over-report (t-1) $\\times$ Prior 10--20\\%",
                               "Over-report (t-1) $\\times$ Prior 20--30\\%",
                               "Over-report (t-1) $\\times$ Prior 30--40\\%",
                               "Over-report (t-1) $\\times$ Prior 50--60\\%",
                               "Over-report (t-1) $\\times$ Prior 60--70\\%",
                               "Over-report (t-1) $\\times$ Prior 70--80\\%",
                               "Over-report (t-1) $\\times$ Prior 80--90\\%",
                               "Over-report (t-1) $\\times$ Prior 90--100\\%"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Impact of Confirmations on Beliefs -- Prior Belief Heterogeneity",
          notes = "SEs clustered by subject. Reference group: prior 40--50\\%. Prior = belief before initial signal (t-2).",
          out = paste0(tab_outpath, "/03_tab_confirm_beliefdiff_prior.", output_extension))
}


######################################################
# FIGURE — Confirmation Prior Belief U-shape (new for revision)
######################################################
if (.should_run("fig_conf_prior")) {
.tick("Figure — Confirmation Prior Belief U-shape")

# Re-run with lm_robust for clustered vcov (if tab_conf_prior didn't run first)
if (!exists("ols_conf_prior")) {
  df_confirm$belief_lag2_bin <- cut(df_confirm$belief_lag2,
      breaks = seq(0, 1, 0.1), include.lowest = TRUE,
      labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))
  df_confirm$belief_lag2_bin <- relevel(factor(df_confirm$belief_lag2_bin), ref = "40-50")
}
rob_conf_prior <- lm_robust(over_report ~ over_report_lag1 * belief_lag2_bin,
                             data = df_confirm, clusters = id, se_type = "CR2")

# Extract total over_report_lag1 effect per bin
beta <- coef(rob_conf_prior)
V <- vcov(rob_conf_prior)
base_idx <- which(names(beta) == "over_report_lag1")
base_coef <- beta[base_idx]

bin_labels <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100")

total_effects <- data.frame(bin = bin_labels, effect = NA, se = NA, n = NA)

for (i in seq_along(bin_labels)) {
  bl <- bin_labels[i]
  total_effects$n[i] <- sum(df_confirm$belief_lag2_bin == bl, na.rm = TRUE)

  if (bl == "40-50") {
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
  geom_text(aes(label = paste0("n=", n), y = -0.6), size = 2.5) +
  scale_x_discrete(name = "Prior belief before initial signal (t-2)") +
  ylab("Over-report (t-1) coefficient") +
  theme_classic()

ggsave(paste0(fig_outpath, "/02_fig_confirm_prior_ushape.jpg"), plot = fig_conf_prior, width = 7.5, height = 5, units = "in", dpi = set_dpi)
}


######################################################
# TABLE 12
######################################################
if (.should_run("tab12")) {
.tick("Table 12")

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
stargazer(ols_uninf_1a, #ols_uninf_1b, 
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
          out = paste0(tab_outpath, "/03_tab_uninf_vs_retract_hist.", output_extension))
}


######################################################
# TABLE 13
######################################################
if (.should_run("tab13")) {
.tick("Table 13")

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
stargazer(ols_inf_1a,ols_inf_1b,
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
          out = paste0(tab_outpath, "/03_tab_inf_vs_confirm_hist.", output_extension))
}


######################################################
# TABLE 14
######################################################
if (.should_run("tab14")) {
.tick("Table 14")

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
stargazer(me_regular_expl1, me_regular_expl2, me_regular_expl3, me_regular_expl4,
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
          out = paste0(tab_outpath, "/03_tab_regular_updating_verifications_llr.", output_extension))
}


######################################################
# TABLE 15
######################################################
if (.should_run("tab15")) {
.tick("Table 15")

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
stargazer(ols_regular_belief_change1, ols_regular_belief_change2, ols_regular_belief_change3, ols_regular_belief_change4,
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
          out = paste0(tab_outpath, "/03_tab_regular_updating_verifications_belief_change.", output_extension))
}

# Final section timing
if (!is.null(.last_section)) {
  elapsed <- proc.time()["elapsed"] - .section_start
  .section_times[[.last_section]] <- elapsed
  cat(sprintf("   done (%.1fs)\n", elapsed))
}
total <- proc.time()["elapsed"] - .timer_start
cat(sprintf("\n>> Figures and Tables.R complete. Total: %.1fs\n", total))
cat("\n>> TIMING SUMMARY (sorted slowest first):\n")
times_vec <- unlist(.section_times)
times_sorted <- sort(times_vec, decreasing = TRUE)
for (nm in names(times_sorted)) {
  cat(sprintf("   %6.1fs  %s\n", times_sorted[nm], nm))
}
