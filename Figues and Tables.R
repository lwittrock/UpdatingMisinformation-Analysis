# Figures and Tables for 'Belief Updating with Misinformation'
# Code by Lars Wittrock
# Date: 12/12/2023


# NOTES 
######################################################
# Please run file 'Preparing Data' first. 
# Then check that paths below are set correctly.
# Also check that all packages are installed.
# Some tables take about a minute to compile, only run the entire script if needed.


######################################################
# TO ADJUST
######################################################

# File locations for input and output
inpath <- "" # Fill in path here
outpath <- "" # Fill in path here

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
library(lme4)
library(stringr)
library(estimatr)

# Reading data
load(file = paste0(inpath, "\\data_main.rda"))
load(file = paste0(inpath, "\\data_subject.rda"))
load(file = paste0(inpath, "\\data_regular.rda"))
load(file = paste0(inpath, "\\data_time.rda"))
load(file = paste0(inpath, "\\data_retract.rda"))
load(file = paste0(inpath, "\\data_confirm.rda"))
load(file = paste0(inpath, "\\data_uninformative.rda"))
load(file = paste0(inpath, "\\data_informative.rda"))

# Changing output type based on selection above
output_extension <- ifelse(output_type=="html", "html", "tex")


######################################################
# FIGURE 5
######################################################

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

ggsave(paste0(outpath, "\\02_fig_retract_diff_group.jpg"), plot = fig_retract_diff_group, width = 7.5, height = 5, units = "in", dpi = set_dpi)


######################################################
# FIGURE 6
######################################################

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

ggsave(paste0(outpath, "\\02_fig_retract_diff_vs_opposite_ball_all.jpg"), plot = fig_regular_diff_group_all, width = 7.5, height = 5, units = "in", dpi = set_dpi)


######################################################
# FIGURE 7
######################################################

df_confirm_temp <- df_confirm[df_confirm$belief_change_rational_lag1!=0,]

# Summary per type
df_confirm_id1 <- df_confirm_temp %>%
  group_by(id) %>%
  summarise(belief_change1 = mean(belief_change_adj_lag1, na.rm = TRUE),
            n = length(id))

df_confirm_id2 <- df_confirm_temp %>%
  group_by(id) %>%
  summarise(belief_change2 = mean(belief_change_adj, na.rm = TRUE),
            n = length(id))

df_confirm_change_sum1 <- df_confirm_id1 %>%
  summarise(belief_change = weighted.mean(belief_change1, n, na.rm = TRUE), 
            SE = std.error(belief_change1, na.rm = TRUE),
            n = sum(n),
            type = "Initial Signal")

df_confirm_change_sum2 <- df_confirm_id2 %>%
  summarise(belief_change = weighted.mean(belief_change2, n, na.rm = TRUE) + df_confirm_change_sum1$belief_change, 
            SE = std.error(belief_change2, na.rm = TRUE),
            n = sum(n),
            type = "Initial Signal + Confirmation")

df_confirm_change_sum <- rbind(df_confirm_change_sum1, df_confirm_change_sum2)

# Express beliefs in %
df_confirm_change_sum$belief_change_pts <- df_confirm_change_sum$belief_change*100
df_confirm_change_sum$SE_pts <- df_confirm_change_sum$SE*100
df_confirm_change_sum$type <- factor(df_confirm_change_sum$type, levels = c("Initial Signal", "Initial Signal + Confirmation"))

# Influence of Confirmations
fig_confirm_change <- ggplot(df_confirm_change_sum, aes(x = factor(type), y = belief_change_pts)) +
  geom_bar(stat="identity", width=0.9, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = belief_change_pts - 1.96*SE_pts, ymax = belief_change_pts + 1.96*SE_pts), position = position_dodge(0.9), width = 0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_hline(yintercept = 100*mean(df_confirm$belief_change_rational_lag1), linetype="dashed") +
  geom_hline(yintercept = 100*mean(df_confirm$belief_change_rational), linetype="dashed") +
  scale_x_discrete(name = "") +
  scale_y_continuous(limits = c(0, 22)) +
  ylab("Belief change (%pts)") +
  #labs(title = "Reaction to Confirmations",
  #     subtitle = "Mean belief change after initial signal and confirmation & 95% CI") +
  theme_classic()

ggsave(paste0(outpath, "\\02_fig_confirm_change.jpg"), plot = fig_confirm_change, width = 7.5, height = 5, units = "in", dpi = set_dpi)




######################################################
# FIGURE 8
######################################################

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

ggsave(paste0(outpath, "\\02_fig_confirm_diff_restricted.jpg"), plot = fig_confirm_diff_restricted, width = 7.5, height = 5, units = "in", dpi = set_dpi)



######################################################
# FIGURE 9
######################################################

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

ggsave(paste0(outpath, "\\02_fig_retract_vs_uninformative.jpg"), plot = fig_retract_uninf, width = 7.5, height = 5, units = "in", dpi = set_dpi)



######################################################
# FIGURE 10
######################################################

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
ggsave(paste0(outpath, "\\02_fig_variance_retract_uninf.jpg"), plot = fig_variance_retract_uninf_all, width = 7.5, height = 5, units = "in", dpi = set_dpi)

# Calculate variances
var(df_uninformative1$belief_diff)
var(df_retract1$belief_diff1)

var(df_uninformative2$belief_diff2)
var(df_retract2$belief_diff2)

var(df_uninformative3$belief_diff3)
var(df_retract3$belief_diff3)


######################################################
# FIGURE 11
######################################################

# Summary per type - SE grouped by subject
df_informative_id <- df_informative %>%
  group_by(id) %>%
  summarise(belief_diff = mean(over_report, na.rm = TRUE),
            belief_change_temp = mean(belief_change_adj, na.rm = TRUE),
            n = length(id))

df_informative_change <- df_informative_id %>%
  group_by() %>%
  summarise(belief_change = weighted.mean(belief_change_temp, n, na.rm = TRUE),
            SE = std.error(belief_change_temp, na.rm = TRUE),
            n = sum(n),
            type = "Informative Signal")

# Express beliefs in %
df_informative_change$belief_change_pts <- df_informative_change$belief_change*100
df_informative_change$SE_pts <- df_informative_change$SE*100


# Merging
df_confirm_informative_change_sum <- rbind(df_confirm_change_sum, df_informative_change)


# Plot
fig_confirm_inf_change <- ggplot(df_confirm_informative_change_sum, aes(x = factor(type), y = belief_change_pts)) +
  geom_bar(stat="identity", width=0.9, fill = "white", colour = "black") +
  geom_errorbar(aes(ymin = belief_change_pts - 1.96*SE_pts, ymax = belief_change_pts + 1.96*SE_pts), position = position_dodge(0.9), width = 0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_hline(yintercept=19.25, linetype="dashed") +
  geom_hline(yintercept = 100*mean(df_confirm$belief_change_rational_lag1), linetype="dashed") +
  scale_x_discrete(name = "Type", drop = FALSE) +
  scale_y_continuous(limits = c(0, 25)) +
  annotate("text", y=1, x=1, label=paste0("n = ", df_confirm_informative_change_sum$n[1]), size=3) +
  annotate("text", y=1, x=2, label=paste0("n = ", df_confirm_informative_change_sum$n[2]), size=3) +
  annotate("text", y=1, x=3, label=paste0("n = ", df_confirm_informative_change_sum$n[3]), size=3) +
  ylab("Belief change (%pts)") +
  #labs(title = "Confirmations vs Informative Signals",
  #     subtitle = "Mean belief change & 95% CI") +
  theme_classic()

ggsave(paste0(outpath, "\\02_fig_confirm_vs_informative_change.jpg"), plot = fig_confirm_inf_change, width = 7.5, height = 5, units = "in", dpi = set_dpi)



######################################################
# FIGURE 12 AND TABLE 2
######################################################


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
          out = paste0(outpath, "\\03_tab_regular_updating.", output_extension))


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
ggsave(paste0(outpath, "\\04_fig_regular_updating_inference_baserate.jpg"), plot = fig_regular_updating, width = 7.5, height = 5, units = "in", dpi = 1000)





######################################################
# FIGURE 13 - EXPANDED
######################################################


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

ggsave(paste0(outpath, "\\02_fig_regular_overreport.jpg"), plot = fig_overreport_regular, width = 7.5, height = 5, units = "in", dpi = set_dpi)




######################################################
# FIGURE 14 AND ADDITIONAL GRAPHS FOR PRESENTATION
######################################################


# Restricting data to make graph clearer - 55obs less.
df_retract_no_outliers <- df_retract[df_retract$belief_change_adj <= 0.75
                                     & df_retract$belief_change_adj >= -0.25
                                     & df_retract$belief_change_adj_lag1 <= 0.75
                                     & df_retract$belief_change_adj_lag1 >= -0.25, ]

# Areas to plot in graph
biased_up <- data.frame(x = c(-0.2,0.75,0.75), y = c(0.25,-0.7,0.25))
biased_down <- data.frame(x = c(-0.25,-0.25,0.7), y = c(0.2,-0.75,-0.75))
rational <- data.frame(x = c(-0.25,-0.25,-0.21,0.75,0.75,0.71), y = c(0.21,0.25,0.25,-0.71,-0.75,-0.75))

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

# Adding highlight to main plot
fig_retract_change_highlight <- fig_retract_change_lm +
  geom_mark_circle(aes(x = 0.1, y = -0.1), color = "red", size = 2 ,inherit.aes = FALSE)

# Area rational update
fig_retract_change_area1 <- fig_retract_change_basic +
  geom_polygon(data=rational, aes(x=x, y=y), alpha=0.3, color=NA, fill="green") +
  annotate("text", x = 0.25, y = -0.25, label = "Rational", angle = -29.5, size = 8)

# Area rational and continued influence
fig_retract_change_area2 <- fig_retract_change_area1 +
  geom_polygon(data=biased_up, aes(x=x, y=y), alpha=0.3, color=NA, fill="red") +
  annotate("text", x = 0.5, y = -0.1, label = "Continued Influence", angle = -29.5, size = 8)

# Area rational and continued influence and reverse influence
fig_retract_change_area3 <- fig_retract_change_area2 +
  geom_polygon(data=biased_down, aes(x=x, y=y), alpha=0.3, color=NA, fill="blue") +
  annotate("text", x = 0, y = -0.35, label = "Reverse influence", angle = -29.5, size = 8)

# Exporting all graphs
#ggsave(paste0(outpath, "\\02_fig_retract_change_basic.jpg"), plot = fig_retract_change_basic, width = 7.5, height = 5, units = "in", dpi = set_dpi)
#ggsave(paste0(outpath, "\\02_fig_retract_change.jpg"), plot = fig_retract_change, width = 7.5, height = 5, units = "in", dpi = set_dpi)
ggsave(paste0(outpath, "\\02_fig_retract_change_lm.jpg"), plot = fig_retract_change_lm, width = 7.5, height = 5, units = "in", dpi = set_dpi)
#ggsave(paste0(outpath, "\\02_fig_retract_change_highlight.jpg"), plot = fig_retract_change_highlight, width = 7.5, height = 5, units = "in", dpi = set_dpi)
#ggsave(paste0(outpath, "\\02_fig_retract_change_area1.jpg"), plot = fig_retract_change_area1, width = 7.5, height = 5, units = "in", dpi = set_dpi)
#ggsave(paste0(outpath, "\\02_fig_retract_change_area2.jpg"), plot = fig_retract_change_area2, width = 7.5, height = 5, units = "in", dpi = set_dpi)
#ggsave(paste0(outpath, "\\02_fig_retract_change_area3.jpg"), plot = fig_retract_change_area3, width = 7.5, height = 5, units = "in", dpi = set_dpi)



######################################################
# FIGURE 15
######################################################

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

ggsave(paste0(outpath, "\\02_fig_retract_diff_cont_noout.jpg"), plot = fig_retract_diff_cont, width = 7.5, height = 5, units = "in", dpi = set_dpi)


######################################################
# FIGURE 16
######################################################

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

ggsave(paste0(outpath, "\\02_fig_confirm_diff.jpg"), plot = fig_confirm_diff, width = 7.5, height = 5, units = "in", dpi = set_dpi)


######################################################
# FIGURE 17
######################################################


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

ggsave(paste0(outpath, "\\02_fig_time_belief_type.jpg"), plot = fig_time_belief_type, width = 7.5, height = 5, units = "in", dpi = set_dpi)



######################################################
# TABLE 1
######################################################

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
          out = paste0(outpath, "\\03_tab_regression_belief.", output_extension))


######################################################
# TABLE 2
######################################################

# above


######################################################
# TABLE 3
######################################################

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
          out = paste0(outpath, "\\03_tab_regular_updating_treat.", output_extension))


######################################################
# TABLE 4
######################################################

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
          out = paste0(outpath, "\\03_tab_retract_beliefdiff_main.", output_extension))




######################################################
# TABLE 5
######################################################

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
          out = paste0(outpath, "\\03_tab_retract_comphist.", output_extension))



######################################################
# TABLE 6
######################################################

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
          out = paste0(outpath, "\\03_tab_retract_categories.", output_extension))





######################################################
# TABLE 7
######################################################

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
  summarise(id = mean(id), n = length(belief), type_subject = reaction_alt)
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
          out = paste0(outpath, "\\03_tab_retract_beliefdiff_types.", output_extension))


######################################################
# TABLE 8
######################################################

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
          out = paste0(outpath, "\\03_tab_retract_beliefdiff_expl.", output_extension))


######################################################
# TABLE 9
######################################################

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
          out = paste0(outpath, "\\03_tab_retract_beliefdiff_priorinduced.", output_extension))


######################################################
# TABLE 10
######################################################

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
          out = paste0(outpath, "\\03_tab_retract_vs_ball.", output_extension))


######################################################
# TABLE 11
######################################################

# Preparation
df_confirm$belief_diff_subj <- df_confirm$belief - df_confirm$post_induced
df_confirm <- mutate(df_confirm, belief_dist_signal_subj = ifelse(ball_red_lag1==1, belief_diff_subj, -belief_diff_subj))
df_confirm_nNA <- df_confirm[df_confirm$belief_dist_signal_subj<=1 & df_confirm$belief_dist_signal_subj>=-1,]

# regressions
ols_confirm_simple_1 <- lm(over_report ~ over_report_lag1, df_confirm)
ols_confirm_simple_2 <- lm(over_report ~ over_report_lag1 + wrong_lag, df_confirm)
ols_confirm_simple_3 <- lm(belief_dist_signal_subj ~ over_report_lag1 + wrong_lag, df_confirm_nNA)

# Table with overview
stargazer(ols_confirm_simple_1, ols_confirm_simple_2, ols_confirm_simple_3,
          type = "latex",
          style = "default",
          dep.var.labels = c("Belief higher than Bayesian"),
          column.labels = c("Standard Bayesian","Standard Bayesian" , "Alternative Bayesian"),
          covariate.labels = c("Constant", "Belief Over-Report Previously * Signal", "Initial Update wrong"),
          intercept.bottom = FALSE,
          no.space = TRUE,
          omit.stat = c("rsq", "f", "ser"),
          title = "Impact of Confirmations on Beliefs",
          out = paste0(outpath, "\\03_tab_confirm_beliefdiff_main.", "tex"))


######################################################
# TABLE 12
######################################################

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
          out = paste0(outpath, "\\03_tab_uninf_vs_retract_hist.", output_extension))


######################################################
# TABLE 13
######################################################

# create variable with all combinations of red and blue informative signals.
df_main$inf_hist <- apply(str_extract_all(df_main$hist, pattern = "[a-z]_inf", simplify = TRUE),1,paste,collapse=" ")
df_main$inf_hist <- gsub("_inf", "", df_main$inf_hist)
df_main$inf_hist <- gsub(" ", "", df_main$inf_hist)
df_main$inf_hist <- ifelse(is.na(df_main$inf_hist), "", df_main$inf_hist)

ord <- c("", "r", "b", "rr", "bb", "rb", "br",
         "rrr", "bbb", "rrb", "bbr",
         "rbb", "brr", "rbr", "brb")
df_main$inf_hist <- factor(df_main$inf_hist,levels=ord)


df_main_nouninf <- df_main[df_main$hist_uninf==0 & df_main$hist_ret==0,]


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
          out = paste0(outpath, "\\03_tab_inf_vs_confirm_hist.", output_extension))


######################################################
# TABLE 14
######################################################

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
          out = paste0(outpath, "\\03_tab_regular_updating_verifications_llr.", output_extension))


######################################################
# TABLE 15
######################################################

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
          dep.var.labels = c("(bt - bt-1) * I(s) TO FIX IN LATEX"),
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
          out = paste0(outpath, "\\03_tab_regular_updating_verifications_belief_change.", output_extension))


