# ──────────────────────────────────────────────────
# 02 — Dynamics
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Paper outputs (Section 5.4):
#   Table B1  — c and d estimates by period block (early / middle / late)
#   Table B2  — c and d for regular signals by verification profile
#   Figure B4 — Observed bias by period block (retractions, confirmations)
#   Figure B6 — Observed bias for regular signals, by verification profile
#   Figure B7 — Observed bias for retractions, by verification profile
#   Figure B8 — Observed bias for confirmations, by verification profile
#
# NOTE on Table B1: the c/d regressions use NO intercept, consistent with
# Table 1, Table B2 and the paper's Section 4.2. The paper's *printed*
# Table B1 was generated with an intercept and therefore differs slightly
# from this output (see docs/known_issues.md).
# ──────────────────────────────────────────────────


######################################################
# SETUP (auto-loads utilities if run standalone)
######################################################
if (!exists(".utils_loaded")) source("code/utils/bootstrap.R")

.tick("Section 2: Dynamics")


######################################################
# SHARED: signal-aligned prior bin for df_regular
# (df_retract and df_confirm already have prior_aligned_bin /
#  belief_lag2_bin from derived_variables.R)
######################################################
df_regular$prior_aligned <- ifelse(
  df_regular$ball_red == 1,
  df_regular$belief_lag1,
  1 - df_regular$belief_lag1
)
df_regular$prior_aligned_bin <- factor(
  cut(df_regular$prior_aligned,
      breaks = prior_bin_breaks, include.lowest = TRUE,
      labels = prior_bin_labels),
  levels = prior_bin_labels
)

# Strictly-prior verification counts (used by Table B2 / Figs B6-B8)
df_regular$cum_ret  <- df_regular$ret_total
df_regular$cum_conf <- df_regular$conf_total

df_retract_nprev <- df_retract %>%
  arrange(id, round) %>%
  group_by(id) %>%
  mutate(n_prev_ret = row_number() - 1L) %>%
  ungroup()
df_confirm_nprev <- df_confirm %>%
  arrange(id, round) %>%
  group_by(id) %>%
  mutate(n_prev_conf = row_number() - 1L) %>%
  ungroup()


######################################################
# TABLE B1 — c and d estimates by period block
# Regular:              Early=2-4, Middle=5-7, Late=8-10
# Retractions/Confirms: Early=3-5, Middle=6-8, Late=9-11
######################################################
if (.should_run("tab_B1")) {
.tick("Table B1 -- c and d by period block")
tryCatch({

assign_period_block_reg <- function(r) {
  ifelse(r %in% 2:4,  "Early",
  ifelse(r %in% 5:7,  "Middle",
  ifelse(r %in% 8:10, "Late", NA_character_)))
}
assign_period_block_retconf <- function(r) {
  ifelse(r %in% 3:5,  "Early",
  ifelse(r %in% 6:8,  "Middle",
  ifelse(r %in% 9:11, "Late", NA_character_)))
}

block_order <- c("Early", "Middle", "Late")
df_reg_pb  <- df_regular
df_ret_pb  <- df_retract
df_conf_pb <- df_confirm
df_reg_pb$period_block  <- assign_period_block_reg(df_reg_pb$round)
df_ret_pb$period_block  <- assign_period_block_retconf(df_ret_pb$round)
df_conf_pb$period_block <- assign_period_block_retconf(df_conf_pb$round)
df_reg_pb  <- df_reg_pb[!is.na(df_reg_pb$period_block), ]
df_ret_pb  <- df_ret_pb[!is.na(df_ret_pb$period_block), ]
df_conf_pb <- df_conf_pb[!is.na(df_conf_pb$period_block), ]

cd_reg_pb  <- run_cd_by(df_reg_pb,  "period_block", signal_var = "signal_ratio")
cd_ret_pb  <- run_cd_by(df_ret_pb,  "period_block", signal_var = "signal_ratio_obj")
cd_conf_pb <- run_cd_by(df_conf_pb, "period_block", signal_var = "signal_ratio_obj")
cd_reg_pb$signal_type  <- "Regular"
cd_ret_pb$signal_type  <- "Retraction"
cd_conf_pb$signal_type <- "Confirmation"
cd_by_period <- rbind(cd_reg_pb, cd_ret_pb, cd_conf_pb)
cd_by_period$period_block <- factor(cd_by_period$period_block, levels = block_order)

period_note <- paste0(
  "SEs in parentheses. One OLS regression per signal type $\\times$ period block ",
  "(9 regressions total). No intercept. Bayesian benchmark: $c=d=1$. ",
  "Round windows: Regular = Early (2--4), Middle (5--7), Late (8--10); ",
  "Retractions/Confirmations = Early (3--5), Middle (6--8), Late (9--11). ",
  "Signal variable: $\\pm$log(0.6/0.4) for regular signals and retractions; ",
  "$\\pm$log(2) for confirmations."
)
write_cd_tex(cd_by_period, "period_block", block_order,
  tab_path("dynamics", "table_B1_cd_by_period"),
  "$c$ and $d$ Estimates by Period Block",
  "tab:cd_by_period",
  note = period_note)

}, error = .fail)
}


######################################################
# VERIFICATION PROFILES (used by Table B2 and Figures B6-B8)
#   r = previous retractions, c = previous confirmations
######################################################
vertype_cases <- list(
  list(profile = "$(H_{0,0})$", caption = "$(H_{0,0})$: 0 retractions / 0 confirmations",
       reg_filter  = quote(cum_ret == 0 & cum_conf == 0),
       ret_filter  = quote(n_prev_ret == 0 & conf_total == 0),
       conf_filter = quote(ret_total == 0 & n_prev_conf == 0)),
  list(profile = "$(H_{1,0})$", caption = "$(H_{1,0})$: 1 retraction / 0 confirmations",
       reg_filter  = quote(cum_ret == 1 & cum_conf == 0),
       ret_filter  = quote(n_prev_ret == 1 & conf_total == 0),
       conf_filter = quote(ret_total == 1 & n_prev_conf == 0)),
  list(profile = "$(H_{0,1})$", caption = "$(H_{0,1})$: 0 retractions / 1 confirmation",
       reg_filter  = quote(cum_ret == 0 & cum_conf == 1),
       ret_filter  = quote(n_prev_ret == 0 & conf_total == 1),
       conf_filter = quote(ret_total == 0 & n_prev_conf == 1)),
  list(profile = "$(H_{2,0})$", caption = "$(H_{2,0})$: 2 retractions / 0 confirmations",
       reg_filter  = quote(cum_ret == 2 & cum_conf == 0),
       ret_filter  = quote(n_prev_ret == 2 & conf_total == 0),
       conf_filter = quote(ret_total == 2 & n_prev_conf == 0)),
  list(profile = "$(H_{0,2})$", caption = "$(H_{0,2})$: 0 retractions / 2 confirmations",
       reg_filter  = quote(cum_ret == 0 & cum_conf == 2),
       ret_filter  = quote(n_prev_ret == 0 & conf_total == 2),
       conf_filter = quote(ret_total == 0 & n_prev_conf == 2)),
  list(profile = "$(H_{1,1})$", caption = "$(H_{1,1})$: 1 retraction / 1 confirmation",
       reg_filter  = quote(cum_ret == 1 & cum_conf == 1),
       ret_filter  = quote(n_prev_ret == 1 & conf_total == 1),
       conf_filter = quote(ret_total == 1 & n_prev_conf == 1))
)


######################################################
# TABLE B2 — c and d for regular signals by verification profile
######################################################
if (.should_run("tab_B2")) {
.tick("Table B2 -- c and d for regular signals by profile")
tryCatch({

stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.1)  return("$^{*}$")
  ""
}

b2_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Updating parameters for regular signals by profile of previously realized verifications}",
  "\\label{tab:cd_by_verification_profile}",
  "\\begin{tabular}{lrr}",
  "\\hline",
  " & Base-Rate Use ($c$) & Inference ($d$) \\\\",
  "\\hline"
)
for (vc in vertype_cases) {
  d   <- df_regular[with(df_regular, eval(vc$reg_filter)), ]
  fit <- fit_pooled_cd(d, "signal_ratio")
  if (is.null(fit)) {
    b2_lines <- c(b2_lines, paste0(vc$profile, " & --- & --- \\\\"))
    next
  }
  s  <- summary(fit)$coefficients
  cc <- s["prior_ratio", ]
  dd <- s["signal_ratio", ]
  b2_lines <- c(b2_lines,
    sprintf("%s & %.3f%s & %.3f%s \\\\", vc$profile,
            cc[1], stars(cc[4]), dd[1], stars(dd[4])),
    sprintf(" & (%.3f) & (%.3f) \\\\", cc[2], dd[2]))
}
b2_lines <- c(b2_lines, "\\hline",
  paste0("\\multicolumn{3}{p{0.7\\textwidth}}{\\small Pooled OLS per profile ",
         "(no intercept). SEs in parentheses. $(H_{k,m})$ = $k$ previous ",
         "retractions and $m$ previous confirmations. Bayesian benchmark: ",
         "$c=d=1$. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01.} \\\\"),
  "\\end{tabular}", "\\end{table}")
writeLines(b2_lines, tab_path("dynamics", "table_B2_cd_by_verification_profile"))

}, error = .fail)
}


######################################################
# FIGURE B4 — Observed bias by period block
# Three bars per prior bin (Early / Middle / Late).
######################################################
if (.should_run("fig_B4")) {
.tick("Figure B4 -- bias by period block")
tryCatch({

assign_period_block_a2 <- function(r) {
  ifelse(r %in% 3:5,  "Early",
  ifelse(r %in% 6:8,  "Middle",
  ifelse(r %in% 9:11, "Late", NA_character_)))
}
block_order_a2 <- c("Early", "Middle", "Late")
block_colors   <- c(Early = "green", Middle = "blue", Late = "red")

ret_blocks <- lapply(block_order_a2, function(pb) {
  df_retract %>%
    filter(assign_period_block_a2(round) == pb) %>%
    group_by(prior_bin = prior_aligned_bin) %>%
    summarise(Mean = mean(over_report_ret, na.rm = TRUE),
              SD   = sd(over_report_ret,   na.rm = TRUE),
              N    = n(), .groups = "drop")
})
names(ret_blocks) <- block_order_a2

conf_blocks <- lapply(block_order_a2, function(pb) {
  df_confirm %>%
    filter(assign_period_block_a2(round) == pb) %>%
    group_by(prior_bin = belief_lag2_bin) %>%
    summarise(Mean = mean(over_report, na.rm = TRUE),
              SD   = sd(over_report,   na.rm = TRUE),
              N    = n(), .groups = "drop")
})
names(conf_blocks) <- block_order_a2

b4_panels <- list(
  list(body = tikz_bias_grouped_body(ret_blocks, unname(block_colors),
                                     LBL_TIKZ_RET, POS_RET),
       caption = "Retractions"),
  list(body = tikz_bias_grouped_body(conf_blocks, unname(block_colors),
                                     LBL_TIKZ_REG, POS_REG),
       caption = "Confirmations")
)
write_tikz_figure(b4_panels,
  tikz_path("dynamics", "figure_B4_bias_by_period"), ncol = 1)

}, error = .fail)
}


######################################################
# FIGURES B6 / B7 / B8 — Observed bias by verification profile
# One combined figure per signal type, six panels (one per profile).
######################################################
if (.should_run("fig_B6") || .should_run("fig_B7") || .should_run("fig_B8")) {
.tick("Figures B6-B8 -- bias by verification profile")
tryCatch({

b6_panels <- list()
b7_panels <- list()
b8_panels <- list()

for (vc in vertype_cases) {
  df_reg_i  <- df_regular[with(df_regular,             eval(vc$reg_filter)),  ]
  df_ret_i  <- df_retract_nprev[with(df_retract_nprev, eval(vc$ret_filter)),  ]
  df_conf_i <- df_confirm_nprev[with(df_confirm_nprev, eval(vc$conf_filter)), ]

  reg_i <- df_reg_i %>%
    group_by(prior_bin = prior_aligned_bin) %>%
    summarise(Mean = mean(over_report,     na.rm = TRUE),
              SD   = sd(over_report,       na.rm = TRUE),
              N    = n(), .groups = "drop")
  ret_i <- df_ret_i %>%
    group_by(prior_bin = prior_aligned_bin) %>%
    summarise(Mean = mean(over_report_ret, na.rm = TRUE),
              SD   = sd(over_report_ret,   na.rm = TRUE),
              N    = n(), .groups = "drop")
  conf_i <- df_conf_i %>%
    group_by(prior_bin = belief_lag2_bin) %>%
    summarise(Mean = mean(over_report,     na.rm = TRUE),
              SD   = sd(over_report,       na.rm = TRUE),
              N    = n(), .groups = "drop")

  b6_panels <- c(b6_panels, list(list(
    body = tikz_bias_body(reg_i, LBL_TIKZ_REG, POS_REG), caption = vc$caption)))
  b7_panels <- c(b7_panels, list(list(
    body = tikz_bias_body(ret_i, LBL_TIKZ_RET, POS_RET), caption = vc$caption)))
  b8_panels <- c(b8_panels, list(list(
    body = tikz_bias_body(conf_i, LBL_TIKZ_REG, POS_REG), caption = vc$caption)))
}

if (.should_run("fig_B6"))
  write_tikz_figure(b6_panels,
    tikz_path("dynamics", "figure_B6_regular_bias_by_profile"), ncol = 2)
if (.should_run("fig_B7"))
  write_tikz_figure(b7_panels,
    tikz_path("dynamics", "figure_B7_retraction_bias_by_profile"), ncol = 2)
if (.should_run("fig_B8"))
  write_tikz_figure(b8_panels,
    tikz_path("dynamics", "figure_B8_confirmation_bias_by_profile"), ncol = 2)

}, error = .fail)
}
