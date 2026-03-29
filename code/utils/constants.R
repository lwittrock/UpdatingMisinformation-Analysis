# ──────────────────────────────────────────────────
# Shared Constants
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Purpose: Define shared constants used across analysis files
# ──────────────────────────────────────────────────

# Figure dimensions (in inches)
fig_width <- 7.5
fig_height <- 5

# For binning priors in prior-belief analyses
prior_bin_breaks <- seq(0, 1, 0.1)
prior_bin_labels <- c("0-10", "11-20", "21-30", "31-40", "41-50",
                       "51-60", "61-70", "71-80", "81-90", "91-100")
prior_bin_ref <- "41-50"
