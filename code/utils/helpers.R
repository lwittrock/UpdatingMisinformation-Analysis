# ──────────────────────────────────────────────────
# Helper Functions
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Purpose: Shared helper functions for output, clustering, and stargazer
# ──────────────────────────────────────────────────

# Output path helpers
outpath <- "output"
fig_path <- function(topic, name) paste0(outpath, "/", topic, "/figures/", name, ".jpg")
tab_path <- function(topic, name) {
  ext <- ifelse(output_type == "html", "html", "tex")
  paste0(outpath, "/", topic, "/tables/", name, ".", ext)
}

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
