# ──────────────────────────────────────────────────
# Helper Functions
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Purpose: Shared helper functions for output, clustering, and stargazer
# ──────────────────────────────────────────────────

# Output path helpers.
# Topics map to paper sections: "main" (Table 1, Figs 5-7, B1-B2),
# "dynamics" (Tables B1-B2, Figs B4, B6-B8), "treatments" (Figs B9-B11).
outpath <- "output"
fig_path  <- function(topic, name) paste0(outpath, "/", topic, "/figures/", name, ".jpg")
tikz_path <- function(topic, name) paste0(outpath, "/", topic, "/figures/", name, ".tex")
tab_path  <- function(topic, name) {
  ext <- ifelse(output_type == "html", "html", "tex")
  paste0(outpath, "/", topic, "/tables/", name, ".", ext)
}

# Create output directories
for (topic in c("main", "dynamics", "treatments")) {
  dir.create(paste0(outpath, "/", topic, "/figures"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(outpath, "/", topic, "/tables"), recursive = TRUE, showWarnings = FALSE)
}

# Helper: write stargazer output without the auto-generated timestamp comment
# (prevents spurious git diffs when re-running the pipeline with unchanged results)
write_stargazer <- function(..., out) {
  lines <- capture.output(stargazer::stargazer(...))
  lines <- lines[!grepl("^% Date and time:|^<!-- Date and time:", lines)]
  writeLines(lines, out)
}
