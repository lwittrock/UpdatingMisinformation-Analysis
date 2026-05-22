# ──────────────────────────────────────────────────
# Helpers — constants, output paths, table writer, run control
# Part of: Belief Updating with Misinformation analysis pipeline
# ──────────────────────────────────────────────────


######################################################
# CONSTANTS
######################################################

# Figure dimensions (inches) — used by the figure_B1 ggplot
fig_width  <- 7.5
fig_height <- 5

# Prior bins for the bias-by-prior analyses
prior_bin_breaks <- seq(0, 1, 0.1)
prior_bin_labels <- c("0-10", "11-20", "21-30", "31-40", "41-50",
                      "51-60", "61-70", "71-80", "81-90", "91-100")
prior_bin_ref    <- "41-50"


######################################################
# OUTPUT PATHS
# Topics map to paper sections: "main", "dynamics", "treatments".
######################################################
outpath <- "output"
fig_path  <- function(topic, name) paste0(outpath, "/", topic, "/", name, ".jpg")
tikz_path <- function(topic, name) paste0(outpath, "/", topic, "/", name, ".tex")
tab_path  <- function(topic, name) {
  ext <- ifelse(output_type == "html", "html", "tex")
  paste0(outpath, "/", topic, "/", name, ".", ext)
}

for (topic in c("main", "dynamics", "treatments")) {
  dir.create(paste0(outpath, "/", topic), recursive = TRUE, showWarnings = FALSE)
}


######################################################
# TABLE WRITER
######################################################

# Write stargazer output without the auto-generated timestamp comment
# (prevents spurious git diffs when re-running with unchanged results).
write_stargazer <- function(..., out) {
  lines <- capture.output(stargazer::stargazer(...))
  lines <- lines[!grepl("^% Date and time:|^<!-- Date and time:", lines)]
  writeLines(lines, out)
}


######################################################
# RUN CONTROL — section selector and timing
######################################################

# Within-file section dependencies. Each analysis file computes its shared
# summaries up-front, outside the section guards, so this stays empty.
.deps <- list()

.should_run <- function(section) {
  if (identical(run_sections, "all")) return(TRUE)
  if (identical(run_sections, "figures")) return(grepl("^fig", section))
  if (identical(run_sections, "tables")) return(grepl("^tab", section) || section %in% unlist(.deps[run_sections[grepl("^tab", run_sections)]]))
  section %in% run_sections || section %in% unlist(.deps[run_sections])
}

# Timing infrastructure
.timer_start    <- proc.time()["elapsed"]
.section_start  <- .timer_start
.last_section   <- NULL
.section_times  <- list()
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
  .last_section  <<- label
}

.fail <- function(e) {
  .section_status[[.last_section]] <<- conditionMessage(e)
  cat(sprintf("   !! FAILED: %s\n", conditionMessage(e)))
}

# Run summary (call at end of pipeline)
.print_summary <- function() {
  if (!is.null(.last_section)) {
    elapsed <- proc.time()["elapsed"] - .section_start
    .section_times[[.last_section]] <<- elapsed
    if (is.null(.section_status[[.last_section]])) .section_status[[.last_section]] <<- "OK"
    cat(sprintf("   done (%.1fs)\n", elapsed))
  }
  total <- proc.time()["elapsed"] - .timer_start

  n_ok   <- sum(unlist(.section_status) == "OK")
  n_fail <- sum(unlist(.section_status) != "OK")
  cat(sprintf("\n========================================\n"))
  cat(sprintf("  Pipeline complete\n"))
  cat(sprintf("  %d sections run: %d OK, %d failed\n", n_ok + n_fail, n_ok, n_fail))
  cat(sprintf("  Total: %.0fs (%.1f min)\n", total, total / 60))
  cat(sprintf("========================================\n"))

  if (n_fail > 0) {
    cat("\n  FAILURES:\n")
    for (nm in names(.section_status)) {
      if (.section_status[[nm]] != "OK") {
        cat(sprintf("    !! %s: %s\n", nm, .section_status[[nm]]))
      }
    }
  }

  cat("\n  Section times (slowest first):\n")
  times_sorted <- sort(unlist(.section_times), decreasing = TRUE)
  for (nm in names(times_sorted)) {
    st <- .section_status[[nm]]
    status <- if (is.null(st) || st == "OK") " " else "!"
    cat(sprintf("  %s %5.1fs  %s\n", status, times_sorted[nm], nm))
  }
}
