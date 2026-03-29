# ──────────────────────────────────────────────────
# Run Control
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Purpose: Section selector, dependency resolution, and timing
# ──────────────────────────────────────────────────

# Dependencies between sections (section -> prerequisite)
# These handle within-file ordering; cross-file dependencies are resolved
# by derived_variables.R (run before any analysis file).
.deps <- list(
  # Retractions: fig5 creates df_retract_sum_all
  fig6 = "fig5",
  fig9 = "fig5",
  # Confirmations: fig7 creates df_confirm_temp and df_confirm_change_sum
  fig8 = "fig7",
  fig11 = "fig7",
  # Confirmations: fig8 creates df_confirm_type
  fig16 = "fig8"
)

.should_run <- function(section) {
  if (identical(run_sections, "all")) return(TRUE)
  if (identical(run_sections, "figures")) return(grepl("^fig", section))
  if (identical(run_sections, "tables")) return(grepl("^tab", section) || section %in% unlist(.deps[run_sections[grepl("^tab", run_sections)]]))
  section %in% run_sections || section %in% unlist(.deps[run_sections])
}

# Timing infrastructure
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

# Run summary (call at end of pipeline)
.print_summary <- function() {
  if (!is.null(.last_section)) {
    elapsed <- proc.time()["elapsed"] - .section_start
    .section_times[[.last_section]] <<- elapsed
    if (is.null(.section_status[[.last_section]])) .section_status[[.last_section]] <<- "OK"
    cat(sprintf("   done (%.1fs)\n", elapsed))
  }
  total <- proc.time()["elapsed"] - .timer_start

  n_ok <- sum(unlist(.section_status) == "OK")
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
  times_vec <- unlist(.section_times)
  times_sorted <- sort(times_vec, decreasing = TRUE)
  for (nm in names(times_sorted)) {
    st <- .section_status[[nm]]
    status <- if (is.null(st) || st == "OK") " " else "!"
    cat(sprintf("  %s %5.1fs  %s\n", status, times_sorted[nm], nm))
  }
}
