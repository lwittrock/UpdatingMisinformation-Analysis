# ──────────────────────────────────────────────────
# Bootstrap — one-stop setup for the analysis pipeline
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Loads packages, applies the stargazer patch, sources the utilities and the
# data, and sets .utils_loaded. Sourced once by run_all.R, and by each
# analysis file (01-03) when it is run on its own.
# ──────────────────────────────────────────────────

# Configuration defaults (set these before sourcing to override)
if (!exists("inpath"))       inpath       <- "data/processed"
if (!exists("output_type"))  output_type  <- "latex"   # "latex" or "html"
if (!exists("set_dpi"))      set_dpi      <- 400
if (!exists("run_sections")) run_sections <- "all"


######################################################
# PACKAGES
######################################################
required_packages <- c("ggplot2", "dplyr", "stargazer", "estimatr")
missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nInstall with: install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))")
}

library(ggplot2)
library(dplyr)
library(estimatr)

library(stargazer)
# Required for stargazer 5.2.3 with R >= 4.2. Can be removed when stargazer is updated.
# Bug: if(is.na(s)) is called on a vector at line ~2104 of .stargazer.wrap.
# Fix: add a length check before the scalar ops.
local({
  sg_env <- environment(stargazer::stargazer)
  sw <- get(".stargazer.wrap", envir = sg_env)
  sw_body <- deparse(body(sw))
  sw_body <- gsub(
    "if (is.na(s)) {",
    "if (length(s) > 1) { return(\"\") }\n        if (is.na(s)) {",
    sw_body, fixed = TRUE)
  body(sw) <- parse(text = paste(sw_body, collapse = "\n"))[[1]]
  unlockBinding(".stargazer.wrap", sg_env)
  assign(".stargazer.wrap", sw, envir = sg_env)
  lockBinding(".stargazer.wrap", sg_env)
})


######################################################
# UTILITIES & DATA
######################################################
source("code/utils/helpers.R")
source("code/utils/figure_helpers.R")
.utils_loaded <- TRUE

source("code/utils/derived_variables.R")
