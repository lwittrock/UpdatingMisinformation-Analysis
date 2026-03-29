# ──────────────────────────────────────────────────
# Package Loading
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Purpose: Check and load all required packages, apply stargazer patch
# ──────────────────────────────────────────────────

required_packages <- c("ggplot2", "ggsci", "gridExtra", "tidyr", "dplyr",
                        "plotrix", "ggpubr", "ggforce", "stargazer", "lme4",
                        "stringr", "estimatr")
missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nInstall with: install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))")
}

library(ggplot2)
library(ggsci)
library(gridExtra)
library(tidyr)
library(dplyr)
library(plotrix)
library(ggpubr)
library(ggforce)

library(stargazer)
# Required for stargazer 5.2.3 with R >= 4.2. Can be removed when stargazer is updated.
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
