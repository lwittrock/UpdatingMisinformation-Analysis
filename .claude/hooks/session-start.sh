#!/bin/bash
set -euo pipefail

# Only run in remote Claude Code on the web sessions
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

echo "Installing R packages for UpdatingMisinformation-Analysis..."

Rscript -e "
pkgs <- c(
  'tidyr', 'dplyr', 'plotrix', 'stringr', 'anytime',
  'ggplot2', 'ggsci', 'gridExtra', 'ggpubr', 'ggforce',
  'stargazer', 'lme4', 'estimatr', 'languageserver'
)
missing <- pkgs[!pkgs %in% installed.packages()[,'Package']]
if (length(missing) > 0) {
  install.packages(missing, repos='https://cloud.r-project.org', quiet=TRUE)
  cat('Installed:', paste(missing, collapse=', '), '\n')
} else {
  cat('All R packages already installed.\n')
}
"
