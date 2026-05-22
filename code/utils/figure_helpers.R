# ──────────────────────────────────────────────────
# Figure & Table Helpers
# Part of: Belief Updating with Misinformation analysis pipeline
#
# Purpose: Shared builders for the bias-by-prior TikZ figures and the c/d
#          estimate tables. Used by the 01-03 analysis files.
#
# A bias figure is produced as a TikZ snippet (\input into the paper). The
# observed data bars are generated here; the red model-prediction curve is
# added by hand in the paper at the "% ADD THEORY CURVE BELOW" marker.
# Multi-panel paper figures are assembled with write_tikz_figure().
# ──────────────────────────────────────────────────


# Standard TikZ axis labels — used consistently across all bias figures
LBL_TIKZ_REG <- "\\small{Over-report}"
LBL_TIKZ_RET <- "\\small{$b_{t+1}(R|r,\\xmark)$}"
POS_REG      <- "Overreacts"   # positive bias direction for reg / conf
POS_RET      <- "Underreacts"  # positive bias direction for ret / opp


######################################################
# HELPER: single-series TikZ bias-bar body
#
# tbl: data.frame with columns prior_bin, Mean, SD, N (10 rows)
# Returns the \begin{tikzpicture}...\end{tikzpicture} lines.
######################################################
tikz_bias_body <- function(tbl, ylabel, positive_label = POS_REG) {
  tbl <- tbl[order(factor(tbl$prior_bin, levels = prior_bin_labels)), ]
  bin_mids <- seq(0.05, 0.95, 0.1)

  # 95% CI half-widths: 1.96 * SE
  ci <- 1.96 * tbl$SD / sqrt(tbl$N)

  # y-axis range: cover mean ± 95% CI, always include zero
  y_lo <- min(c(0, tbl$Mean - ci), na.rm = TRUE)
  y_hi <- max(c(0, tbl$Mean + ci), na.rm = TRUE)
  pad  <- max(abs(c(y_lo, y_hi))) * 0.2
  y_lo <- floor((y_lo - pad) * 10) / 10
  y_hi <- ceiling((y_hi + pad) * 10) / 10

  neg_label <- if (positive_label == "Overreacts") "Underreacts" else "Overreacts"
  pos_mid <- y_hi * 0.6
  neg_mid <- y_lo * 0.6

  lines <- c(
    "\\begin{tikzpicture}[scale=12]",
    sprintf("\\draw[help lines,step=0.1] (0,%.1f) grid (1,%.1f);", y_lo, y_hi)
  )

  for (i in 1:10) {
    m  <- tbl$Mean[i]
    hw <- ci[i]
    if (is.na(m) || is.na(hw)) next
    x <- bin_mids[i]
    lines <- c(lines,
      sprintf("\\filldraw[opaque,white!80!black] (%.2f,0) rectangle (%.2f,%.4f);",
              x - 0.02, x + 0.02, m),
      sprintf("\\draw[line width=0.6] (%.2f,0) rectangle (%.2f,%.4f);",
              x - 0.02, x + 0.02, m),
      sprintf("\\draw[line width=0.6,|-|] (%.2f,%.4f) -- (%.2f,%.4f);",
              x, m - hw, x, m + hw)
    )
  }

  c(lines,
    "\\draw[line width=0.8,->]  (0,0) -- (1.1,0) node[below] {\\small{$p_{t-1}(R)$}};",
    sprintf("\\draw[line width=0.8,<->] (0,%.1f) -- (0,%.1f) node[above] {%s};",
            y_lo, y_hi, ylabel),
    sprintf("\\draw (0,%.2f) node[above,rotate=90] {\\small{%s}};",
            pos_mid, positive_label),
    sprintf("\\draw (0,%.2f) node[above,rotate=90] {\\small{%s}};",
            neg_mid, neg_label),
    "% ADD THEORY CURVE BELOW",
    "\\end{tikzpicture}"
  )
}


######################################################
# HELPER: grouped TikZ bias-bar body (N bars per bin)
#
# tbl_list: named list of data.frames (each with prior_bin, Mean, SD, N)
# group_colors: TikZ color names, one per group
# Returns the \begin{tikzpicture}...\end{tikzpicture} lines.
######################################################
tikz_bias_grouped_body <- function(tbl_list, group_colors, ylabel,
                                    positive_label = POS_REG) {
  n_groups <- length(tbl_list)
  bar_w    <- 0.02
  total_w  <- n_groups * bar_w
  bin_starts <- seq(0, 0.9, 0.1)

  all_m  <- unlist(lapply(tbl_list, `[[`, "Mean"))
  all_sd <- unlist(lapply(tbl_list, `[[`, "SD"))
  all_n  <- unlist(lapply(tbl_list, `[[`, "N"))
  all_ci <- 1.96 * all_sd / sqrt(pmax(all_n, 1))
  y_lo <- min(c(0, all_m - all_ci), na.rm = TRUE)
  y_hi <- max(c(0, all_m + all_ci), na.rm = TRUE)
  pad  <- max(abs(c(y_lo, y_hi))) * 0.2
  y_lo <- floor((y_lo - pad) * 10) / 10
  y_hi <- ceiling((y_hi + pad) * 10) / 10

  neg_label <- if (positive_label == "Overreacts") "Underreacts" else "Overreacts"
  pos_mid   <- y_hi * 0.6
  neg_mid   <- y_lo * 0.6

  lines <- c(
    "\\begin{tikzpicture}[scale=12]",
    sprintf("\\draw[help lines,step=0.1] (0,%.1f) grid (1,%.1f);", y_lo, y_hi)
  )

  for (i in 1:10) {
    x0 <- bin_starts[i] + (0.1 - total_w) / 2
    for (j in seq_along(tbl_list)) {
      tbl <- tbl_list[[j]]
      tbl <- tbl[order(factor(tbl$prior_bin, levels = prior_bin_labels)), ]
      m  <- if (i <= nrow(tbl)) tbl$Mean[i] else NA_real_
      sd <- if (i <= nrow(tbl)) tbl$SD[i]   else NA_real_
      n  <- if (i <= nrow(tbl)) tbl$N[i]    else NA_real_
      if (is.na(m)) next
      hw <- 1.96 * sd / sqrt(max(n, 1))
      if (is.na(hw)) next

      xl <- x0 + (j - 1) * bar_w
      xr <- xl + bar_w
      xc <- (xl + xr) / 2
      col <- group_colors[j]

      lines <- c(lines,
        sprintf("\\filldraw[opaque,white!80!%s] (%.4f,0) rectangle (%.4f,%.4f);",
                col, xl, xr, m),
        sprintf("\\draw[line width=0.6] (%.4f,0) rectangle (%.4f,%.4f);",
                xl, xr, m),
        sprintf("\\draw[line width=0.6,|-|] (%.4f,%.4f) -- (%.4f,%.4f);",
                xc, m - hw, xc, m + hw)
      )
    }
  }

  lines <- c(lines,
    "\\draw[line width=0.8,->]  (0,0) -- (1.1,0) node[below] {\\small{$p_{t-1}(R)$}};",
    sprintf("\\draw[line width=0.8,<->] (0,%.1f) -- (0,%.1f) node[above] {%s};",
            y_lo, y_hi, ylabel),
    sprintf("\\draw (0,%.2f) node[above,rotate=90] {\\small{%s}};",
            pos_mid, positive_label),
    sprintf("\\draw (0,%.2f) node[above,rotate=90] {\\small{%s}};",
            neg_mid, neg_label)
  )

  grp_names <- names(tbl_list)
  for (j in seq_along(grp_names)) {
    lx <- 0.62 + (j - 1) * 0.13
    ly <- y_hi - 0.03 * (y_hi - y_lo)
    lines <- c(lines,
      sprintf("\\filldraw[white!80!%s] (%.2f,%.4f) rectangle (%.2f,%.4f);",
              group_colors[j], lx, ly - 0.025, lx + 0.025, ly),
      sprintf("\\node[right] at (%.2f,%.4f) {\\scriptsize{%s}};",
              lx + 0.025, ly - 0.012, grp_names[j])
    )
  }

  c(lines, "\\end{tikzpicture}")
}


######################################################
# WRITERS: single-panel figures write one .tex directly;
# write_tikz_figure assembles several panels into one.
######################################################
write_tikz_bias <- function(tbl, out_path, ylabel, positive_label = POS_REG) {
  neg_label <- if (positive_label == "Overreacts") "Underreacts" else "Overreacts"
  writeLines(c(
    "% Auto-generated by the analysis pipeline — do not edit",
    paste0("% y > 0: ", positive_label, "  |  y < 0: ", neg_label),
    tikz_bias_body(tbl, ylabel, positive_label)
  ), out_path)
  invisible(tbl)
}

write_tikz_bias_grouped <- function(tbl_list, group_colors, out_path, ylabel,
                                     positive_label = POS_REG) {
  writeLines(c(
    "% Auto-generated by the analysis pipeline — do not edit",
    tikz_bias_grouped_body(tbl_list, group_colors, ylabel, positive_label)
  ), out_path)
  invisible(tbl_list)
}

# Assemble several bias panels into one multi-panel figure .tex.
# panels: list of list(body = <tikz lines>, caption = <subcaption text>)
# ncol:   panels per row (2 for the 6-panel B6-B8, 1 for the 2-panel B4/B9)
write_tikz_figure <- function(panels, out_path, ncol = 2) {
  panel_width <- if (ncol == 1) "\\textwidth"
                 else sprintf("%.3f\\textwidth", 0.97 / ncol)
  out <- c(
    "% Auto-generated by the analysis pipeline — do not edit",
    "% \\input this inside a figure environment.",
    "% Requires the subcaption and graphicx packages.",
    "% Each panel carries its own % ADD THEORY CURVE BELOW marker."
  )
  n <- length(panels)
  for (i in seq_len(n)) {
    out <- c(out,
      sprintf("\\begin{subfigure}[t]{%s}", panel_width),
      "\\centering",
      "\\resizebox{\\linewidth}{!}{",
      panels[[i]]$body,
      "}",
      sprintf("\\subcaption{%s}", panels[[i]]$caption),
      "\\end{subfigure}")
    if (i < n) out <- c(out, if (i %% ncol == 0) "\\\\[1.5ex]" else "\\hfill")
  }
  writeLines(out, out_path)
  invisible(panels)
}


######################################################
# HELPER: c and d by subgroup (separate OLS per cell)
#
# signal_var: "signal_ratio"     for regular signals
#             "signal_ratio_obj" for retractions/confirmations
#   For regular signals, signal_ratio = signal_ratio_obj = ±log(0.6/0.4):
#   both recover the same objective value since posterior_subj is computed
#   from the true urn probabilities, so added_info() just inverts back to 0.6/0.4.
#   For retractions/confirmations, signal_ratio_obj is used because the
#   objective signal magnitude is fixed by design (±log(0.6/0.4) and ±log(2)).
######################################################
run_cd_by <- function(df, group_var, signal_var = "signal_ratio") {
  formula_str <- paste0("obs_log_post_ratio ~ 0 + ", signal_var, " + prior_ratio")
  groups <- sort(unique(df[[group_var]]))
  results <- lapply(groups, function(g) {
    sub <- df[!is.na(df[[group_var]]) & df[[group_var]] == g, ]
    if (nrow(sub) < 10) return(NULL)  # skip underpowered cells
    # Need variation in signal_var — skip if constant within cell
    if (length(unique(sub[[signal_var]])) < 2) return(NULL)
    fit <- tryCatch(
      lm(as.formula(formula_str), data = sub),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)
    beta <- coef(fit)
    se   <- sqrt(diag(vcov(fit)))
    data.frame(
      group     = g,
      param     = c("c", "d"),
      estimate  = c(beta["prior_ratio"],   beta[[signal_var]]),
      std.error = c(se["prior_ratio"],      se[[signal_var]]),
      n         = nrow(sub),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, results)
  names(out)[names(out) == "group"] <- group_var
  out
}


######################################################
# HELPER: Two-panel LaTeX c/d table
#   Panel A: c (base-rate use) by signal type
#   Panel B: d (inference)     by signal type
#   Rows = round or prior bin;  Cols = Regular | Retraction | Confirmation
#   SE on separate row in parentheses (standard economics format)
######################################################
write_cd_tex <- function(cd_long, group_var, bins_ordered,
                          out_path, caption, label,
                          note = NULL) {
  get_cell <- function(g, param, sig) {
    row <- cd_long[cd_long[[group_var]] == g &
                     cd_long$param == param &
                     cd_long$signal_type == sig, ]
    if (nrow(row) == 0 || is.na(row$estimate[1]))
      return(list(est = "---", se = ""))
    list(est = sprintf("%.3f", row$estimate[1]),
         se  = sprintf("(%.3f)", row$std.error[1]))
  }

  grp_label <- tools::toTitleCase(gsub("_", " ", group_var))
  sig_types <- c("Regular", "Retraction", "Confirmation")

  header <- paste0(grp_label, " & Regular & Retraction & Confirmation \\\\")

  make_panel <- function(param_name, panel_label) {
    lines <- c(
      sprintf("\\multicolumn{4}{l}{\\textit{Panel %s: %s}} \\\\", panel_label,
              if (param_name == "c") "Base-Rate Use ($c$)" else "Inference ($d$)"),
      "\\hline",
      header,
      "\\hline"
    )
    for (g in bins_ordered) {
      cells <- lapply(sig_types, function(s) get_cell(g, param_name, s))
      # estimate row
      lines <- c(lines,
        paste(c(as.character(g), sapply(cells, `[[`, "est")), collapse = " & "),
        " \\\\"
      )
      # SE row (blank first column)
      se_vals <- sapply(cells, `[[`, "se")
      if (any(nzchar(se_vals))) {
        lines <- c(lines,
          paste(c("", se_vals), collapse = " & "),
          " \\\\"
        )
      }
    }
    lines <- c(lines, "\\hline")
    lines
  }

  default_note <- paste0(
    "SEs in parentheses. Separate OLS per cell. ",
    "Cells with $n<10$ or no signal variation omitted (---). ",
    "Bayesian benchmark: $c=d=1$. ",
    "Signal variable: $\\pm$log(0.6/0.4) for regular signals and retractions; ",
    "$\\pm$log(2) for confirmations."
  )

  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\begin{tabular}{lrrr}",
    "\\hline",
    make_panel("c", "A"),
    "",
    make_panel("d", "B"),
    "",
    paste0("\\multicolumn{4}{p{0.85\\textwidth}}{\\small ",
           if (!is.null(note)) note else default_note, "} \\\\"),
    "\\end{tabular}",
    "\\end{table}"
  )
  writeLines(lines, out_path)
  invisible(lines)
}


# Pooled OLS helper — returns lm fit or NULL (needs >= 10 obs + signal variation)
fit_pooled_cd <- function(df, signal_var) {
  if (is.null(df) || nrow(df) < 10) return(NULL)
  if (length(unique(df[[signal_var]])) < 2) return(NULL)
  tryCatch(
    lm(as.formula(paste0("obs_log_post_ratio ~ 0 + ", signal_var, " + prior_ratio")),
       data = df),
    error = function(e) NULL
  )
}

# Extract a pair of c/d rows from one lm fit
cd_row_from_fit <- function(fit, group_val, signal_type, signal_var, group_col) {
  if (is.null(fit)) return(NULL)
  beta <- coef(fit)
  se   <- sqrt(diag(vcov(fit)))
  if (anyNA(c(beta["prior_ratio"], beta[[signal_var]]))) return(NULL)
  row <- data.frame(
    param       = c("c", "d"),
    estimate    = c(beta["prior_ratio"], beta[[signal_var]]),
    std.error   = c(se["prior_ratio"],   se[[signal_var]]),
    signal_type = signal_type,
    n           = nrow(fit$model),
    stringsAsFactors = FALSE
  )
  row[[group_col]] <- as.character(group_val)
  row
}
