# Generate Hard Clean person-level figures and supporting tables.
#
# Inputs:
#   analysis/hard_clean_person_level_with_giq.csv
#
# Outputs:
#   analysis/images/hard_clean_person_figures/*.png
#   analysis/hard_clean_person_*_results.csv
#   analysis/hard_clean_person_figures_summary.txt

suppressPackageStartupMessages({
  library(stats)
  library(grDevices)
  library(graphics)
})

set.seed(20260622)

source_path <- file.path("analysis", "hard_clean_person_level_with_giq.csv")
output_dir <- file.path("analysis", "images", "hard_clean_person_figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(source_path)) {
  stop("Missing input file: ", source_path, ". Run analysis/build_hard_clean_person_level_csv.R first.")
}

raw <- read.csv(source_path, check.names = FALSE, stringsAsFactors = FALSE)

theme <- list(
  ink = "#202423",
  muted = "#52635B",
  grid = "#D8E4DA",
  green = "#1B8A5A",
  blue = "#1F77B4",
  orange = "#D97924",
  red = "#B23A3A",
  purple = "#7B4BA0",
  pale = "#F4F8F5"
)

condition_levels <- c(
  "C0_baseline", "C1_shake", "C2_zoom", "C4_shake_zoom",
  "C3_recoil", "C5_shake_recoil", "C6_zoom_recoil", "C7_all"
)

condition_labels <- c(
  C0_baseline = "Control",
  C1_shake = "Shake",
  C2_zoom = "Zoom",
  C3_recoil = "Recoil",
  C4_shake_zoom = "Shake + Zoom",
  C5_shake_recoil = "Shake + Recoil",
  C6_zoom_recoil = "Zoom + Recoil",
  C7_all = "Shake + Zoom + Recoil"
)

palette <- c(
  "#4E79A7", "#F28E2B", "#59A14F", "#76B7B2",
  "#E15759", "#B07AA1", "#EDC948", "#9C755F"
)

as_bool <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "si", "sí")
}

fmt_p <- function(p) {
  ifelse(!is.finite(p), "", ifelse(p < 0.001, "p<.001", sprintf("p=%.3f", p)))
}

metric_specs <- data.frame(
  metric = c(
    "giq_mean", "giq_engagement", "giq_engrossment", "giq_total_immersion",
    "duration_seconds", "kill_rate", "input_rate", "damage_taken_rate",
    "hits_rate", "jitter_rate", "distance_rate", "nearest_enemy_dist_mean",
    "low_hp_ratio"
  ),
  label = c(
    "Promedio en GIQ", "Engagement", "Engrossment", "Inmersion total",
    "Supervivencia (s)", "Kills/s", "Inputs/s", "Dano recibido/s",
    "Golpes recibidos/s", "Jitter/s", "Distancia/s", "Distancia al enemigo",
    "HP bajo"
  ),
  stringsAsFactors = FALSE
)
metric_specs <- metric_specs[metric_specs$metric %in% names(raw), , drop = FALSE]

raw$condition <- factor(raw$condition, levels = condition_levels)
raw$condition_label <- factor(unname(condition_labels[as.character(raw$condition)]), levels = unname(condition_labels))
raw$Shake <- factor(as_bool(raw$camera_shake), levels = c(FALSE, TRUE), labels = c("Ausente", "Presente"))
raw$Zoom <- factor(as_bool(raw$camera_zoom), levels = c(FALSE, TRUE), labels = c("Ausente", "Presente"))
raw$Recoil <- factor(as_bool(raw$camera_recoil), levels = c(FALSE, TRUE), labels = c("Ausente", "Presente"))

open_png <- function(filename, width = 2500, height = 1500, res = 200) {
  png(file.path(output_dir, filename), width = width, height = height, res = res)
  par(bg = "white", fg = theme$ink, col.axis = theme$ink, col.lab = theme$ink,
      col.main = theme$ink, family = "sans")
}

close_png <- function() dev.off()

draw_title <- function(title, subtitle = NULL) {
  title(title, adj = 0, cex.main = 1.35, font.main = 2, line = 1)
  if (!is.null(subtitle) && nzchar(subtitle)) {
    mtext(subtitle, side = 3, adj = 0, line = -0.1, cex = 0.86, col = theme$muted)
  }
}

mean_ci <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(mean = NA_real_, lo = NA_real_, hi = NA_real_, n = 0))
  se <- sd(x) / sqrt(length(x))
  margin <- qt(0.975, df = max(length(x) - 1, 1)) * se
  c(mean = mean(x), lo = mean(x) - margin, hi = mean(x) + margin, n = length(x))
}

brown_forsythe_p <- function(df) {
  cell <- interaction(df$Shake, df$Zoom, df$Recoil, drop = TRUE)
  med <- ave(df$Y, cell, FUN = function(x) median(x, na.rm = TRUE))
  fit <- aov(abs(df$Y - med) ~ cell)
  as.numeric(summary(fit)[[1]]$`Pr(>F)`[1])
}

prepare_metric <- function(metric) {
  df <- raw[
    is.finite(suppressWarnings(as.numeric(raw[[metric]]))) &
      !is.na(raw$Shake) & !is.na(raw$Zoom) & !is.na(raw$Recoil),
  ]
  if (nrow(df) < 8) return(NULL)
  df$Y <- suppressWarnings(as.numeric(df[[metric]]))
  df
}

assumption_rows <- list()
art_rows <- list()

for (i in seq_len(nrow(metric_specs))) {
  metric <- metric_specs$metric[i]
  label <- metric_specs$label[i]
  df <- prepare_metric(metric)
  if (is.null(df)) next

  fit <- aov(Y ~ Shake * Zoom * Recoil, data = df)
  resid <- residuals(fit)
  shapiro_p <- if (length(resid) >= 3 && length(resid) <= 5000) shapiro.test(resid)$p.value else NA_real_
  bf_p <- tryCatch(brown_forsythe_p(df), error = function(e) NA_real_)
  assumption_rows[[metric]] <- data.frame(
    metric = metric,
    metric_label = label,
    n = nrow(df),
    shapiro_p = shapiro_p,
    brown_forsythe_p = bf_p,
    residual_normality_ok = is.finite(shapiro_p) & shapiro_p >= 0.05,
    variance_ok = is.finite(bf_p) & bf_p >= 0.05,
    stringsAsFactors = FALSE
  )

  fit_rank <- aov(rank(Y, ties.method = "average") ~ Shake * Zoom * Recoil, data = df)
  tab <- as.data.frame(summary(fit_rank)[[1]])
  tab$term <- rownames(tab)
  tab <- tab[tab$term != "Residuals", , drop = FALSE]
  res_ss <- sum(residuals(fit_rank)^2)
  art_rows[[metric]] <- data.frame(
    metric = metric,
    metric_label = label,
    term = trimws(tab$term),
    F = tab$`F value`,
    p = tab$`Pr(>F)`,
    rank_eta_p2 = tab$`Sum Sq` / (tab$`Sum Sq` + res_ss),
    stringsAsFactors = FALSE
  )
}

assumptions <- do.call(rbind, assumption_rows)
art_results <- do.call(rbind, art_rows)
art_results <- art_results[is.finite(art_results$p), , drop = FALSE]
art_results$q_all <- p.adjust(art_results$p, method = "BH")
write.csv(assumptions, file.path("analysis", "hard_clean_person_assumption_checks.csv"), row.names = FALSE)
write.csv(art_results, file.path("analysis", "hard_clean_person_art_results.csv"), row.names = FALSE)

count_metrics <- c(total_kills = "Kills", input_total = "Inputs", hits_taken = "Golpes recibidos")
count_rows <- list()
for (metric in names(count_metrics)) {
  if (!metric %in% names(raw) || !"duration_seconds" %in% names(raw)) next
  df <- prepare_metric(metric)
  if (is.null(df)) next
  df$count <- pmax(0, round(suppressWarnings(as.numeric(df[[metric]]))))
  df$duration_seconds <- pmax(1, suppressWarnings(as.numeric(df$duration_seconds)))
  fit <- tryCatch(
    glm(count ~ Shake * Zoom * Recoil + offset(log(duration_seconds)), data = df, family = quasipoisson()),
    error = function(e) NULL
  )
  if (is.null(fit)) next
  tab <- drop1(fit, test = "F")
  rows <- rownames(tab)
  keep <- rows[!rows %in% c("<none>")]
  count_rows[[metric]] <- data.frame(
    metric = metric,
    metric_label = count_metrics[[metric]],
    model = "quasi-Poisson count model with duration offset",
    term = keep,
    statistic = tab[keep, "F value"],
    p = tab[keep, "Pr(>F)"],
    dispersion = summary(fit)$dispersion,
    stringsAsFactors = FALSE
  )
}
count_results <- if (length(count_rows) > 0) do.call(rbind, count_rows) else data.frame()
if (nrow(count_results) > 0) count_results$q_all <- p.adjust(count_results$p, method = "BH")
write.csv(count_results, file.path("analysis", "hard_clean_person_count_model_results.csv"), row.names = FALSE)

baseline_metrics <- intersect(
  c("giq_mean", "duration_seconds", "kill_rate", "input_rate", "damage_taken_rate",
    "hits_rate", "distance_rate", "nearest_enemy_dist_mean", "low_hp_ratio"),
  names(raw)
)
baseline_rows <- list()
for (metric in baseline_metrics) {
  label <- metric_specs$label[match(metric, metric_specs$metric)]
  control <- suppressWarnings(as.numeric(raw[[metric]][raw$condition == "C0_baseline"]))
  control <- control[is.finite(control)]
  if (length(control) < 2) next
  for (condition in condition_levels[condition_levels != "C0_baseline"]) {
    values <- suppressWarnings(as.numeric(raw[[metric]][raw$condition == condition]))
    values <- values[is.finite(values)]
    if (length(values) < 2) next
    test <- tryCatch(t.test(values, control), error = function(e) NULL)
    pooled <- sqrt((var(values) + var(control)) / 2)
    est_std <- ifelse(is.finite(pooled) && pooled > 0, (mean(values) - mean(control)) / pooled, NA_real_)
    ci_std <- if (!is.null(test) && is.finite(pooled) && pooled > 0) test$conf.int / pooled else c(NA_real_, NA_real_)
    baseline_rows[[paste(metric, condition, sep = "_")]] <- data.frame(
      metric = metric,
      metric_label = label,
      condition = condition,
      condition_label = condition_labels[[condition]],
      estimate = mean(values) - mean(control),
      estimate_std = est_std,
      lo = ci_std[1],
      hi = ci_std[2],
      p_value = if (!is.null(test)) test$p.value else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}
baseline <- do.call(rbind, baseline_rows)
baseline$q_value <- p.adjust(baseline$p_value, method = "BH")
write.csv(baseline, file.path("analysis", "hard_clean_person_baseline_contrasts.csv"), row.names = FALSE)

tradeoff_metrics <- intersect(
  c("duration_seconds", "kill_rate", "input_rate", "damage_taken_rate", "hits_rate", "distance_rate"),
  names(raw)
)
tradeoff_rows <- list()
for (metric in tradeoff_metrics) {
  x <- suppressWarnings(as.numeric(raw$giq_mean))
  y <- suppressWarnings(as.numeric(raw[[metric]]))
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 5) next
  test <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
  tradeoff_rows[[metric]] <- data.frame(
    metric = metric,
    metric_label = metric_specs$label[match(metric, metric_specs$metric)],
    rho = as.numeric(test$estimate),
    p = test$p.value,
    n = sum(ok),
    stringsAsFactors = FALSE
  )
}
tradeoff <- do.call(rbind, tradeoff_rows)
tradeoff$q <- p.adjust(tradeoff$p, method = "BH")
write.csv(tradeoff, file.path("analysis", "hard_clean_person_tradeoff_correlations.csv"), row.names = FALSE)

# Figure 01: assumption diagnostics.
open_png("01_assumption_diagnostics.png")
par(mar = c(7, 10, 5.5, 3))
plot_df <- assumptions[order(assumptions$shapiro_p), ]
y <- seq_len(nrow(plot_df))
xlim <- c(0, 1)
plot(plot_df$shapiro_p, y, pch = 19, col = theme$blue, xlim = xlim, yaxt = "n",
     xlab = "p-value", ylab = "", main = "")
points(plot_df$brown_forsythe_p, y, pch = 17, col = theme$orange)
abline(v = 0.05, lty = 2, col = theme$red)
axis(2, at = y, labels = plot_df$metric_label, las = 1, cex.axis = 0.75)
grid(nx = NA, ny = NULL, col = theme$grid)
legend("bottomright", legend = c("Shapiro residual", "Brown-Forsythe"), pch = c(19, 17),
       col = c(theme$blue, theme$orange), bty = "n")
draw_title("Supuestos del modelo", "p < .05 indica alerta de normalidad residual o varianza")
close_png()

# Figure 02: GIQ by condition.
open_png("02_giq_by_condition.png")
par(mar = c(10, 5, 5.5, 2))
boxplot(giq_mean ~ condition_label, data = raw, col = palette, border = theme$ink,
        las = 2, ylab = "Promedio en GIQ", xlab = "", outline = FALSE)
stripchart(giq_mean ~ condition_label, data = raw, vertical = TRUE, method = "jitter",
           pch = 16, cex = 0.55, col = adjustcolor(theme$ink, alpha.f = 0.45), add = TRUE)
draw_title("Promedio en GIQ por tratamiento")
close_png()

# Figure 03: GIQ interaction.
open_png("03_giq_interaction_shake_zoom.png")
par(mfrow = c(1, 2), mar = c(5, 5, 5, 2), oma = c(0, 0, 3, 0))
for (recoil in levels(raw$Recoil)) {
  df <- raw[raw$Recoil == recoil & is.finite(raw$giq_mean), ]
  means <- aggregate(giq_mean ~ Shake + Zoom, df, mean)
  plot(c(1, 2), range(raw$giq_mean, na.rm = TRUE), type = "n", xaxt = "n",
       xlab = "Shake", ylab = "Promedio en GIQ", main = paste("Recoil", recoil))
  axis(1, at = c(1, 2), labels = levels(raw$Shake))
  for (zoom in levels(raw$Zoom)) {
    part <- means[means$Zoom == zoom, ]
    x <- match(part$Shake, levels(raw$Shake))
    lines(x, part$giq_mean, type = "b", pch = 19,
          col = ifelse(zoom == "Ausente", theme$blue, theme$orange), lwd = 2)
  }
  legend("bottomleft", legend = paste("Zoom", levels(raw$Zoom)),
         col = c(theme$blue, theme$orange), lwd = 2, pch = 19, bty = "n")
}
mtext("Interaccion GIQ: Shake x Zoom", side = 3, outer = TRUE, adj = 0, font = 2, cex = 1.3)
close_png()

forest_plot <- function(df, filename, title_text, subtitle_text = "") {
  df <- df[is.finite(df$estimate_std) & is.finite(df$lo) & is.finite(df$hi), , drop = FALSE]
  if (nrow(df) == 0) return(invisible(NULL))
  df <- df[order(abs(df$estimate_std), decreasing = TRUE), ]
  if (nrow(df) > 16) df <- df[seq_len(16), ]
  df <- df[rev(seq_len(nrow(df))), ]
  open_png(filename, width = 2600, height = 1500)
  par(mar = c(6, 15, 5.8, 4))
  y <- seq_len(nrow(df))
  xlim <- range(c(df$lo, df$hi, 0), na.rm = TRUE)
  plot(df$estimate_std, y, xlim = xlim, yaxt = "n", ylab = "",
       xlab = "Diferencia estandarizada vs Control", pch = 19,
       col = ifelse(df$estimate_std >= 0, theme$green, theme$red))
  segments(df$lo, y, df$hi, y, col = theme$ink, lwd = 2)
  abline(v = 0, lty = 2, col = theme$muted)
  axis(2, at = y, labels = paste(df$condition_label, df$metric_label, sep = " · "),
       las = 1, cex.axis = 0.72)
  grid(nx = NULL, ny = NA, col = theme$grid)
  draw_title(title_text, subtitle_text)
  close_png()
}

forest_plot(
  baseline[!baseline$metric %in% c("giq_mean"), ],
  "04_gameplay_synthesis_vs_control.png",
  "Sintesis de gameplay vs Control",
  "Valores positivos indican mayor valor que Control"
)

heatmap_plot <- function(df, filename, title_text, value_col, p_col = NULL) {
  if (nrow(df) == 0) return(invisible(NULL))
  terms <- unique(gsub(":", " x ", df$term))
  metrics <- unique(df$metric_label)
  mat <- matrix(NA_real_, nrow = length(metrics), ncol = length(terms),
                dimnames = list(metrics, terms))
  labels <- matrix("", nrow = length(metrics), ncol = length(terms),
                   dimnames = list(metrics, terms))
  for (i in seq_len(nrow(df))) {
    r <- match(df$metric_label[i], metrics)
    c <- match(gsub(":", " x ", df$term[i]), terms)
    mat[r, c] <- df[[value_col]][i]
    if (!is.null(p_col)) labels[r, c] <- fmt_p(df[[p_col]][i])
  }
  open_png(filename, width = 2600, height = 1450)
  par(mar = c(8, 12, 5.5, 3))
  image(seq_len(ncol(mat)), seq_len(nrow(mat)), t(mat[nrow(mat):1, , drop = FALSE]),
        axes = FALSE, xlab = "", ylab = "", col = hcl.colors(20, "YlOrRd", rev = TRUE))
  axis(1, at = seq_len(ncol(mat)), labels = colnames(mat), las = 2, cex.axis = 0.72)
  axis(2, at = seq_len(nrow(mat)), labels = rev(rownames(mat)), las = 1, cex.axis = 0.74)
  for (r in seq_len(nrow(mat))) {
    for (c in seq_len(ncol(mat))) {
      text(c, nrow(mat) - r + 1, labels[r, c], cex = 0.7, col = theme$ink)
    }
  }
  draw_title(title_text, "Celdas muestran p-value; color resume tamano de efecto")
  close_png()
}

heatmap_plot(art_results, "05_art_factorial_effect_map.png", "ART factorial: mapa de efectos", "rank_eta_p2", "p")
if (nrow(count_results) > 0) {
  count_results$effect_size <- -log10(pmax(count_results$p, 1e-12))
  heatmap_plot(count_results, "06_count_model_effect_map.png", "Modelos de conteo: mapa de efectos", "effect_size", "p")
}

forest_plot(
  baseline,
  "07_baseline_forest_vs_control.png",
  "Contrastes frente a Control",
  "Diferencias estandarizadas con IC 95%"
)

# Figure 08: immersion control matrix.
imm_metrics <- intersect(c("giq_mean", "giq_engagement", "giq_engrossment", "giq_total_immersion"), names(raw))
if (length(imm_metrics) > 0) {
  mat <- matrix(NA_real_, nrow = length(imm_metrics), ncol = length(condition_levels) - 1)
  rownames(mat) <- metric_specs$label[match(imm_metrics, metric_specs$metric)]
  colnames(mat) <- unname(condition_labels[condition_levels[-1]])
  for (i in seq_along(imm_metrics)) {
    base <- mean(suppressWarnings(as.numeric(raw[[imm_metrics[i]]][raw$condition == "C0_baseline"])), na.rm = TRUE)
    for (j in seq_along(condition_levels[-1])) {
      cond <- condition_levels[-1][j]
      mat[i, j] <- mean(suppressWarnings(as.numeric(raw[[imm_metrics[i]]][raw$condition == cond])), na.rm = TRUE) - base
    }
  }
  open_png("08_immersion_control_matrix.png", width = 2600, height = 1300)
  par(mar = c(10, 10, 5.5, 3))
  lim <- max(abs(mat), na.rm = TRUE)
  breaks <- seq(-lim, lim, length.out = 21)
  cols <- hcl.colors(20, "Blue-Red 3")
  image(seq_len(ncol(mat)), seq_len(nrow(mat)), t(mat[nrow(mat):1, , drop = FALSE]),
        axes = FALSE, xlab = "", ylab = "", col = cols, breaks = breaks)
  axis(1, at = seq_len(ncol(mat)), labels = colnames(mat), las = 2, cex.axis = 0.74)
  axis(2, at = seq_len(nrow(mat)), labels = rev(rownames(mat)), las = 1, cex.axis = 0.8)
  for (r in seq_len(nrow(mat))) {
    for (c in seq_len(ncol(mat))) {
      text(c, nrow(mat) - r + 1, sprintf("%+.2f", mat[r, c]), cex = 0.82, col = theme$ink)
    }
  }
  draw_title("Matriz de inmersion vs Control", "Diferencia promedio respecto a Control")
  close_png()
}

# Figure 09: GIQ-gameplay trade-off.
if ("input_rate" %in% names(raw)) {
  open_png("09_tradeoff_giq_gameplay.png")
  par(mar = c(6, 6, 5.5, 2))
  x <- suppressWarnings(as.numeric(raw$giq_mean))
  y <- suppressWarnings(as.numeric(raw$input_rate))
  plot(x, y, pch = 19, col = palette[as.numeric(raw$condition)], xlab = "Promedio en GIQ",
       ylab = "Inputs/s", main = "")
  abline(lm(y ~ x), col = theme$ink, lwd = 2)
  legend("topleft", legend = levels(raw$condition_label), col = palette, pch = 19, cex = 0.68, bty = "n")
  draw_title("Trade-off inmersion vs desempeno", "Asociacion visual entre GIQ e inputs por segundo")
  close_png()
}

summary_lines <- c(
  "Hard Clean person-level figure summary",
  "======================================",
  sprintf("Source: %s", source_path),
  sprintf("Rows in source: %d", nrow(raw)),
  sprintf("Rows with valid GIQ used for linked figures: %d", sum(is.finite(raw$giq_mean))),
  "",
  "Assumption check:",
  paste(
    sprintf(
      "- %s: Shapiro %s; Brown-Forsythe %s",
      assumptions$metric_label,
      fmt_p(assumptions$shapiro_p),
      fmt_p(assumptions$brown_forsythe_p)
    ),
    collapse = "\n"
  ),
  "",
  "ART nominal p < .05:",
  if (any(is.finite(art_results$p) & art_results$p < 0.05, na.rm = TRUE)) {
    sig_art <- art_results[is.finite(art_results$p) & art_results$p < 0.05, , drop = FALSE]
    paste(
      sprintf(
        "- %s / %s: F=%.2f, %s, q_all=%.2f",
        sig_art$metric_label,
        gsub(":", " x ", sig_art$term),
        sig_art$F,
        fmt_p(sig_art$p),
        sig_art$q_all
      ),
      collapse = "\n"
    )
  } else {
    "None."
  },
  "",
  "Generated figures:",
  paste(
    paste0("- ", file.path(output_dir, list.files(output_dir, pattern = "\\.png$"))),
    collapse = "\n"
  )
)

writeLines(summary_lines, file.path("analysis", "hard_clean_person_figures_summary.txt"))
cat(paste(summary_lines, collapse = "\n"), "\n")
