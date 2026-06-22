# Generate presentation-ready figures using real Supabase exports and the GIQ form.
#
# Outputs are written to:
#   LaTeX/images/presentation_figures/
#
# This script uses base R only, so it does not require ggplot2 or tidyverse.

suppressPackageStartupMessages({
  library(stats)
  library(grDevices)
})

output_dir <- file.path("analysis", "images", "presentation_figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("analysis", "listings"), recursive = TRUE, showWarnings = FALSE)

FIG_WIDTH <- 2400
FIG_HEIGHT <- 1267
FIG_RES <- 200

data_dir <- file.path("supabase_data", "r_outputs_existing_metrics")
run_level_path <- file.path(data_dir, "juicy_vs_existing_metrics_run_level.csv")
desc_path <- file.path(data_dir, "descriptives_by_condition.csv")
anova_path <- file.path(data_dir, "factorial_anova_results.csv")
dunnett_path <- file.path(data_dir, "dunnett_vs_baseline.csv")
form_path <- "Formulario Integrado InvCC26a Experimentos .csv"
cfg_path <- file.path("supabase_data", "PlayerIDConfig_rows.csv")

stopifnot(file.exists(run_level_path), file.exists(desc_path), file.exists(anova_path))
stopifnot(file.exists(form_path), file.exists(cfg_path))

run_level <- read.csv(run_level_path, check.names = FALSE, stringsAsFactors = FALSE)
descriptives <- read.csv(desc_path, check.names = FALSE, stringsAsFactors = FALSE)
anova <- read.csv(anova_path, check.names = FALSE, stringsAsFactors = FALSE)
dunnett <- if (file.exists(dunnett_path)) read.csv(dunnett_path, check.names = FALSE, stringsAsFactors = FALSE) else NULL
form <- read.csv(form_path, check.names = FALSE, stringsAsFactors = FALSE)
cfg <- read.csv(cfg_path, check.names = FALSE, stringsAsFactors = FALSE)

condition_levels <- c(
  "C0_baseline", "C1_shake", "C2_zoom", "C3_recoil",
  "C4_shake_zoom", "C5_shake_recoil", "C6_zoom_recoil", "C7_all"
)
condition_labels <- c("C0", "C1", "C2", "C3", "C4", "C5", "C6", "C7")
condition_axis_labels <- c(
  "C0\nBase", "C1\nShake", "C2\nZoom", "C3\nRecoil",
  "C4\nS+Z", "C5\nS+R", "C6\nZ+R", "C7\nAll"
)
condition_treatment_axis_labels <- c(
  "C0\nBase", "C1\nShake", "C2\nZoom", "C3\nRecoil",
  "C4\nShake\n+Zoom", "C5\nShake\n+Recoil", "C6\nZoom\n+Recoil", "C7\nAll"
)
condition_full_labels <- c(
  "C0 Base", "C1 Shake", "C2 Zoom", "C3 Recoil",
  "C4 Shake+Zoom", "C5 Shake+Recoil", "C6 Zoom+Recoil", "C7 All"
)

metric_labels <- c(
  duration_seconds = "Duración (s)",
  final_level = "Nivel final",
  xp_rate = "XP/s",
  damage_taken_rate = "Daño recibido/s",
  hits_rate = "Golpes recibidos/s",
  kill_rate = "Kills/s",
  input_rate = "Inputs/s",
  jitter_rate = "Cambios de dirección/s",
  distance_rate = "Distancia recorrida/s",
  speed_mean = "Velocidad promedio",
  low_hp_ratio = "Tiempo con HP bajo (%)",
  enemies_alive_mean = "Enemigos vivos",
  nearest_enemy_dist_mean = "Dist. enemigo",
  projectiles_alive_mean = "Proyectiles activos",
  fps_mean = "FPS promedio",
  fps_min = "FPS mínimo"
)

effect_labels <- c(
  camera_shake = "Shake",
  camera_zoom = "Zoom",
  camera_recoil = "Recoil",
  `camera_shake:camera_zoom` = "Shake × Zoom",
  `camera_shake:camera_recoil` = "Shake × Recoil",
  `camera_zoom:camera_recoil` = "Zoom × Recoil",
  `camera_shake:camera_zoom:camera_recoil` = "Shake × Zoom × Recoil"
)

scale_metric <- function(variable, values) {
  if (variable %in% c("low_hp_ratio", "fps_drop_ratio")) return(values * 100)
  values
}

is_nonnegative_metric <- function(variable) {
  variable %in% c(
    "duration_seconds", "final_round", "final_level", "damage_taken_rate",
    "hits_rate", "total_damage_taken", "kill_rate", "xp_rate", "jitter_rate",
    "input_rate", "distance_rate", "speed_mean", "enemies_alive_mean",
    "enemies_alive_max", "nearest_enemy_dist_mean", "projectiles_alive_mean",
    "low_hp_ratio", "fps_mean", "fps_min", "fps_drop_ratio"
  )
}

theme <- list(
  ink = "#212525",
  muted = "#677d64",
  border = "#485346",
  accent = "#7fee64",
  soft = "#def0dd",
  pale = "#f5fbf3",
  grid = "#e2eadf",
  warn = "#8aa378"
)

figure_names <- c(
  "01_condition_matrix.png" = "Figura · Diseño factorial",
  "02_giq_subscales.png" = "Figura · GIQ y subescalas",
  "03_giq_by_condition.png" = "Figura · GIQ por condición",
  "04_duration_by_condition.png" = "Figura · Supervivencia",
  "05_kill_rate_by_condition.png" = "Figura · Kills por segundo",
  "06_damage_rate_by_condition.png" = "Figura · Daño recibido",
  "07_input_rate_by_condition.png" = "Figura · Input del jugador",
  "08_distance_rate_by_condition.png" = "Figura · Distancia recorrida",
  "09_jitter_rate_by_condition.png" = "Figura · Jitter de control",
  "10_enemy_distance_by_condition.png" = "Figura · Presión espacial",
  "12_factorial_anova_effect_map.png" = "Figura · ANOVA factorial: mapa de efectos",
  "12_factorial_anova_boxplots.png" = "Figura · ANOVA factorial",
  "12a_anova_kill_rate_shake_zoom.png" = "Figura · ANOVA factorial: Kills/s por Shake×Zoom",
  "12b_anova_kill_rate_shake_recoil.png" = "Figura · ANOVA factorial: Kills/s por Shake×Recoil",
  "12c_anova_input_rate_shake_zoom.png" = "Figura · ANOVA factorial: Inputs/s por Shake×Zoom",
  "12d_anova_input_rate_shake_recoil.png" = "Figura · ANOVA factorial: Inputs/s por Shake×Recoil",
  "13_dunnett_significant_vs_c0.png" = "Figura · Dunnett vs C0",
  "14_immersion_control_matrix.png" = "Figura · Matriz inmersión-control",
  "15_interaction_giq_total.png" = "Figura · Interacción: GIQ total",
  "16_interaction_survival_time.png" = "Figura · Interacción: supervivencia",
  "17_interaction_damage_rate.png" = "Figura · Interacción: daño recibido",
  "18_interaction_jitter_rate.png" = "Figura · Interacción: jitter",
  "19_forest_control_index_vs_c0.png" = "Figura · Efectos vs C0",
  "20_tradeoff_giq_survival.png" = "Figura · Trade-off: GIQ y supervivencia",
  "21_tradeoff_giq_damage.png" = "Figura · Trade-off: GIQ y daño",
  "22_correlation_heatmap.png" = "Figura · Correlaciones",
  "23_fps_technical_control.png" = "Figura · Control técnico FPS"
)
current_figure_label <- ""

open_png <- function(filename, width = FIG_WIDTH, height = FIG_HEIGHT, res = FIG_RES) {
  current_figure_label <<- if (filename %in% names(figure_names)) {
    figure_names[[filename]]
  } else {
    sub("\\.png$", "", gsub("_", " ", basename(filename)))
  }
  png(file.path(output_dir, filename), width = width, height = height, res = res)
  par(
    bg = "white",
    fg = theme$ink,
    col.axis = theme$ink,
    col.lab = theme$ink,
    col.main = theme$ink,
    family = "sans",
    mar = c(5.2, 5.2, 4.2, 2.2),
    xaxs = "i",
    yaxs = "i"
  )
}

close_png <- function() {
  dev.off()
}

draw_title <- function(title, subtitle = NULL) {
  figure_title <- if (nzchar(current_figure_label)) current_figure_label else title
  title(figure_title, adj = 0, cex.main = 1.45, font.main = 2, line = 1.75)
  if (!is.null(subtitle)) {
    mtext(subtitle, side = 3, adj = 0, line = 0.55, cex = 0.98, col = theme$border)
  }
}

format_p <- function(p) {
  ifelse(p < 0.001, "p<.001", sprintf("p=%.3f", p))
}

draw_point_label <- function(x, y, labels, cex = 0.82, font = 2, col = theme$ink, adj = c(0.5, 0.5)) {
  for (i in seq_along(labels)) {
    if (!is.finite(x[i]) || !is.finite(y[i]) || is.na(labels[i])) next
    text(x[i], y[i], labels[i], cex = cex, font = font, col = col, adj = adj, xpd = NA)
  }
}

draw_violin <- function(x, values, width = 0.36, fill = theme$pale, border = theme$border) {
  values <- values[is.finite(values)]
  if (length(values) < 2 || length(unique(values)) < 2) {
    segments(x - width * 0.5, values[1], x + width * 0.5, values[1], col = border, lwd = 1.4)
    return(invisible(NULL))
  }
  dens <- density(values, na.rm = TRUE, n = 160)
  keep <- dens$x >= min(values, na.rm = TRUE) & dens$x <= max(values, na.rm = TRUE)
  dens$x <- dens$x[keep]
  dens$y <- dens$y[keep]
  scaled <- dens$y / max(dens$y, na.rm = TRUE) * width
  polygon(
    c(x - scaled, rev(x + scaled)),
    c(dens$x, rev(dens$x)),
    col = fill,
    border = border,
    lwd = 1.4
  )
}

draw_violin_set <- function(groups, values, group_levels, width = 0.36) {
  for (i in seq_along(group_levels)) {
    draw_violin(i, values[groups == group_levels[i]], width = width)
  }
}

draw_violin_summary <- function(x, values, mean_value, ci_low, ci_high, box_width = 0.16) {
  values <- values[is.finite(values)]
  if (length(values) == 0) return(invisible(NULL))
  qs <- as.numeric(quantile(values, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE))
  segments(x, ci_low, x, ci_high, col = theme$ink, lwd = 2.6)
  rect(x - box_width / 2, qs[1], x + box_width / 2, qs[3], col = theme$ink, border = theme$ink, lwd = 1.2)
  points(x, qs[2], pch = 21, bg = theme$warn, col = "white", cex = 1.45, lwd = 1.1)
}

draw_violin_summaries <- function(groups, values, group_levels, means, lo, hi, box_width = 0.16) {
  for (i in seq_along(group_levels)) {
    draw_violin_summary(i, values[groups == group_levels[i]], means[i], lo[i], hi[i], box_width = box_width)
  }
}

draw_boxplot_set <- function(groups, values, group_levels, width = 0.46, outline = FALSE) {
  df <- data.frame(
    group = factor(groups, levels = group_levels),
    value = as.numeric(values)
  )
  df <- df[is.finite(df$value) & !is.na(df$group), ]
  if (nrow(df) == 0) return(invisible(NULL))

  boxplot(
    value ~ group,
    data = df,
    at = seq_along(group_levels),
    add = TRUE,
    axes = FALSE,
    ann = FALSE,
    outline = outline,
    boxwex = width,
    col = theme$pale,
    border = theme$border,
    staplewex = 0.45,
    whisklty = 1,
    medcol = theme$ink,
    medlwd = 2.0,
    lwd = 1.35
  )
}

draw_boxplot_summaries <- function(groups, values, group_levels, means, lo, hi, marker = c("mean", "median")) {
  marker <- match.arg(marker)
  for (i in seq_along(group_levels)) {
    x <- values[groups == group_levels[i]]
    x <- x[is.finite(x)]
    if (length(x) == 0) next
    marker_value <- if (marker == "mean") means[i] else median(x, na.rm = TRUE)
    segments(i, lo[i], i, hi[i], col = theme$ink, lwd = 2.4)
    points(i, marker_value, pch = 21, bg = theme$accent, col = theme$ink, cex = 1.65, lwd = 1.2)
  }
}

draw_jittered_points <- function(groups, values, group_levels, cex = 0.52) {
  df <- data.frame(
    group = factor(groups, levels = group_levels),
    value = as.numeric(values)
  )
  df <- df[is.finite(df$value) & !is.na(df$group), ]
  if (nrow(df) == 0) return(invisible(NULL))
  stripchart(
    value ~ group,
    data = df,
    vertical = TRUE,
    method = "jitter",
    jitter = 0.10,
    pch = 16,
    cex = cex,
    col = adjustcolor(theme$muted, alpha.f = 0.42),
    add = TRUE
  )
}

trimmed_ylim <- function(values, lo, hi, probs = c(0.02, 0.98), min_zero = FALSE) {
  q <- as.numeric(quantile(values[is.finite(values)], probs = probs, na.rm = TRUE, names = FALSE))
  ylim <- range(c(q, lo, hi), na.rm = TRUE)
  pad <- diff(ylim) * 0.14
  if (!is.finite(pad) || pad == 0) pad <- max(abs(ylim), na.rm = TRUE) * 0.08 + 0.1
  ylim <- ylim + c(-pad, pad)
  if (min_zero) ylim[1] <- max(0, ylim[1])
  ylim
}

remove_group_outliers <- function(df, group_col, value_col) {
  keep <- rep(FALSE, nrow(df))
  for (g in unique(df[[group_col]])) {
    idx <- which(df[[group_col]] == g & is.finite(df[[value_col]]))
    x <- df[[value_col]][idx]
    if (length(x) < 4) {
      keep[idx] <- TRUE
      next
    }
    qs <- as.numeric(quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE))
    iqr <- qs[2] - qs[1]
    if (!is.finite(iqr) || iqr == 0) {
      keep[idx] <- x == qs[1]
      next
    }
    lo <- qs[1] - 1.5 * iqr
    hi <- qs[2] + 1.5 * iqr
    keep[idx] <- x >= lo & x <= hi
  }
  df[keep, ]
}

condition_interval_plot <- function(variable, filename, title, subtitle = NULL) {
  df <- run_level[is.finite(run_level[[variable]]), ]
  df$condition <- factor(df$condition, levels = condition_levels)
  df <- df[!is.na(df$condition), ]
  df$value <- scale_metric(variable, df[[variable]])
  groups <- condition_levels

  means <- sapply(groups, function(g) mean(df$value[df$condition == g], na.rm = TRUE))
  ses <- sapply(groups, function(g) {
    x <- df$value[df$condition == g]
    sd(x, na.rm = TRUE) / sqrt(sum(is.finite(x)))
  })
  lo <- means - 1.96 * ses
  hi <- means + 1.96 * ses
  ylim <- trimmed_ylim(df$value, lo, hi, min_zero = is_nonnegative_metric(variable))
  if (variable == "low_hp_ratio") ylim[2] <- max(1, ylim[2])
  if (variable == "fps_drop_ratio") ylim[2] <- max(5, ylim[2])

  open_png(filename)
  par(mar = c(6.0, 5.6, 5.6, 2.2), xaxs = "i", yaxs = "r")
  plot(
    seq_along(groups), means,
    type = "n",
    xaxt = "n",
    xlab = "",
    ylab = metric_labels[[variable]],
    xlim = range(seq_along(groups)) + c(-0.55, 0.55),
    ylim = ylim,
    cex.axis = 0.78,
    cex.lab = 0.92
  )
  axis(1, at = seq_along(condition_treatment_axis_labels), labels = condition_treatment_axis_labels, tick = FALSE, cex.axis = 0.68, line = 0.2)
  abline(h = pretty(ylim), col = theme$grid, lwd = 1)
  draw_boxplot_set(df$condition, df$value, groups, width = 0.50)
  draw_jittered_points(df$condition, df$value, groups)
  draw_boxplot_summaries(df$condition, df$value, groups, means, lo, hi, marker = "mean")
  draw_title(title, subtitle)
  box(col = theme$grid)
  close_png()
}

giq_interval_plot <- function(giq) {
  score_cols <- c("giq_total", "engagement", "engrossment", "total_immersion")
  labels <- c("GIQ\ntotal", "Engagement", "Engrossment", "Inmersión\ntotal")
  long <- stack(giq[, score_cols])
  long$ind <- factor(long$ind, levels = score_cols, labels = labels)
  long$values <- as.numeric(long$values)
  long <- long[is.finite(long$values), ]
  means <- sapply(labels, function(v) mean(long$values[long$ind == v], na.rm = TRUE))
  ses <- sapply(labels, function(v) {
    x <- long$values[long$ind == v]
    sd(x, na.rm = TRUE) / sqrt(sum(is.finite(x)))
  })
  lo <- means - 1.96 * ses
  hi <- means + 1.96 * ses

  open_png("02_giq_subscales.png")
  par(mar = c(5.8, 5.6, 5.6, 2.2), xaxs = "i", yaxs = "i")
  plot(
    seq_along(labels), means,
    type = "n",
    xaxt = "n",
    xlab = "",
    ylab = "Promedio Likert (1-5)",
    xlim = range(seq_along(labels)) + c(-0.55, 0.55),
    ylim = c(1, 5),
    cex.lab = 0.92
  )
  axis(1, at = seq_along(labels), labels = labels, tick = FALSE, cex.axis = 0.88, line = 0.2)
  abline(h = 1:5, col = theme$grid)
  draw_boxplot_set(long$ind, long$values, labels, width = 0.50)
  draw_jittered_points(long$ind, long$values, labels)
  draw_boxplot_summaries(long$ind, long$values, labels, means, lo, hi, marker = "mean")
  draw_title("La encuesta resume la inmersión reportada", paste0("n = ", nrow(giq), "; punto verde = media, barra negra = IC 95%"))
  box(col = theme$grid)
  close_png()
}

giq_condition_plot <- function(giq) {
  df <- giq[is.finite(giq$giq_total) & !is.na(giq$condition), ]
  df$condition <- factor(df$condition, levels = condition_levels)
  groups <- condition_levels
  means <- sapply(groups, function(g) mean(df$giq_total[df$condition == g], na.rm = TRUE))
  ses <- sapply(groups, function(g) {
    x <- df$giq_total[df$condition == g]
    sd(x, na.rm = TRUE) / sqrt(sum(is.finite(x)))
  })
  lo <- means - 1.96 * ses
  hi <- means + 1.96 * ses

  open_png("03_giq_by_condition.png")
  par(mar = c(6.0, 5.6, 5.6, 2.2), xaxs = "i", yaxs = "i")
  plot(
    seq_along(groups), means,
    type = "n",
    xaxt = "n",
    xlab = "",
    ylab = "GIQ total (1-5)",
    xlim = range(seq_along(groups)) + c(-0.55, 0.55),
    ylim = c(1, 5),
    cex.lab = 0.92
  )
  axis(1, at = seq_along(condition_treatment_axis_labels), labels = condition_treatment_axis_labels, tick = FALSE, cex.axis = 0.68, line = 0.2)
  abline(h = 1:5, col = theme$grid)
  draw_boxplot_set(df$condition, df$giq_total, groups, width = 0.50)
  draw_jittered_points(df$condition, df$giq_total, groups)
  draw_boxplot_summaries(df$condition, df$giq_total, groups, means, lo, hi, marker = "mean")
  draw_title("La inmersión puede compararse por condición", "GIQ total enlazado con condición por identificador; escala Likert completa")
  box(col = theme$grid)
  close_png()
}

errbar_plot <- function(metrics, filename, title, subtitle) {
  d <- descriptives[descriptives$variable %in% metrics, ]
  d$condition <- factor(d$condition, levels = condition_levels)
  d <- d[order(match(d$variable, metrics), d$condition), ]

  open_png(filename, height = 1200)
  layout(matrix(seq_along(metrics), nrow = 2, byrow = TRUE))
  old_mar <- par("mar")
  for (m in metrics) {
    par(mar = c(5.4, 5.4, 3.4, 1.2), xaxs = "i", yaxs = "i")
    dm <- d[d$variable == m, ]
    y <- scale_metric(m, dm$mean)
    lo <- scale_metric(m, dm$ci95_low)
    hi <- scale_metric(m, dm$ci95_high)
    if (is_nonnegative_metric(m)) {
      lo <- pmax(0, lo)
    }
    if (length(unique(round(c(y, lo, hi), 8))) <= 1) {
      plot(
        seq_along(y), y,
        type = "n",
        xaxt = "n",
        xlab = "",
        ylab = metric_labels[[m]],
        ylim = c(0, 1),
        main = metric_labels[[m]],
        cex.main = 1.05
      )
      axis(1, at = seq_along(condition_axis_labels), labels = condition_axis_labels, cex.axis = 0.72, tick = FALSE)
      abline(h = 0, col = theme$grid, lwd = 1)
      points(seq_along(y), rep(0, length(y)), pch = 21, bg = theme$accent, col = theme$ink, cex = 1.4, lwd = 1.2)
      text(mean(seq_along(y)), 0.55, "Sin variación registrada", cex = 0.95, col = theme$muted)
      box(col = theme$grid)
      next
    }
    ylim <- range(c(lo, hi), na.rm = TRUE)
    pad <- diff(ylim) * 0.22
    if (!is.finite(pad) || pad == 0) pad <- max(abs(ylim), na.rm = TRUE) * 0.1 + 0.1
    ylim <- c(ylim[1] - pad, ylim[2] + pad)
    if (is_nonnegative_metric(m)) ylim[1] <- 0
    if (m %in% c("low_hp_ratio", "fps_drop_ratio")) ylim[2] <- max(5, ylim[2])
    plot(
      seq_along(y), y,
      type = "n",
      xaxt = "n",
      xlab = "",
      ylab = metric_labels[[m]],
      ylim = ylim,
      main = metric_labels[[m]],
      cex.main = 1.05
    )
    abline(h = pretty(ylim), col = theme$grid, lwd = 1)
    segments(seq_along(y), lo, seq_along(y), hi, col = theme$border, lwd = 2)
    points(seq_along(y), y, pch = 21, bg = theme$accent, col = theme$ink, cex = 1.7, lwd = 1.2)
    axis(1, at = seq_along(condition_axis_labels), labels = condition_axis_labels, cex.axis = 0.72, tick = FALSE)
    box(col = theme$grid)
  }
  par(mar = old_mar)
  mtext(title, side = 3, outer = TRUE, line = -1.2, adj = 0.02, font = 2, cex = 1.3)
  mtext(subtitle, side = 3, outer = TRUE, line = -2.8, adj = 0.02, col = theme$muted, cex = 0.85)
  close_png()
}

condition_matrix <- function() {
  open_png("01_condition_matrix.png")
  par(mar = c(3.4, 3.0, 5.6, 1.2), bty = "n")
  plot.new()
  plot.window(xlim = c(0.15, 7.65), ylim = c(0.15, 3.85))
  draw_title("Ocho condiciones separan cada efecto", "Diseño factorial 2x2x2: shake, zoom y recoil")
  headers <- c("Cond.", "Shake", "Zoom", "Recoil")
  x <- c(0.95, 2.75, 4.65, 6.55)
  y_header <- 3.35
  for (i in seq_along(headers)) text(x[i], y_header, headers[i], font = 2, cex = 1.28)
  vals <- data.frame(
    cond = condition_full_labels,
    shake = c(0, 1, 0, 0, 1, 1, 0, 1),
    zoom = c(0, 0, 1, 0, 1, 0, 1, 1),
    recoil = c(0, 0, 0, 1, 0, 1, 1, 1)
  )
  y <- seq(2.95, 0.45, length.out = 8)
  for (r in seq_len(nrow(vals))) {
    rect(0.28, y[r] - 0.18, 7.35, y[r] + 0.18, col = if (r %% 2 == 0) "white" else theme$pale, border = NA)
    text(x[1], y[r], vals$cond[r], font = 2, cex = 1.02)
    for (c in 2:4) {
      active <- vals[r, c] == 1
      symbols(x[c], y[r], circles = 0.125, inches = FALSE, add = TRUE, bg = if (active) theme$accent else "white", fg = theme$ink, lwd = 1.6)
      text(x[c], y[r], if (active) "Sí" else "No", cex = 0.88)
    }
  }
  box(col = NA)
  close_png()
}

condition_from_flags <- function(shake, zoom, recoil) {
  if (!shake && !zoom && !recoil) return("C0_baseline")
  if ( shake && !zoom && !recoil) return("C1_shake")
  if (!shake &&  zoom && !recoil) return("C2_zoom")
  if (!shake && !zoom &&  recoil) return("C3_recoil")
  if ( shake &&  zoom && !recoil) return("C4_shake_zoom")
  if ( shake && !zoom &&  recoil) return("C5_shake_recoil")
  if (!shake &&  zoom &&  recoil) return("C6_zoom_recoil")
  "C7_all"
}

as_bool <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "si", "sí")
}

build_giq <- function() {
  giq <- form[, 38:61]
  giq[] <- lapply(giq, function(x) suppressWarnings(as.numeric(x)))
  out <- data.frame(
    player_id = trimws(toupper(form[[3]])),
    giq_total = rowMeans(giq, na.rm = TRUE),
    engagement = rowMeans(giq[, 1:9], na.rm = TRUE),
    engrossment = rowMeans(giq[, 10:16], na.rm = TRUE),
    total_immersion = rowMeans(giq[, 17:24], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  cfg2 <- cfg
  cfg2$id <- trimws(toupper(cfg2$id))
  cfg2$camera_shake <- as_bool(cfg2$camera_shake)
  cfg2$camera_zoom <- as_bool(cfg2$camera_zoom)
  cfg2$camera_recoil <- as_bool(cfg2$camera_recoil)
  cfg2$condition <- mapply(condition_from_flags, cfg2$camera_shake, cfg2$camera_zoom, cfg2$camera_recoil)
  out <- merge(
    out,
    cfg2[, c("id", "condition", "camera_shake", "camera_zoom", "camera_recoil")],
    by.x = "player_id",
    by.y = "id",
    all.x = TRUE
  )
  out$condition <- factor(out$condition, levels = condition_levels)
  out
}

giq_figure <- function() {
  giq <- build_giq()
  score_cols <- c("giq_total", "engagement", "engrossment", "total_immersion")
  labels <- c("GIQ\ntotal", "Engagement", "Engrossment", "Total\nimmersion")
  means <- sapply(score_cols, function(v) mean(giq[[v]], na.rm = TRUE))
  ses <- sapply(score_cols, function(v) sd(giq[[v]], na.rm = TRUE) / sqrt(sum(is.finite(giq[[v]]))))
  open_png("02_giq_subscales.png", width = 1600, height = 1000)
  bp <- barplot(
    means,
    names.arg = labels,
    ylim = c(1, 5),
    col = c(theme$accent, theme$pale, theme$pale, theme$pale),
    border = theme$ink,
    ylab = "Promedio Likert (1-5)",
    las = 1,
    cex.names = 0.8
  )
  abline(h = 1:5, col = theme$grid)
  arrows(bp, means - 1.96 * ses, bp, means + 1.96 * ses, angle = 90, code = 3, length = 0.05, col = theme$ink, lwd = 1.8)
  text(bp, means + 0.22, sprintf("%.2f", means), cex = 0.95)
  draw_title("La encuesta resume la inmersión reportada", paste0("n = ", nrow(giq), " respuestas; barras con IC95 aproximado"))
  close_png()

  agg <- aggregate(
    giq_total ~ condition,
    data = giq,
    FUN = function(x) c(mean = mean(x, na.rm = TRUE), se = sd(x, na.rm = TRUE) / sqrt(sum(is.finite(x))), n = sum(is.finite(x)))
  )
  vals <- data.frame(condition = agg$condition, agg$giq_total)
  open_png("03_giq_by_condition.png", width = 1700, height = 1000)
  y <- vals$mean
  lo <- y - 1.96 * vals$se
  hi <- y + 1.96 * vals$se
  ylim <- range(c(lo, hi), na.rm = TRUE)
  ylim <- c(max(1, ylim[1] - 0.25), min(5, ylim[2] + 0.25))
  plot(seq_along(y), y, type = "n", xaxt = "n", xlab = "", ylab = "GIQ total (1-5)", ylim = ylim, xlim = c(0.6, 8.4))
  abline(h = pretty(ylim), col = theme$grid)
  segments(seq_along(y), lo, seq_along(y), hi, col = theme$border, lwd = 2)
  points(seq_along(y), y, pch = 21, bg = theme$accent, col = theme$ink, cex = 1.9, lwd = 1.2)
  axis(1, at = seq_along(condition_axis_labels), labels = condition_axis_labels, cex.axis = 0.78, tick = FALSE)
  text(seq_along(y), pmin(ylim[2] - 0.03, y + 0.12), paste0("n=", vals$n), cex = 0.68, col = theme$muted)
  draw_title("La inmersión puede compararse por condición", "GIQ total enlazado por identificador; eje ajustado al rango observado")
  box(col = theme$grid)
  close_png()

  invisible(giq)
}

effect_group <- function(df, effect) {
  parts <- strsplit(effect, ":", fixed = TRUE)[[1]]
  short <- c(camera_shake = "Shake", camera_zoom = "Zoom", camera_recoil = "Recoil")
  flags <- lapply(parts, function(p) as_bool(df[[p]]))

  if (length(parts) == 1) {
    label <- ifelse(flags[[1]], short[[parts[1]]], paste("Sin", short[[parts[1]]]))
    levels <- c(paste("Sin", short[[parts[1]]]), short[[parts[1]]])
    return(factor(label, levels = levels))
  }

  label <- paste(
    ifelse(flags[[1]], short[[parts[1]]], paste("Sin", short[[parts[1]]])),
    ifelse(flags[[2]], short[[parts[2]]], paste("Sin", short[[parts[2]]])),
    sep = "\n"
  )
  levels <- c(
    paste(paste("Sin", short[[parts[1]]]), paste("Sin", short[[parts[2]]]), sep = "\n"),
    paste(short[[parts[1]]], paste("Sin", short[[parts[2]]]), sep = "\n"),
    paste(paste("Sin", short[[parts[1]]]), short[[parts[2]]], sep = "\n"),
    paste(short[[parts[1]]], short[[parts[2]]], sep = "\n")
  )
  factor(label, levels = levels)
}

draw_box_ci_panel <- function(variable) {
  df <- run_level[is.finite(run_level[[variable]]), ]
  df$condition <- factor(df$condition, levels = condition_levels)
  df <- df[!is.na(df$condition), ]
  df$value <- scale_metric(variable, df[[variable]])
  groups <- condition_levels

  bp <- boxplot(value ~ condition, data = df, plot = FALSE)
  means <- sapply(groups, function(g) mean(df$value[df$condition == g], na.rm = TRUE))
  ses <- sapply(groups, function(g) {
    x <- df$value[df$condition == g]
    sd(x, na.rm = TRUE) / sqrt(sum(is.finite(x)))
  })
  lo <- means - 1.96 * ses
  hi <- means + 1.96 * ses

  ylim <- range(c(bp$stats, means, lo, hi), na.rm = TRUE)
  pad <- diff(ylim) * 0.16
  if (!is.finite(pad) || pad == 0) pad <- max(abs(ylim), na.rm = TRUE) * 0.08 + 0.1
  ylim <- ylim + c(-pad, pad)
  if (is_nonnegative_metric(variable)) ylim[1] <- max(0, ylim[1])

  boxplot(
    value ~ condition,
    data = df,
    ylim = ylim,
    outline = FALSE,
    col = theme$pale,
    border = theme$border,
    ylab = metric_labels[[variable]],
    xlab = "",
    main = "",
    xaxt = "n",
    cex.main = 1.05,
    cex.axis = 0.72,
    cex.lab = 0.84,
    lwd = 1.4
  )
  axis(1, at = seq_along(condition_treatment_axis_labels), labels = condition_treatment_axis_labels, tick = FALSE, cex.axis = 0.66, line = 0.2)
  abline(h = pretty(ylim), col = theme$grid, lwd = 1)
  stripchart(
    value ~ condition,
    data = df,
    vertical = TRUE,
    method = "jitter",
    jitter = 0.11,
    pch = 16,
    cex = 0.58,
    col = adjustcolor(theme$muted, alpha.f = 0.48),
    add = TRUE
  )

  x <- seq_along(groups)
  segments(x, lo, x, hi, col = theme$ink, lwd = 2.6)
  points(x, means, pch = 21, bg = theme$accent, col = theme$ink, cex = 1.75, lwd = 1.25)
  box(col = theme$grid)
}

draw_factor_effect_box_ci_panel <- function(variable, effect) {
  df <- run_level[is.finite(run_level[[variable]]), ]
  df$value <- scale_metric(variable, df[[variable]])
  df$effect_group <- effect_group(df, effect)
  df <- df[!is.na(df$effect_group), ]
  groups <- levels(df$effect_group)

  bp <- boxplot(value ~ effect_group, data = df, plot = FALSE, outline = FALSE)
  means <- sapply(groups, function(g) mean(df$value[df$effect_group == g], na.rm = TRUE))
  ses <- sapply(groups, function(g) {
    x <- df$value[df$effect_group == g]
    sd(x, na.rm = TRUE) / sqrt(sum(is.finite(x)))
  })
  lo <- means - 1.96 * ses
  hi <- means + 1.96 * ses

  ylim <- range(c(bp$stats, means, lo, hi), na.rm = TRUE)
  pad <- diff(ylim) * 0.18
  if (!is.finite(pad) || pad == 0) pad <- max(abs(ylim), na.rm = TRUE) * 0.08 + 0.1
  ylim <- ylim + c(-pad, pad)
  if (is_nonnegative_metric(variable)) ylim[1] <- max(0, ylim[1])

  boxplot(
    value ~ effect_group,
    data = df,
    ylim = ylim,
    outline = FALSE,
    col = theme$pale,
    border = theme$border,
    ylab = metric_labels[[variable]],
    xlab = "",
    main = "",
    xaxt = "n",
    cex.axis = 0.78,
    cex.lab = 0.90,
    boxwex = 0.52,
    lwd = 1.4,
    medcol = theme$ink,
    medlwd = 2.0
  )
  axis(1, at = seq_along(groups), labels = groups, tick = FALSE, cex.axis = 0.78, line = 0.2)
  abline(h = pretty(ylim), col = theme$grid, lwd = 1)
  stripchart(
    value ~ effect_group,
    data = df,
    vertical = TRUE,
    method = "jitter",
    jitter = 0.10,
    pch = 16,
    cex = 0.54,
    col = adjustcolor(theme$muted, alpha.f = 0.42),
    add = TRUE
  )
  x <- seq_along(groups)
  segments(x, lo, x, hi, col = theme$ink, lwd = 2.5)
  points(x, means, pch = 21, bg = theme$accent, col = theme$ink, cex = 1.65, lwd = 1.2)
  box(col = theme$grid)
}

factorial_anova_boxplot_figure <- function() {
  sig <- anova[
    is.finite(anova$p_value) &
      anova$p_value < 0.05 &
      anova$effect != "Residuals" &
      anova$variable %in% c("kill_rate", "input_rate"),
  ]
  sig <- sig[order(sig$variable, sig$partial_eta_squared, decreasing = TRUE), ]
  write.csv(
    sig,
    file.path("LaTeX", "listings", "anova_significant_effects_ranked.csv"),
    row.names = FALSE,
    na = ""
  )
  old_files <- c(
    "12_factorial_anova_boxplots.png",
    "12a_anova_xp_rate.png",
    "12b_anova_input_rate.png",
    "12c_anova_final_level.png",
    "12d_anova_distance_rate.png"
  )
  unlink(file.path(output_dir, old_files))

  panels <- data.frame(
    filename = c(
      "12a_anova_kill_rate_shake_zoom.png",
      "12b_anova_kill_rate_shake_recoil.png",
      "12c_anova_input_rate_shake_zoom.png",
      "12d_anova_input_rate_shake_recoil.png"
    ),
    variable = c("kill_rate", "kill_rate", "input_rate", "input_rate"),
    effect = c(
      "camera_shake:camera_zoom",
      "camera_shake:camera_recoil",
      "camera_shake:camera_zoom",
      "camera_shake:camera_recoil"
    ),
    subtitle = c(
      "Boxplot por combinaciones Shake×Zoom; punto verde = media, barra negra = IC 95%",
      "Boxplot por combinaciones Shake×Recoil; punto verde = media, barra negra = IC 95%",
      "Boxplot por combinaciones Shake×Zoom; punto verde = media, barra negra = IC 95%",
      "Boxplot por combinaciones Shake×Recoil; punto verde = media, barra negra = IC 95%"
    ),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(panels))) {
    open_png(panels$filename[i])
    par(mar = c(5.8, 5.6, 5.0, 2.2), xaxs = "i", yaxs = "r")
    draw_factor_effect_box_ci_panel(panels$variable[i], panels$effect[i])
    draw_title(
      metric_labels[[panels$variable[i]]],
      panels$subtitle[i]
    )
    close_png()
  }
}

safe_bool_factor <- function(x) {
  factor(
    as_bool(x),
    levels = c(FALSE, TRUE),
    labels = c("Ausente", "Presente")
  )
}

partial_eta_squared_value <- function(ss_effect, ss_residual) {
  ss_effect / (ss_effect + ss_residual)
}

run_factorial_anova_one <- function(df, outcome, outcome_label) {
  required <- c(outcome, "camera_shake", "camera_zoom", "camera_recoil")
  if (!all(required %in% names(df))) return(NULL)

  df <- df[
    is.finite(df[[outcome]]) &
      !is.na(df$camera_shake) &
      !is.na(df$camera_zoom) &
      !is.na(df$camera_recoil),
  ]
  if (nrow(df) < 8) return(NULL)

  df$Shake <- safe_bool_factor(df$camera_shake)
  df$Zoom <- safe_bool_factor(df$camera_zoom)
  df$Recoil <- safe_bool_factor(df$camera_recoil)
  df$Y <- as.numeric(df[[outcome]])

  if (length(unique(df$Shake)) < 2 || length(unique(df$Zoom)) < 2 || length(unique(df$Recoil)) < 2) {
    return(NULL)
  }

  fit <- aov(Y ~ Shake * Zoom * Recoil, data = df)
  tab <- as.data.frame(summary(fit)[[1]])
  tab$effect <- trimws(rownames(tab))
  rownames(tab) <- NULL

  residual_ss <- tab$`Sum Sq`[tab$effect == "Residuals"]
  residual_df <- tab$Df[tab$effect == "Residuals"]
  tab <- tab[tab$effect != "Residuals", ]
  if (nrow(tab) == 0 || length(residual_ss) == 0) return(NULL)

  data.frame(
    variable = outcome,
    variable_label = outcome_label,
    effect = tab$effect,
    df_effect = tab$Df,
    df_residual = residual_df,
    F_value = tab$`F value`,
    p_value = tab$`Pr(>F)`,
    partial_eta_squared = partial_eta_squared_value(tab$`Sum Sq`, residual_ss),
    stringsAsFactors = FALSE
  )
}

build_factorial_anova_research_summary <- function(giq) {
  metric_map <- c(
    giq_total = "GIQ total",
    duration_seconds = "Supervivencia",
    kill_rate = "Kills/s",
    damage_taken_rate = "Daño recibido/s",
    input_rate = "Inputs/s",
    jitter_rate = "Jitter de control",
    low_hp_ratio = "HP bajo (%)",
    fps_mean = "FPS promedio"
  )

  results <- list()
  if ("giq_total" %in% names(giq)) {
    results[["giq_total"]] <- run_factorial_anova_one(giq, "giq_total", metric_map[["giq_total"]])
  }

  for (v in setdiff(names(metric_map), "giq_total")) {
    if (!v %in% names(run_level)) next
    temp <- run_level
    if (v == "low_hp_ratio") temp[[v]] <- temp[[v]] * 100
    results[[v]] <- run_factorial_anova_one(temp, v, metric_map[[v]])
  }

  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) return(NULL)

  out <- do.call(rbind, results)
  out$q_value <- p.adjust(out$p_value, method = "BH")
  out$effect_label <- gsub(":", " × ", out$effect)
  out$significance <- ifelse(
    out$p_value < 0.001, "***",
    ifelse(out$p_value < 0.01, "**",
      ifelse(out$p_value < 0.05, "*", "")
    )
  )
  out
}

plot_factorial_anova_effect_map <- function(giq) {
  anova_research <- build_factorial_anova_research_summary(giq)
  if (is.null(anova_research) || nrow(anova_research) == 0) return(invisible(NULL))

  write.csv(
    anova_research,
    file.path("LaTeX", "listings", "factorial_anova_research_summary.csv"),
    row.names = FALSE,
    na = ""
  )

  effect_order <- c(
    "Shake",
    "Zoom",
    "Recoil",
    "Shake:Zoom",
    "Shake:Recoil",
    "Zoom:Recoil",
    "Shake:Zoom:Recoil"
  )
  effect_labels_plot <- c("Shake", "Zoom", "Recoil", "S×Z", "S×R", "Z×R", "S×Z×R")
  variable_order <- unique(anova_research$variable_label)

  mat <- matrix(
    NA_real_,
    nrow = length(variable_order),
    ncol = length(effect_order),
    dimnames = list(variable_order, effect_labels_plot)
  )
  stars <- matrix("", nrow = nrow(mat), ncol = ncol(mat), dimnames = dimnames(mat))
  ptext <- matrix("", nrow = nrow(mat), ncol = ncol(mat), dimnames = dimnames(mat))

  for (i in seq_len(nrow(anova_research))) {
    r <- match(anova_research$variable_label[i], variable_order)
    c <- match(anova_research$effect[i], effect_order)
    if (!is.na(r) && !is.na(c)) {
      mat[r, c] <- anova_research$partial_eta_squared[i]
      stars[r, c] <- anova_research$significance[i]
      ptext[r, c] <- ifelse(
        anova_research$p_value[i] < 0.001,
        "<.001",
        sprintf("%.3f", anova_research$p_value[i])
      )
    }
  }

  open_png("12_factorial_anova_effect_map.png")
  par(mar = c(7.8, 9.8, 5.8, 2.0), xaxs = "i", yaxs = "i")
  pal <- colorRampPalette(c("white", theme$pale, theme$warn, theme$border))(100)
  zmax <- max(mat, na.rm = TRUE)
  if (!is.finite(zmax) || zmax <= 0) zmax <- 0.01

  image(
    x = seq_len(ncol(mat)),
    y = seq_len(nrow(mat)),
    z = t(mat[nrow(mat):1, ]),
    col = pal,
    zlim = c(0, zmax),
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  axis(1, at = seq_len(ncol(mat)), labels = colnames(mat), tick = FALSE, cex.axis = 0.92)
  axis(2, at = seq_len(nrow(mat)), labels = rev(rownames(mat)), tick = FALSE, las = 1, cex.axis = 0.86)
  abline(v = seq(0.5, ncol(mat) + 0.5, by = 1), col = theme$grid, lwd = 1)
  abline(h = seq(0.5, nrow(mat) + 0.5, by = 1), col = theme$grid, lwd = 1)

  for (r in seq_len(nrow(mat))) {
    for (c in seq_len(ncol(mat))) {
      rr <- nrow(mat) - r + 1
      if (!is.finite(mat[r, c])) next
      label <- paste0("η²p=", sprintf("%.2f", mat[r, c]), "\n", "p=", ptext[r, c], stars[r, c])
      text(c, rr, label, cex = 0.70, font = ifelse(stars[r, c] != "", 2, 1), col = theme$ink)
    }
  }

  draw_title(
    "ANOVA factorial: mapa de efectos",
    "Color principal = η² parcial; texto dentro de celda = η² parcial y p-value"
  )
  par(xpd = NA)
  legend_x <- c(1.05, 2.65, 4.25, 5.85)
  legend_y <- -0.95
  legend_cols <- c("white", theme$pale, theme$warn, theme$border)
  legend_labels <- c("p ≥ .05", "p < .05", "p < .01", "p < .001")
  text(0.45, legend_y + 0.11, "Leyenda p", adj = 0, cex = 0.82, font = 2, col = theme$ink)
  for (i in seq_along(legend_x)) {
    rect(
      legend_x[i],
      legend_y,
      legend_x[i] + 0.38,
      legend_y + 0.24,
      col = legend_cols[i],
      border = theme$border,
      lwd = 1.1
    )
    text(legend_x[i] + 0.48, legend_y + 0.12, legend_labels[i], adj = 0, cex = 0.78, col = theme$ink)
  }
  par(xpd = FALSE)
  box(col = theme$grid)
  close_png()
}

dunnett_figure <- function() {
  if (is.null(dunnett) || nrow(dunnett) == 0) return(invisible(NULL))
  keep <- dunnett[dunnett$p.value < 0.05, ]
  keep <- keep[keep$variable %in% c("duration_seconds", "kill_rate", "input_rate", "jitter_rate", "damage_taken_rate", "xp_rate", "nearest_enemy_dist_mean"), ]
  keep <- keep[order(keep$p.value), ]
  if (nrow(keep) == 0) return(invisible(NULL))
  metric_sd <- sapply(keep$variable, function(v) sd(run_level[[v]], na.rm = TRUE))
  metric_sd[!is.finite(metric_sd) | metric_sd == 0] <- NA_real_
  keep$estimate_std <- keep$estimate / metric_sd
  keep$lo <- (keep$estimate - 1.96 * keep$SE) / metric_sd
  keep$hi <- (keep$estimate + 1.96 * keep$SE) / metric_sd
  contrast_condition <- gsub(" - C0_baseline", "", keep$contrast)
  contrast_condition <- condition_full_labels[match(contrast_condition, condition_levels)]
  metric_name <- metric_labels[keep$variable]
  metric_name[is.na(metric_name)] <- keep$variable[is.na(metric_name)]
  keep$label <- paste(contrast_condition, metric_name, sep = " · ")
  keep$axis_label <- keep$label
  keep$axis_label <- gsub("Zoom\\+Recoil", "Z+R", keep$axis_label)
  keep$axis_label <- gsub("Shake\\+Recoil", "S+R", keep$axis_label)
  keep$axis_label <- gsub("Dist\\. enemigo", "Dist. enem.", keep$axis_label)
  keep <- keep[is.finite(keep$estimate_std) & is.finite(keep$lo) & is.finite(keep$hi), ]
  keep$p_label <- format_p(keep$p.value)
  keep <- keep[order(keep$estimate_std), ]

  open_png("13_dunnett_significant_vs_c0.png")
  par(mar = c(7.4, 12.8, 5.6, 4.8), xaxs = "r", yaxs = "i")
  y <- rev(seq_len(nrow(keep)))
  xlim_core <- range(c(keep$lo, keep$hi, 0), na.rm = TRUE)
  pad <- diff(xlim_core) * 0.18
  if (!is.finite(pad) || pad == 0) pad <- 0.25
  xlim <- xlim_core + c(-pad, pad * 1.9)
  p_x <- xlim[2] - pad * 0.55

  plot(
    keep$estimate_std,
    y,
    type = "n",
    yaxt = "n",
    xlab = "Diferencia estandarizada vs C0",
    ylab = "",
    xlim = xlim,
    ylim = range(y) + c(-0.8, 0.9)
  )
  abline(v = pretty(xlim_core), col = theme$grid, lwd = 1)
  abline(v = 0, col = theme$ink, lwd = 1.7)
  abline(h = y, col = theme$grid, lwd = 0.8)
  axis(2, at = y, labels = keep$axis_label, tick = FALSE, las = 1, cex.axis = 0.86)
  segments(keep$lo, y, keep$hi, y, col = theme$ink, lwd = 2.8)
  points(keep$estimate_std, y, pch = 21, bg = theme$accent, col = theme$ink, cex = 1.85, lwd = 1.35)
  text(p_x, y, keep$p_label, adj = 0, cex = 0.82, col = theme$ink, xpd = NA)
  text(par("usr")[1], max(y) + 0.62, "Métrica / contraste", adj = 0, cex = 0.86, font = 2, col = theme$ink, xpd = NA)
  text(p_x, max(y) + 0.62, "p-value", adj = 0, cex = 0.86, font = 2, col = theme$ink, xpd = NA)
  mtext("Valores positivos indican mayor valor que C0.", side = 1, line = 5.2, adj = 1, cex = 0.86, col = theme$border)
  draw_title("Dunnett vs C0", "Forest plot horizontal; puntos = diferencia estandarizada, barras = IC 95%")
  box(col = theme$grid)
  close_png()
}

synthesis_figure <- function(giq) {
  perf <- aggregate(
    cbind(duration_seconds, kill_rate, damage_taken_rate, jitter_rate) ~ condition,
    data = run_level,
    FUN = mean,
    na.rm = TRUE
  )
  g <- aggregate(giq_total ~ condition, data = giq, FUN = mean, na.rm = TRUE)
  m <- merge(g, perf, by = "condition")
  z <- function(x) as.numeric(scale(x))
  m$control_index <- z(m$duration_seconds) + z(m$kill_rate) - z(m$damage_taken_rate) - z(m$jitter_rate)
  m$condition <- factor(m$condition, levels = condition_levels)
  m <- m[order(m$condition), ]
  treatment_names <- condition_full_labels[match(m$condition, condition_levels)]

  open_png("14_immersion_control_matrix.png")
  par(mar = c(7.0, 5.8, 5.0, 2.2), xaxs = "r", yaxs = "r")
  control_xlim <- range(m$control_index, na.rm = TRUE) + c(-0.45, 0.45)
  giq_ylim <- range(m$giq_total, na.rm = TRUE) + c(-0.18, 0.18)
  giq_ylim[1] <- max(1, giq_ylim[1])
  giq_ylim[2] <- min(5, giq_ylim[2])
  plot(
    m$control_index, m$giq_total,
    type = "n",
    xlab = "Índice relativo de desempeño/control",
    ylab = "GIQ total (1-5)",
    xlim = control_xlim,
    ylim = giq_ylim
  )
  abline(v = pretty(control_xlim), col = theme$grid, lwd = 1)
  abline(h = pretty(giq_ylim), col = theme$grid, lwd = 1)
  abline(v = 0, col = theme$ink, lwd = 1.5)
  abline(h = mean(m$giq_total, na.rm = TRUE), col = theme$border, lwd = 1.6, lty = 2)
  abline(v = mean(m$control_index, na.rm = TRUE), col = theme$border, lwd = 1.6, lty = 2)
  points(
    m$control_index, m$giq_total,
    pch = 21,
    bg = theme$accent,
    col = theme$ink,
    cex = 2.2,
    lwd = 1.25
  )
  label_offsets <- data.frame(
    condition = condition_levels,
    dy = c(0.08, 0.06, 0.06, 0.07, 0.06, 0.05, 0.05, 0.07),
    stringsAsFactors = FALSE
  )
  label_offsets <- label_offsets[match(as.character(m$condition), label_offsets$condition), ]
  text(
    m$control_index,
    m$giq_total + label_offsets$dy,
    treatment_names,
    cex = 0.76,
    font = 2,
    adj = c(0.5, 0),
    xpd = NA
  )
  draw_title(
    "Matriz inmersión-control",
    "Cada punto resume una condición experimental con inmersión subjetiva y desempeño/control relativo"
  )
  mtext(
    "Índice = z(duración) + z(kills/s) - z(daño/s) - z(jitter/s)",
    side = 1,
    adj = 0,
    line = 4.4,
    cex = 0.84,
    col = theme$border
  )
  mtext(
    "Valores > 0 indican mejor desempeño/control relativo entre condiciones; líneas punteadas = media de condiciones.",
    side = 1,
    adj = 1,
    line = 5.5,
    cex = 0.84,
    col = theme$border
  )
  box(col = theme$grid)
  close_png()
}

prepare_run_factors <- function(df) {
  df$camera_shake <- as_bool(df$camera_shake)
  df$camera_zoom <- as_bool(df$camera_zoom)
  df$camera_recoil <- as_bool(df$camera_recoil)
  df$condition <- factor(df$condition, levels = condition_levels)
  df
}

interaction_plot <- function(df, outcome, filename, ylab, subtitle) {
  df <- prepare_run_factors(df)
  df <- df[is.finite(df[[outcome]]) & !is.na(df$camera_shake) & !is.na(df$camera_zoom) & !is.na(df$camera_recoil), ]
  df$value <- scale_metric(outcome, df[[outcome]])
  if (nrow(df) == 0) return(invisible(NULL))

  agg <- aggregate(
    value ~ camera_recoil + camera_zoom + camera_shake,
    data = df,
    FUN = function(x) c(mean = mean(x, na.rm = TRUE), se = sd(x, na.rm = TRUE) / sqrt(sum(is.finite(x))), n = sum(is.finite(x)))
  )
  agg <- data.frame(agg[, 1:3], agg$value)
  names(agg)[4:6] <- c("mean", "se", "n")
  agg$lo <- agg$mean - 1.96 * agg$se
  agg$hi <- agg$mean + 1.96 * agg$se

  ylim <- range(c(agg$lo, agg$hi, df$value), na.rm = TRUE)
  pad <- diff(ylim) * 0.14
  if (!is.finite(pad) || pad == 0) pad <- max(abs(ylim), na.rm = TRUE) * 0.08 + 0.1
  ylim <- ylim + c(-pad, pad)
  if (is_nonnegative_metric(outcome)) ylim[1] <- max(0, ylim[1])
  if (outcome %in% c("giq_total")) ylim <- c(1, 5)

  open_png(filename)
  layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE), widths = c(1, 1), heights = c(1, 0.16))
  par(oma = c(0.4, 0, 4.5, 0))
  colors <- c(`FALSE` = theme$border, `TRUE` = theme$accent)
  pchs <- c(`FALSE` = 21, `TRUE` = 24)
  for (recoil in c(FALSE, TRUE)) {
    par(mar = c(5.0, 5.2, 3.2, 1.2), xaxs = "i", yaxs = "r")
    plot(
      c(1, 2), c(NA, NA),
      type = "n",
      xaxt = "n",
      xlab = "Shake",
      ylab = if (!recoil) ylab else "",
      xlim = c(0.75, 2.25),
      ylim = ylim
    )
    axis(1, at = c(1, 2), labels = c("Ausente", "Presente"), tick = FALSE)
    abline(h = pretty(ylim), col = theme$grid, lwd = 1)
    for (zoom in c(FALSE, TRUE)) {
      rows <- agg[agg$camera_recoil == recoil & agg$camera_zoom == zoom, ]
      rows <- rows[order(rows$camera_shake), ]
      x <- ifelse(rows$camera_shake, 2, 1)
      lines(x, rows$mean, col = colors[[as.character(zoom)]], lwd = 2.4)
      segments(x, rows$lo, x, rows$hi, col = colors[[as.character(zoom)]], lwd = 2)
      points(x, rows$mean, pch = pchs[[as.character(zoom)]], bg = colors[[as.character(zoom)]], col = theme$ink, cex = 1.8, lwd = 1.2)
    }
    title(if (recoil) "Displacement/Recoil presente" else "Displacement/Recoil ausente", cex.main = 1.0, font.main = 2)
    box(col = theme$grid)
  }
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend(
    "center",
    legend = c("Zoom ausente", "Zoom presente"),
    col = c(theme$border, theme$accent),
    pt.bg = c(theme$border, theme$accent),
    pch = c(21, 24),
    lwd = 2.4,
    bty = "n",
    cex = 0.95,
    horiz = TRUE
  )
  mtext(current_figure_label, side = 3, outer = TRUE, adj = 0.02, line = 2.35, font = 2, cex = 1.32)
  mtext(subtitle, side = 3, outer = TRUE, adj = 0.02, line = 1.2, cex = 0.86, col = theme$border)
  close_png()
}

merged_giq_runs <- function(giq) {
  r <- prepare_run_factors(run_level)
  g <- giq[, c("player_id", "condition", "giq_total")]
  names(g)[2] <- "giq_condition"
  m <- merge(r, g, by = "player_id", all.x = FALSE)
  m <- m[as.character(m$condition) == as.character(m$giq_condition), ]
  m
}

forest_control_index_vs_c0 <- function() {
  df <- prepare_run_factors(run_level)
  z <- function(x) as.numeric(scale(x))
  df$control_index <- z(df$duration_seconds) + z(df$kill_rate) - z(df$damage_taken_rate) - z(df$jitter_rate)
  base <- df$control_index[df$condition == "C0_baseline"]
  out <- data.frame()
  for (cond in condition_levels[-1]) {
    x <- df$control_index[df$condition == cond]
    estimate <- mean(x, na.rm = TRUE) - mean(base, na.rm = TRUE)
    se <- sqrt(var(x, na.rm = TRUE) / sum(is.finite(x)) + var(base, na.rm = TRUE) / sum(is.finite(base)))
    out <- rbind(out, data.frame(condition = cond, estimate = estimate, lo = estimate - 1.96 * se, hi = estimate + 1.96 * se))
  }
  out$label <- condition_full_labels[match(out$condition, condition_levels)]
  out <- out[order(out$estimate), ]
  y <- seq_len(nrow(out))
  xlim <- range(c(out$lo, out$hi, 0), na.rm = TRUE)
  xlim <- xlim + c(-0.18, 0.18)

  open_png("19_forest_control_index_vs_c0.png")
  par(mar = c(5.6, 9.8, 5.6, 2.2), xaxs = "r", yaxs = "i")
  plot(out$estimate, y, type = "n", yaxt = "n", xlab = "Diferencia del índice desempeño/control vs C0", ylab = "", xlim = xlim, ylim = range(y) + c(-0.55, 0.8))
  abline(v = 0, col = theme$ink, lwd = 1.5)
  abline(v = pretty(xlim), col = theme$grid, lwd = 1)
  segments(out$lo, y, out$hi, y, col = theme$ink, lwd = 2.8)
  points(out$estimate, y, pch = 21, bg = theme$accent, col = theme$ink, cex = 1.9, lwd = 1.35)
  axis(2, at = y, labels = out$label, tick = FALSE, las = 1, cex.axis = 0.9)
  draw_title("Efectos estimados contra C0", "Una línea por condición; índice = z(duración) + z(kills/s) - z(daño/s) - z(jitter/s)")
  box(col = theme$grid)
  close_png()
}

tradeoff_scatter <- function(merged, yvar, filename, ylab, subtitle) {
  df <- merged[is.finite(merged$giq_total) & is.finite(merged[[yvar]]), ]
  df$y <- scale_metric(yvar, df[[yvar]])
  df$condition <- factor(df$condition, levels = condition_levels)
  cols <- c("#212525", "#4b6f44", "#7fee64", "#9aa85d", "#5f8f88", "#b7c36f", "#8aa378", "#677d64")
  pchs <- c(21, 22, 24, 25, 21, 22, 24, 25)
  open_png(filename)
  par(mar = c(8.0, 5.8, 5.6, 2.2), xaxs = "r", yaxs = "r")
  plot(df$giq_total, df$y, type = "n", xlab = "GIQ total (1-5)", ylab = ylab)
  abline(v = pretty(range(df$giq_total, na.rm = TRUE)), col = theme$grid, lwd = 1)
  abline(h = pretty(range(df$y, na.rm = TRUE)), col = theme$grid, lwd = 1)
  for (i in seq_along(condition_levels)) {
    rows <- df$condition == condition_levels[i]
    points(df$giq_total[rows], df$y[rows], pch = pchs[i], bg = cols[i], col = theme$ink, cex = 1.35, lwd = 1.1)
  }
  legend(
    "bottom",
    legend = condition_full_labels,
    pch = pchs,
    pt.bg = cols,
    col = theme$ink,
    bty = "n",
    cex = 0.72,
    ncol = 4,
    inset = c(0, -0.30),
    xpd = NA
  )
  draw_title("Trade-off inmersión vs desempeño", subtitle)
  box(col = theme$grid)
  close_png()
}

correlation_heatmap <- function(merged) {
  df <- merged[, c("giq_total", "duration_seconds", "damage_taken_rate", "kill_rate", "jitter_rate", "low_hp_ratio", "nearest_enemy_dist_mean", "fps_mean")]
  names(df) <- c("GIQ", "Supervivencia", "Daño/s", "Kills/s", "Jitter/s", "HP bajo", "Dist. enemigo", "FPS")
  df$`HP bajo` <- df$`HP bajo` * 100
  cm <- cor(df, use = "pairwise.complete.obs")
  open_png("22_correlation_heatmap.png")
  par(mar = c(8.0, 8.0, 5.6, 2.2), xaxs = "i", yaxs = "i")
  pal <- colorRampPalette(c("#485346", "white", "#7fee64"))(101)
  image(seq_len(ncol(cm)), seq_len(nrow(cm)), t(cm[nrow(cm):1, ]), col = pal, zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "")
  axis(1, at = seq_len(ncol(cm)), labels = colnames(cm), las = 2, tick = FALSE, cex.axis = 0.82)
  axis(2, at = seq_len(nrow(cm)), labels = rev(rownames(cm)), las = 1, tick = FALSE, cex.axis = 0.82)
  for (i in seq_len(nrow(cm))) {
    for (j in seq_len(ncol(cm))) {
      text(j, nrow(cm) - i + 1, sprintf("%.2f", cm[i, j]), cex = 0.78, font = 2)
    }
  }
  draw_title("Mapa de correlaciones", "Correlaciones Pearson entre encuesta, desempeño, control, presión y FPS")
  box(col = theme$grid)
  close_png()
}

fps_technical_control <- function() {
  fps_metrics <- c("fps_mean", "fps_min", "fps_drop_ratio")
  filenames <- "23_fps_technical_control.png"
  d <- prepare_run_factors(run_level)
  open_png(filenames, height = 1700)
  layout(matrix(seq_along(fps_metrics), nrow = 3))
  par(oma = c(2.2, 0, 5.4, 0))
  for (m in fps_metrics) {
    df <- d[is.finite(d[[m]]) & !is.na(d$condition), ]
    df$value <- scale_metric(m, df[[m]])
    groups <- condition_levels
    means <- sapply(groups, function(g) mean(df$value[df$condition == g], na.rm = TRUE))
    ses <- sapply(groups, function(g) {
      x <- df$value[df$condition == g]
      sd(x, na.rm = TRUE) / sqrt(sum(is.finite(x)))
    })
    lo <- means - 1.96 * ses
    hi <- means + 1.96 * ses
    ylim <- range(c(df$value, lo, hi), na.rm = TRUE)
    pad <- diff(ylim) * 0.16
    if (!is.finite(pad) || pad == 0) pad <- 1
    ylim <- ylim + c(-pad, pad)
    if (is_nonnegative_metric(m)) ylim[1] <- max(0, ylim[1])
    par(mar = c(4.4, 5.4, 2.8, 1.4), xaxs = "i", yaxs = "r")
    plot(seq_along(groups), means, type = "n", xaxt = "n", xlab = "", ylab = if (m == "fps_drop_ratio") "Caídas FPS (%)" else metric_labels[[m]], xlim = range(seq_along(groups)) + c(-0.55, 0.55), ylim = ylim)
    axis(1, at = seq_along(condition_axis_labels), labels = condition_axis_labels, tick = FALSE, cex.axis = 0.66)
    abline(h = pretty(ylim), col = theme$grid, lwd = 1)
    draw_boxplot_set(df$condition, df$value, groups, width = 0.46)
    stripchart(value ~ condition, data = df, vertical = TRUE, method = "jitter", jitter = 0.10, pch = 16, cex = 0.42, col = adjustcolor(theme$muted, alpha.f = 0.42), add = TRUE)
    x <- seq_along(groups)
    segments(x, lo, x, hi, col = theme$ink, lwd = 2.2)
    points(x, means, pch = 21, bg = theme$accent, col = theme$ink, cex = 1.45, lwd = 1.1)
    title(if (m == "fps_drop_ratio") "Caídas de FPS" else metric_labels[[m]], cex.main = 1.0, font.main = 2)
    box(col = theme$grid)
  }
  mtext(current_figure_label, side = 3, outer = TRUE, adj = 0.02, line = 3.2, font = 2, cex = 1.32)
  mtext("Control metodológico: estabilidad técnica por condición", side = 3, outer = TRUE, adj = 0.02, line = 2.0, cex = 0.86, col = theme$border)
  close_png()
}

condition_matrix()
unlink(file.path(output_dir, "11_low_hp_by_condition.png"))
giq <- build_giq()
giq_interval_plot(giq)
giq_condition_plot(giq)
condition_interval_plot(
  "duration_seconds",
  "04_duration_by_condition.png",
  "La supervivencia resume el desempeño global",
  "Distribución de duración por condición experimental"
)
condition_interval_plot(
  "kill_rate",
  "05_kill_rate_by_condition.png",
  "La tasa de kills muestra desempeño ofensivo",
  "Distribución de kills por segundo por condición experimental"
)
condition_interval_plot(
  "damage_taken_rate",
  "06_damage_rate_by_condition.png",
  "El daño recibido aproxima vulnerabilidad",
  "Distribución de daño recibido por segundo por condición experimental"
)
condition_interval_plot(
  "input_rate",
  "07_input_rate_by_condition.png",
  "El input revela cambios en control del jugador",
  "Distribución de inputs por segundo por condición experimental"
)
condition_interval_plot(
  "distance_rate",
  "08_distance_rate_by_condition.png",
  "La distancia recorrida aproxima estabilidad de movimiento",
  "Distribución de distancia recorrida por segundo por condición experimental"
)
condition_interval_plot(
  "jitter_rate",
  "09_jitter_rate_by_condition.png",
  "El jitter captura cambios abruptos de dirección",
  "Distribución de cambios de dirección por segundo por condición experimental"
)
condition_interval_plot(
  "nearest_enemy_dist_mean",
  "10_enemy_distance_by_condition.png",
  "La distancia al enemigo contextualiza la presión",
  "Distribución de distancia al enemigo más cercano"
)
plot_factorial_anova_effect_map(giq)
factorial_anova_boxplot_figure()
dunnett_figure()
synthesis_figure(giq)
interaction_plot(giq, "giq_total", "15_interaction_giq_total.png", "GIQ total (1-5)", "Shake en X; zoom como línea; displacement/recoil como faceta")
interaction_plot(run_level, "duration_seconds", "16_interaction_survival_time.png", "Duración (s)", "Shake en X; zoom como línea; displacement/recoil como faceta")
interaction_plot(run_level, "damage_taken_rate", "17_interaction_damage_rate.png", "Daño recibido/s", "Shake en X; zoom como línea; displacement/recoil como faceta")
interaction_plot(run_level, "jitter_rate", "18_interaction_jitter_rate.png", "Cambios de dirección/s", "Shake en X; zoom como línea; displacement/recoil como faceta")
forest_control_index_vs_c0()
merged <- merged_giq_runs(giq)
tradeoff_scatter(merged, "duration_seconds", "20_tradeoff_giq_survival.png", "Duración (s)", "GIQ total frente a supervivencia por condición")
tradeoff_scatter(merged, "damage_taken_rate", "21_tradeoff_giq_damage.png", "Daño recibido/s", "GIQ total frente a daño recibido por condición")
correlation_heatmap(merged)
fps_technical_control()

cat("Presentation figures written to: ", output_dir, "\n", sep = "")
