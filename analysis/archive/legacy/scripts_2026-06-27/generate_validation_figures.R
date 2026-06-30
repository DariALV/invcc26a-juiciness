# Generate validation calculations and figures for the final research tests.
#
# Scope:
#   - No XP metrics.
#   - FPS is not included in ANOVA; it remains only in Hard Clean criteria.
#   - Uses Hard Clean telemetry and mapped GIQ responses.
#
# Outputs:
#   analysis/images/validation_figures/*.png
#   analysis/validation_baseline_contrasts_no_xp.csv
#   analysis/validation_tradeoff_spearman_no_xp.csv
#   analysis/art_factorial_results_hard_clean.csv
#   analysis/validation_summary_no_xp_no_fps_anova.txt

suppressPackageStartupMessages({
  library(stats)
  library(MASS)
  library(emmeans)
  library(survival)
})

set.seed(20260622)

output_dir <- file.path("analysis", "images", "validation_figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

data_dir <- file.path("supabase_data", "r_outputs_existing_metrics")
run_level_path <- file.path(data_dir, "juicy_vs_existing_metrics_run_level.csv")
flags_path <- file.path("analysis", "problematic_flags_long.csv")
cfg_path <- file.path("supabase_data", "PlayerIDConfig_rows.csv")
form_path <- "Formulario Integrado InvCC26a Experimentos .csv"
if (!file.exists(form_path)) {
  form_path <- file.path("analysis", "Formulario Integrado InvCC26a Experimentos .csv")
}

required_outputs <- c(
  "analysis/anova_assumption_checks_hard_clean.csv",
  "analysis/anova_permutation_sensitivity_hard_clean.csv",
  "analysis/anova_type1_vs_type3_hard_clean.csv",
  "analysis/art_factorial_results_hard_clean.csv",
  "analysis/count_model_alternatives_hard_clean.csv",
  "analysis/survival_model_alternative_hard_clean.csv"
)
missing_outputs <- required_outputs[!file.exists(required_outputs)]
if (length(missing_outputs) > 0) {
  stop("Run analysis/anova_assumptions_and_alternatives.R first. Missing: ", paste(missing_outputs, collapse = ", "))
}

stopifnot(file.exists(run_level_path), file.exists(flags_path), file.exists(cfg_path), file.exists(form_path))

theme <- list(
  ink = "#222525",
  muted = "#647466",
  grid = "#dce7dd",
  accent = "#55a868",
  warn = "#d95f02",
  pale = "#f4faf2",
  blue = "#4c78a8"
)

condition_levels <- c(
  "C0_baseline", "C1_shake", "C2_zoom", "C3_recoil",
  "C4_shake_zoom", "C5_shake_recoil", "C6_zoom_recoil", "C7_all"
)

condition_labels <- c(
  "C0 Base", "C1 Shake", "C2 Zoom", "C3 Recoil",
  "C4 Shake+Zoom", "C5 Shake+Recoil", "C6 Zoom+Recoil", "C7 All"
)

metric_labels <- c(
  giq_total = "GIQ total",
  engagement = "Engagement",
  engrossment = "Engrossment",
  total_immersion = "Inmersion total",
  duration_seconds = "Supervivencia",
  total_kills = "Kills",
  input_total = "Inputs",
  hits_taken = "Golpes recibidos",
  damage_events_total = "Eventos de dano",
  kill_rate = "Kills/s",
  input_rate = "Inputs/s",
  damage_taken_rate = "Dano/s",
  jitter_rate = "Jitter/s",
  distance_rate = "Distancia/s",
  nearest_enemy_dist_mean = "Dist. enemigo",
  low_hp_ratio = "HP bajo (%)"
)

as_bool <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "si", "sí")
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

safe_bool_factor <- function(x) {
  factor(as_bool(x), levels = c(FALSE, TRUE), labels = c("Ausente", "Presente"))
}

open_png <- function(filename, width = 2400, height = 1400, res = 200) {
  png(file.path(output_dir, filename), width = width, height = height, res = res)
  par(
    bg = "white",
    fg = theme$ink,
    col.axis = theme$ink,
    col.lab = theme$ink,
    col.main = theme$ink,
    family = "sans"
  )
}

close_png <- function() {
  dev.off()
}

format_p <- function(p) {
  ifelse(!is.finite(p), "",
    ifelse(p < 0.001, "p<.001", sprintf("p=%.3f", p))
  )
}

format_q <- function(q) {
  ifelse(!is.finite(q), "",
    ifelse(q < 0.001, "q<.001", sprintf("q=%.2f", q))
  )
}

mean_ci <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(mean = NA_real_, lo = NA_real_, hi = NA_real_, n = 0))
  se <- sd(x) / sqrt(length(x))
  margin <- qt(0.975, df = max(length(x) - 1, 1)) * se
  c(mean = mean(x), lo = mean(x) - margin, hi = mean(x) + margin, n = length(x))
}

build_giq <- function(form, cfg) {
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

prepare_factors <- function(df) {
  df$condition <- factor(df$condition, levels = condition_levels)
  df$Shake <- safe_bool_factor(df$camera_shake)
  df$Zoom <- safe_bool_factor(df$camera_zoom)
  df$Recoil <- safe_bool_factor(df$camera_recoil)
  df
}

hard_clean_rules <- c(
  "duration_lt_30s",
  "zero_input_total",
  "zero_kills_after_120s",
  "fps_min_lt_15_or_drop_gt_10pct"
)

run_level_all <- read.csv(run_level_path, check.names = FALSE, stringsAsFactors = FALSE)
run_level_all$row_number <- seq_len(nrow(run_level_all))
flags <- read.csv(flags_path, check.names = FALSE, stringsAsFactors = FALSE)
hard_clean_rows <- unique(flags$row_number[flags$rule %in% hard_clean_rules])
run_level <- run_level_all[!run_level_all$row_number %in% hard_clean_rows, ]
run_level <- prepare_factors(run_level)

form <- read.csv(form_path, check.names = FALSE, stringsAsFactors = FALSE)
cfg <- read.csv(cfg_path, check.names = FALSE, stringsAsFactors = FALSE)
giq <- prepare_factors(build_giq(form, cfg))

assumptions <- read.csv("analysis/anova_assumption_checks_hard_clean.csv", check.names = FALSE)
permutation <- read.csv("analysis/anova_permutation_sensitivity_hard_clean.csv", check.names = FALSE)
anova_compare <- read.csv("analysis/anova_type1_vs_type3_hard_clean.csv", check.names = FALSE)
art_results <- read.csv("analysis/art_factorial_results_hard_clean.csv", check.names = FALSE)
count_models <- read.csv("analysis/count_model_alternatives_hard_clean.csv", check.names = FALSE)
survival_model <- read.csv("analysis/survival_model_alternative_hard_clean.csv", check.names = FALSE)

assumptions <- assumptions[!grepl("XP|FPS", assumptions$metric_label, ignore.case = TRUE), ]
permutation <- permutation[!grepl("XP|FPS", permutation$metric_label, ignore.case = TRUE), ]
anova_compare <- anova_compare[!grepl("XP|FPS", anova_compare$metric_label, ignore.case = TRUE), ]
art_results <- art_results[!grepl("XP|FPS", art_results$metric_label, ignore.case = TRUE), ]
count_models <- count_models[!grepl("XP|FPS", count_models$metric_label, ignore.case = TRUE), ]

plot_assumption_diagnostics <- function() {
  d <- assumptions
  d$metric_label <- factor(d$metric_label, levels = rev(d$metric_label))
  x1 <- -log10(pmax(d$residual_shapiro_p, 1e-12))
  x2 <- -log10(pmax(d$brown_forsythe_p, 1e-12))
  cutoff <- -log10(0.05)
  xlim <- c(0, max(c(x1, x2, cutoff), na.rm = TRUE) * 1.08)

  open_png("01_assumption_diagnostics.png", width = 2400, height = 1500)
  par(mfrow = c(1, 2), mar = c(5.4, 9.8, 4.6, 1.2), oma = c(0, 0, 2.2, 0))
  for (i in 1:2) {
    x <- if (i == 1) x1 else x2
    title <- if (i == 1) "Normalidad residual" else "Homogeneidad de varianza"
    subtitle <- if (i == 1) "Shapiro-Wilk sobre residuos" else "Brown-Forsythe por celda factorial"
    cols <- ifelse(x > cutoff, theme$warn, theme$accent)
    plot(
      x,
      seq_len(nrow(d)),
      type = "n",
      xlim = xlim,
      ylim = c(0.5, nrow(d) + 0.5),
      yaxt = "n",
      xlab = "-log10(p)",
      ylab = ""
    )
    abline(v = pretty(xlim), col = theme$grid, lwd = 1)
    abline(v = cutoff, col = theme$ink, lwd = 1.5, lty = 2)
    abline(h = seq_len(nrow(d)), col = theme$grid, lwd = 0.8)
    axis(2, at = seq_len(nrow(d)), labels = as.character(d$metric_label), las = 1, tick = FALSE, cex.axis = 0.84)
    points(x, seq_len(nrow(d)), pch = 21, bg = cols, col = theme$ink, cex = 1.45, lwd = 1.1)
    text(x + diff(xlim) * 0.025, seq_len(nrow(d)), format_p(if (i == 1) d$residual_shapiro_p else d$brown_forsythe_p), adj = 0, cex = 0.72)
    title(main = title, sub = subtitle, adj = 0, cex.main = 1.18, cex.sub = 0.84)
    box(col = theme$grid)
  }
  mtext("Validacion de supuestos antes del ANOVA", side = 3, outer = TRUE, adj = 0, line = 0.3, cex = 1.35, font = 2)
  close_png()
}

plot_giq_interaction <- function() {
  d <- giq[is.finite(giq$giq_total) & !is.na(giq$Shake) & !is.na(giq$Zoom) & !is.na(giq$Recoil), ]
  open_png("02_giq_factorial_interaction.png", width = 2200, height = 1200)
  par(mfrow = c(1, 2), mar = c(5.8, 5.4, 4.2, 1.4), oma = c(0, 0, 2.2, 0))
  cols <- c(theme$blue, theme$accent)
  names(cols) <- levels(d$Zoom)
  for (recoil_level in levels(d$Recoil)) {
    sub <- d[d$Recoil == recoil_level, ]
    plot(
      c(1, 2),
      c(1, 5),
      type = "n",
      xaxt = "n",
      xlab = "Shake",
      ylab = "GIQ total (1-5)",
      ylim = c(1, 5),
      xlim = c(0.75, 2.25)
    )
    abline(h = seq(1, 5, 0.5), col = theme$grid, lwd = 0.8)
    axis(1, at = c(1, 2), labels = levels(d$Shake), tick = FALSE)
    for (zoom_level in levels(d$Zoom)) {
      means <- lapply(levels(d$Shake), function(shake_level) {
        mean_ci(sub$giq_total[sub$Zoom == zoom_level & sub$Shake == shake_level])
      })
      means <- do.call(rbind, means)
      x <- c(1, 2)
      lines(x, means[, "mean"], col = cols[[zoom_level]], lwd = 2.5)
      segments(x, means[, "lo"], x, means[, "hi"], col = cols[[zoom_level]], lwd = 2)
      points(x, means[, "mean"], pch = 21, bg = cols[[zoom_level]], col = theme$ink, cex = 1.5, lwd = 1.1)
    }
    legend("bottomleft", legend = paste("Zoom", levels(d$Zoom)), col = cols, pch = 21, pt.bg = cols, lwd = 2, bty = "n", cex = 0.82)
    title(main = paste("Displacement/Recoil:", recoil_level), adj = 0, cex.main = 1.05)
    box(col = theme$grid)
  }
  mtext("GIQ total: grafico de interaccion factorial", side = 3, outer = TRUE, adj = 0, line = 0.2, cex = 1.35, font = 2)
  close_png()
}

plot_count_model_map <- function() {
  d <- count_models
  if (nrow(d) == 0) return(invisible(NULL))
  selected <- do.call(rbind, lapply(split(d, d$metric), function(x) {
    nb <- x[grepl("negative binomial", x$model), ]
    if (nrow(nb) > 0) return(nb)
    x[grepl("quasi-Poisson", x$model), ]
  }))
  terms_order <- c("Shake", "Zoom", "Recoil", "Shake:Zoom", "Shake:Recoil", "Zoom:Recoil", "Shake:Zoom:Recoil")
  metrics_order <- unique(selected$metric_label)
  metrics_axis <- gsub(" con offset por duracion", "", metrics_order)
  metrics_axis <- gsub("Eventos de dano", "Eventos dano", metrics_axis)
  metrics_axis <- gsub("Golpes recibidos", "Golpes", metrics_axis)
  mat <- matrix(NA_real_, nrow = length(metrics_order), ncol = length(terms_order), dimnames = list(metrics_order, terms_order))
  labels <- matrix("", nrow = nrow(mat), ncol = ncol(mat), dimnames = dimnames(mat))
  for (i in seq_len(nrow(selected))) {
    r <- match(selected$metric_label[i], metrics_order)
    c <- match(selected$term[i], terms_order)
    if (!is.na(r) && !is.na(c)) {
      mat[r, c] <- -log10(pmax(selected$p_value[i], 1e-12))
      labels[r, c] <- paste0(format_p(selected$p_value[i]), "\n", format_q(selected$q_value[i]))
    }
  }

  open_png("03_count_model_effect_map.png", width = 2400, height = 1200)
  par(mar = c(6.8, 6.2, 4.8, 1.4), xaxs = "i", yaxs = "i")
  pal <- colorRampPalette(c("white", "#d9f0d3", theme$accent, theme$ink))(101)
  z <- mat[nrow(mat):1, , drop = FALSE]
  zmax <- max(3, max(z, na.rm = TRUE))
  image(seq_len(ncol(z)), seq_len(nrow(z)), t(z), col = pal, zlim = c(0, zmax), axes = FALSE, xlab = "", ylab = "")
  axis(1, at = seq_len(ncol(mat)), labels = c("Shake", "Zoom", "Recoil", "SxZ", "SxR", "ZxR", "SxZxR"), tick = FALSE, cex.axis = 0.88)
  axis(2, at = seq_len(nrow(mat)), labels = rev(metrics_axis), las = 1, tick = FALSE, cex.axis = 0.90)
  for (r in seq_len(nrow(mat))) {
    rr <- nrow(mat) - r + 1
    for (c in seq_len(ncol(mat))) {
      if (labels[r, c] == "") next
      label_col <- ifelse(!is.na(mat[r, c]) && mat[r, c] >= zmax * 0.55, "white", theme$ink)
      text(c, rr, labels[r, c], cex = 0.70, font = ifelse(!is.na(mat[r, c]) && mat[r, c] > -log10(0.05), 2, 1), col = label_col)
    }
  }
  title("Modelos de conteo con offset por duracion", sub = "Binomial negativa cuando converge; quasi-Poisson como respaldo. XP excluido.", adj = 0, cex.main = 1.25, cex.sub = 0.88)
  box(col = theme$grid)
  close_png()
}

plot_art_factorial_map <- function() {
  failed_metrics <- assumptions$metric[assumptions$residual_shapiro_p < 0.05]
  d <- art_results[art_results$metric %in% failed_metrics, ]
  if (nrow(d) == 0) return(invisible(NULL))

  terms_order <- c("Shake", "Zoom", "Recoil", "Shake:Zoom", "Shake:Recoil", "Zoom:Recoil", "Shake:Zoom:Recoil")
  terms_axis <- c("Shake", "Zoom", "Recoil", "SxZ", "SxR", "ZxR", "SxZxR")
  metrics_order <- assumptions$metric_label[match(failed_metrics, assumptions$metric)]
  metrics_order <- metrics_order[!is.na(metrics_order)]
  metrics_axis <- metrics_order
  metrics_axis <- gsub("GIQ engagement", "GIQ engage.", metrics_axis)
  metrics_axis <- gsub("Dano recibido/s", "Dano/s", metrics_axis)
  metrics_axis <- gsub("Distancia a enemigo", "Dist. enemigo", metrics_axis)
  metrics_axis <- gsub("Proporcion HP bajo", "HP bajo", metrics_axis)

  mat <- matrix(NA_real_, nrow = length(metrics_order), ncol = length(terms_order), dimnames = list(metrics_order, terms_order))
  qmat <- matrix(NA_real_, nrow = length(metrics_order), ncol = length(terms_order), dimnames = list(metrics_order, terms_order))
  labels <- matrix("", nrow = nrow(mat), ncol = ncol(mat), dimnames = dimnames(mat))
  for (i in seq_len(nrow(d))) {
    r <- match(d$metric_label[i], metrics_order)
    c <- match(d$term[i], terms_order)
    if (!is.na(r) && !is.na(c)) {
      mat[r, c] <- d$partial_eta_squared_rank[i]
      qmat[r, c] <- d$q_value[i]
      labels[r, c] <- paste0(format_p(d$p_value[i]), "\n", format_q(d$q_value[i]))
    }
  }

  open_png("07_art_factorial_effect_map.png", width = 2600, height = 1600)
  par(mar = c(6.8, 7.4, 5.0, 1.4), xaxs = "i", yaxs = "i")
  pal <- colorRampPalette(c("white", "#d9f0d3", theme$accent, theme$ink))(101)
  z <- mat[nrow(mat):1, , drop = FALSE]
  zmax <- max(0.14, max(z, na.rm = TRUE))
  image(seq_len(ncol(z)), seq_len(nrow(z)), t(z), col = pal, zlim = c(0, zmax), axes = FALSE, xlab = "", ylab = "")
  axis(1, at = seq_len(ncol(mat)), labels = terms_axis, tick = FALSE, cex.axis = 0.88)
  axis(2, at = seq_len(nrow(mat)), labels = rev(metrics_axis), las = 1, tick = FALSE, cex.axis = 0.86)
  for (r in seq_len(nrow(mat))) {
    rr <- nrow(mat) - r + 1
    for (c in seq_len(ncol(mat))) {
      if (labels[r, c] == "") next
      label_col <- ifelse(!is.na(mat[r, c]) && mat[r, c] >= zmax * 0.55, "white", theme$ink)
      text(c, rr, labels[r, c], cex = 0.64, font = ifelse(!is.na(qmat[r, c]) && qmat[r, c] < 0.05, 2, 1), col = label_col)
    }
  }
  title(
    "ART factorial: métricas sin normalidad residual",
    sub = "Color = eta parcial en rangos alineados; texto = p nominal y q FDR. XP excluido; FPS solo control técnico.",
    adj = 0,
    cex.main = 1.25,
    cex.sub = 0.84
  )
  box(col = theme$grid)
  close_png()
}

plot_survival_cox <- function() {
  d <- run_level[is.finite(run_level$duration_seconds) & run_level$duration_seconds > 0, ]
  d$event <- as.integer(as_bool(d$died))
  fit <- survfit(Surv(duration_seconds, event) ~ condition, data = d)
  cols <- c("#222525", "#4c78a8", "#55a868", "#d95f02", "#8172b2", "#c44e52", "#64b5cd", "#8c8c8c")

  open_png("04_survival_cox_curves.png", width = 2400, height = 1350)
  par(mar = c(5.6, 5.8, 4.8, 2.0), xaxs = "r", yaxs = "r")
  plot(
    fit,
    col = cols,
    lwd = 2.1,
    conf.int = FALSE,
    xlab = "Tiempo de gameplay (s)",
    ylab = "Probabilidad de seguir sin morir",
    mark.time = TRUE,
    xlim = c(0, max(d$duration_seconds, na.rm = TRUE))
  )
  abline(h = pretty(c(0, 1)), col = theme$grid, lwd = 0.8)
  legend("bottomleft", legend = condition_labels, col = cols, lwd = 2.1, bty = "n", cex = 0.76, ncol = 2)
  min_p <- min(survival_model$p_value, na.rm = TRUE)
  title("Modelo Cox: supervivencia por condicion", sub = paste0("Curvas Kaplan-Meier; menor p factorial Cox = ", format_p(min_p)), adj = 0, cex.main = 1.25, cex.sub = 0.88)
  box(col = theme$grid)
  close_png()
}

baseline_contrasts <- function() {
  rows <- list()
  add_lm_contrasts <- function(df, variable, label) {
    x <- df[is.finite(df[[variable]]) & !is.na(df$condition), ]
    x$condition <- factor(x$condition, levels = condition_levels)
    if (nrow(x) < 16 || length(unique(x$condition)) < 2) return(NULL)
    fit <- lm(as.formula(paste(variable, "~ condition")), data = x)
    emm <- emmeans(fit, ~ condition)
    ct <- as.data.frame(contrast(emm, method = "trt.vs.ctrl", ref = "C0_baseline", adjust = "dunnettx"))
    names(ct)[names(ct) == "p.value"] <- "p_value"
    metric_sd <- sd(x[[variable]], na.rm = TRUE)
    if (!is.finite(metric_sd) || metric_sd == 0) return(NULL)
    ct$metric <- variable
    ct$metric_label <- label
    ct$estimate_std <- ct$estimate / metric_sd
    ct$lo_std <- (ct$estimate - 1.96 * ct$SE) / metric_sd
    ct$hi_std <- (ct$estimate + 1.96 * ct$SE) / metric_sd
    ct
  }

  giq_rows <- add_lm_contrasts(giq, "giq_total", metric_labels[["giq_total"]])
  if (!is.null(giq_rows)) rows[["giq_total"]] <- giq_rows

  telemetry_specs <- c(
    duration_seconds = "Supervivencia",
    kill_rate = "Kills/s",
    input_rate = "Inputs/s",
    damage_taken_rate = "Dano/s",
    jitter_rate = "Jitter/s",
    nearest_enemy_dist_mean = "Dist. enemigo",
    low_hp_ratio = "HP bajo (%)"
  )
  temp <- run_level
  temp$low_hp_ratio <- temp$low_hp_ratio * 100
  for (variable in names(telemetry_specs)) {
    if (!variable %in% names(temp)) next
    r <- add_lm_contrasts(temp, variable, telemetry_specs[[variable]])
    if (!is.null(r)) rows[[variable]] <- r
  }

  out <- do.call(rbind, rows)
  out$q_value <- p.adjust(out$p_value, method = "BH")
  condition_name <- gsub(" - C0_baseline", "", out$contrast)
  out$condition_label <- condition_labels[match(condition_name, condition_levels)]
  out$label <- paste(out$condition_label, out$metric_label, sep = " · ")
  write.csv(out, "analysis/validation_baseline_contrasts_no_xp.csv", row.names = FALSE, na = "")
  out
}

plot_baseline_forest <- function(contrasts) {
  d <- contrasts[is.finite(contrasts$p_value), ]
  d <- d[order(d$p_value), ]
  d <- d[d$p_value < 0.10, ]
  if (nrow(d) == 0) {
    d <- contrasts[order(abs(contrasts$estimate_std), decreasing = TRUE), ][seq_len(min(14, nrow(contrasts))), ]
  }
  d <- d[seq_len(min(18, nrow(d))), ]
  d <- d[order(d$estimate_std), ]
  y <- seq_len(nrow(d))
  xlim <- range(c(d$lo_std, d$hi_std, 0), na.rm = TRUE)
  pad <- diff(xlim) * 0.20
  if (!is.finite(pad) || pad == 0) pad <- 0.25
  xlim <- xlim + c(-pad, pad)

  open_png("05_baseline_forest_vs_c0.png", width = 2400, height = 1500)
  par(mar = c(6.0, 12.8, 4.8, 2.0), xaxs = "r", yaxs = "i")
  plot(d$estimate_std, y, type = "n", yaxt = "n", xlab = "Diferencia estandarizada vs C0", ylab = "", xlim = xlim, ylim = c(0.5, nrow(d) + 0.8))
  abline(v = pretty(xlim), col = theme$grid, lwd = 0.8)
  abline(v = 0, col = theme$ink, lwd = 1.6)
  abline(h = y, col = theme$grid, lwd = 0.8)
  axis(2, at = y, labels = d$label, las = 1, tick = FALSE, cex.axis = 0.80)
  segments(d$lo_std, y, d$hi_std, y, col = theme$ink, lwd = 2.4)
  points(d$estimate_std, y, pch = 21, bg = ifelse(d$q_value < 0.05, theme$accent, theme$pale), col = theme$ink, cex = 1.55, lwd = 1.1)
  text(xlim[2], y, paste0(format_p(d$p_value), "; ", format_q(d$q_value)), adj = 1, cex = 0.74)
  title("Contrastes contra C0", sub = "Dunnett por condicion; XP excluido; FPS no evaluado en ANOVA", adj = 0, cex.main = 1.25, cex.sub = 0.88)
  mtext("Valores positivos indican mayor valor que C0.", side = 1, line = 4.2, adj = 1, cex = 0.82, col = theme$muted)
  box(col = theme$grid)
  close_png()
}

tradeoff_data <- function() {
  g <- giq[, c("player_id", "condition", "giq_total")]
  names(g)[2] <- "giq_condition"
  m <- merge(run_level, g, by = "player_id")
  m <- m[as.character(m$condition) == as.character(m$giq_condition), ]
  m$condition <- factor(m$condition, levels = condition_levels)
  m
}

tradeoff_spearman <- function(merged) {
  vars <- c("duration_seconds", "kill_rate", "input_rate", "damage_taken_rate", "jitter_rate", "nearest_enemy_dist_mean")
  rows <- lapply(vars, function(v) {
    x <- merged[is.finite(merged$giq_total) & is.finite(merged[[v]]), ]
    if (nrow(x) < 4) return(NULL)
    test <- suppressWarnings(cor.test(x$giq_total, x[[v]], method = "spearman", exact = FALSE))
    data.frame(
      metric = v,
      metric_label = metric_labels[[v]],
      n = nrow(x),
      rho = unname(test$estimate),
      p_value = test$p.value,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  out$q_value <- p.adjust(out$p_value, method = "BH")
  write.csv(out, "analysis/validation_tradeoff_spearman_no_xp.csv", row.names = FALSE, na = "")
  out
}

plot_tradeoff <- function(merged, spearman) {
  vars <- c("duration_seconds", "kill_rate", "damage_taken_rate", "jitter_rate")
  ylabels <- c("Supervivencia (s)", "Kills/s", "Dano/s", "Jitter/s")
  cols <- c("#222525", "#4c78a8", "#55a868", "#d95f02", "#8172b2", "#c44e52", "#64b5cd", "#8c8c8c")
  names(cols) <- condition_levels
  open_png("06_tradeoff_giq_gameplay.png", width = 2400, height = 1500)
  par(mfrow = c(2, 2), mar = c(5.0, 5.2, 4.0, 1.0), oma = c(0, 0, 2.0, 0))
  for (i in seq_along(vars)) {
    v <- vars[i]
    d <- merged[is.finite(merged$giq_total) & is.finite(merged[[v]]) & !is.na(merged$condition), ]
    plot(
      d$giq_total,
      d[[v]],
      type = "n",
      xlab = "GIQ total (1-5)",
      ylab = ylabels[i]
    )
    abline(v = pretty(range(d$giq_total, na.rm = TRUE)), col = theme$grid, lwd = 0.8)
    abline(h = pretty(range(d[[v]], na.rm = TRUE)), col = theme$grid, lwd = 0.8)
    for (cond in condition_levels) {
      rows <- d$condition == cond
      points(d$giq_total[rows], d[[v]][rows], pch = 21, bg = cols[[cond]], col = theme$ink, cex = 1.1, lwd = 0.9)
    }
    if (nrow(d) >= 3) {
      abline(lm(d[[v]] ~ d$giq_total), col = theme$ink, lwd = 1.3, lty = 2)
    }
    sp <- spearman[spearman$metric == v, ]
    title(main = ylabels[i], sub = if (nrow(sp) == 1) paste0("Spearman rho=", sprintf("%.2f", sp$rho), "; ", format_p(sp$p_value)) else "", adj = 0, cex.main = 1.02, cex.sub = 0.78)
    box(col = theme$grid)
  }
  mtext("Trade-off: inmersion subjetiva vs gameplay", side = 3, outer = TRUE, adj = 0, line = 0.2, cex = 1.35, font = 2)
  close_png()
}

plot_assumption_diagnostics()
plot_giq_interaction()
plot_count_model_map()
plot_art_factorial_map()
plot_survival_cox()
baseline <- baseline_contrasts()
plot_baseline_forest(baseline)
merged <- tradeoff_data()
spearman <- tradeoff_spearman(merged)
plot_tradeoff(merged, spearman)

summary_lines <- c(
  "Validation outputs - no XP metrics, no FPS in ANOVA",
  paste0("Telemetry runs original: ", nrow(run_level_all)),
  paste0("Telemetry runs retained after Hard Clean: ", nrow(run_level)),
  paste0("GIQ responses with condition mapping: ", sum(!is.na(giq$condition))),
  "",
  "Generated figures:",
  "  - analysis/images/validation_figures/01_assumption_diagnostics.png",
  "  - analysis/images/validation_figures/02_giq_factorial_interaction.png",
  "  - analysis/images/validation_figures/03_count_model_effect_map.png",
  "  - analysis/images/validation_figures/04_survival_cox_curves.png",
  "  - analysis/images/validation_figures/05_baseline_forest_vs_c0.png",
  "  - analysis/images/validation_figures/06_tradeoff_giq_gameplay.png",
  "  - analysis/images/validation_figures/07_art_factorial_effect_map.png",
  "",
  "Generated calculation tables:",
  "  - analysis/validation_baseline_contrasts_no_xp.csv",
  "  - analysis/validation_tradeoff_spearman_no_xp.csv",
  "  - analysis/art_factorial_results_hard_clean.csv"
)
writeLines(summary_lines, "analysis/validation_summary_no_xp_no_fps_anova.txt")
cat(paste(summary_lines, collapse = "\n"), "\n")
