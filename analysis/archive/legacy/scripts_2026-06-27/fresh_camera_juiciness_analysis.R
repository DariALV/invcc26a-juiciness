# Analyze the identified fresh camera juiciness dataset.
#
# Primary input:
#   analysis/data/camera_juiciness_identified_fresh.csv
#
# The script adapts the Hard Clean workflow to the identified camera CSV. It
# keeps the attached Rmd's core logic for GIQ (factorial linear models,
# residual checks, variance checks, and emmeans for relevant interactions), but
# adds the analyses needed for the research questions:
#
#   RQ1: GIQ / immersion effects.
#   RQ2: gameplay telemetry effects.
#   RQ3: immersion-performance relationship and baseline comparisons.
#
# Outputs:
#   analysis/data/fresh_camera_*.csv|txt
#   analysis/images/fresh_camera_figures/*.png

suppressPackageStartupMessages({
  library(stats)
  library(grDevices)
  library(graphics)
})

set.seed(20260627)

data_path <- file.path("analysis", "data", "camera_juiciness_identified_fresh.csv")
out_data_dir <- file.path("analysis", "data")
out_fig_dir <- file.path("analysis", "images", "fresh_camera_figures")
dir.create(out_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_path)) {
  stop("Missing input file: ", data_path)
}

has_emmeans <- requireNamespace("emmeans", quietly = TRUE)
has_art <- requireNamespace("ARTool", quietly = TRUE)
has_mass <- requireNamespace("MASS", quietly = TRUE)
has_survival <- requireNamespace("survival", quietly = TRUE)
has_car <- requireNamespace("car", quietly = TRUE)

raw <- read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)

required <- c(
  "player_id", "run_id", "shake", "zoom", "recoil", "imm_total",
  "ingame_survival_s", "total_kills", "input_total", "dmg_per_min",
  "hits_per_min", "fps_mean"
)
missing_required <- setdiff(required, names(raw))
if (length(missing_required) > 0) {
  stop("Missing required columns: ", paste(missing_required, collapse = ", "))
}

to_num <- function(x) suppressWarnings(as.numeric(x))

raw$player_id <- trimws(toupper(raw$player_id))
raw$run_id <- trimws(as.character(raw$run_id))
raw$shake_num <- as.integer(to_num(raw$shake))
raw$zoom_num <- as.integer(to_num(raw$zoom))
raw$recoil_num <- as.integer(to_num(raw$recoil))
raw$duration_min <- to_num(raw$ingame_survival_s) / 60
raw$hits_total_est <- round(to_num(raw$hits_per_min) * raw$duration_min)
raw$damage_amount_total_est <- to_num(raw$dmg_per_min) * raw$duration_min
raw$death_event <- tolower(trimws(raw$end_reason)) == "death"

treatment_from_flags <- function(shake, zoom, recoil) {
  out <- rep("Control", length(shake))
  out[shake == 1 & zoom == 0 & recoil == 0] <- "Shake"
  out[shake == 0 & zoom == 1 & recoil == 0] <- "Zoom"
  out[shake == 0 & zoom == 0 & recoil == 1] <- "Recoil"
  out[shake == 1 & zoom == 1 & recoil == 0] <- "Shake + Zoom"
  out[shake == 1 & zoom == 0 & recoil == 1] <- "Shake + Recoil"
  out[shake == 0 & zoom == 1 & recoil == 1] <- "Zoom + Recoil"
  out[shake == 1 & zoom == 1 & recoil == 1] <- "Shake + Zoom + Recoil"
  out
}

treatment_levels <- c(
  "Control", "Shake", "Zoom", "Recoil", "Shake + Zoom",
  "Shake + Recoil", "Zoom + Recoil", "Shake + Zoom + Recoil"
)

raw$treatment <- factor(
  treatment_from_flags(raw$shake_num, raw$zoom_num, raw$recoil_num),
  levels = treatment_levels
)
raw$Shake <- factor(raw$shake_num, levels = c(0, 1), labels = c("Ausente", "Presente"))
raw$Zoom <- factor(raw$zoom_num, levels = c(0, 1), labels = c("Ausente", "Presente"))
raw$Recoil <- factor(raw$recoil_num, levels = c(0, 1), labels = c("Ausente", "Presente"))

metric_specs <- data.frame(
  metric = c(
    "imm_total", "imm_atraccion", "imm_foco", "imm_presencia",
    "ingame_survival_s", "kills_per_min", "inputs_per_min",
    "dmg_per_min", "hits_per_min", "enemies_mean"
  ),
  label = c(
    "Promedio en GIQ", "GIQ - atraccion", "GIQ - foco", "GIQ - presencia",
    "Supervivencia (s)", "Kills/min", "Inputs/min",
    "Dano/min", "Hits/min", "Enemigos promedio"
  ),
  domain = c(
    "Immersion", "Immersion", "Immersion", "Immersion",
    "Gameplay", "Gameplay", "Gameplay", "Gameplay", "Gameplay", "Gameplay"
  ),
  log_model = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
metric_specs <- metric_specs[metric_specs$metric %in% names(raw), , drop = FALSE]

primary_metrics <- c(
  "imm_total", "ingame_survival_s", "kills_per_min", "inputs_per_min",
  "dmg_per_min", "hits_per_min", "enemies_mean"
)

fmt_p <- function(p) {
  ifelse(!is.finite(p), "NA", ifelse(p < 0.001, "<.001", sprintf("%.3f", p)))
}

fmt_num <- function(x, digits = 2) {
  ifelse(!is.finite(x), "NA", formatC(x, format = "f", digits = digits))
}

term_label <- function(term) {
  out <- gsub(":", " x ", term)
  out <- gsub("Shake", "Shake", out)
  out <- gsub("Zoom", "Zoom", out)
  out <- gsub("Recoil", "Recoil", out)
  out
}

open_png <- function(filename, width = 2400, height = 1400, res = 200) {
  png(file.path(out_fig_dir, filename), width = width, height = height, res = res)
  par(bg = "white", fg = "#202423", col.axis = "#202423", col.lab = "#202423",
      col.main = "#202423", family = "sans")
}

close_png <- function() dev.off()

# -------------------------------------------------------------------------
# Hard Clean adapted to the identified fresh camera CSV.
# -------------------------------------------------------------------------

raw <- raw[order(raw$player_id, raw$run_id), ]
raw$person_row_index <- ave(seq_len(nrow(raw)), raw$player_id, FUN = seq_along)

flag_rows <- list()
add_flag <- function(rows, rule, variable, value, threshold, reason) {
  rows <- rows[!is.na(rows)]
  if (length(rows) == 0) return(invisible(NULL))
  flag_rows[[length(flag_rows) + 1]] <<- data.frame(
    row_number = rows,
    player_id = raw$player_id[rows],
    run_id = raw$run_id[rows],
    treatment = as.character(raw$treatment[rows]),
    rule = rule,
    variable = variable,
    value = value,
    threshold = threshold,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

add_flag(
  which(raw$person_row_index > 1),
  "duplicate_player_row",
  "player_id",
  raw$player_id[raw$person_row_index > 1],
  "retain first row only",
  "Participant appears more than once in the identified CSV."
)
add_flag(
  which(is.finite(raw$duration_min) & raw$duration_min * 60 < 30),
  "duration_lt_30s",
  "ingame_survival_s",
  raw$ingame_survival_s[is.finite(raw$duration_min) & raw$duration_min * 60 < 30],
  "< 30 seconds",
  "Run is too short to represent stable gameplay."
)
add_flag(
  which(is.finite(to_num(raw$input_total)) & to_num(raw$input_total) <= 0),
  "zero_input_total",
  "input_total",
  raw$input_total[is.finite(to_num(raw$input_total)) & to_num(raw$input_total) <= 0],
  "> 0",
  "Run has no recorded input."
)
add_flag(
  which(
    is.finite(to_num(raw$ingame_survival_s)) &
      is.finite(to_num(raw$total_kills)) &
      to_num(raw$ingame_survival_s) >= 120 &
      to_num(raw$total_kills) <= 0
  ),
  "zero_kills_after_120s",
  "total_kills",
  raw$total_kills[
    is.finite(to_num(raw$ingame_survival_s)) &
      is.finite(to_num(raw$total_kills)) &
      to_num(raw$ingame_survival_s) >= 120 &
      to_num(raw$total_kills) <= 0
  ],
  "> 0 when survival >= 120s",
  "Long run has no kills, suggesting failed telemetry or non-representative play."
)
add_flag(
  which(is.finite(to_num(raw$fps_mean)) & to_num(raw$fps_mean) < 15),
  "fps_mean_lt_15",
  "fps_mean",
  raw$fps_mean[is.finite(to_num(raw$fps_mean)) & to_num(raw$fps_mean) < 15],
  ">= 15 FPS mean",
  "Run has severe average frame-rate instability."
)

flags <- if (length(flag_rows) > 0) do.call(rbind, flag_rows) else data.frame()
hard_clean_rows <- sort(unique(flags$row_number))
removed <- raw[raw$person_row_index > 1 | seq_len(nrow(raw)) %in% hard_clean_rows, , drop = FALSE]
hard <- raw[!(raw$person_row_index > 1 | seq_len(nrow(raw)) %in% hard_clean_rows), , drop = FALSE]

write.csv(flags, file.path(out_data_dir, "fresh_camera_hard_clean_flags.csv"), row.names = FALSE, na = "")
write.csv(removed, file.path(out_data_dir, "fresh_camera_hard_clean_removed.csv"), row.names = FALSE, na = "")
write.csv(hard, file.path(out_data_dir, "fresh_camera_hard_clean.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Descriptive tables.
# -------------------------------------------------------------------------

condition_summary_rows <- list()
for (metric in metric_specs$metric) {
  values <- to_num(hard[[metric]])
  temp <- data.frame(treatment = hard$treatment, value = values)
  temp <- temp[is.finite(temp$value), , drop = FALSE]
  if (nrow(temp) == 0) next
  agg <- aggregate(value ~ treatment, temp, function(x) {
    c(n = length(x), mean = mean(x), median = median(x), sd = sd(x))
  })
  unpacked <- data.frame(
    metric = metric,
    metric_label = metric_specs$label[match(metric, metric_specs$metric)],
    treatment = agg$treatment,
    n = agg$value[, "n"],
    mean = agg$value[, "mean"],
    median = agg$value[, "median"],
    sd = agg$value[, "sd"],
    stringsAsFactors = FALSE
  )
  condition_summary_rows[[metric]] <- unpacked
}
condition_summary <- do.call(rbind, condition_summary_rows)
write.csv(condition_summary, file.path(out_data_dir, "fresh_camera_condition_summary.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# ANOVA, assumptions, Rmd-style model comparisons, and emmeans.
# -------------------------------------------------------------------------

brown_forsythe_p <- function(df) {
  cell <- interaction(df$Shake, df$Zoom, df$Recoil, drop = TRUE)
  med <- ave(df$Y_model, cell, FUN = median, na.rm = TRUE)
  abs_dev <- abs(df$Y_model - med)
  fit <- aov(abs_dev ~ cell)
  as.numeric(summary(fit)[[1]]$`Pr(>F)`[1])
}

extract_anova <- function(fit, metric, label, model_scale) {
  tab <- as.data.frame(anova(fit))
  tab$term <- trimws(rownames(tab))
  rownames(tab) <- NULL
  residual_ss <- tab$`Sum Sq`[tab$term == "Residuals"]
  residual_df <- tab$Df[tab$term == "Residuals"]
  tab <- tab[tab$term != "Residuals", , drop = FALSE]
  if (nrow(tab) == 0) return(NULL)
  data.frame(
    metric = metric,
    metric_label = label,
    model_scale = model_scale,
    term = tab$term,
    df_effect = tab$Df,
    df_residual = residual_df,
    f_value = tab$`F value`,
    p_value = tab$`Pr(>F)`,
    partial_eta_squared = tab$`Sum Sq` / (tab$`Sum Sq` + residual_ss),
    stringsAsFactors = FALSE
  )
}

prep_metric_df <- function(metric, log_model = FALSE) {
  temp <- hard
  temp$Y_raw <- to_num(temp[[metric]])
  temp <- temp[
    is.finite(temp$Y_raw) &
      !is.na(temp$Shake) & !is.na(temp$Zoom) & !is.na(temp$Recoil),
    ,
    drop = FALSE
  ]
  if (log_model) {
    temp$Y_model <- log1p(pmax(temp$Y_raw, 0))
    model_scale <- "log1p"
  } else {
    temp$Y_model <- temp$Y_raw
    model_scale <- "raw"
  }
  attr(temp, "model_scale") <- model_scale
  temp
}

assumption_rows <- list()
anova_rows <- list()
emmeans_rows <- list()
rmd_comparison_rows <- list()

for (i in seq_len(nrow(metric_specs))) {
  metric <- metric_specs$metric[i]
  label <- metric_specs$label[i]
  log_model <- metric_specs$log_model[i]
  temp <- prep_metric_df(metric, log_model)
  if (nrow(temp) < 16) next
  model_scale <- attr(temp, "model_scale")

  fit_full <- lm(Y_model ~ Shake * Zoom * Recoil, data = temp)
  fit_main <- lm(Y_model ~ Shake + Zoom + Recoil, data = temp)
  fit_two_way <- lm(Y_model ~ Shake * Zoom + Shake * Recoil + Zoom * Recoil, data = temp)

  comp <- as.data.frame(anova(fit_main, fit_two_way, fit_full))
  comp$model <- c("main_effects", "rmd_pairwise_two_way", "full_three_way")
  comp$metric <- metric
  comp$metric_label <- label
  comp$model_scale <- model_scale
  comp$comparison_note <- c(
    "Efectos principales",
    "Agrega interacciones de dos vias como el Rmd adjunto",
    "Agrega la interaccion triple"
  )
  rmd_comparison_rows[[metric]] <- comp[
    ,
    c("metric", "metric_label", "model_scale", "model", "comparison_note", "Res.Df", "RSS", "Df", "Sum of Sq", "F", "Pr(>F)")
  ]

  residuals_fit <- residuals(fit_full)
  shapiro_p <- if (length(residuals_fit) >= 3 && length(residuals_fit) <= 5000) {
    shapiro.test(residuals_fit)$p.value
  } else {
    NA_real_
  }
  bartlett_p <- tryCatch(
    bartlett.test(Y_model ~ interaction(Shake, Zoom, Recoil, drop = TRUE), data = temp)$p.value,
    error = function(e) NA_real_
  )
  bf_p <- tryCatch(brown_forsythe_p(temp), error = function(e) NA_real_)

  assumption_rows[[metric]] <- data.frame(
    metric = metric,
    metric_label = label,
    model_scale = model_scale,
    n = nrow(temp),
    duplicated_players = sum(duplicated(temp$player_id)),
    shapiro_residual_p = shapiro_p,
    bartlett_p = bartlett_p,
    brown_forsythe_p = bf_p,
    normality_ok = is.finite(shapiro_p) & shapiro_p >= 0.05,
    variance_ok_brown_forsythe = is.finite(bf_p) & bf_p >= 0.05,
    stringsAsFactors = FALSE
  )

  anova_rows[[metric]] <- extract_anova(fit_full, metric, label, model_scale)

  metric_anova <- anova_rows[[metric]]
  interaction_hits <- metric_anova[
    grepl(":", metric_anova$term) &
      is.finite(metric_anova$p_value) &
      metric_anova$p_value < 0.05,
    ,
    drop = FALSE
  ]

  if (nrow(interaction_hits) > 0 && has_emmeans) {
    for (term in interaction_hits$term) {
      em <- tryCatch({
        if (term == "Shake:Zoom") {
          emmeans::emmeans(fit_full, pairwise ~ Shake | Zoom)
        } else if (term == "Shake:Recoil") {
          emmeans::emmeans(fit_full, pairwise ~ Shake | Recoil)
        } else if (term == "Zoom:Recoil") {
          emmeans::emmeans(fit_full, pairwise ~ Zoom | Recoil)
        } else {
          emmeans::emmeans(fit_full, pairwise ~ Shake | Zoom * Recoil)
        }
      }, error = function(e) NULL)
      if (!is.null(em)) {
        contrasts <- as.data.frame(em$contrasts)
        contrasts$metric <- metric
        contrasts$metric_label <- label
        contrasts$model_scale <- model_scale
        contrasts$trigger_term <- term
        emmeans_rows[[paste(metric, term, sep = "_")]] <- contrasts
      }
    }
  }
}

assumptions <- do.call(rbind, assumption_rows)
anova_results <- do.call(rbind, anova_rows)
anova_results$q_value <- p.adjust(anova_results$p_value, method = "BH")
emmeans_results <- if (length(emmeans_rows) > 0) do.call(rbind, emmeans_rows) else data.frame()
rmd_comparison <- do.call(rbind, rmd_comparison_rows)

write.csv(assumptions, file.path(out_data_dir, "fresh_camera_assumption_checks.csv"), row.names = FALSE, na = "")
write.csv(anova_results, file.path(out_data_dir, "fresh_camera_factorial_anova.csv"), row.names = FALSE, na = "")
write.csv(emmeans_results, file.path(out_data_dir, "fresh_camera_emmeans_interactions.csv"), row.names = FALSE, na = "")
write.csv(rmd_comparison, file.path(out_data_dir, "fresh_camera_rmd_style_model_comparison.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# ART nonparametric factorial checks.
# -------------------------------------------------------------------------

art_rows <- list()
if (has_art) {
  for (i in seq_len(nrow(metric_specs))) {
    metric <- metric_specs$metric[i]
    label <- metric_specs$label[i]
    temp <- prep_metric_df(metric, FALSE)
    temp$Y <- temp$Y_raw
    if (nrow(temp) < 16) next
    art_fit <- tryCatch(ARTool::art(Y ~ Shake * Zoom * Recoil, data = temp), error = function(e) NULL)
    if (is.null(art_fit)) next
    tab <- tryCatch(as.data.frame(anova(art_fit)), error = function(e) NULL)
    if (is.null(tab) || nrow(tab) == 0) next
    tab$term <- trimws(rownames(tab))
    rownames(tab) <- NULL
    f_col <- intersect(c("F", "F value", "F.value"), names(tab))[1]
    p_col <- intersect(c("Pr(>F)", "Pr..F.", "p.value"), names(tab))[1]
    df_col <- intersect(c("Df", "Df1"), names(tab))[1]
    df_res_col <- intersect(c("Df.res", "Df2", "Den Df"), names(tab))[1]
    if (is.na(f_col) || is.na(p_col)) next
    out <- data.frame(
      metric = metric,
      metric_label = label,
      term = tab$term,
      df_effect = if (!is.na(df_col)) tab[[df_col]] else NA_real_,
      df_residual = if (!is.na(df_res_col)) tab[[df_res_col]] else NA_real_,
      f_value = tab[[f_col]],
      p_value = tab[[p_col]],
      stringsAsFactors = FALSE
    )
    out <- out[!grepl("Residual", out$term, ignore.case = TRUE), , drop = FALSE]
    art_rows[[metric]] <- out
  }
}
art_results <- if (length(art_rows) > 0) do.call(rbind, art_rows) else data.frame()
if (nrow(art_results) > 0) {
  art_results$q_value <- p.adjust(art_results$p_value, method = "BH")
}
write.csv(art_results, file.path(out_data_dir, "fresh_camera_art_results.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Count models with duration offset for event totals.
# -------------------------------------------------------------------------

count_specs <- data.frame(
  metric = c("total_kills", "input_total", "hits_total_est"),
  label = c("Kills totales", "Inputs totales", "Hits totales estimados"),
  stringsAsFactors = FALSE
)
count_specs <- count_specs[count_specs$metric %in% names(hard), , drop = FALSE]

count_rows <- list()
if (has_mass && nrow(count_specs) > 0) {
  for (i in seq_len(nrow(count_specs))) {
    metric <- count_specs$metric[i]
    label <- count_specs$label[i]
    temp <- hard
    temp$Y <- round(to_num(temp[[metric]]))
    temp$duration_min <- to_num(temp$duration_min)
    temp <- temp[
      is.finite(temp$Y) & temp$Y >= 0 &
        is.finite(temp$duration_min) & temp$duration_min > 0,
      ,
      drop = FALSE
    ]
    if (nrow(temp) < 16) next
    fit <- tryCatch(
      MASS::glm.nb(Y ~ Shake * Zoom * Recoil + offset(log(duration_min)), data = temp),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    tab <- tryCatch(as.data.frame(drop1(fit, test = "Chisq")), error = function(e) NULL)
    if (is.null(tab)) next
    tab$term <- trimws(rownames(tab))
    rownames(tab) <- NULL
    tab <- tab[tab$term != "<none>", , drop = FALSE]
    p_col <- intersect(c("Pr(>Chi)", "Pr..Chi."), names(tab))[1]
    stat_col <- intersect(c("LRT", "LR stat.", "Chi-square"), names(tab))[1]
    if (is.na(p_col)) next
    count_rows[[metric]] <- data.frame(
      metric = metric,
      metric_label = label,
      model = "negative binomial with duration offset",
      term = tab$term,
      statistic = if (!is.na(stat_col)) tab[[stat_col]] else NA_real_,
      p_value = tab[[p_col]],
      theta = fit$theta,
      stringsAsFactors = FALSE
    )
  }
}
count_results <- if (length(count_rows) > 0) do.call(rbind, count_rows) else data.frame()
if (nrow(count_results) > 0) {
  count_results$q_value <- p.adjust(count_results$p_value, method = "BH")
}
write.csv(count_results, file.path(out_data_dir, "fresh_camera_count_models.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Baseline contrasts against Control and trade-off correlations.
# -------------------------------------------------------------------------

baseline_rows <- list()
for (metric in primary_metrics[primary_metrics %in% names(hard)]) {
  spec <- metric_specs[match(metric, metric_specs$metric), ]
  if (nrow(spec) == 0) next
  temp <- prep_metric_df(metric, spec$log_model)
  if (nrow(temp) < 16 || length(unique(temp$treatment)) < 2) next
  fit <- lm(Y_model ~ treatment, data = temp)
  emm_p <- NULL
  if (has_emmeans) {
    emm <- tryCatch(emmeans::emmeans(fit, "treatment"), error = function(e) NULL)
    if (!is.null(emm)) {
      emm_p <- tryCatch(
        as.data.frame(emmeans::contrast(emm, method = "trt.vs.ctrl", ref = 1, adjust = "dunnettx")),
        error = function(e) NULL
      )
    }
  }
  control_vals <- temp$Y_raw[temp$treatment == "Control"]
  rows <- list()
  for (tr in treatment_levels[treatment_levels != "Control"]) {
    vals <- temp$Y_raw[temp$treatment == tr]
    if (length(vals) < 2 || length(control_vals) < 2) next
    pooled_sd <- sqrt(((length(vals) - 1) * var(vals) + (length(control_vals) - 1) * var(control_vals)) /
      (length(vals) + length(control_vals) - 2))
    diff_raw <- mean(vals) - mean(control_vals)
    std_diff <- ifelse(is.finite(pooled_sd) && pooled_sd > 0, diff_raw / pooled_sd, NA_real_)
    se_raw <- sqrt(var(vals) / length(vals) + var(control_vals) / length(control_vals))
    ci_raw <- diff_raw + c(-1, 1) * qt(0.975, df = max(length(vals) + length(control_vals) - 2, 1)) * se_raw
    ci_std <- ifelse(is.finite(pooled_sd) && pooled_sd > 0, ci_raw / pooled_sd, NA_real_)
    p_value <- NA_real_
    if (!is.null(emm_p)) {
      hit <- emm_p[grepl(tr, emm_p$contrast, fixed = TRUE), , drop = FALSE]
      if (nrow(hit) > 0 && "p.value" %in% names(hit)) p_value <- hit$p.value[1]
    }
    rows[[tr]] <- data.frame(
      metric = metric,
      metric_label = spec$label,
      treatment = tr,
      contrast = paste(tr, "vs Control"),
      model_scale = attr(temp, "model_scale"),
      mean_control = mean(control_vals),
      mean_treatment = mean(vals),
      diff_raw = diff_raw,
      std_diff = std_diff,
      std_low = ci_std[1],
      std_high = ci_std[2],
      p_value = p_value,
      stringsAsFactors = FALSE
    )
  }
  baseline_rows[[metric]] <- do.call(rbind, rows)
}
baseline <- if (length(baseline_rows) > 0) do.call(rbind, baseline_rows) else data.frame()
if (nrow(baseline) > 0) baseline$q_value <- p.adjust(baseline$p_value, method = "BH")
write.csv(baseline, file.path(out_data_dir, "fresh_camera_baseline_contrasts.csv"), row.names = FALSE, na = "")

tradeoff_metrics <- c("ingame_survival_s", "kills_per_min", "inputs_per_min", "dmg_per_min", "hits_per_min", "enemies_mean")
tradeoff_metrics <- tradeoff_metrics[tradeoff_metrics %in% names(hard)]
tradeoff_rows <- list()
for (metric in tradeoff_metrics) {
  temp <- hard
  x <- to_num(temp$imm_total)
  y <- to_num(temp[[metric]])
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 8) next
  ct <- suppressWarnings(cor.test(x[keep], y[keep], method = "spearman", exact = FALSE))
  tradeoff_rows[[metric]] <- data.frame(
    metric = metric,
    metric_label = metric_specs$label[match(metric, metric_specs$metric)],
    n = sum(keep),
    rho = unname(ct$estimate),
    p_value = ct$p.value,
    stringsAsFactors = FALSE
  )
}
tradeoff <- if (length(tradeoff_rows) > 0) do.call(rbind, tradeoff_rows) else data.frame()
if (nrow(tradeoff) > 0) tradeoff$q_value <- p.adjust(tradeoff$p_value, method = "BH")
write.csv(tradeoff, file.path(out_data_dir, "fresh_camera_tradeoff_spearman.csv"), row.names = FALSE, na = "")

survival_rows <- data.frame()
if (has_survival && "end_reason" %in% names(hard)) {
  temp <- hard[
    is.finite(to_num(hard$ingame_survival_s)) &
      !is.na(hard$Shake) & !is.na(hard$Zoom) & !is.na(hard$Recoil),
    ,
    drop = FALSE
  ]
  if (nrow(temp) >= 16 && length(unique(temp$death_event)) > 1) {
    surv_fit <- tryCatch(
      survival::coxph(
        survival::Surv(to_num(ingame_survival_s), death_event) ~ Shake * Zoom * Recoil,
        data = temp
      ),
      error = function(e) NULL
    )
    if (!is.null(surv_fit)) {
      tab <- tryCatch(as.data.frame(drop1(surv_fit, test = "Chisq")), error = function(e) NULL)
      if (!is.null(tab)) {
        tab$term <- trimws(rownames(tab))
        rownames(tab) <- NULL
        tab <- tab[tab$term != "<none>", , drop = FALSE]
        p_col <- intersect(c("Pr(>|Chi|)", "Pr..Chi.."), names(tab))[1]
        survival_rows <- data.frame(
          term = tab$term,
          statistic = if ("LRT" %in% names(tab)) tab$LRT else NA_real_,
          p_value = if (!is.na(p_col)) tab[[p_col]] else NA_real_,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}
if (nrow(survival_rows) > 0) survival_rows$q_value <- p.adjust(survival_rows$p_value, method = "BH")
write.csv(survival_rows, file.path(out_data_dir, "fresh_camera_survival_model.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Figures.
# -------------------------------------------------------------------------

palette <- c(
  "Control" = "#4D4D4D",
  "Shake" = "#0072B2",
  "Zoom" = "#009E73",
  "Recoil" = "#D55E00",
  "Shake + Zoom" = "#56B4E9",
  "Shake + Recoil" = "#CC79A7",
  "Zoom + Recoil" = "#E69F00",
  "Shake + Zoom + Recoil" = "#7B3294"
)

open_png("01_hard_clean_flow.png")
par(mar = c(5, 6, 4, 2))
counts <- c(
  "CSV fresco" = nrow(raw),
  "Removidos" = nrow(removed),
  "Hard Clean" = nrow(hard)
)
barplot(
  counts,
  horiz = TRUE,
  las = 1,
  col = c("#6BAED6", "#F1695B", "#31A354"),
  border = NA,
  xlab = "Filas",
  main = "Flujo Hard Clean"
)
text(counts + max(counts) * 0.03, seq_along(counts) * 1.2 - 0.5, labels = counts, xpd = NA)
close_png()

open_png("02_giq_by_treatment.png", width = 2600, height = 1500)
par(mar = c(7, 10, 4, 2))
boxplot(
  imm_total ~ treatment,
  data = hard,
  horizontal = TRUE,
  las = 1,
  col = palette[levels(hard$treatment)],
  border = "#243B35",
  xlab = "Promedio en GIQ",
  ylab = "Tratamiento",
  main = "Promedio en GIQ por tratamiento",
  outline = FALSE
)
stripchart(
  imm_total ~ treatment,
  data = hard,
  horizontal = TRUE,
  method = "jitter",
  pch = 21,
  bg = "#F7F7F7",
  col = adjustcolor("#202423", 0.55),
  add = TRUE
)
close_png()

open_png("03_performance_summary_vs_control.png", width = 2600, height = 1550)
perf_metrics <- c("ingame_survival_s", "kills_per_min", "inputs_per_min", "dmg_per_min", "hits_per_min", "enemies_mean")
perf_metrics <- perf_metrics[perf_metrics %in% names(hard)]
heat_rows <- list()
for (metric in perf_metrics) {
  vals <- to_num(hard[[metric]])
  control <- vals[hard$treatment == "Control"]
  pooled <- sd(vals, na.rm = TRUE)
  for (tr in treatment_levels[treatment_levels != "Control"]) {
    tr_vals <- vals[hard$treatment == tr]
    diff <- mean(tr_vals, na.rm = TRUE) - mean(control, na.rm = TRUE)
    heat_rows[[paste(metric, tr)]] <- data.frame(
      metric = metric_specs$label[match(metric, metric_specs$metric)],
      treatment = tr,
      std_diff = ifelse(is.finite(pooled) && pooled > 0, diff / pooled, NA_real_),
      mean_value = mean(tr_vals, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
}
heat <- do.call(rbind, heat_rows)
metric_order <- unique(heat$metric)
tr_order <- treatment_levels[treatment_levels != "Control"]
mat <- matrix(NA_real_, nrow = length(metric_order), ncol = length(tr_order), dimnames = list(metric_order, tr_order))
txt <- matrix("", nrow = nrow(mat), ncol = ncol(mat), dimnames = dimnames(mat))
for (i in seq_len(nrow(heat))) {
  r <- match(heat$metric[i], metric_order)
  c <- match(heat$treatment[i], tr_order)
  mat[r, c] <- heat$std_diff[i]
  txt[r, c] <- sprintf("%+.2f", heat$std_diff[i])
}
par(mar = c(9, 9, 4, 3))
breaks <- seq(-1.5, 1.5, length.out = 101)
cols <- colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100)
image(
  seq_len(ncol(mat)),
  seq_len(nrow(mat)),
  t(mat[nrow(mat):1, , drop = FALSE]),
  col = cols,
  breaks = breaks,
  axes = FALSE,
  xlab = "Tratamiento",
  ylab = "",
  main = "Sintesis de desempeno frente a Control"
)
axis(1, at = seq_len(ncol(mat)), labels = colnames(mat), las = 2)
axis(2, at = seq_len(nrow(mat)), labels = rev(rownames(mat)), las = 1)
for (r in seq_len(nrow(mat))) {
  for (c in seq_len(ncol(mat))) {
    text(c, nrow(mat) - r + 1, txt[r, c], cex = 0.86)
  }
}
box()
close_png()

effect_map <- function(results, filename, title, value_col = "partial_eta_squared", p_col = "p_value", q_col = "q_value") {
  if (is.null(results) || nrow(results) == 0) return(invisible(NULL))
  plot_rows <- results[results$metric %in% primary_metrics, , drop = FALSE]
  if (nrow(plot_rows) == 0) return(invisible(NULL))
  terms <- c("Shake", "Zoom", "Recoil", "Shake:Zoom", "Shake:Recoil", "Zoom:Recoil", "Shake:Zoom:Recoil")
  metrics <- unique(plot_rows$metric_label)
  mat <- matrix(NA_real_, nrow = length(metrics), ncol = length(terms), dimnames = list(metrics, terms))
  labels <- matrix("", nrow = nrow(mat), ncol = ncol(mat), dimnames = dimnames(mat))
  for (i in seq_len(nrow(plot_rows))) {
    r <- match(plot_rows$metric_label[i], metrics)
    c <- match(plot_rows$term[i], terms)
    if (is.na(r) || is.na(c)) next
    p <- plot_rows[[p_col]][i]
    q <- if (q_col %in% names(plot_rows)) plot_rows[[q_col]][i] else NA_real_
    val <- if (value_col %in% names(plot_rows)) plot_rows[[value_col]][i] else -log10(p)
    mat[r, c] <- val
    labels[r, c] <- ifelse(is.finite(q),
      paste0("p=", fmt_p(p), "\nq=", fmt_p(q)),
      paste0("p=", fmt_p(p))
    )
  }
  open_png(filename, width = 2600, height = 1450)
  par(mar = c(7.5, 9, 4, 4))
  zlim_max <- max(mat, na.rm = TRUE)
  if (!is.finite(zlim_max) || zlim_max == 0) zlim_max <- 1
  image(
    seq_len(ncol(mat)),
    seq_len(nrow(mat)),
    t(mat[nrow(mat):1, , drop = FALSE]),
    col = hcl.colors(80, "Inferno", rev = TRUE),
    axes = FALSE,
    xlab = "Efecto factorial",
    ylab = "",
    main = title,
    zlim = c(0, zlim_max)
  )
  axis(1, at = seq_len(ncol(mat)), labels = gsub(":", " x ", terms), las = 2)
  axis(2, at = seq_len(nrow(mat)), labels = rev(metrics), las = 1)
  for (r in seq_len(nrow(mat))) {
    for (c in seq_len(ncol(mat))) {
      text(c, nrow(mat) - r + 1, labels[r, c], cex = 0.72, col = "white")
    }
  }
  box()
  close_png()
}

effect_map(anova_results, "04_factorial_anova_effect_map.png", "ANOVA factorial")
if (nrow(art_results) > 0) {
  art_results$minus_log10_p <- -log10(pmax(art_results$p_value, 1e-12))
  effect_map(art_results, "05_art_effect_map.png", "ART factorial", value_col = "minus_log10_p")
}
if (nrow(count_results) > 0) {
  count_results$minus_log10_p <- -log10(pmax(count_results$p_value, 1e-12))
  effect_map(count_results, "06_count_model_effect_map.png", "Modelos de conteo", value_col = "minus_log10_p")
}

open_png("07_baseline_forest_vs_control.png", width = 2600, height = 1700)
plot_base <- baseline[is.finite(baseline$std_diff), , drop = FALSE]
plot_base$abs_std <- abs(plot_base$std_diff)
plot_base <- plot_base[order(plot_base$p_value, -plot_base$abs_std), ]
if (nrow(plot_base) > 16) plot_base <- plot_base[seq_len(16), ]
plot_base <- plot_base[order(plot_base$std_diff), ]
par(mar = c(5.5, 15, 4, 4))
y <- seq_len(nrow(plot_base))
xlim <- range(c(plot_base$std_low, plot_base$std_high, 0), na.rm = TRUE)
plot(
  plot_base$std_diff,
  y,
  xlim = xlim,
  yaxt = "n",
  pch = 21,
  bg = ifelse(plot_base$p_value < 0.05, "#D55E00", "#56B4E9"),
  col = "#202423",
  xlab = "Diferencia estandarizada vs Control",
  ylab = "",
  main = "Contrastes contra Control"
)
segments(plot_base$std_low, y, plot_base$std_high, y, col = "#202423", lwd = 2)
abline(v = 0, lty = 2, col = "#777777")
axis(2, at = y, labels = paste(plot_base$treatment, plot_base$metric_label, sep = " · "), las = 1, cex.axis = 0.72)
text(xlim[2], y, labels = paste0("p=", fmt_p(plot_base$p_value)), adj = 1, cex = 0.72)
mtext("Valores positivos indican mayor valor que Control", side = 1, line = 4, cex = 0.82)
box()
close_png()

open_png("08_tradeoff_giq_performance.png", width = 2500, height = 1450)
plot_trade <- tradeoff[order(tradeoff$rho), , drop = FALSE]
par(mar = c(6, 9, 4, 3))
y <- seq_len(nrow(plot_trade))
plot(
  plot_trade$rho,
  y,
  xlim = c(-1, 1),
  yaxt = "n",
  pch = 21,
  bg = ifelse(plot_trade$q_value < 0.05, "#009E73", "#F0E442"),
  col = "#202423",
  xlab = "Correlacion Spearman con Promedio en GIQ",
  ylab = "",
  main = "Relacion inmersion-desempeno"
)
abline(v = 0, lty = 2, col = "#777777")
axis(2, at = y, labels = plot_trade$metric_label, las = 1)
text(plot_trade$rho, y, labels = paste0("  p=", fmt_p(plot_trade$p_value), "; q=", fmt_p(plot_trade$q_value)), adj = 0, cex = 0.78)
box()
close_png()

# -------------------------------------------------------------------------
# Summary.
# -------------------------------------------------------------------------

nominal <- anova_results[is.finite(anova_results$p_value) & anova_results$p_value < 0.05, ]
nominal <- nominal[order(nominal$p_value), , drop = FALSE]
fdr <- anova_results[is.finite(anova_results$q_value) & anova_results$q_value < 0.05, ]
fdr <- fdr[order(fdr$q_value), , drop = FALSE]
art_fdr <- if (nrow(art_results) > 0) {
  art_results[is.finite(art_results$q_value) & art_results$q_value < 0.05, , drop = FALSE]
} else {
  data.frame()
}
count_fdr <- if (nrow(count_results) > 0) {
  count_results[is.finite(count_results$q_value) & count_results$q_value < 0.05, , drop = FALSE]
} else {
  data.frame()
}
tradeoff_fdr <- if (nrow(tradeoff) > 0) {
  tradeoff[is.finite(tradeoff$q_value) & tradeoff$q_value < 0.05, , drop = FALSE]
} else {
  data.frame()
}

condition_counts <- as.data.frame(table(hard$treatment), stringsAsFactors = FALSE)
names(condition_counts) <- c("treatment", "n")

summary_lines <- c(
  "Fresh camera juiciness analysis",
  "===============================",
  sprintf("Source: %s", data_path),
  sprintf("Rows in identified CSV: %d", nrow(raw)),
  sprintf("Unique players in identified CSV: %d", length(unique(raw$player_id))),
  sprintf("Hard Clean removed rows: %d", nrow(removed)),
  sprintf("Hard Clean retained rows: %d", nrow(hard)),
  sprintf("Duplicated players retained after Hard Clean: %d", sum(duplicated(hard$player_id))),
  "",
  "Hard Clean rules:",
  "  - duplicate player rows beyond the first occurrence",
  "  - ingame_survival_s < 30",
  "  - input_total <= 0",
  "  - total_kills <= 0 when ingame_survival_s >= 120",
  "  - fps_mean < 15, used only as technical quality control",
  "",
  "Condition counts after Hard Clean:",
  paste(sprintf("  - %s: %d", condition_counts$treatment, condition_counts$n), collapse = "\n"),
  "",
  "Rmd viability check:",
  "  - Viable as a preliminary GIQ analysis: it uses factorial linear models, residual inspection, variance checks, and emmeans for interactions.",
  "  - Incomplete for the full paper: it does not apply Hard Clean, does not model gameplay telemetry, does not use count models with duration offsets, does not test GIQ-performance trade-offs, and uses ad-hoc removal of low GIQ attraction values.",
  "  - This script keeps the useful Rmd structure as a sensitivity path but uses predefined Hard Clean criteria and adds the missing RQ2/RQ3 analyses.",
  "",
  "Assumption notes:",
  paste(
    sprintf(
      "  - %s (%s): Shapiro p=%s; Brown-Forsythe p=%s",
      assumptions$metric_label,
      assumptions$model_scale,
      fmt_p(assumptions$shapiro_residual_p),
      fmt_p(assumptions$brown_forsythe_p)
    ),
    collapse = "\n"
  ),
  "",
  "Nominal ANOVA effects (p < .05):",
  if (nrow(nominal) == 0) {
    "  None."
  } else {
    paste(
      sprintf(
        "  - %s / %s: F=%.2f, p=%s, q=%s, eta_p2=%.3f",
        nominal$metric_label,
        term_label(nominal$term),
        nominal$f_value,
        fmt_p(nominal$p_value),
        fmt_p(nominal$q_value),
        nominal$partial_eta_squared
      ),
      collapse = "\n"
    )
  },
  "",
  "FDR-corrected ANOVA effects (q < .05):",
  if (nrow(fdr) == 0) {
    "  None."
  } else {
    paste(
      sprintf("  - %s / %s: p=%s, q=%s", fdr$metric_label, term_label(fdr$term), fmt_p(fdr$p_value), fmt_p(fdr$q_value)),
      collapse = "\n"
    )
  },
  "",
  "FDR-corrected ART effects (q < .05):",
  if (nrow(art_fdr) == 0) {
    "  None."
  } else {
    paste(
      sprintf("  - %s / %s: F=%.2f, p=%s, q=%s", art_fdr$metric_label, term_label(art_fdr$term), art_fdr$f_value, fmt_p(art_fdr$p_value), fmt_p(art_fdr$q_value)),
      collapse = "\n"
    )
  },
  "",
  "FDR-corrected count-model effects (q < .05):",
  if (nrow(count_fdr) == 0) {
    "  None."
  } else {
    paste(
      sprintf("  - %s / %s: p=%s, q=%s", count_fdr$metric_label, term_label(count_fdr$term), fmt_p(count_fdr$p_value), fmt_p(count_fdr$q_value)),
      collapse = "\n"
    )
  },
  "",
  "FDR-corrected GIQ-performance correlations (q < .05):",
  if (nrow(tradeoff_fdr) == 0) {
    "  None."
  } else {
    paste(
      sprintf("  - Promedio en GIQ vs %s: rho=%.3f, p=%s, q=%s", tradeoff_fdr$metric_label, tradeoff_fdr$rho, fmt_p(tradeoff_fdr$p_value), fmt_p(tradeoff_fdr$q_value)),
      collapse = "\n"
    )
  },
  "",
  "Generated figures:",
  "  - analysis/images/fresh_camera_figures/01_hard_clean_flow.png",
  "  - analysis/images/fresh_camera_figures/02_giq_by_treatment.png",
  "  - analysis/images/fresh_camera_figures/03_performance_summary_vs_control.png",
  "  - analysis/images/fresh_camera_figures/04_factorial_anova_effect_map.png",
  if (nrow(art_results) > 0) "  - analysis/images/fresh_camera_figures/05_art_effect_map.png" else NULL,
  if (nrow(count_results) > 0) "  - analysis/images/fresh_camera_figures/06_count_model_effect_map.png" else NULL,
  "  - analysis/images/fresh_camera_figures/07_baseline_forest_vs_control.png",
  "  - analysis/images/fresh_camera_figures/08_tradeoff_giq_performance.png"
)

writeLines(summary_lines, file.path(out_data_dir, "fresh_camera_summary.txt"))
cat(paste(summary_lines, collapse = "\n"), "\n")
