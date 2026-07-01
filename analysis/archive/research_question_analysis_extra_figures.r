# Canonical research-question analysis for the camera juiciness experiment.
#
# This script follows the useful order from programajuic.rmd:
#   1. Fit factorial models for GIQ.
#   2. Check residual normality and variance homogeneity.
#   3. Use interaction follow-up only when the factorial model supports it.
#
# It then adds the missing pieces required by the research questions:
#   - predefined quality flags;
#   - motor-performance telemetry;
#   - count models with duration offsets;
#   - baseline contrasts and GIQ-performance trade-off tests.
#
# Outputs:
#   analysis/results/research_questions/*.csv|txt|md
#   analysis/images/research_questions/*.png

suppressPackageStartupMessages({
  library(stats)
  library(grDevices)
  library(graphics)
})

set.seed(20260627)

find_project_root <- function() {
  candidates <- unique(normalizePath(c(getwd(), dirname(getwd())), winslash = "/", mustWork = TRUE))
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "analysis", "research_question_analysis.R"))) {
      return(candidate)
    }
  }
  stop("Could not find project root containing analysis/research_question_analysis.R.")
}

project_root <- find_project_root()
project_file <- function(...) file.path(project_root, ...)
project_rel <- function(path) {
  norm_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  norm_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  sub(paste0("^", norm_root, "/?"), "", norm_path)
}

input_dir <- project_file("analysis", "data")
out_dir <- project_file("analysis", "results", "research_questions")
fig_dir <- project_file("analysis", "images", "research_questions")
scratch_fig_dir <- file.path(tempdir(), "juiciness_research_question_scratch")

paths <- list(
  input_dataset = file.path(input_dir, "juiciness_clean_dataset.csv"),
  results = out_dir,
  figures = fig_dir,
  scratch_figures = scratch_fig_dir
)
final_figure_files <- c(
  "RQ1_immersion_giq_effects.png",
  "RQ1_immersion_vs_control_forest.png",
  "RQ2_damage_per_min.png",
  "RQ2_hits_per_min.png",
  "RQ2_factor_effects_forest.png",
  "RQ2_count_models_duration_offset.png",
  "RQ2_count_model_rate_ratios.png",
  "RQ3_baseline_tradeoff_forest.png",
  "RQ3_tradeoff_quadrants.png",
  "RQ3_giq_performance_correlation.png"
)

if (!file.exists(paths$input_dataset)) {
  stop("Missing input CSV: ", paths$input_dataset)
}
data_path <- paths$input_dataset

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(scratch_fig_dir, recursive = TRUE, showWarnings = FALSE)

has_emmeans <- requireNamespace("emmeans", quietly = TRUE)
has_art <- requireNamespace("ARTool", quietly = TRUE)
has_mass <- requireNamespace("MASS", quietly = TRUE)

to_num <- function(x) suppressWarnings(as.numeric(x))

fmt_p <- function(p) {
  ifelse(!is.finite(p), "NA", ifelse(p < 0.001, "<.001", sprintf("%.3f", p)))
}

fmt_num <- function(x, digits = 2) {
  ifelse(!is.finite(x), "NA", formatC(x, format = "f", digits = digits))
}

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
  "Control", "Shake", "Zoom", "Shake + Zoom", "Recoil",
  "Shake + Recoil", "Zoom + Recoil", "Shake + Zoom + Recoil"
)
isolated_treatments <- c("Control", "Shake", "Zoom", "Recoil")
factor_terms <- c("Shake", "Zoom", "Recoil", "Shake:Zoom", "Shake:Recoil", "Zoom:Recoil", "Shake:Zoom:Recoil")

metric_specs <- data.frame(
  metric = c(
    "imm_total",
    "ingame_survival_s",
    "kills_per_min",
    "dmg_per_min",
    "hits_per_min",
    "enemies_mean"
  ),
  label = c(
    "Inmersi\u00f3n percibida",
    "Supervivencia (s)",
    "Kills/min",
    "Da\u00f1o recibido por minuto",
    "Golpes recibidos por minuto",
    "Enemigos promedio"
  ),
  rq = c("RQ1", rep("RQ2", 5)),
  log_model = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

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

raw$player_id <- trimws(toupper(raw$player_id))
raw$run_id <- trimws(as.character(raw$run_id))
raw$shake_num <- as.integer(to_num(raw$shake))
raw$zoom_num <- as.integer(to_num(raw$zoom))
raw$recoil_num <- as.integer(to_num(raw$recoil))
raw$duration_min <- to_num(raw$ingame_survival_s) / 60
raw$hits_total_est <- round(to_num(raw$hits_per_min) * raw$duration_min)
raw$damage_amount_total_est <- to_num(raw$dmg_per_min) * raw$duration_min
raw$treatment <- factor(
  treatment_from_flags(raw$shake_num, raw$zoom_num, raw$recoil_num),
  levels = treatment_levels
)
raw$Shake <- factor(raw$shake_num, levels = c(0, 1), labels = c("Ausente", "Presente"))
raw$Zoom <- factor(raw$zoom_num, levels = c(0, 1), labels = c("Ausente", "Presente"))
raw$Recoil <- factor(raw$recoil_num, levels = c(0, 1), labels = c("Ausente", "Presente"))

# -------------------------------------------------------------------------
# 1. Juiciness clean dataset and quality audit.
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
  "A participant must contribute only one gameplay run."
)
add_flag(
  which(is.finite(raw$duration_min) & raw$duration_min * 60 < 30),
  "duration_lt_30s",
  "ingame_survival_s",
  raw$ingame_survival_s[is.finite(raw$duration_min) & raw$duration_min * 60 < 30],
  ">= 30 seconds",
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
  "Run has severe average frame-rate instability; FPS is only a technical quality criterion."
)

flags <- if (length(flag_rows) > 0) do.call(rbind, flag_rows) else data.frame()
quality_flag_rows <- sort(unique(flags$row_number))
removed <- raw[FALSE, , drop = FALSE]
hard <- raw

write.csv(flags, file.path(out_dir, "juiciness_clean_quality_flags.csv"), row.names = FALSE, na = "")
write.csv(removed, file.path(out_dir, "juiciness_clean_removed.csv"), row.names = FALSE, na = "")
write.csv(hard, file.path(out_dir, "juiciness_clean_dataset.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Helpers for factorial models.
# -------------------------------------------------------------------------

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
    attr(temp, "model_scale") <- "log1p"
  } else {
    temp$Y_model <- temp$Y_raw
    attr(temp, "model_scale") <- "raw"
  }
  temp
}

brown_forsythe_p <- function(df) {
  cell <- interaction(df$Shake, df$Zoom, df$Recoil, drop = TRUE)
  med <- ave(df$Y_model, cell, FUN = median, na.rm = TRUE)
  fit <- aov(abs(df$Y_model - med) ~ cell)
  as.numeric(summary(fit)[[1]]$`Pr(>F)`[1])
}

extract_anova <- function(fit, metric, label, rq, model_scale) {
  tab <- as.data.frame(anova(fit))
  tab$term <- trimws(rownames(tab))
  rownames(tab) <- NULL
  residual_ss <- tab$`Sum Sq`[tab$term == "Residuals"]
  residual_df <- tab$Df[tab$term == "Residuals"]
  tab <- tab[tab$term != "Residuals", , drop = FALSE]
  data.frame(
    rq = rq,
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

mean_ci <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(mean = NA_real_, lo = NA_real_, hi = NA_real_, n = 0))
  se <- sd(x) / sqrt(length(x))
  margin <- qt(0.975, df = max(length(x) - 1, 1)) * se
  c(mean = mean(x), lo = mean(x) - margin, hi = mean(x) + margin, n = length(x))
}

# -------------------------------------------------------------------------
# 2. RQ1/RQ2 factorial models and assumption checks.
# -------------------------------------------------------------------------

assumption_rows <- list()
anova_rows <- list()
rmd_rows <- list()
emmeans_rows <- list()
art_rows <- list()

for (i in seq_len(nrow(metric_specs))) {
  spec <- metric_specs[i, ]
  temp <- prep_metric_df(spec$metric, spec$log_model)
  if (nrow(temp) < 16) next
  model_scale <- attr(temp, "model_scale")

  fit_main <- lm(Y_model ~ Shake + Zoom + Recoil, data = temp)
  fit_two_way <- lm(Y_model ~ Shake * Zoom + Shake * Recoil + Zoom * Recoil, data = temp)
  fit_full <- lm(Y_model ~ Shake * Zoom * Recoil, data = temp)

  comp <- as.data.frame(anova(fit_main, fit_two_way, fit_full))
  comp$model <- c("main_effects", "two_way_interactions_rmd", "full_three_way")
  comp$rq <- spec$rq
  comp$metric <- spec$metric
  comp$metric_label <- spec$label
  comp$model_scale <- model_scale
  rmd_rows[[spec$metric]] <- comp[
    ,
    c("rq", "metric", "metric_label", "model_scale", "model", "Res.Df", "RSS", "Df", "Sum of Sq", "F", "Pr(>F)")
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

  assumption_rows[[spec$metric]] <- data.frame(
    rq = spec$rq,
    metric = spec$metric,
    metric_label = spec$label,
    model_scale = model_scale,
    n = nrow(temp),
    duplicated_players = sum(duplicated(temp$player_id)),
    shapiro_residual_p = shapiro_p,
    bartlett_p = bartlett_p,
    brown_forsythe_p = bf_p,
    normality_ok = is.finite(shapiro_p) & shapiro_p >= 0.05,
    variance_ok_brown_forsythe = is.finite(bf_p) & bf_p >= 0.05,
    primary_parametric_ok = is.finite(shapiro_p) & shapiro_p >= 0.05 & is.finite(bf_p) & bf_p >= 0.05,
    stringsAsFactors = FALSE
  )

  model_anova <- extract_anova(fit_full, spec$metric, spec$label, spec$rq, model_scale)
  anova_rows[[spec$metric]] <- model_anova

  if (has_art) {
    art_fit <- tryCatch(ARTool::art(Y_raw ~ Shake * Zoom * Recoil, data = temp), error = function(e) NULL)
    if (!is.null(art_fit)) {
      tab <- tryCatch(as.data.frame(anova(art_fit)), error = function(e) NULL)
      if (!is.null(tab) && nrow(tab) > 0) {
        tab$term <- trimws(rownames(tab))
        rownames(tab) <- NULL
        f_col <- intersect(c("F", "F value", "F.value"), names(tab))[1]
        p_col <- intersect(c("Pr(>F)", "Pr..F.", "p.value"), names(tab))[1]
        df_col <- intersect(c("Df", "Df1"), names(tab))[1]
        df_res_col <- intersect(c("Df.res", "Df2", "Den Df"), names(tab))[1]
        if (!is.na(f_col) && !is.na(p_col)) {
          art_rows[[spec$metric]] <- data.frame(
            rq = spec$rq,
            metric = spec$metric,
            metric_label = spec$label,
            term = tab$term,
            df_effect = if (!is.na(df_col)) tab[[df_col]] else NA_real_,
            df_residual = if (!is.na(df_res_col)) tab[[df_res_col]] else NA_real_,
            f_value = tab[[f_col]],
            p_value = tab[[p_col]],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  interaction_hits <- model_anova[
    grepl(":", model_anova$term) & is.finite(model_anova$p_value) & model_anova$p_value < 0.05,
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
        contrasts$rq <- spec$rq
        contrasts$metric <- spec$metric
        contrasts$metric_label <- spec$label
        contrasts$model_scale <- model_scale
        contrasts$trigger_term <- term
        emmeans_rows[[paste(spec$metric, term, sep = "_")]] <- contrasts
      }
    }
  }
}

assumptions <- do.call(rbind, assumption_rows)
anova_results <- do.call(rbind, anova_rows)
anova_results$q_value <- p.adjust(anova_results$p_value, method = "BH")
rmd_model_sequence <- do.call(rbind, rmd_rows)
emmeans_results <- if (length(emmeans_rows) > 0) do.call(rbind, emmeans_rows) else data.frame()
art_results <- if (length(art_rows) > 0) do.call(rbind, art_rows) else data.frame()
if (nrow(art_results) > 0) {
  art_results <- art_results[!grepl("Residual", art_results$term, ignore.case = TRUE), , drop = FALSE]
  art_results$q_value <- p.adjust(art_results$p_value, method = "BH")
}

write.csv(assumptions, file.path(out_dir, "01_assumption_checks.csv"), row.names = FALSE, na = "")
write.csv(rmd_model_sequence, file.path(out_dir, "02_rmd_style_model_sequence.csv"), row.names = FALSE, na = "")
write.csv(anova_results, file.path(out_dir, "03_factorial_anova.csv"), row.names = FALSE, na = "")
write.csv(art_results, file.path(out_dir, "04_art_factorial_sensitivity.csv"), row.names = FALSE, na = "")
write.csv(emmeans_results, file.path(out_dir, "05_interaction_emmeans_followup.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# 3. Count models for event totals with duration offsets.
# -------------------------------------------------------------------------

count_specs <- data.frame(
  metric = c("total_kills", "input_total", "hits_total_est"),
  label = c(
    "Kills totales ajustados por duraci\u00f3n de partida",
    "Inputs totales ajustados por duraci\u00f3n de partida",
    "Golpes totales ajustados por duraci\u00f3n de partida"
  ),
  stringsAsFactors = FALSE
)
count_rows <- list()
if (has_mass) {
  for (i in seq_len(nrow(count_specs))) {
    spec <- count_specs[i, ]
    if (!spec$metric %in% names(hard)) next
    temp <- hard
    temp$Y <- round(to_num(temp[[spec$metric]]))
    temp$duration_min <- to_num(temp$duration_min)
    temp <- temp[
      is.finite(temp$Y) & temp$Y >= 0 &
        is.finite(temp$duration_min) & temp$duration_min > 0,
      ,
      drop = FALSE
    ]
    if (nrow(temp) < 16) next
    fit <- tryCatch(MASS::glm.nb(Y ~ Shake + Zoom + Recoil + offset(log(duration_min)), data = temp), error = function(e) NULL)
    if (is.null(fit)) next
    tab <- tryCatch(as.data.frame(drop1(fit, test = "Chisq")), error = function(e) NULL)
    if (is.null(tab)) next
    tab$term <- trimws(rownames(tab))
    rownames(tab) <- NULL
    tab <- tab[tab$term != "<none>", , drop = FALSE]
    p_col <- intersect(c("Pr(>Chi)", "Pr..Chi."), names(tab))[1]
    stat_col <- intersect(c("LRT", "LR stat.", "Chi-square"), names(tab))[1]
    if (is.na(p_col)) next
    count_rows[[spec$metric]] <- data.frame(
      rq = "RQ2",
      metric = spec$metric,
      metric_label = spec$label,
      model = "negative binomial main effects with duration offset",
      term = tab$term,
      statistic = if (!is.na(stat_col)) tab[[stat_col]] else NA_real_,
      p_value = tab[[p_col]],
      theta = fit$theta,
      stringsAsFactors = FALSE
    )
  }
}
count_results <- if (length(count_rows) > 0) do.call(rbind, count_rows) else data.frame()
if (nrow(count_results) > 0) count_results$q_value <- p.adjust(count_results$p_value, method = "BH")
write.csv(count_results, file.path(out_dir, "06_count_models_duration_offset.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# 4. RQ3 baseline contrasts and GIQ-performance correlations.
# -------------------------------------------------------------------------

baseline_metrics <- metric_specs$metric
baseline_rows <- list()
for (metric in baseline_metrics) {
  spec <- metric_specs[metric_specs$metric == metric, , drop = FALSE]
  temp <- prep_metric_df(metric, spec$log_model)
  temp <- temp[temp$treatment %in% isolated_treatments, , drop = FALSE]
  temp$treatment <- factor(as.character(temp$treatment), levels = isolated_treatments)
  if (nrow(temp) < 12) next
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
  for (tr in isolated_treatments[isolated_treatments != "Control"]) {
    vals <- temp$Y_raw[temp$treatment == tr]
    if (length(vals) < 2 || length(control_vals) < 2) next
    pooled_sd <- sqrt(((length(vals) - 1) * var(vals) + (length(control_vals) - 1) * var(control_vals)) /
      (length(vals) + length(control_vals) - 2))
    diff_raw <- mean(vals) - mean(control_vals)
    std_diff <- ifelse(is.finite(pooled_sd) && pooled_sd > 0, diff_raw / pooled_sd, NA_real_)
    se_raw <- sqrt(var(vals) / length(vals) + var(control_vals) / length(control_vals))
    ci_raw <- diff_raw + c(-1, 1) * qt(0.975, df = max(length(vals) + length(control_vals) - 2, 1)) * se_raw
    ci_std <- if (is.finite(pooled_sd) && pooled_sd > 0) ci_raw / pooled_sd else c(NA_real_, NA_real_)
    p_value <- NA_real_
    if (!is.null(emm_p)) {
      hit <- emm_p[grepl(tr, emm_p$contrast, fixed = TRUE), , drop = FALSE]
      if (nrow(hit) > 0 && "p.value" %in% names(hit)) p_value <- hit$p.value[1]
    }
    baseline_rows[[paste(metric, tr, sep = "_")]] <- data.frame(
      rq = "RQ3",
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
}
baseline <- if (length(baseline_rows) > 0) do.call(rbind, baseline_rows) else data.frame()
if (nrow(baseline) > 0) baseline$q_value <- p.adjust(baseline$p_value, method = "BH")
write.csv(baseline, file.path(out_dir, "07_rq3_isolated_vs_control.csv"), row.names = FALSE, na = "")

tradeoff_metrics <- metric_specs$metric[metric_specs$rq == "RQ2"]
tradeoff_rows <- list()
for (metric in tradeoff_metrics) {
  temp <- hard
  x <- to_num(temp$imm_total)
  y <- to_num(temp[[metric]])
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 8) next
  ct <- suppressWarnings(cor.test(x[keep], y[keep], method = "spearman", exact = FALSE))
  tradeoff_rows[[metric]] <- data.frame(
    rq = "RQ3",
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
write.csv(tradeoff, file.path(out_dir, "08_rq3_giq_performance_spearman.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# Derived summaries and scratch diagnostics.
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

open_png <- function(filename, width = 2600, height = 1500, res = 200, directory = fig_dir) {
  png(file.path(directory, filename), width = width, height = height, res = res)
  par(bg = "white", fg = "#202423", col.axis = "#202423", col.lab = "#202423",
      col.main = "#202423", family = "sans")
}
close_png <- function() dev.off()

draw_title <- function(title, subtitle = "") {
  title(title, adj = 0, cex.main = 1.35, font.main = 2, line = 1)
  if (nzchar(subtitle)) {
    mtext(subtitle, side = 3, adj = 0, line = -0.15, cex = 0.84, col = "#52635B")
  }
}

factor_effect_summary <- function(metric, label) {
  rows <- list()
  for (factor_name in c("Shake", "Zoom", "Recoil")) {
    vals_abs <- to_num(hard[[metric]][hard[[factor_name]] == "Ausente"])
    vals_pre <- to_num(hard[[metric]][hard[[factor_name]] == "Presente"])
    vals_abs <- vals_abs[is.finite(vals_abs)]
    vals_pre <- vals_pre[is.finite(vals_pre)]
    pooled <- sqrt(((length(vals_pre) - 1) * var(vals_pre) + (length(vals_abs) - 1) * var(vals_abs)) /
      (length(vals_pre) + length(vals_abs) - 2))
    rows[[factor_name]] <- data.frame(
      metric = metric,
      metric_label = label,
      factor = factor_name,
      absent_mean = mean(vals_abs),
      present_mean = mean(vals_pre),
      std_diff = ifelse(is.finite(pooled) && pooled > 0, (mean(vals_pre) - mean(vals_abs)) / pooled, NA_real_),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

# RQ1 derived summaries: GIQ factor effects plus the Rmd-relevant Shake x Zoom interaction.
rq1_effect <- factor_effect_summary("imm_total", "Promedio en GIQ")
write.csv(rq1_effect, file.path(out_dir, "09_rq1_factor_mean_differences.csv"), row.names = FALSE, na = "")

open_png("RQ1_immersion_factor_diagnostics.png", width = 2700, height = 1500, directory = scratch_fig_dir)
layout(matrix(c(1, 2), nrow = 1), widths = c(1, 1.1))
par(mar = c(6, 5.5, 5.2, 2))
means <- rbind(
  data.frame(factor = rq1_effect$factor, level = "Ausente", mean = rq1_effect$absent_mean),
  data.frame(factor = rq1_effect$factor, level = "Presente", mean = rq1_effect$present_mean)
)
ylim <- range(means$mean, hard$imm_total, na.rm = TRUE)
plot(seq_len(nrow(rq1_effect)), rq1_effect$absent_mean, type = "n", xaxt = "n", ylim = ylim,
     xlab = "Efecto de camara", ylab = "Promedio en GIQ")
axis(1, at = seq_len(nrow(rq1_effect)), labels = rq1_effect$factor)
points(seq_len(nrow(rq1_effect)) - 0.08, rq1_effect$absent_mean, pch = 21, bg = "#C9D6DF", cex = 1.8)
points(seq_len(nrow(rq1_effect)) + 0.08, rq1_effect$present_mean, pch = 21, bg = "#0072B2", cex = 1.8)
segments(seq_len(nrow(rq1_effect)) - 0.08, rq1_effect$absent_mean,
         seq_len(nrow(rq1_effect)) + 0.08, rq1_effect$present_mean, col = "#202423", lwd = 2)
legend("bottomleft", legend = c("Ausente", "Presente"), pch = 21,
       pt.bg = c("#C9D6DF", "#0072B2"), bty = "n")
draw_title("RQ1: inmersion por efecto aislado", "Promedio marginal observado; el modelo factorial se reporta en tabla")

par(mar = c(6, 5.5, 5.2, 2))
temp <- hard[is.finite(to_num(hard$imm_total)), ]
summary_int <- aggregate(imm_total ~ Shake + Zoom, temp, mean)
plot(c(1, 2), range(temp$imm_total, na.rm = TRUE), type = "n", xaxt = "n",
     xlab = "Shake", ylab = "Promedio en GIQ")
axis(1, at = c(1, 2), labels = c("Ausente", "Presente"))
for (zoom_level in levels(temp$Zoom)) {
  part <- summary_int[summary_int$Zoom == zoom_level, ]
  x <- match(part$Shake, levels(temp$Shake))
  col <- ifelse(zoom_level == "Ausente", "#52635B", "#D55E00")
  lines(x, part$imm_total, type = "b", pch = 21, bg = col, col = col, lwd = 2)
}
legend("bottomleft", legend = paste("Zoom", levels(temp$Zoom)), col = c("#52635B", "#D55E00"),
       pt.bg = c("#52635B", "#D55E00"), pch = 21, lwd = 2, bty = "n")
draw_title("Interaccion Shake x Zoom", "Seguimiento solo porque el modelo detecta senal nominal")
close_png()

# RQ2 derived summaries: ART/factorial evidence and observed standardized direction.
rq2_effect_rows <- list()
for (metric in metric_specs$metric[metric_specs$rq == "RQ2"]) {
  spec <- metric_specs[metric_specs$metric == metric, ]
  rq2_effect_rows[[metric]] <- factor_effect_summary(metric, spec$label)
}
rq2_effect <- do.call(rbind, rq2_effect_rows)
rq2_effect$anova_p <- NA_real_
rq2_effect$anova_q <- NA_real_
rq2_effect$art_p <- NA_real_
rq2_effect$art_q <- NA_real_
for (i in seq_len(nrow(rq2_effect))) {
  hit_anova <- anova_results[
    anova_results$metric == rq2_effect$metric[i] &
      anova_results$term == rq2_effect$factor[i],
    ,
    drop = FALSE
  ]
  if (nrow(hit_anova) > 0) {
    rq2_effect$anova_p[i] <- hit_anova$p_value[1]
    rq2_effect$anova_q[i] <- hit_anova$q_value[1]
  }
  if (nrow(art_results) > 0) {
    hit_art <- art_results[
      art_results$metric == rq2_effect$metric[i] &
        art_results$term == rq2_effect$factor[i],
      ,
      drop = FALSE
    ]
    if (nrow(hit_art) > 0) {
      rq2_effect$art_p[i] <- hit_art$p_value[1]
      rq2_effect$art_q[i] <- hit_art$q_value[1]
    }
  }
}
write.csv(rq2_effect, file.path(out_dir, "10_rq2_factor_mean_differences.csv"), row.names = FALSE, na = "")

metrics <- unique(rq2_effect$metric_label)
factors <- c("Shake", "Zoom", "Recoil")
mat <- matrix(NA_real_, nrow = length(metrics), ncol = length(factors), dimnames = list(metrics, factors))
labels <- matrix("", nrow = nrow(mat), ncol = ncol(mat), dimnames = dimnames(mat))
for (i in seq_len(nrow(rq2_effect))) {
  r <- match(rq2_effect$metric_label[i], metrics)
  c <- match(rq2_effect$factor[i], factors)
  mat[r, c] <- rq2_effect$std_diff[i]
  p_use <- ifelse(is.finite(rq2_effect$art_p[i]), rq2_effect$art_p[i], rq2_effect$anova_p[i])
  q_use <- ifelse(is.finite(rq2_effect$art_q[i]), rq2_effect$art_q[i], rq2_effect$anova_q[i])
  labels[r, c] <- paste0(sprintf("%+.2f", rq2_effect$std_diff[i]), "\n", "p=", fmt_p(p_use), "\nq=", fmt_p(q_use))
}
open_png("RQ2_motor_performance_effects.png", width = 2600, height = 1500, directory = scratch_fig_dir)
par(mar = c(7.5, 9.5, 5, 3))
lim <- max(abs(mat), na.rm = TRUE)
if (!is.finite(lim) || lim == 0) lim <- 1
breaks <- seq(-lim, lim, length.out = 101)
cols <- colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100)
image(seq_len(ncol(mat)), seq_len(nrow(mat)), t(mat[nrow(mat):1, , drop = FALSE]),
      col = cols, breaks = breaks, axes = FALSE, xlab = "Efecto aislado", ylab = "")
axis(1, at = seq_len(ncol(mat)), labels = colnames(mat), cex.axis = 0.95)
axis(2, at = seq_len(nrow(mat)), labels = rev(rownames(mat)), las = 1, cex.axis = 0.86)
for (r in seq_len(nrow(mat))) {
  for (c in seq_len(ncol(mat))) {
    text(c, nrow(mat) - r + 1, labels[r, c], cex = 0.78, col = "#202423")
  }
}
box()
draw_title("RQ2: telemetria por efecto aislado", "Valor = diferencia estandarizada Presente-Ausente; p/q priorizan ART cuando esta disponible")
close_png()

# RQ2 scratch diagnostic count-model figure.
if (nrow(count_results) > 0) {
  count_plot <- count_results[count_results$term %in% c("Shake", "Zoom", "Recoil"), , drop = FALSE]
  if (nrow(count_plot) > 0) {
    open_png("RQ2_count_models_duration_offset_diagnostics.png", width = 2500, height = 1350, directory = scratch_fig_dir)
    par(mar = c(6, 10.5, 5, 3))
    count_plot$label <- paste(count_plot$metric_label, count_plot$term, sep = " · ")
    count_plot <- count_plot[order(count_plot$p_value, decreasing = TRUE), ]
    y <- seq_len(nrow(count_plot))
    x <- -log10(pmax(count_plot$p_value, 1e-12))
    plot(x, y, yaxt = "n", xlab = "-log10(p)", ylab = "", pch = 21,
         bg = ifelse(count_plot$q_value < 0.05, "#009E73", "#E69F00"), cex = 1.6)
    axis(2, at = y, labels = count_plot$label, las = 1, cex.axis = 0.82)
    abline(v = -log10(0.05), lty = 2, col = "#B2182B")
    text(x, y + 0.18, paste0("p=", fmt_p(count_plot$p_value), "; q=", fmt_p(count_plot$q_value)), cex = 0.78)
    draw_title("RQ2: conteos con offset por duracion", "Modelo binomial negativo para golpes totales ajustados por duracion")
    close_png()
  }
}

# RQ3 scratch diagnostic trade-off correlations.
open_png("RQ3_giq_performance_correlation_diagnostics.png", width = 2450, height = 1350, directory = scratch_fig_dir)
plot_trade <- tradeoff[order(tradeoff$rho), , drop = FALSE]
par(mar = c(6, 9, 5, 3))
y <- seq_len(nrow(plot_trade))
plot(plot_trade$rho, y, xlim = c(-1, 1), yaxt = "n", pch = 21,
     bg = ifelse(plot_trade$q_value < 0.05, "#009E73", "#F0E442"),
     col = "#202423", cex = 1.5,
     xlab = "Correlacion Spearman con Promedio en GIQ", ylab = "")
abline(v = 0, lty = 2, col = "#777777")
axis(2, at = y, labels = plot_trade$metric_label, las = 1, cex.axis = 0.9)
text(plot_trade$rho, y + 0.2, labels = paste0("p=", fmt_p(plot_trade$p_value), "; q=", fmt_p(plot_trade$q_value)), cex = 0.8)
draw_title("RQ3: relacion inmersion-desempeno", "Puntos verdes indican correlaciones que sobreviven FDR")
close_png()

# -------------------------------------------------------------------------
# Final RStudio-facing figures.
# The figures below are the only persistent PNG outputs for RQ1-RQ3.
# -------------------------------------------------------------------------

unlink(file.path(fig_dir, final_figure_files))

truth_order <- c(
  "Control",
  "Shake",
  "Zoom",
  "Shake + Zoom",
  "Recoil",
  "Shake + Recoil",
  "Zoom + Recoil",
  "Shake + Zoom + Recoil"
)

absent_symbol <- "\u2014"
treatment_axis_labels <- c(
  paste(absent_symbol, absent_symbol, absent_symbol, sep = "\n"),
  paste("Shake", absent_symbol, absent_symbol, sep = "\n"),
  paste(absent_symbol, "Zoom", absent_symbol, sep = "\n"),
  paste("Shake", "Zoom", absent_symbol, sep = "\n"),
  paste(absent_symbol, absent_symbol, "Recoil", sep = "\n"),
  paste("Shake", absent_symbol, "Recoil", sep = "\n"),
  paste(absent_symbol, "Zoom", "Recoil", sep = "\n"),
  paste("Shake", "Zoom", "Recoil", sep = "\n")
)
names(treatment_axis_labels) <- truth_order

box_fill <- c(
  "Control" = "#AAB4BE",
  "Shake" = "#2F83D0",
  "Zoom" = "#1FB06E",
  "Shake + Zoom" = "#08A8C8",
  "Recoil" = "#E36F43",
  "Shake + Recoil" = "#C85BB0",
  "Zoom + Recoil" = "#D8A719",
  "Shake + Zoom + Recoil" = "#8D63D2"
)
box_border <- c(
  "Control" = "#62717E",
  "Shake" = "#1265AD",
  "Zoom" = "#0B8254",
  "Shake + Zoom" = "#007E99",
  "Recoil" = "#AF4B28",
  "Shake + Recoil" = "#973C83",
  "Zoom + Recoil" = "#9C7510",
  "Shake + Zoom + Recoil" = "#6742A7"
)
ink <- "#202423"
soft_ink <- "#52635B"
grid_col <- "#DDE8DD"
minor_grid_col <- "#B9CABF"
panel_border <- "#DCE6DC"
mean_col <- "#3FA65D"
point_col <- adjustcolor("#4F5E54", alpha.f = 0.34)
add_box_footer <- function(stat_note = "", show_treatment_note = TRUE, line_start = 5.35, cex = 0.68) {
  lines <- character()
  if (show_treatment_note) {
    lines <- c(
      lines,
      paste0("Tratamientos ordenados de peor a mejor por media; etiquetas: Shake / Zoom / Recoil; ", absent_symbol, " = ausente")
    )
  }
  if (nzchar(stat_note)) lines <- c(lines, stat_note)
  for (i in seq_along(lines)) {
    mtext(lines[i], side = 1, line = line_start + (i - 1) * 0.68, cex = cex, col = soft_ink)
  }
}

axis_label <- function(x, scale_values = x) {
  finite <- scale_values[is.finite(scale_values)]
  if (length(finite) == 0) return(character(length(x)))
  fractional <- abs(finite - round(finite)) > 1e-8
  if (any(fractional)) {
    digits <- if (all(abs(finite * 2 - round(finite * 2)) < 1e-8)) {
      1
    } else if (all(abs(finite * 4 - round(finite * 4)) < 1e-8)) {
      2
    } else {
      2
    }
    return(formatC(x, format = "f", digits = digits))
  }
  out <- formatC(x, format = "fg", digits = 4)
  sub("\\.$", "", out)
}

draw_y_axis_with_minor <- function(ylim, major_n = 5, minor_n = 11) {
  major <- pretty(ylim, n = major_n)
  major <- major[major >= ylim[1] & major <= ylim[2]]
  minor <- pretty(ylim, n = minor_n)
  minor <- minor[minor >= ylim[1] & minor <= ylim[2]]
  minor_only <- minor[!minor %in% major]
  scale_values <- sort(unique(c(major, minor_only)))
  if (length(minor_only) > 0) {
    abline(h = minor_only, col = adjustcolor(minor_grid_col, alpha.f = 0.68), lwd = 0.75, lty = 3)
    axis(
      2,
      at = minor_only,
      labels = axis_label(minor_only, scale_values),
      las = 1,
      cex.axis = 0.78,
      col = panel_border,
      col.axis = adjustcolor(soft_ink, alpha.f = 0.78),
      tcl = -0.14
    )
  }
  abline(h = major, col = grid_col, lwd = 1)
  axis(2, at = major, labels = axis_label(major, scale_values), las = 1, cex.axis = 1.05, col = panel_border, col.axis = ink)
}

draw_x_axis_with_minor <- function(xlim, major_n = 5, minor_n = 11) {
  major <- pretty(xlim, n = major_n)
  major <- major[major >= xlim[1] & major <= xlim[2]]
  minor <- seq(floor(xlim[1] * 4) / 4, ceiling(xlim[2] * 4) / 4, by = 0.25)
  minor <- minor[minor >= xlim[1] & minor <= xlim[2]]
  minor_only <- minor[!minor %in% major]
  scale_values <- sort(unique(c(major, minor_only)))
  if (length(minor_only) > 0) {
    abline(v = minor_only, col = adjustcolor(minor_grid_col, alpha.f = 0.68), lwd = 0.75, lty = 3)
    axis(
      1,
      at = minor_only,
      labels = axis_label(minor_only, scale_values),
      cex.axis = 0.62,
      col = panel_border,
      col.axis = adjustcolor(soft_ink, alpha.f = 0.78),
      tcl = -0.14
    )
  }
  abline(v = major, col = grid_col, lwd = 1)
  axis(1, at = major, labels = axis_label(major, scale_values), cex.axis = 1.02, col = panel_border, col.axis = ink)
}

metric_note <- function(metric, preferred = c("art", "anova")) {
  preferred <- match.arg(preferred)
  if (preferred == "art" && nrow(art_results) > 0) {
    rows <- art_results[art_results$metric == metric & is.finite(art_results$p_value), , drop = FALSE]
    if (nrow(rows) > 0) {
      rows <- rows[order(rows$p_value), , drop = FALSE]
      return(sprintf("ART: %s p=%s, q=%s", gsub(":", " x ", rows$term[1]), fmt_p(rows$p_value[1]), fmt_p(rows$q_value[1])))
    }
  }
  rows <- anova_results[anova_results$metric == metric & is.finite(anova_results$p_value), , drop = FALSE]
  if (nrow(rows) > 0) {
    rows <- rows[order(rows$p_value), , drop = FALSE]
    return(sprintf("ANOVA: %s p=%s, q=%s", gsub(":", " x ", rows$term[1]), fmt_p(rows$p_value[1]), fmt_p(rows$q_value[1])))
  }
  ""
}

count_note <- function(metric) {
  rows <- count_results[count_results$metric == metric & is.finite(count_results$p_value), , drop = FALSE]
  if (nrow(rows) == 0) return("")
  rows <- rows[order(rows$p_value), , drop = FALSE]
  sprintf("Modelo NB con offset: %s p=%s, q=%s", rows$term[1], fmt_p(rows$p_value[1]), fmt_p(rows$q_value[1]))
}

plot_df <- function(metric) {
  temp <- data.frame(
    treatment = factor(as.character(hard$treatment), levels = truth_order),
    value = to_num(hard[[metric]]),
    stringsAsFactors = FALSE
  )
  temp[is.finite(temp$value) & !is.na(temp$treatment), , drop = FALSE]
}

metric_higher_is_better <- function(metric) {
  metric %in% c("imm_total", "ingame_survival_s", "kills_per_min", "total_kills")
}

metric_order <- function(temp, metric) {
  means <- tapply(temp$value, temp$treatment, mean, na.rm = TRUE)
  names(sort(means, decreasing = !metric_higher_is_better(metric), na.last = TRUE))
}

draw_legacy_box <- function(metric, ylab, title_text = "", subtitle = "", ylim = NULL,
                            x_note = TRUE, title_cex = 1.34, sort_by_mean = TRUE) {
  temp <- plot_df(metric)
  if (nrow(temp) == 0) return(invisible(NULL))
  level_order <- if (sort_by_mean) metric_order(temp, metric) else truth_order
  temp$treatment <- factor(as.character(temp$treatment), levels = level_order)
  by_level <- split(temp$value, temp$treatment, drop = FALSE)
  ci <- do.call(rbind, lapply(by_level, mean_ci))
  if (is.null(ylim)) {
    ylim <- range(c(temp$value, ci[, "lo"], ci[, "hi"]), na.rm = TRUE)
    pad <- diff(ylim) * 0.12
    if (!is.finite(pad) || pad == 0) pad <- max(abs(ylim), 1, na.rm = TRUE) * 0.08
    ylim <- ylim + c(-pad, pad)
  }

  plot(
    seq_along(level_order),
    rep(NA_real_, length(level_order)),
    type = "n",
    xlim = c(0.45, length(level_order) + 0.55),
    ylim = ylim,
    axes = FALSE,
    xlab = "",
    ylab = ylab,
    cex.lab = 1.18
  )
  draw_y_axis_with_minor(ylim)
  boxplot(
    value ~ treatment,
    data = temp,
    at = seq_along(level_order),
    add = TRUE,
    axes = FALSE,
    outline = FALSE,
    boxwex = 0.50,
    col = adjustcolor(box_fill[level_order], alpha.f = 0.42),
    border = box_border[level_order],
    medcol = ink,
    whiskcol = box_border[level_order],
    staplecol = box_border[level_order],
    boxlwd = 1.45,
    whisklwd = 1.45,
    staplelwd = 1.45,
    medlwd = 2.25,
    boxlty = 1,
    whisklty = 1,
    staplelty = 1
  )
  stripchart(
    value ~ treatment,
    data = temp,
    at = seq_along(level_order),
    vertical = TRUE,
    method = "jitter",
    jitter = 0.12,
    pch = 16,
    cex = 0.48,
    col = point_col,
    add = TRUE
  )
  x <- seq_along(level_order)
  segments(x, ci[, "lo"], x, ci[, "hi"], col = ink, lwd = 2.1)
  points(x, ci[, "mean"], pch = 21, bg = mean_col, col = ink, cex = 1.45, lwd = 1.05)
  axis(
    1,
    at = seq_along(level_order),
    labels = treatment_axis_labels[level_order],
    tick = FALSE,
    cex.axis = 0.90,
    line = 1.18
  )
  box(col = panel_border, lwd = 1.15)
  if (nzchar(title_text)) title(title_text, adj = 0, cex.main = title_cex, font.main = 2, line = 1.45)
  add_box_footer(subtitle, show_treatment_note = x_note)
}

open_png("RQ1_immersion_giq_effects.png", width = 3000, height = 1688)
par(mar = c(10.7, 7.4, 4.8, 2.0), mgp = c(3.85, 0.78, 0), tcl = -0.25)
draw_legacy_box(
  "imm_total",
  "Inmersi\u00f3n percibida (1-5)",
  "Inmersi\u00f3n percibida",
  metric_note("imm_total", "anova"),
  ylim = c(1, 5)
)
close_png()

open_png("RQ2_damage_per_min.png", width = 3000, height = 1688)
par(mar = c(10.7, 7.4, 4.8, 2.0), mgp = c(3.85, 0.78, 0), tcl = -0.25)
draw_legacy_box(
  "dmg_per_min",
  "Da\u00f1o/min",
  "Da\u00f1o recibido por minuto",
  metric_note("dmg_per_min", "art"),
  title_cex = 1.34
)
close_png()

open_png("RQ2_hits_per_min.png", width = 3000, height = 1688)
par(mar = c(10.7, 7.4, 4.8, 2.0), mgp = c(3.85, 0.78, 0), tcl = -0.25)
draw_legacy_box(
  "hits_per_min",
  "Golpes/min",
  "Golpes recibidos por minuto",
  metric_note("hits_per_min", "art"),
  title_cex = 1.34
)
close_png()

open_png("RQ2_count_models_duration_offset.png", width = 3000, height = 1688)
par(mar = c(10.7, 7.4, 4.8, 2.0), mgp = c(3.85, 0.78, 0), tcl = -0.25)
draw_legacy_box(
  "hits_total_est",
  "Golpes totales ajustados por duraci\u00f3n de partida",
  "Golpes totales ajustados por duraci\u00f3n de partida",
  count_note("hits_total_est")
)
close_png()

baseline_focus_metrics <- c("imm_total", "dmg_per_min", "hits_per_min")
baseline_focus_labels <- c("Inmersi\u00f3n percibida", "Da\u00f1o/min", "Golpes/Min")
baseline_focus <- baseline[
  baseline$metric %in% baseline_focus_metrics &
    baseline$treatment %in% isolated_treatments[-1] &
    is.finite(baseline$std_diff),
  ,
  drop = FALSE
]
baseline_focus$metric_label <- baseline_focus_labels[match(baseline_focus$metric, baseline_focus_metrics)]
baseline_focus$treatment <- factor(baseline_focus$treatment, levels = isolated_treatments[-1])
baseline_focus$metric <- factor(baseline_focus$metric, levels = baseline_focus_metrics)
baseline_focus <- baseline_focus[order(baseline_focus$metric, baseline_focus$treatment), ]

open_png("RQ3_baseline_tradeoff_forest.png", width = 3000, height = 1180)
op <- par(
  mfrow = c(1, 3),
  mar = c(3.35, 5.15, 2.55, 1.05),
  oma = c(2.9, 0, 4.55, 0),
  mgp = c(2.05, 0.66, 0),
  tcl = -0.25
)
if (nrow(baseline_focus) > 0) {
  xlim <- range(c(baseline_focus$std_low, baseline_focus$std_high, 0), na.rm = TRUE)
  pad <- diff(xlim) * 0.18
  if (!is.finite(pad) || pad == 0) pad <- 0.35
  xlim <- xlim + c(-pad, pad)
  y_levels <- rev(isolated_treatments[-1])
  y_pos <- seq_along(y_levels)
  names(y_pos) <- y_levels
  for (metric_name in baseline_focus_metrics) {
    panel <- baseline_focus[as.character(baseline_focus$metric) == metric_name, , drop = FALSE]
    plot(
      NA,
      NA,
      xlim = xlim,
      ylim = c(0.78, length(y_levels) + 0.22),
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = baseline_focus_labels[match(metric_name, baseline_focus_metrics)],
      cex.main = 1.16
    )
    x_major <- pretty(xlim, n = 5)
    x_major <- x_major[x_major >= xlim[1] & x_major <= xlim[2]]
    abline(v = x_major, col = grid_col, lwd = 1)
    axis(1, at = x_major, labels = axis_label(x_major, x_major), cex.axis = 0.98, col = panel_border, col.axis = ink)
    abline(v = 0, lty = 2, col = "#8A918B", lwd = 1.1)
    abline(h = y_pos, col = adjustcolor(grid_col, alpha.f = 0.62), lwd = 1)
    y <- y_pos[as.character(panel$treatment)]
    segments(panel$std_low, y, panel$std_high, y, col = ink, lwd = 2)
    points(
      panel$std_diff,
      y,
      pch = 21,
      bg = adjustcolor(box_fill[as.character(panel$treatment)], alpha.f = 0.68),
      col = box_border[as.character(panel$treatment)],
      cex = 1.55,
      lwd = 1.2
    )
    if (any(is.finite(panel$q_value) & panel$q_value < 0.05)) {
      sig <- panel[is.finite(panel$q_value) & panel$q_value < 0.05, , drop = FALSE]
      sig_y <- y_pos[as.character(sig$treatment)]
      text(
        sig$std_high,
        sig_y,
        labels = paste0("p=", fmt_p(sig$p_value), " q=", fmt_p(sig$q_value)),
        pos = 4,
        cex = 0.70,
        col = soft_ink
      )
    }
    axis(2, at = y_pos, labels = names(y_pos), las = 1, tick = FALSE, cex.axis = 1.04)
    box(col = panel_border, lwd = 1.15)
  }
}
mtext("RQ3: diferencias frente al control en inmersi\u00f3n, da\u00f1o y golpes recibidos", side = 3, outer = TRUE, adj = 0.02, line = 2.75, cex = 1.25, font = 2, col = ink)
mtext("0 indica ausencia de diferencia respecto a Control", side = 3, outer = TRUE, adj = 0.02, line = 1.52, cex = 0.88, col = soft_ink)
mtext("Diferencia estandarizada frente a Control", side = 1, outer = TRUE, line = 0.8, cex = 0.96, col = ink)
par(op)
close_png()

open_png("RQ3_giq_performance_correlation.png", width = 3200, height = 1800)
par(mfrow = c(1, 2), mar = c(10.7, 6.8, 4.8, 1.7), mgp = c(3.65, 0.78, 0), tcl = -0.25)
trade_hit <- tradeoff[tradeoff$metric == "hits_per_min", , drop = FALSE]
trade_note <- if (nrow(trade_hit) > 0) {
  sprintf("Spearman con Golpes/min: rho=%.2f, p=%s, q=%s", trade_hit$rho[1], fmt_p(trade_hit$p_value[1]), fmt_p(trade_hit$q_value[1]))
} else {
  ""
}
draw_legacy_box(
  "imm_total",
  "Inmersi\u00f3n percibida (1-5)",
  "Inmersi\u00f3n percibida",
  trade_note,
  ylim = c(1, 5),
  title_cex = 1.20
)
draw_legacy_box(
  "hits_per_min",
  "Golpes/min",
  "Desempe\u00f1o del jugador",
  "Lectura conjunta para evaluar posible intercambio inmersi\u00f3n-desempe\u00f1o",
  title_cex = 1.20
)
close_png()


# -------------------------------------------------------------------------
# Additional final figures: effect size, rate ratios and RQ3 trade-off.
# Paste this block after RQ3_giq_performance_correlation.png and before
# "Summary and guide".
# -------------------------------------------------------------------------

compute_treatment_contrasts <- function(metrics = metric_specs$metric) {
  rows <- list()
  for (metric in metrics) {
    spec <- metric_specs[metric_specs$metric == metric, , drop = FALSE]
    if (nrow(spec) == 0) next

    temp <- prep_metric_df(metric, spec$log_model[1])
    temp$treatment <- factor(as.character(temp$treatment), levels = truth_order)
    temp <- temp[!is.na(temp$treatment), , drop = FALSE]
    if (nrow(temp) < 12 || sum(temp$treatment == "Control") < 2) next

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
    for (tr in truth_order[truth_order != "Control"]) {
      vals <- temp$Y_raw[temp$treatment == tr]
      if (length(vals) < 2 || length(control_vals) < 2) next

      pooled_sd <- sqrt(((length(vals) - 1) * var(vals) + (length(control_vals) - 1) * var(control_vals)) /
        (length(vals) + length(control_vals) - 2))
      diff_raw <- mean(vals) - mean(control_vals)
      std_diff <- ifelse(is.finite(pooled_sd) && pooled_sd > 0, diff_raw / pooled_sd, NA_real_)
      se_raw <- sqrt(var(vals) / length(vals) + var(control_vals) / length(control_vals))
      df <- max(length(vals) + length(control_vals) - 2, 1)
      ci_raw <- diff_raw + c(-1, 1) * qt(0.975, df = df) * se_raw
      ci_std <- if (is.finite(pooled_sd) && pooled_sd > 0) ci_raw / pooled_sd else c(NA_real_, NA_real_)

      p_value <- NA_real_
      if (!is.null(emm_p) && "p.value" %in% names(emm_p)) {
        hit <- emm_p[grepl(tr, emm_p$contrast, fixed = TRUE), , drop = FALSE]
        if (nrow(hit) > 0) p_value <- hit$p.value[1]
      }

      rows[[paste(metric, tr, sep = "_")]] <- data.frame(
        metric = metric,
        metric_label = spec$label[1],
        treatment = tr,
        contrast = paste(tr, "vs Control"),
        diff_raw = diff_raw,
        raw_low = ci_raw[1],
        raw_high = ci_raw[2],
        std_diff = std_diff,
        std_low = ci_std[1],
        std_high = ci_std[2],
        p_value = p_value,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  if (nrow(out) > 0) out$q_value <- p.adjust(out$p_value, method = "BH")
  out
}

treatment_contrasts_all <- compute_treatment_contrasts()
write.csv(treatment_contrasts_all, file.path(out_dir, "11_all_treatments_vs_control.csv"), row.names = FALSE, na = "")

factor_effect_ci_summary <- function(metric, label) {
  rows <- list()
  for (factor_name in c("Shake", "Zoom", "Recoil")) {
    vals_abs <- to_num(hard[[metric]][hard[[factor_name]] == "Ausente"])
    vals_pre <- to_num(hard[[metric]][hard[[factor_name]] == "Presente"])
    vals_abs <- vals_abs[is.finite(vals_abs)]
    vals_pre <- vals_pre[is.finite(vals_pre)]
    if (length(vals_abs) < 2 || length(vals_pre) < 2) next

    pooled_sd <- sqrt(((length(vals_pre) - 1) * var(vals_pre) + (length(vals_abs) - 1) * var(vals_abs)) /
      (length(vals_pre) + length(vals_abs) - 2))
    diff_raw <- mean(vals_pre) - mean(vals_abs)
    se_raw <- sqrt(var(vals_pre) / length(vals_pre) + var(vals_abs) / length(vals_abs))
    df <- max(length(vals_pre) + length(vals_abs) - 2, 1)
    ci_raw <- diff_raw + c(-1, 1) * qt(0.975, df = df) * se_raw
    ci_std <- if (is.finite(pooled_sd) && pooled_sd > 0) ci_raw / pooled_sd else c(NA_real_, NA_real_)

    rows[[factor_name]] <- data.frame(
      metric = metric,
      metric_label = label,
      factor = factor_name,
      std_diff = ifelse(is.finite(pooled_sd) && pooled_sd > 0, diff_raw / pooled_sd, NA_real_),
      std_low = ci_std[1],
      std_high = ci_std[2],
      p_value = NA_real_,
      q_value = NA_real_,
      test = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  out <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  for (i in seq_len(nrow(out))) {
    if (nrow(art_results) > 0) {
      hit <- art_results[
        art_results$metric == out$metric[i] &
          art_results$term == out$factor[i],
        ,
        drop = FALSE
      ]
      if (nrow(hit) > 0) {
        out$p_value[i] <- hit$p_value[1]
        out$q_value[i] <- hit$q_value[1]
        out$test[i] <- "ART"
      }
    }
    if (!is.finite(out$p_value[i])) {
      hit <- anova_results[
        anova_results$metric == out$metric[i] &
          anova_results$term == out$factor[i],
        ,
        drop = FALSE
      ]
      if (nrow(hit) > 0) {
        out$p_value[i] <- hit$p_value[1]
        out$q_value[i] <- hit$q_value[1]
        out$test[i] <- "ANOVA"
      }
    }
  }
  out
}

rq2_effect_ci <- do.call(rbind, lapply(metric_specs$metric[metric_specs$rq == "RQ2"], function(metric) {
  spec <- metric_specs[metric_specs$metric == metric, , drop = FALSE]
  factor_effect_ci_summary(metric, spec$label[1])
}))
write.csv(rq2_effect_ci, file.path(out_dir, "12_rq2_factor_effect_ci.csv"), row.names = FALSE, na = "")

count_rate_ratio_rows <- list()
if (has_mass) {
  for (i in seq_len(nrow(count_specs))) {
    spec <- count_specs[i, ]
    if (!spec$metric %in% names(hard)) next
    temp <- hard
    temp$Y <- round(to_num(temp[[spec$metric]]))
    temp$duration_min <- to_num(temp$duration_min)
    temp <- temp[
      is.finite(temp$Y) & temp$Y >= 0 &
        is.finite(temp$duration_min) & temp$duration_min > 0,
      ,
      drop = FALSE
    ]
    if (nrow(temp) < 16) next

    fit <- tryCatch(MASS::glm.nb(Y ~ Shake + Zoom + Recoil + offset(log(duration_min)), data = temp), error = function(e) NULL)
    if (is.null(fit)) next

    coef_tab <- as.data.frame(summary(fit)$coefficients)
    coef_tab$term_raw <- rownames(coef_tab)
    rownames(coef_tab) <- NULL
    for (factor_name in c("Shake", "Zoom", "Recoil")) {
      term_name <- paste0(factor_name, "Presente")
      hit <- coef_tab[coef_tab$term_raw == term_name, , drop = FALSE]
      if (nrow(hit) == 0) next
      beta <- hit$Estimate[1]
      se <- hit$`Std. Error`[1]
      count_rate_ratio_rows[[paste(spec$metric, factor_name, sep = "_")]] <- data.frame(
        metric = spec$metric,
        metric_label = spec$label,
        factor = factor_name,
        rate_ratio = exp(beta),
        rr_low = exp(beta - 1.96 * se),
        rr_high = exp(beta + 1.96 * se),
        p_value = hit$`Pr(>|z|)`[1],
        stringsAsFactors = FALSE
      )
    }
  }
}
count_rate_ratios <- if (length(count_rate_ratio_rows) > 0) do.call(rbind, count_rate_ratio_rows) else data.frame()
if (nrow(count_rate_ratios) > 0) count_rate_ratios$q_value <- p.adjust(count_rate_ratios$p_value, method = "BH")
write.csv(count_rate_ratios, file.path(out_dir, "13_count_model_rate_ratios.csv"), row.names = FALSE, na = "")

draw_forest_panels <- function(df, file, title, subtitle, xlab,
                               panel_col, row_col,
                               est_col = "std_diff", low_col = "std_low", high_col = "std_high",
                               null_value = 0, log_x = FALSE,
                               width = 3000, height = 1450) {
  df <- df[is.finite(df[[est_col]]) & is.finite(df[[low_col]]) & is.finite(df[[high_col]]), , drop = FALSE]
  if (nrow(df) == 0) return(invisible(NULL))

  panels <- unique(as.character(df[[panel_col]]))
  open_png(file, width = width, height = height)
  op <- par(
    mfrow = c(1, length(panels)),
    mar = c(3.55, 5.1, 2.75, 1.05),
    oma = c(2.9, 0, 4.7, 0),
    mgp = c(2.05, 0.66, 0),
    tcl = -0.25
  )

  xlim <- range(c(df[[low_col]], df[[high_col]], null_value), na.rm = TRUE)
  if (log_x) {
    xlim[1] <- max(xlim[1], 0.01)
    xlim <- exp(log(xlim) + c(-0.12, 0.12) * diff(log(xlim)))
  } else {
    pad <- diff(xlim) * 0.16
    if (!is.finite(pad) || pad == 0) pad <- 0.35
    xlim <- xlim + c(-pad, pad)
  }

  for (pn in panels) {
    panel <- df[as.character(df[[panel_col]]) == pn, , drop = FALSE]
    y_levels <- rev(unique(as.character(panel[[row_col]])))
    y_pos <- seq_along(y_levels)
    names(y_pos) <- y_levels

    plot(
      NA, NA,
      xlim = xlim,
      ylim = c(0.78, length(y_levels) + 0.22),
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = pn,
      cex.main = 1.08,
      log = if (log_x) "x" else ""
    )

    x_major <- pretty(xlim, n = 5)
    x_major <- x_major[x_major >= xlim[1] & x_major <= xlim[2]]
    abline(v = x_major, col = grid_col, lwd = 1)
    abline(v = null_value, lty = 2, col = "#8A918B", lwd = 1.1)
    abline(h = y_pos, col = adjustcolor(grid_col, alpha.f = 0.62), lwd = 1)

    y <- y_pos[as.character(panel[[row_col]])]
    segments(panel[[low_col]], y, panel[[high_col]], y, col = ink, lwd = 2)
    point_fill <- if (row_col == "treatment") {
      adjustcolor(box_fill[as.character(panel[[row_col]])], alpha.f = 0.72)
    } else {
      adjustcolor(c("Shake" = "#2F83D0", "Zoom" = "#1FB06E", "Recoil" = "#E36F43")[as.character(panel[[row_col]])], alpha.f = 0.72)
    }
    point_border <- if (row_col == "treatment") {
      box_border[as.character(panel[[row_col]])]
    } else {
      c("Shake" = "#1265AD", "Zoom" = "#0B8254", "Recoil" = "#AF4B28")[as.character(panel[[row_col]])]
    }
    points(panel[[est_col]], y, pch = 21, bg = point_fill, col = point_border, cex = 1.45, lwd = 1.15)

    if ("q_value" %in% names(panel) && any(is.finite(panel$q_value) & panel$q_value < 0.05)) {
      sig <- panel[is.finite(panel$q_value) & panel$q_value < 0.05, , drop = FALSE]
      sig_y <- y_pos[as.character(sig[[row_col]])]
      text(sig[[est_col]], sig_y + 0.20, labels = paste0("q=", fmt_p(sig$q_value)), cex = 0.70, col = soft_ink)
    }

    axis(1, at = x_major, labels = axis_label(x_major, x_major), cex.axis = 0.92, col = panel_border, col.axis = ink)
    axis(2, at = y_pos, labels = names(y_pos), las = 1, tick = FALSE, cex.axis = 0.98)
    box(col = panel_border, lwd = 1.15)
  }

  mtext(title, side = 3, outer = TRUE, adj = 0.02, line = 2.85, cex = 1.22, font = 2, col = ink)
  mtext(subtitle, side = 3, outer = TRUE, adj = 0.02, line = 1.62, cex = 0.86, col = soft_ink)
  mtext(xlab, side = 1, outer = TRUE, line = 0.8, cex = 0.96, col = ink)
  par(op)
  close_png()
  invisible(TRUE)
}

# RQ1: effect-size figure that answers "en que medida" directly.
rq1_control <- treatment_contrasts_all[
  treatment_contrasts_all$metric == "imm_total" &
    is.finite(treatment_contrasts_all$diff_raw),
  ,
  drop = FALSE
]
rq1_control$treatment <- factor(rq1_control$treatment, levels = truth_order[truth_order != "Control"])
rq1_control <- rq1_control[order(rq1_control$diff_raw), , drop = FALSE]
draw_forest_panels(
  rq1_control,
  file = "RQ1_immersion_vs_control_forest.png",
  title = "RQ1: cambio en inmersion frente a Control",
  subtitle = "Punto: diferencia media en GIQ; barra: IC 95%; 0 = sin diferencia frente a Control",
  xlab = "Cambio en GIQ frente a Control",
  panel_col = "metric_label",
  row_col = "treatment",
  est_col = "diff_raw",
  low_col = "raw_low",
  high_col = "raw_high",
  null_value = 0,
  width = 2650,
  height = 1450
)

# RQ2: factor effects with uncertainty.
rq2_effect_ci$metric_label <- factor(rq2_effect_ci$metric_label, levels = metric_specs$label[metric_specs$rq == "RQ2"])
rq2_effect_ci <- rq2_effect_ci[order(rq2_effect_ci$metric_label, rq2_effect_ci$factor), , drop = FALSE]
draw_forest_panels(
  rq2_effect_ci,
  file = "RQ2_factor_effects_forest.png",
  title = "RQ2: efectos de camara sobre metricas de desempeno",
  subtitle = "Punto: diferencia estandarizada Presente-Ausente; barra: IC 95%; q se muestra solo si q < 0.05",
  xlab = "Diferencia estandarizada Presente-Ausente",
  panel_col = "metric_label",
  row_col = "factor",
  est_col = "std_diff",
  low_col = "std_low",
  high_col = "std_high",
  null_value = 0,
  width = 3400,
  height = 1450
)

# RQ2: rate ratios from the duration-offset count model.
if (nrow(count_rate_ratios) > 0) {
  count_rate_ratios$metric_label <- factor(count_rate_ratios$metric_label, levels = count_specs$label)
  count_rate_ratios <- count_rate_ratios[order(count_rate_ratios$metric_label, count_rate_ratios$factor), , drop = FALSE]
  draw_forest_panels(
    count_rate_ratios,
    file = "RQ2_count_model_rate_ratios.png",
    title = "RQ2: frecuencia esperada de eventos ajustada por duracion",
    subtitle = "Modelo binomial negativo con offset de duracion; 1 = sin cambio frente a Ausente",
    xlab = "Rate ratio Presente-Ausente",
    panel_col = "metric_label",
    row_col = "factor",
    est_col = "rate_ratio",
    low_col = "rr_low",
    high_col = "rr_high",
    null_value = 1,
    log_x = TRUE,
    width = 3300,
    height = 1450
  )
}

# RQ3: quadrant view of immersion against damage/hits.
trade_quad <- merge(
  treatment_contrasts_all[treatment_contrasts_all$metric == "imm_total", c("treatment", "diff_raw", "raw_low", "raw_high")],
  treatment_contrasts_all[treatment_contrasts_all$metric %in% c("dmg_per_min", "hits_per_min"),
                          c("metric", "metric_label", "treatment", "diff_raw", "raw_low", "raw_high", "q_value")],
  by = "treatment",
  suffixes = c("_giq", "_perf")
)
trade_quad$treatment <- factor(trade_quad$treatment, levels = truth_order[truth_order != "Control"])
trade_quad$metric_label <- factor(
  trade_quad$metric_label,
  levels = c("Da\u00f1o recibido por minuto", "Golpes recibidos por minuto"),
  labels = c("Da\u00f1o/min", "Golpes/Min")
)

open_png("RQ3_tradeoff_quadrants.png", width = 3000, height = 1450)
op <- par(
  mfrow = c(1, 2),
  mar = c(4.35, 5.45, 2.75, 1.15),
  oma = c(2.8, 0, 4.65, 0),
  mgp = c(2.75, 0.75, 0),
  tcl = -0.25
)
if (nrow(trade_quad) > 0) {
  xlim <- range(c(trade_quad$raw_low_giq, trade_quad$raw_high_giq, 0), na.rm = TRUE)
  ylim <- range(c(trade_quad$raw_low_perf, trade_quad$raw_high_perf, 0), na.rm = TRUE)
  xpad <- diff(xlim) * 0.18
  ypad <- diff(ylim) * 0.18
  if (!is.finite(xpad) || xpad == 0) xpad <- 0.20
  if (!is.finite(ypad) || ypad == 0) ypad <- 0.20
  xlim <- xlim + c(-xpad, xpad)
  ylim <- ylim + c(-ypad, ypad)

  for (metric_name in levels(trade_quad$metric_label)) {
    panel <- trade_quad[as.character(trade_quad$metric_label) == metric_name, , drop = FALSE]
    plot(
      NA, NA,
      xlim = xlim,
      ylim = ylim,
      axes = FALSE,
      xlab = "Cambio en GIQ frente a Control",
      ylab = paste("Cambio en", metric_name, "frente a Control"),
      main = metric_name,
      cex.main = 1.12,
      cex.lab = 1.02
    )
    draw_x_axis_with_minor(xlim)
    draw_y_axis_with_minor(ylim)
    abline(v = 0, h = 0, lty = 2, col = "#8A918B", lwd = 1.1)

    segments(panel$raw_low_giq, panel$diff_raw_perf, panel$raw_high_giq, panel$diff_raw_perf, col = ink, lwd = 1.7)
    segments(panel$diff_raw_giq, panel$raw_low_perf, panel$diff_raw_giq, panel$raw_high_perf, col = ink, lwd = 1.7)
    points(
      panel$diff_raw_giq,
      panel$diff_raw_perf,
      pch = 21,
      bg = adjustcolor(box_fill[as.character(panel$treatment)], alpha.f = 0.72),
      col = box_border[as.character(panel$treatment)],
      cex = 1.35,
      lwd = 1.1
    )
    text(
      panel$diff_raw_giq,
      panel$diff_raw_perf,
      labels = as.character(panel$treatment),
      pos = 4,
      cex = 0.68,
      col = soft_ink,
      xpd = TRUE
    )
    box(col = panel_border, lwd = 1.15)
  }
}
mtext("RQ3: inmersion frente a dano y golpes recibidos", side = 3, outer = TRUE, adj = 0.02, line = 2.75, cex = 1.22, font = 2, col = ink)
mtext("Derecha = mayor inmersion; arriba = mas dano o golpes recibidos frente a Control", side = 3, outer = TRUE, adj = 0.02, line = 1.50, cex = 0.86, col = soft_ink)
mtext("Barras: IC 95%; lineas punteadas: sin cambio frente a Control", side = 1, outer = TRUE, line = 0.75, cex = 0.86, col = soft_ink)
par(op)
close_png()

# -------------------------------------------------------------------------
# Summary and guide.
# -------------------------------------------------------------------------

condition_counts <- as.data.frame(table(factor(as.character(hard$treatment), levels = treatment_levels)), stringsAsFactors = FALSE)
names(condition_counts) <- c("treatment", "n")

rq1_hits <- anova_results[
  anova_results$rq == "RQ1" & is.finite(anova_results$p_value) & anova_results$p_value < 0.05,
  ,
  drop = FALSE
]
rq2_art_hits <- if (nrow(art_results) > 0) {
  art_results[art_results$rq == "RQ2" & is.finite(art_results$p_value) & art_results$p_value < 0.05, , drop = FALSE]
} else {
  data.frame()
}
rq3_hits <- tradeoff[is.finite(tradeoff$q_value) & tradeoff$q_value < 0.05, , drop = FALSE]

summary_lines <- c(
  "Research-question analysis",
  "==========================",
  sprintf("Source: %s", project_rel(data_path)),
  sprintf("Rows in juiciness clean dataset: %d", nrow(raw)),
  sprintf("Unique players in juiciness clean dataset: %d", length(unique(raw$player_id))),
  sprintf("Rows removed from analysis: %d", nrow(removed)),
  sprintf("Rows retained for analysis: %d", nrow(hard)),
  sprintf("Quality-flagged rows retained for analysis: %d", length(quality_flag_rows)),
  sprintf("Duplicated players retained: %d", sum(duplicated(hard$player_id))),
  "",
  "Order of calculations:",
  "1. Load juiciness_clean_dataset.csv.",
  "2. Retain all rows, including runs previously excluded by the hard-clean FPS rule.",
  "3. Confirm one row per participant.",
  "4. RQ1: factorial linear model for perceived immersion, residual checks, variance checks, emmeans only for significant interactions.",
  "5. RQ2: factorial models for telemetry; use log1p for skewed positive rates and ART as nonparametric sensitivity.",
  "6. RQ2 counts: negative-binomial model with duration offset for total hits.",
  "7. RQ3: isolated-treatment contrasts against Control and Spearman GIQ-performance correlations.",
  "",
  "Rmd alignment:",
  "The attached Rmd is useful as an exploratory GIQ workflow because it models shake, zoom and recoil, checks residuals/variance, and follows interactions with emmeans.",
  "This canonical script keeps that order but uses the current juiciness_clean_dataset.csv without excluding the low-FPS runs.",
  "",
  "Condition counts in juiciness clean dataset:",
  paste(sprintf("- %s: %d", condition_counts$treatment, condition_counts$n), collapse = "\n"),
  "",
  "RQ1 nominal factorial effects:",
  if (nrow(rq1_hits) == 0) {
    "None."
  } else {
    paste(sprintf("- %s / %s: F=%.2f, p=%s, q=%s, eta_p2=%.3f",
      rq1_hits$metric_label, gsub(":", " x ", rq1_hits$term), rq1_hits$f_value,
      fmt_p(rq1_hits$p_value), fmt_p(rq1_hits$q_value), rq1_hits$partial_eta_squared
    ), collapse = "\n")
  },
  "",
  "RQ2 nominal ART effects:",
  if (nrow(rq2_art_hits) == 0) {
    "None."
  } else {
    paste(sprintf("- %s / %s: F=%.2f, p=%s, q=%s",
      rq2_art_hits$metric_label, gsub(":", " x ", rq2_art_hits$term), rq2_art_hits$f_value,
      fmt_p(rq2_art_hits$p_value), fmt_p(rq2_art_hits$q_value)
    ), collapse = "\n")
  },
  "",
  "RQ3 FDR-corrected GIQ-performance correlations:",
  if (nrow(rq3_hits) == 0) {
    "None."
  } else {
    paste(sprintf("- GIQ vs %s: rho=%.3f, p=%s, q=%s",
      rq3_hits$metric_label, rq3_hits$rho, fmt_p(rq3_hits$p_value), fmt_p(rq3_hits$q_value)
    ), collapse = "\n")
  },
  "",
  "Final figures:",
  "- analysis/images/research_questions/RQ1_immersion_giq_effects.png",
  "- analysis/images/research_questions/RQ1_immersion_vs_control_forest.png",
  "- analysis/images/research_questions/RQ2_damage_per_min.png",
  "- analysis/images/research_questions/RQ2_hits_per_min.png",
  "- analysis/images/research_questions/RQ2_factor_effects_forest.png",
  "- analysis/images/research_questions/RQ2_count_models_duration_offset.png",
  "- analysis/images/research_questions/RQ2_count_model_rate_ratios.png",
  "- analysis/images/research_questions/RQ3_baseline_tradeoff_forest.png",
  "- analysis/images/research_questions/RQ3_tradeoff_quadrants.png",
  "- analysis/images/research_questions/RQ3_giq_performance_correlation.png"
)

writeLines(summary_lines, file.path(out_dir, "summary.txt"))

guide_lines <- c(
  "# Final Research Figures",
  "",
  "Use only these figures for the academic presentation unless a reviewer asks for diagnostics.",
  "",
  "## RQ1",
  "",
  "- `RQ1_immersion_giq_effects.png`: Inmersión percibida by treatment, ordered from worst to best mean. It includes raw observations, median/IQR, mean, 95% CI and the relevant factorial p-value.",
  "- `RQ1_immersion_vs_control_forest.png`: difference in GIQ against Control with 95% CI.",
  "",
  "## RQ2",
  "",
  "- `RQ2_damage_per_min.png`: boxplot for Daño/min by treatment, ordered from worst to best mean.",
  "- `RQ2_hits_per_min.png`: boxplot for Golpes/min by treatment, ordered from worst to best mean.",
  "- `RQ2_factor_effects_forest.png`: standardized factor effects for RQ2 metrics with 95% CI.",
  "- `RQ2_count_models_duration_offset.png`: boxplot for Golpes totales ajustados por duración de partida, ordered from worst to best mean, with the negative-binomial duration-offset p/q value.",
  "- `RQ2_count_model_rate_ratios.png`: duration-offset count-model rate ratios with 95% CI.",
  "",
  "## RQ3",
  "",
  "- `RQ3_baseline_tradeoff_forest.png`: RQ3 forest plot with horizontal panels for Inmersión percibida, Daño/min and Golpes/Min against Control.",
  "- `RQ3_tradeoff_quadrants.png`: change in GIQ against change in Daño/min and Golpes/Min versus Control.",
  "- `RQ3_giq_performance_correlation.png`: paired boxplots for perceived immersion and Golpes/min by treatment, each ordered from worst to best mean. The footer reports the GIQ vs Golpes/min Spearman result.",
  "",
  "## Calculation Order",
  "",
  "1. juiciness_clean_dataset.csv.",
  "2. Retain all rows and keep quality flags as audit metadata.",
  "3. RQ1 GIQ factorial model with assumptions.",
  "4. RQ2 telemetry factorial/ART checks.",
  "5. RQ2 count models with duration offset.",
  "6. RQ3 isolated contrasts vs Control.",
  "7. RQ3 Spearman correlations between GIQ and gameplay metrics.",
  "",
  "The older analysis assets are archived under `analysis/archive/`."
)
writeLines(guide_lines, file.path(fig_dir, "README.md"))

cat(paste(summary_lines, collapse = "\n"), "\n")

open_rstudio_figure_gallery <- function(fig_dir, figure_files) {
  figure_paths <- file.path(fig_dir, figure_files)
  figure_paths <- figure_paths[file.exists(figure_paths)]
  
  if (length(figure_paths) == 0) {
    warning("No figure files found to open in RStudio.")
    return(invisible(NULL))
  }
  
  gallery_path <- file.path(fig_dir, "research_question_figures_gallery.html")
  
  img_blocks <- paste0(
    '<section style="margin-bottom: 40px;">',
    '<h2 style="font-family: sans-serif;">', basename(figure_paths), '</h2>',
    '<img src="', basename(figure_paths), '" ',
    'style="max-width: 100%; height: auto; border: 1px solid #ddd;">',
    '</section>',
    collapse = "\n"
  )
  
  html <- paste0(
    '<!doctype html>',
    '<html>',
    '<head>',
    '<meta charset="utf-8">',
    '<title>Research Question Figures</title>',
    '</head>',
    '<body style="margin: 24px; background: white;">',
    '<h1 style="font-family: sans-serif;">Final Research Question Figures</h1>',
    img_blocks,
    '</body>',
    '</html>'
  )
  
  writeLines(html, gallery_path, useBytes = TRUE)
  
  if (
    interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
  ) {
    rstudioapi::viewer(normalizePath(gallery_path, winslash = "/", mustWork = TRUE))
  } else if (interactive()) {
    utils::browseURL(normalizePath(gallery_path, winslash = "/", mustWork = TRUE))
  }
  
  message("Figure gallery created: ", normalizePath(gallery_path, winslash = "/", mustWork = TRUE))
  invisible(gallery_path)
}

open_rstudio_figure_gallery(fig_dir, final_figure_files)