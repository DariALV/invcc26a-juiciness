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

# Intentar renderizar correctamente tildes y ñ en los PNG.
invisible(try(Sys.setlocale("LC_CTYPE", "es_CR.UTF-8"), silent = TRUE))
invisible(try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE))
utf8_text <- function(x) enc2utf8(as.character(x))

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
  "00_assumption_validation_main_metrics.png",
  "00_art_factorial_main_metrics.png",
  "RQ1_immersion_giq_effects.png",
  "RQ1_immersion_vs_control_forest.png",
  "RQ2_kills_per_min.png",
  "RQ2_hits_per_min.png",
  "RQ2_performance_effects_directional_forest.png",
  "RQ2_count_models_duration_offset.png",
  "RQ2_count_model_rate_ratios.png",
  "RQ2_survival_time_context.png",
  "RQ3_immersion_vs_control_forest.png",
  "RQ3_hits_vs_control_forest.png",
  "RQ3_immersion_hits_tradeoff_quadrants.png",
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

sig_class <- function(p, q) {
  if (!is.finite(p) && !is.finite(q)) return("no_disponible")
  if (is.finite(q) && q < 0.05) return("significativo_q")
  if (is.finite(p) && p < 0.05) return("nominal_p")
  "no_significativo"
}

sig_label <- function(p, q, p_prefix = "p", q_prefix = "q") {
  cls <- sig_class(p, q)
  if (identical(cls, "significativo_q")) return(paste0(q_prefix, "=", fmt_p(q)))
  if (identical(cls, "nominal_p")) return(paste0(p_prefix, "=", fmt_p(p)))
  ""
}

display_stat_label <- function(p, q, p_prefix = "p", q_prefix = "q") {
  if (is.finite(q)) return(paste0(q_prefix, "=", fmt_p(q)))
  if (is.finite(p)) return(paste0(p_prefix, "=", fmt_p(p)))
  "n/d"
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
    "kills_per_min",
    "hits_per_min",
    "ingame_survival_s"
  ),
  label = c(
    "Inmersi\u00f3n percibida",
    "Kills/min",
    "Golpes recibidos/min",
    "Supervivencia (s)"
  ),
  rq = c("RQ1", "RQ2", "RQ2", "context"),
  log_model = c(FALSE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

raw <- read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)
required <- c(
  "player_id", "run_id", "shake", "zoom", "recoil", "imm_total",
  "ingame_survival_s", "total_kills", "input_total", "hits_per_min"
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
if (!"kills_per_min" %in% names(raw)) {
  raw$kills_per_min <- to_num(raw$total_kills) / raw$duration_min
}
raw$hits_total_est <- round(to_num(raw$hits_per_min) * raw$duration_min)
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

# No se eliminan runs al inicio.
# La auditoría de calidad no excluye observaciones en esta versión.
flags <- data.frame()
quality_flag_rows <- integer(0)
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
# Primary factorial evidence: ANOVA only when assumptions are viable; ART otherwise.
# -------------------------------------------------------------------------

primary_method_for_metric <- function(metric) {
  row <- assumptions[assumptions$metric == metric, , drop = FALSE]
  if (nrow(row) == 0) return("ART")
  if (isTRUE(row$normality_ok[1]) && isTRUE(row$variance_ok_brown_forsythe[1])) "ANOVA" else "ART"
}

pairwise_control_p <- function(vals, control_vals, method) {
  vals <- vals[is.finite(vals)]
  control_vals <- control_vals[is.finite(control_vals)]
  if (length(vals) < 2 || length(control_vals) < 2) return(NA_real_)

  if (identical(method, "ANOVA")) {
    return(tryCatch(t.test(vals, control_vals, var.equal = FALSE)$p.value, error = function(e) NA_real_))
  }

  tryCatch(wilcox.test(vals, control_vals, exact = FALSE)$p.value, error = function(e) NA_real_)
}

primary_metric_order <- c("imm_total", "ingame_survival_s", "kills_per_min", "hits_per_min")
primary_metric_labels <- c(
  "imm_total" = "Inmersión percibida",
  "ingame_survival_s" = "Supervivencia (s)",
  "kills_per_min" = "Kills/min",
  "hits_per_min" = "Golpes recibidos/min"
)
primary_term_order <- c("Shake", "Zoom", "Recoil", "Shake:Zoom", "Shake:Recoil", "Zoom:Recoil", "Shake:Zoom:Recoil")

primary_rows <- list()
for (metric in primary_metric_order) {
  method <- primary_method_for_metric(metric)
  source <- if (identical(method, "ANOVA")) anova_results else art_results
  metric_rows <- source[source$metric == metric & source$term %in% primary_term_order, , drop = FALSE]
  if (nrow(metric_rows) == 0) next
  metric_rows$primary_method <- method
  metric_rows$assumption_normality_ok <- assumptions$normality_ok[match(metric_rows$metric, assumptions$metric)]
  metric_rows$assumption_variance_ok <- assumptions$variance_ok_brown_forsythe[match(metric_rows$metric, assumptions$metric)]
  if (!"f_value" %in% names(metric_rows) && "statistic" %in% names(metric_rows)) metric_rows$f_value <- metric_rows$statistic
  primary_rows[[metric]] <- metric_rows[
    ,
    intersect(
      c("metric", "metric_label", "term", "primary_method", "assumption_normality_ok",
        "assumption_variance_ok", "df_effect", "df_residual", "f_value", "p_value", "q_value"),
      names(metric_rows)
    ),
    drop = FALSE
  ]
}
primary_factorial_results <- if (length(primary_rows) > 0) do.call(rbind, primary_rows) else data.frame()
if (nrow(primary_factorial_results) > 0) {
  primary_factorial_results$q_value_primary_family <- p.adjust(primary_factorial_results$p_value, method = "BH")
  primary_factorial_results <- primary_factorial_results[order(match(primary_factorial_results$metric, primary_metric_order), match(primary_factorial_results$term, primary_term_order)), , drop = FALSE]
}
write.csv(primary_factorial_results, file.path(out_dir, "00_primary_factorial_tests_main_metrics.csv"), row.names = FALSE, na = "")

primary_doc_lines <- c(
  "Primary factorial tests for simplified metrics",
  "============================================",
  "",
  "Rule: use ANOVA only when residual normality and Brown-Forsythe variance checks are both viable; otherwise use ART.",
  "",
  if (nrow(primary_factorial_results) == 0) {
    "No primary factorial results were available."
  } else {
    paste(
      sprintf(
        "- %s / %s / %s: p=%s, q=%s",
        primary_factorial_results$primary_method,
        primary_factorial_results$metric_label,
        gsub(":", " x ", primary_factorial_results$term),
        fmt_p(primary_factorial_results$p_value),
        fmt_p(primary_factorial_results$q_value_primary_family)
      ),
      collapse = "\n"
    )
  }
)
writeLines(primary_doc_lines, file.path(out_dir, "00_primary_factorial_tests_main_metrics.txt"))


# -------------------------------------------------------------------------
# 3. Count models for event totals with duration offsets.
# -------------------------------------------------------------------------

count_specs <- data.frame(
  metric = c("total_kills", "hits_total_est"),
  label = c(
    "Kills totales ajustados por duraci\u00f3n de partida",
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
baseline_treatments <- treatment_levels
baseline_contrast_treatments <- baseline_treatments[baseline_treatments != "Control"]

match_dunnett_contrast <- function(contrast_table, treatment_name) {
  if (is.null(contrast_table) || nrow(contrast_table) == 0 || !"contrast" %in% names(contrast_table)) {
    return(contrast_table[FALSE, , drop = FALSE])
  }

  contrast_clean <- gsub("\\s+", " ", trimws(as.character(contrast_table$contrast)))
  exact_patterns <- c(
    paste(treatment_name, "- Control"),
    paste(treatment_name, "vs Control"),
    paste(treatment_name, "/ Control")
  )

  hit <- contrast_table[contrast_clean %in% exact_patterns, , drop = FALSE]
  if (nrow(hit) > 0) return(hit)

  control_suffix <- grepl("\\s*(-|vs|/)\\s*Control\\s*$", contrast_clean)
  left_side <- trimws(sub("\\s*(-|vs|/)\\s*Control\\s*$", "", contrast_clean))
  contrast_table[control_suffix & left_side == treatment_name, , drop = FALSE]
}

baseline_rows <- list()
for (metric in baseline_metrics) {
  spec <- metric_specs[metric_specs$metric == metric, , drop = FALSE]
  temp <- prep_metric_df(metric, spec$log_model)
  temp <- temp[as.character(temp$treatment) %in% baseline_treatments, , drop = FALSE]
  temp$treatment <- factor(as.character(temp$treatment), levels = baseline_treatments)
  if (nrow(temp) < length(baseline_treatments) * 2) next

  primary_method <- primary_method_for_metric(metric)
  fit <- lm(Y_model ~ treatment, data = temp)

  emm_p <- NULL
  if (identical(primary_method, "ANOVA") && has_emmeans) {
    emm <- tryCatch(emmeans::emmeans(fit, "treatment"), error = function(e) NULL)
    if (!is.null(emm)) {
      emm_p <- tryCatch(
        as.data.frame(emmeans::contrast(emm, method = "trt.vs.ctrl", ref = "Control", adjust = "none")),
        error = function(e) NULL
      )
    }
  }

  control_vals <- temp$Y_raw[temp$treatment == "Control"]
  for (tr in baseline_contrast_treatments) {
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
    p_source <- NA_character_
    if (!is.null(emm_p)) {
      hit <- match_dunnett_contrast(emm_p, tr)
      if (nrow(hit) > 0 && "p.value" %in% names(hit)) {
        p_value <- hit$p.value[1]
        p_source <- "ANOVA emmeans vs Control"
      }
    }
    if (!is.finite(p_value)) {
      p_value <- pairwise_control_p(vals, control_vals, primary_method)
      p_source <- ifelse(identical(primary_method, "ANOVA"), "ANOVA fallback Welch t-test", "ART fallback Wilcoxon rank-sum")
    }

    baseline_rows[[paste(metric, tr, sep = "_")]] <- data.frame(
      rq = "RQ3",
      metric = metric,
      metric_label = spec$label,
      primary_method = primary_method,
      treatment = tr,
      contrast = paste(tr, "vs Control"),
      model_scale = attr(temp, "model_scale"),
      mean_control = mean(control_vals),
      mean_treatment = mean(vals),
      diff_raw = diff_raw,
      std_diff = std_diff,
      std_low = ci_std[1],
      std_high = ci_std[2],
      raw_low = ci_raw[1],
      raw_high = ci_raw[2],
      p_value = p_value,
      p_value_source = p_source,
      stringsAsFactors = FALSE
    )
  }
}
baseline <- if (length(baseline_rows) > 0) do.call(rbind, baseline_rows) else data.frame()
if (nrow(baseline) > 0) {
  baseline$q_value <- NA_real_
  finite_p <- is.finite(baseline$p_value)
  baseline$q_value[finite_p] <- p.adjust(baseline$p_value[finite_p], method = "BH")
  baseline$significant_q05 <- is.finite(baseline$q_value) & baseline$q_value < 0.05
}
write.csv(baseline, file.path(out_dir, "07_rq3_all_treatments_vs_control.csv"), row.names = FALSE, na = "")


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
draw_title("Inmersión por efecto", "Promedio marginal observado")

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
draw_title("Desempeño por efecto", "Diferencia estandarizada Presente–Ausente")
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
    draw_title("Conteos ajustados por duración", "Modelo binomial negativo con offset de duración")
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
draw_title("Relación entre inmersión y desempeño", "Puntos verdes: correlaciones que sobreviven FDR")
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

draw_y_axis_with_minor <- function(ylim, major_n = 5, minor_n = 11,
                                   major_cex = 1.05, minor_cex = 0.78) {
  major <- pretty(ylim, n = major_n)
  major <- major[major >= ylim[1] & major <= ylim[2]]
  minor <- pretty(ylim, n = minor_n)
  minor <- minor[minor >= ylim[1] & minor <= ylim[2]]
  minor_only <- minor[!minor %in% major]
  if (length(minor_only) == 0 && length(major) >= 2) {
    step <- min(diff(sort(unique(major)))) / 2
    if (is.finite(step) && step > 0) {
      minor <- seq(floor(ylim[1] / step) * step, ceiling(ylim[2] / step) * step, by = step)
      minor <- minor[minor >= ylim[1] & minor <= ylim[2]]
      minor_only <- minor[!vapply(minor, function(v) any(abs(v - major) < 1e-8), logical(1))]
    }
  }
  scale_values <- sort(unique(c(major, minor_only)))
  if (length(minor_only) > 0) {
    abline(h = minor_only, col = adjustcolor(minor_grid_col, alpha.f = 0.68), lwd = 0.75, lty = 3)
    axis(
      2,
      at = minor_only,
      labels = axis_label(minor_only, scale_values),
      las = 1,
      cex.axis = minor_cex,
      col = panel_border,
      col.axis = adjustcolor(soft_ink, alpha.f = 0.78),
      tcl = -0.14
    )
  }
  abline(h = major, col = grid_col, lwd = 1)
  axis(2, at = major, labels = axis_label(major, scale_values), las = 1, cex.axis = major_cex, col = panel_border, col.axis = ink)
}

draw_x_axis_with_minor <- function(xlim, major_n = 5, minor_n = 9,
                                   major_cex = 1.02, minor_cex = 0.62) {
  major <- pretty(xlim, n = major_n)
  major <- major[major >= xlim[1] & major <= xlim[2]]
  minor <- pretty(xlim, n = minor_n)
  minor <- minor[minor >= xlim[1] & minor <= xlim[2]]
  minor_only <- minor[!vapply(minor, function(v) any(abs(v - major) < 1e-8), logical(1))]
  if (length(minor_only) == 0 && length(major) >= 2) {
    step <- min(diff(sort(unique(major)))) / 2
    if (is.finite(step) && step > 0) {
      minor <- seq(floor(xlim[1] / step) * step, ceiling(xlim[2] / step) * step, by = step)
      minor <- minor[minor >= xlim[1] & minor <= xlim[2]]
      minor_only <- minor[!vapply(minor, function(v) any(abs(v - major) < 1e-8), logical(1))]
    }
  }
  scale_values <- sort(unique(c(major, minor_only)))
  if (length(minor_only) > 0) {
    abline(v = minor_only, col = adjustcolor(minor_grid_col, alpha.f = 0.68), lwd = 0.75, lty = 3)
    axis(
      1,
      at = minor_only,
      labels = axis_label(minor_only, scale_values),
      cex.axis = minor_cex,
      col = panel_border,
      col.axis = adjustcolor(soft_ink, alpha.f = 0.78),
      tcl = -0.14
    )
  }
  abline(v = major, col = grid_col, lwd = 1)
  axis(1, at = major, labels = axis_label(major, scale_values), cex.axis = major_cex, col = panel_border, col.axis = ink)
}


draw_x_axis_with_minor_log <- function(xlim, major_n = 5,
                                       major_cex = 1.02, minor_cex = 0.68) {
  if (!all(is.finite(xlim)) || any(xlim <= 0)) return(invisible(NULL))
  log_lim <- log10(xlim)
  major_log <- pretty(log_lim, n = major_n)
  major_log <- major_log[major_log >= log_lim[1] & major_log <= log_lim[2]]
  if (length(major_log) >= 2) {
    step <- min(diff(sort(unique(major_log)))) / 2
  } else {
    step <- 0.1
  }
  minor_log <- seq(floor(log_lim[1] / step) * step, ceiling(log_lim[2] / step) * step, by = step)
  minor_log <- minor_log[minor_log >= log_lim[1] & minor_log <= log_lim[2]]
  major <- 10^major_log
  minor <- 10^minor_log
  tol <- 1e-8
  minor_only <- minor[sapply(minor, function(v) all(abs(log10(v) - major_log) > tol))]
  scale_values <- sort(unique(c(major, minor_only)))
  if (length(minor_only) > 0) {
    abline(v = minor_only, col = adjustcolor(minor_grid_col, alpha.f = 0.68), lwd = 0.75, lty = 3)
    axis(
      1,
      at = minor_only,
      labels = axis_label(minor_only, scale_values),
      cex.axis = minor_cex,
      col = panel_border,
      col.axis = adjustcolor(soft_ink, alpha.f = 0.78),
      tcl = -0.14
    )
  }
  abline(v = major, col = grid_col, lwd = 1)
  axis(1, at = major, labels = axis_label(major, scale_values), cex.axis = major_cex, col = panel_border, col.axis = ink)
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
  if (nzchar(title_text)) title(utf8_text(title_text), adj = 0, cex.main = title_cex, font.main = 2, line = 1.45)
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

open_png("RQ2_kills_per_min.png", width = 3000, height = 1688)
par(mar = c(10.7, 7.4, 4.8, 2.0), mgp = c(3.85, 0.78, 0), tcl = -0.25)
draw_legacy_box(
  "kills_per_min",
  "Kills/min",
  "Kills por minuto",
  metric_note("kills_per_min", "art"),
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

open_png("RQ2_survival_time_context.png", width = 3000, height = 1688)
par(mar = c(10.7, 7.4, 4.8, 2.0), mgp = c(3.85, 0.78, 0), tcl = -0.25)
draw_legacy_box(
  "ingame_survival_s",
  "Supervivencia (s)",
  "Supervivencia por tratamiento",
  "Métrica contextual: interpreta tasas y conteos; no define por sí sola el desempeño.",
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

baseline_focus_metrics <- c("imm_total", "hits_per_min")
baseline_focus_labels <- c("Inmersi\u00f3n percibida", "Golpes recibidos/min")
baseline_focus <- baseline[
  baseline$metric %in% baseline_focus_metrics &
    baseline$treatment %in% truth_order[truth_order != "Control"] &
    is.finite(baseline$std_diff),
  ,
  drop = FALSE
]
baseline_focus$metric_label <- baseline_focus_labels[match(baseline_focus$metric, baseline_focus_metrics)]
baseline_focus$treatment <- factor(baseline_focus$treatment, levels = truth_order[truth_order != "Control"])
baseline_focus$metric <- factor(baseline_focus$metric, levels = baseline_focus_metrics)
baseline_focus <- baseline_focus[order(baseline_focus$metric, baseline_focus$treatment), ]

draw_rq3_single_metric_forest <- function(metric_name, output_file, title_text, xlab_text) {
  panel <- baseline_focus[as.character(baseline_focus$metric) == metric_name, , drop = FALSE]
  if (nrow(panel) == 0) return(invisible(NULL))
  panel <- panel[order(panel$treatment), , drop = FALSE]
  y_levels <- rev(truth_order[truth_order != "Control"])
  y_pos <- seq_along(y_levels)
  names(y_pos) <- y_levels

  xlim <- range(c(panel$std_low, panel$std_high, 0), na.rm = TRUE)
  pad <- diff(xlim) * 0.10
  if (!is.finite(pad) || pad == 0) pad <- 0.20
  xlim <- xlim + c(-pad, pad)

  open_png(output_file, width = 2850, height = 1500)
  op <- par(
    mar = c(3.85, 11.2, 3.55, 1.15),
    oma = c(1.1, 0.35, 0, 0),
    mgp = c(2.40, 0.70, 0),
    tcl = -0.25
  )
  plot(
    NA,
    NA,
    xlim = xlim,
    ylim = c(0.62, length(y_levels) + 0.38),
    axes = FALSE,
    xlab = utf8_text(xlab_text),
    ylab = "",
    main = utf8_text(title_text),
    cex.main = 1.22,
    cex.lab = 0.98
  )

  # Bandas alternas para mejorar lectura por fila.
  for (i in seq_along(y_pos)) {
    if (i %% 2 == 0) {
      rect(xlim[1], y_pos[i] - 0.45, xlim[2], y_pos[i] + 0.45,
           col = adjustcolor("#EEF3EF", alpha.f = 0.58), border = NA)
    }
  }

  draw_x_axis_with_minor(xlim, major_cex = 0.98, minor_cex = 0.72)
  abline(v = 0, lty = 2, col = "#8A918B", lwd = 1.25)
  abline(h = y_pos, col = adjustcolor(grid_col, alpha.f = 0.35), lwd = 0.85)

  y <- y_pos[as.character(panel$treatment)]
  segments(panel$std_low, y, panel$std_high, y, col = ink, lwd = 1.65)

  # Topes de intervalo de confianza.
  cap <- 0.105
  segments(panel$std_low, y - cap, panel$std_low, y + cap, col = ink, lwd = 1.65)
  segments(panel$std_high, y - cap, panel$std_high, y + cap, col = ink, lwd = 1.65)

  panel$sig_class <- mapply(sig_class, panel$p_value, panel$q_value)
  point_border <- ifelse(panel$sig_class == "significativo_q", "#111111", box_border[as.character(panel$treatment)])
  point_lwd <- ifelse(panel$sig_class == "significativo_q", 2.55, 1.35)
  point_cex <- ifelse(panel$sig_class == "significativo_q", 2.05, 1.85)
  points(
    panel$std_diff,
    y,
    pch = 21,
    bg = adjustcolor(box_fill[as.character(panel$treatment)], alpha.f = 0.78),
    col = point_border,
    cex = point_cex,
    lwd = point_lwd
  )

  # Etiquetas p/q en columna fija para evitar tapar el estimador.
  panel$label_text <- mapply(display_stat_label, panel$p_value, panel$q_value)
  label_col <- ifelse(panel$sig_class == "significativo_q", "#111111", ifelse(panel$sig_class == "nominal_p", "#8B5A10", soft_ink))
  label_cex <- ifelse(panel$sig_class == "significativo_q", 0.86, 0.78)
  label_font <- ifelse(panel$sig_class == "significativo_q", 2, 1)
  text(xlim[2], y, labels = panel$label_text,
       adj = 1, cex = label_cex, font = label_font, col = label_col)

  axis(2, at = y_pos, labels = utf8_text(names(y_pos)), las = 1, tick = FALSE, cex.axis = 0.98)
  box(col = panel_border, lwd = 1.15)
  par(op)
  close_png()
  invisible(TRUE)
}

draw_rq3_single_metric_forest(
  "imm_total",
  "RQ3_immersion_vs_control_forest.png",
  "Inmersión frente a Control",
  "Diferencia estandarizada de inmersi\u00f3n frente a Control"
)
draw_rq3_single_metric_forest(
  "hits_per_min",
  "RQ3_hits_vs_control_forest.png",
  "Golpes recibidos frente a Control",
  "Diferencia estandarizada de golpes/min frente a Control"
)

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

    primary_method <- primary_method_for_metric(metric)
    fit <- lm(Y_model ~ treatment, data = temp)
    emm_p <- NULL
    if (has_emmeans) {
      emm <- tryCatch(emmeans::emmeans(fit, "treatment"), error = function(e) NULL)
      if (!is.null(emm)) {
        emm_p <- tryCatch(
          as.data.frame(emmeans::contrast(emm, method = "trt.vs.ctrl", ref = "Control", adjust = "none")),
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
      p_source <- NA_character_
      if (identical(primary_method, "ANOVA") && !is.null(emm_p) && "p.value" %in% names(emm_p)) {
        hit <- match_dunnett_contrast(emm_p, tr)
        if (nrow(hit) > 0) {
          p_value <- hit$p.value[1]
          p_source <- "ANOVA emmeans vs Control"
        }
      }
      if (!is.finite(p_value)) {
        p_value <- pairwise_control_p(vals, control_vals, primary_method)
        p_source <- ifelse(identical(primary_method, "ANOVA"), "ANOVA fallback Welch t-test", "ART fallback Wilcoxon rank-sum")
      }

      rows[[paste(metric, tr, sep = "_")]] <- data.frame(
        metric = metric,
        metric_label = spec$label[1],
        primary_method = primary_method,
        treatment = tr,
        contrast = paste(tr, "vs Control"),
        diff_raw = diff_raw,
        raw_low = ci_raw[1],
        raw_high = ci_raw[2],
        std_diff = std_diff,
        std_low = ci_std[1],
        std_high = ci_std[2],
        p_value = p_value,
        p_value_source = p_source,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  if (nrow(out) > 0) {
    out$q_value <- NA_real_
    finite_p <- is.finite(out$p_value)
    out$q_value[finite_p] <- p.adjust(out$p_value[finite_p], method = "BH")
  }
  out
}

treatment_contrasts_all <- compute_treatment_contrasts()
write.csv(treatment_contrasts_all, file.path(out_dir, "11_all_treatments_vs_control.csv"), row.names = FALSE, na = "")

if (nrow(treatment_contrasts_all) > 0) {
  contrast_doc_lines <- c(
    "Treatment-vs-Control contrasts used in figures",
    "=============================================",
    "",
    "P-values are selected according to the primary method for each metric: ANOVA when assumptions are viable; ART/Wilcoxon fallback when assumptions are not viable.",
    "",
    paste(
      sprintf(
        "- %s / %s / %s: p=%s, q=%s [%s]",
        treatment_contrasts_all$primary_method,
        treatment_contrasts_all$metric_label,
        treatment_contrasts_all$contrast,
        fmt_p(treatment_contrasts_all$p_value),
        fmt_p(treatment_contrasts_all$q_value),
        treatment_contrasts_all$p_value_source
      ),
      collapse = "\n"
    )
  )
  writeLines(contrast_doc_lines, file.path(out_dir, "11_all_treatments_vs_control_pq_values.txt"))
}

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
    temp$treatment <- factor(as.character(temp$treatment), levels = truth_order)
    temp <- temp[
      is.finite(temp$Y) & temp$Y >= 0 &
        is.finite(temp$duration_min) & temp$duration_min > 0 &
        !is.na(temp$treatment),
      ,
      drop = FALSE
    ]
    if (nrow(temp) < 16 || sum(temp$treatment == "Control") < 2) next

    fit <- tryCatch(MASS::glm.nb(Y ~ treatment + offset(log(duration_min)), data = temp), error = function(e) NULL)
    if (is.null(fit)) next

    coef_tab <- as.data.frame(summary(fit)$coefficients)
    coef_tab$term_raw <- rownames(coef_tab)
    rownames(coef_tab) <- NULL
    for (tr in truth_order[truth_order != "Control"]) {
      term_name <- paste0("treatment", tr)
      hit <- coef_tab[coef_tab$term_raw == term_name, , drop = FALSE]
      if (nrow(hit) == 0) next
      beta <- hit$Estimate[1]
      se <- hit$`Std. Error`[1]
      count_rate_ratio_rows[[paste(spec$metric, tr, sep = "_")]] <- data.frame(
        metric = spec$metric,
        metric_label = spec$label,
        treatment = tr,
        contrast = paste(tr, "vs Control"),
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
if (nrow(count_rate_ratios) > 0) {
  control_rate_rows <- count_specs[count_specs$metric %in% unique(count_rate_ratios$metric), , drop = FALSE]
  if (nrow(control_rate_rows) > 0) {
    control_rate_rows <- data.frame(
      metric = control_rate_rows$metric,
      metric_label = control_rate_rows$label,
      treatment = "Control",
      contrast = "Control reference",
      rate_ratio = 1,
      rr_low = 1,
      rr_high = 1,
      p_value = NA_real_,
      stringsAsFactors = FALSE
    )
    count_rate_ratios <- rbind(control_rate_rows, count_rate_ratios)
  }
  count_rate_ratios$q_value <- NA_real_
  finite_p <- is.finite(count_rate_ratios$p_value)
  count_rate_ratios$q_value[finite_p] <- p.adjust(count_rate_ratios$p_value[finite_p], method = "BH")
}
write.csv(count_rate_ratios, file.path(out_dir, "13_count_model_rate_ratios.csv"), row.names = FALSE, na = "")

draw_forest_panels <- function(df, file, title, subtitle, xlab,
                               panel_col, row_col,
                               est_col = "std_diff", low_col = "std_low", high_col = "std_high",
                               null_value = 0, log_x = FALSE,
                               width = 3000, height = 1450,
                               left_mar = NULL, y_axis_cex = 0.98,
                               per_panel_xlim = FALSE, symmetric_zero = FALSE,
                               x_major_cex = 1.02, x_minor_cex = 0.62) {
  df <- df[is.finite(df[[est_col]]) & is.finite(df[[low_col]]) & is.finite(df[[high_col]]), , drop = FALSE]
  if (nrow(df) == 0) return(invisible(NULL))

  panels <- unique(as.character(df[[panel_col]]))
  if (is.null(left_mar)) {
    left_mar <- if (identical(row_col, "treatment")) 10.8 else 5.1
  }

  top_oma <- if (nzchar(subtitle)) 4.15 else 3.15
  open_png(file, width = width, height = height)
  op <- par(
    mfrow = c(1, length(panels)),
    mar = c(3.35, left_mar, 2.05, 0.90),
    oma = c(2.55, 0.35, top_oma, 0),
    mgp = c(1.95, 0.64, 0),
    tcl = -0.25
  )

  global_xlim <- range(c(df[[low_col]], df[[high_col]], null_value), na.rm = TRUE)
  if (log_x) {
    global_xlim[1] <- max(global_xlim[1], 0.01)
    global_xlim <- exp(log(global_xlim) + c(-0.08, 0.08) * diff(log(global_xlim)))
  } else {
    if (symmetric_zero) {
      max_abs <- max(abs(c(df[[low_col]], df[[high_col]], null_value)), na.rm = TRUE)
      if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
      max_abs <- max_abs * 1.08
      global_xlim <- c(-max_abs, max_abs)
    } else {
      pad <- diff(global_xlim) * 0.10
      if (!is.finite(pad) || pad == 0) pad <- 0.25
      global_xlim <- global_xlim + c(-pad, pad)
    }
  }

  for (pn in panels) {
    panel <- df[as.character(df[[panel_col]]) == pn, , drop = FALSE]
    y_levels <- rev(unique(as.character(panel[[row_col]])))
    y_pos <- seq_along(y_levels)
    names(y_pos) <- y_levels

    xlim <- global_xlim
    if (per_panel_xlim) {
      xlim <- range(c(panel[[low_col]], panel[[high_col]], null_value), na.rm = TRUE)
      if (log_x) {
        xlim[1] <- max(xlim[1], 0.01)
        xlim <- exp(log(xlim) + c(-0.08, 0.08) * diff(log(xlim)))
      } else if (symmetric_zero) {
        max_abs <- max(abs(c(panel[[low_col]], panel[[high_col]], null_value)), na.rm = TRUE)
        if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
        xlim <- c(-1, 1) * max_abs * 1.08
      } else {
        pad <- diff(xlim) * 0.10
        if (!is.finite(pad) || pad == 0) pad <- 0.25
        xlim <- xlim + c(-pad, pad)
      }
    }

    plot(
      NA, NA,
      xlim = xlim,
      ylim = c(0.62, length(y_levels) + 0.38),
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = utf8_text(pn),
      cex.main = 1.08,
      log = if (log_x) "x" else ""
    )

    # Bandas alternas por fila para aprovechar mejor el espacio y guiar la lectura.
    for (i in seq_along(y_pos)) {
      if (i %% 2 == 0) {
        rect(xlim[1], y_pos[i] - 0.45, xlim[2], y_pos[i] + 0.45,
             col = adjustcolor("#EEF3EF", alpha.f = 0.58), border = NA)
      }
    }

    if (log_x) {
      draw_x_axis_with_minor_log(xlim, major_cex = x_major_cex, minor_cex = x_minor_cex)
    } else {
      draw_x_axis_with_minor(xlim, major_cex = x_major_cex, minor_cex = x_minor_cex)
    }
    abline(v = null_value, lty = 2, col = "#8A918B", lwd = 1.25)
    abline(h = y_pos, col = adjustcolor(grid_col, alpha.f = 0.35), lwd = 0.85)

    y <- y_pos[as.character(panel[[row_col]])]
    segments(panel[[low_col]], y, panel[[high_col]], y, col = ink, lwd = 1.65)

    # Topes de intervalo de confianza.
    cap <- 0.105
    segments(panel[[low_col]], y - cap, panel[[low_col]], y + cap, col = ink, lwd = 1.65)
    segments(panel[[high_col]], y - cap, panel[[high_col]], y + cap, col = ink, lwd = 1.65)

    point_fill <- if (row_col == "treatment") {
      adjustcolor(box_fill[as.character(panel[[row_col]])], alpha.f = 0.78)
    } else {
      adjustcolor(c("Shake" = "#2F83D0", "Zoom" = "#1FB06E", "Recoil" = "#E36F43")[as.character(panel[[row_col]])], alpha.f = 0.78)
    }
    point_border <- if (row_col == "treatment") {
      box_border[as.character(panel[[row_col]])]
    } else {
      c("Shake" = "#1265AD", "Zoom" = "#0B8254", "Recoil" = "#AF4B28")[as.character(panel[[row_col]])]
    }

    if (all(c("p_value", "q_value") %in% names(panel))) {
      panel$sig_class <- mapply(sig_class, panel$p_value, panel$q_value)
    } else {
      panel$sig_class <- rep("no_significativo", nrow(panel))
    }

    point_col_use <- ifelse(panel$sig_class == "significativo_q", "#111111", point_border)
    point_lwd_use <- ifelse(panel$sig_class == "significativo_q", 2.55, 1.35)
    point_cex_use <- ifelse(panel$sig_class == "significativo_q", 2.05, 1.85)
    points(panel[[est_col]], y, pch = 21, bg = point_fill, col = point_col_use, cex = point_cex_use, lwd = point_lwd_use)

    # Etiquetas p/q en columna fija a la derecha para no tapar puntos ni IC.
    if (all(c("p_value", "q_value") %in% names(panel))) {
      panel$label_text <- mapply(display_stat_label, panel$p_value, panel$q_value)
      label_col <- ifelse(panel$sig_class == "significativo_q", "#111111", ifelse(panel$sig_class == "nominal_p", "#8B5A10", soft_ink))
      label_cex <- ifelse(panel$sig_class == "significativo_q", 0.86, 0.78)
      label_font <- ifelse(panel$sig_class == "significativo_q", 2, 1)
      text(xlim[2], y, labels = panel$label_text,
           adj = 1, cex = label_cex, font = label_font, col = label_col)
    }

    axis(2, at = y_pos, labels = utf8_text(names(y_pos)), las = 1, tick = FALSE, cex.axis = y_axis_cex)
    box(col = panel_border, lwd = 1.15)
  }

  mtext(utf8_text(title), side = 3, outer = TRUE, adj = 0.02, line = if (nzchar(subtitle)) 2.55 else 1.75, cex = 1.22, font = 2, col = ink)
  if (nzchar(subtitle)) mtext(utf8_text(subtitle), side = 3, outer = TRUE, adj = 0.02, line = 1.35, cex = 0.86, col = soft_ink)
  mtext(utf8_text(xlab), side = 1, outer = TRUE, line = 0.75, cex = 0.96, col = ink)
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
  title = "Cambio en inmersión frente a Control",
  subtitle = "Punto: diferencia media; barra: IC 95%; 0 = sin diferencia",
  xlab = "Cambio en GIQ frente a Control",
  panel_col = "metric_label",
  row_col = "treatment",
  est_col = "diff_raw",
  low_col = "raw_low",
  high_col = "raw_high",
  null_value = 0,
  width = 2950,
  height = 1550,
  left_mar = 11.8,
  y_axis_cex = 1.00,
  x_minor_cex = 0.68
)

# RQ2: directional treatment contrasts with uncertainty.
rq2_performance_plot <- treatment_contrasts_all[
  treatment_contrasts_all$metric %in% c("kills_per_min", "hits_per_min") &
    is.finite(treatment_contrasts_all$diff_raw),
  ,
  drop = FALSE
]
if (nrow(rq2_performance_plot) > 0) {
  direction <- ifelse(rq2_performance_plot$metric == "hits_per_min", -1, 1)
  rq2_performance_plot$diff_directional <- rq2_performance_plot$diff_raw * direction
  low_dir <- rq2_performance_plot$raw_low * direction
  high_dir <- rq2_performance_plot$raw_high * direction
  rq2_performance_plot$low_directional <- pmin(low_dir, high_dir)
  rq2_performance_plot$high_directional <- pmax(low_dir, high_dir)
  rq2_performance_plot$metric_label <- factor(
    rq2_performance_plot$metric_label,
    levels = c("Kills/min", "Golpes recibidos/min")
  )
  rq2_performance_plot$treatment <- factor(
    rq2_performance_plot$treatment,
    levels = truth_order[truth_order != "Control"]
  )
  rq2_performance_plot <- rq2_performance_plot[
    order(rq2_performance_plot$metric_label, rq2_performance_plot$treatment),
    ,
    drop = FALSE
  ]
  write.csv(rq2_performance_plot, file.path(out_dir, "12_rq2_directional_treatment_contrasts.csv"), row.names = FALSE, na = "")
  draw_forest_panels(
    rq2_performance_plot,
    file = "RQ2_performance_effects_directional_forest.png",
    title = "Desempeño frente a Control",
    subtitle = "",
    xlab = "Cambio direccional frente a Control",
    panel_col = "metric_label",
    row_col = "treatment",
    est_col = "diff_directional",
    low_col = "low_directional",
    high_col = "high_directional",
    null_value = 0,
    width = 3300,
    height = 1550,
    left_mar = 11.8,
    y_axis_cex = 0.94,
    per_panel_xlim = TRUE,
    symmetric_zero = TRUE,
    x_major_cex = 0.98,
    x_minor_cex = 0.66
  )
}

# RQ2: rate ratios from the duration-offset count model.
if (nrow(count_rate_ratios) > 0) {
  count_rate_ratios$metric_label <- factor(count_rate_ratios$metric_label, levels = count_specs$label)
  count_rate_ratios$treatment <- factor(count_rate_ratios$treatment, levels = truth_order)
  count_rate_ratios <- count_rate_ratios[order(count_rate_ratios$metric_label, count_rate_ratios$treatment), , drop = FALSE]
  draw_forest_panels(
    count_rate_ratios,
    file = "RQ2_count_model_rate_ratios.png",
    title = "Razones de tasa frente a Control",
    subtitle = "",
    xlab = "Rate ratio frente a Control",
    panel_col = "metric_label",
    row_col = "treatment",
    est_col = "rate_ratio",
    low_col = "rr_low",
    high_col = "rr_high",
    null_value = 1,
    log_x = TRUE,
    width = 3500,
    height = 1550,
    left_mar = 11.8,
    y_axis_cex = 0.94,
    per_panel_xlim = TRUE,
    x_major_cex = 0.98,
    x_minor_cex = 0.68
  )
}

# RQ3: quadrant view of immersion against hits received.
trade_quad <- merge(
  treatment_contrasts_all[treatment_contrasts_all$metric == "imm_total",
                          c("treatment", "diff_raw", "raw_low", "raw_high", "p_value", "q_value")],
  treatment_contrasts_all[treatment_contrasts_all$metric == "hits_per_min",
                          c("treatment", "diff_raw", "raw_low", "raw_high", "p_value", "q_value")],
  by = "treatment",
  suffixes = c("_giq", "_hits")
)
trade_quad$treatment <- factor(trade_quad$treatment, levels = truth_order[truth_order != "Control"])
trade_quad <- trade_quad[order(trade_quad$treatment), , drop = FALSE]

classify_change <- function(low, high, higher_is_better = TRUE) {
  if (!is.finite(low) || !is.finite(high)) return("sin evidencia clara")
  if (low > 0 && high > 0) return(ifelse(higher_is_better, "sube", "empeora"))
  if (low < 0 && high < 0) return(ifelse(higher_is_better, "baja", "mejora"))
  "sin evidencia clara"
}

if (nrow(trade_quad) > 0) {
  trade_quad$immersion_change <- mapply(classify_change, trade_quad$raw_low_giq, trade_quad$raw_high_giq, MoreArgs = list(higher_is_better = TRUE))
  trade_quad$hits_change <- mapply(classify_change, trade_quad$raw_low_hits, trade_quad$raw_high_hits, MoreArgs = list(higher_is_better = FALSE))
  trade_quad$tradeoff_class <- ifelse(
    trade_quad$immersion_change == "sube" & trade_quad$hits_change == "empeora", "trade-off posible",
    ifelse(
      trade_quad$immersion_change == "sube" & trade_quad$hits_change == "mejora", "mejora dominante",
      ifelse(
        trade_quad$immersion_change %in% c("baja", "sin evidencia clara") & trade_quad$hits_change == "empeora", "costo sin beneficio claro",
        ifelse(
          trade_quad$immersion_change == "baja" & trade_quad$hits_change == "mejora", "menor inmersion con mejor control",
          "sin evidencia clara"
        )
      )
    )
  )

  tradeoff_classification <- data.frame(
    treatment = as.character(trade_quad$treatment),
    delta_immersion = trade_quad$diff_raw_giq,
    delta_immersion_low = trade_quad$raw_low_giq,
    delta_immersion_high = trade_quad$raw_high_giq,
    delta_hits_per_min = trade_quad$diff_raw_hits,
    delta_hits_low = trade_quad$raw_low_hits,
    delta_hits_high = trade_quad$raw_high_hits,
    immersion_change = trade_quad$immersion_change,
    hits_change = trade_quad$hits_change,
    tradeoff_class = trade_quad$tradeoff_class,
    p_immersion = trade_quad$p_value_giq,
    q_immersion = trade_quad$q_value_giq,
    p_hits = trade_quad$p_value_hits,
    q_hits = trade_quad$q_value_hits,
    stringsAsFactors = FALSE
  )
  write.csv(tradeoff_classification, file.path(out_dir, "14_rq3_tradeoff_classification.csv"), row.names = FALSE, na = "")
}

open_png("RQ3_immersion_hits_tradeoff_quadrants.png", width = 3000, height = 1450)
op <- par(
  mar = c(4.35, 5.45, 4.15, 1.15),
  oma = c(2.5, 0, 1.0, 0),
  mgp = c(2.75, 0.75, 0),
  tcl = -0.25
)
if (nrow(trade_quad) > 0) {
  trade_quad$sig_class <- mapply(
    function(p1, q1, p2, q2) {
      cls <- c(sig_class(p1, q1), sig_class(p2, q2))
      if ("significativo_q" %in% cls) return("significativo_q")
      if ("nominal_p" %in% cls) return("nominal_p")
      if (all(cls == "no_disponible")) return("no_disponible")
      "no_significativo"
    },
    trade_quad$p_value_giq, trade_quad$q_value_giq,
    trade_quad$p_value_hits, trade_quad$q_value_hits
  )

  xlim <- range(c(trade_quad$diff_raw_giq, 0), na.rm = TRUE)
  ylim <- range(c(trade_quad$diff_raw_hits, 0), na.rm = TRUE)
  xpad <- diff(xlim) * 0.24
  ypad <- diff(ylim) * 0.26
  if (!is.finite(xpad) || xpad == 0) xpad <- 0.20
  if (!is.finite(ypad) || ypad == 0) ypad <- 0.20
  xlim <- xlim + c(-xpad, xpad)
  ylim <- ylim + c(-ypad, ypad)

  plot(
    NA, NA,
    xlim = xlim,
    ylim = ylim,
    axes = FALSE,
    xlab = "Cambio en inmersión frente a Control",
    ylab = "Cambio en golpes recibidos/min frente a Control",
    main = "Inmersión y golpes recibidos",
    cex.main = 1.22,
    cex.lab = 1.02
  )
  draw_x_axis_with_minor(xlim, major_cex = 0.98, minor_cex = 0.68)
  draw_y_axis_with_minor(ylim, major_cex = 1.02, minor_cex = 0.82)
  abline(v = 0, h = 0, lty = 2, col = "#8A918B", lwd = 1.1)

  point_fill <- adjustcolor(box_fill[as.character(trade_quad$treatment)], alpha.f = 0.84)
  point_border <- ifelse(
    trade_quad$sig_class == "significativo_q", "#111111",
    ifelse(trade_quad$sig_class == "nominal_p", "#8B5A10",
           ifelse(trade_quad$sig_class == "no_disponible", "#9EA59B", box_border[as.character(trade_quad$treatment)]))
  )
  point_lwd <- ifelse(trade_quad$sig_class == "significativo_q", 2.35, ifelse(trade_quad$sig_class == "nominal_p", 1.7, 1.3))

  points(
    trade_quad$diff_raw_giq,
    trade_quad$diff_raw_hits,
    pch = 21,
    bg = point_fill,
    col = point_border,
    cex = ifelse(trade_quad$sig_class == "significativo_q", 1.68, 1.55),
    lwd = point_lwd
  )

  text(
    trade_quad$diff_raw_giq,
    trade_quad$diff_raw_hits,
    labels = as.character(trade_quad$treatment),
    pos = 1,
    offset = 0.70,
    cex = 0.84,
    col = ifelse(trade_quad$sig_class == "significativo_q", "#111111", ifelse(trade_quad$sig_class == "nominal_p", "#8B5A10", soft_ink)),
    xpd = TRUE
  )

  trade_quad$label_text <- mapply(
    function(p1, q1, p2, q2) {
      lab <- c(
        display_stat_label(p1, q1, p_prefix = "pI", q_prefix = "qI"),
        display_stat_label(p2, q2, p_prefix = "pH", q_prefix = "qH")
      )
      paste(lab, collapse = " | ")
    },
    trade_quad$p_value_giq, trade_quad$q_value_giq,
    trade_quad$p_value_hits, trade_quad$q_value_hits
  )
  text(
    trade_quad$diff_raw_giq,
    trade_quad$diff_raw_hits,
    labels = trade_quad$label_text,
    pos = 3,
    offset = 0.72,
    cex = ifelse(trade_quad$sig_class == "significativo_q", 0.76, 0.70),
    font = ifelse(trade_quad$sig_class == "significativo_q", 2, 1),
    col = ifelse(trade_quad$sig_class == "significativo_q", "#111111", ifelse(trade_quad$sig_class == "nominal_p", "#8B5A10", soft_ink))
  )

  box(col = panel_border, lwd = 1.15)
}
mtext("Los puntos usan el color del tratamiento; borde negro grueso: q < 0.05; borde café: p < 0.05 pero q ≥ 0.05. Derecha = mayor inmersión; arriba = más golpes recibidos.", side = 1, outer = TRUE, line = 0.55, cex = 0.80, col = soft_ink)
par(op)
close_png()


# -------------------------------------------------------------------------
# Additional simplified diagnostic figures inspired by the uploaded example scripts.
# These use only the simplified metrics kept in the current analysis.
# -------------------------------------------------------------------------

main_metric_order <- c("imm_total", "ingame_survival_s", "kills_per_min", "hits_per_min")
main_metric_label_map <- c(
  "imm_total" = "Inmersión percibida",
  "ingame_survival_s" = "Supervivencia (s)",
  "kills_per_min" = "Kills/min",
  "hits_per_min" = "Golpes recibidos/min"
)
main_term_order <- c("Shake", "Zoom", "Recoil", "Shake:Zoom", "Shake:Recoil", "Zoom:Recoil", "Shake:Zoom:Recoil")
main_term_short <- c("Shake", "Zoom", "Recoil", "SxZ", "SxR", "ZxR", "SxZxR")

# 1) ART factorial matrix for the simplified metrics.
if (nrow(art_results) > 0) {
  art_plot <- merge(
    expand.grid(metric = main_metric_order, term = main_term_order, stringsAsFactors = FALSE),
    art_results[, c("metric", "term", "p_value", "q_value", "f_value")],
    by = c("metric", "term"),
    all.x = TRUE,
    sort = FALSE
  )
  art_plot$metric_label <- main_metric_label_map[art_plot$metric]
  art_plot$term_short <- main_term_short[match(art_plot$term, main_term_order)]
  art_plot$score <- ifelse(
    is.finite(art_plot$q_value), pmax(0, -log10(pmax(art_plot$q_value, 1e-4))),
    ifelse(is.finite(art_plot$p_value), 0.60 * pmax(0, -log10(pmax(art_plot$p_value, 1e-4))), 0)
  )
  art_plot$evidence_class <- ifelse(
    is.finite(art_plot$q_value) & art_plot$q_value < 0.05, "good_q",
    ifelse(is.finite(art_plot$p_value) & art_plot$p_value < 0.05, "good_p", "bad_ns")
  )
  max_score <- max(art_plot$score, na.rm = TRUE)
  if (!is.finite(max_score) || max_score <= 0) max_score <- 1
  green_pal <- grDevices::colorRampPalette(c("#EAF6EA", "#BFE0BE", "#79BE7E", "#3E8D54", "#203B2B"))(100)
  red_pal <- grDevices::colorRampPalette(c("#FBEDED", "#F2C9C9", "#E69797", "#D25E5E", "#A53030"))(100)

  open_png("00_art_factorial_main_metrics.png", width = 3400, height = 1850)
  op <- par(mar = c(4.3, 10.6, 4.4, 1.2), oma = c(2.1, 0, 0.4, 0), mgp = c(2.4, 0.72, 0), tcl = -0.22)
  plot(NA, NA,
       xlim = c(0.5, length(main_term_order) + 0.5),
       ylim = c(0.5, length(main_metric_order) + 0.5),
       axes = FALSE, xlab = "", ylab = "",
       main = utf8_text("ART factorial: métricas principales"), cex.main = 1.28)

  for (r in seq_along(main_metric_order)) {
    for (c in seq_along(main_term_order)) {
      hit <- art_plot[art_plot$metric == main_metric_order[r] & art_plot$term == main_term_order[c], , drop = FALSE]
      score <- hit$score[1]
      fill_col <- if (is.finite(score) && score > 0) {
        idx_col <- max(1, min(100, round(score / max_score * 99) + 1))
        if (identical(hit$evidence_class[1], "good_q")) {
          green_pal[idx_col]
        } else if (identical(hit$evidence_class[1], "good_p")) {
          green_pal[max(1, round(idx_col * 0.70))]
        } else {
          red_pal[max(1, round(max(8, idx_col * 0.65)))]
        }
      } else {
        "#F7EDED"
      }
      yrow <- length(main_metric_order) - r + 1
      rect(c - 0.5, yrow - 0.5, c + 0.5, yrow + 0.5, col = fill_col, border = "#D5DDD5", lwd = 1)

      p_lab <- paste0("p=", fmt_p(hit$p_value[1]))
      q_lab <- paste0("q=", fmt_p(hit$q_value[1]))
      is_q_sig <- is.finite(hit$q_value[1]) && hit$q_value[1] < 0.05
      text(c, yrow + 0.08, utf8_text(p_lab), cex = if (is_q_sig) 0.98 else 0.92,
           font = if (is_q_sig) 2 else 1, col = "#202423")
      text(c, yrow - 0.10, utf8_text(q_lab), cex = if (is_q_sig) 1.00 else 0.94,
           font = if (is_q_sig) 2 else 1, col = "#202423")
    }
  }

  axis(1, at = seq_along(main_term_order), labels = utf8_text(main_term_short), tick = FALSE, cex.axis = 1.12)
  axis(2, at = rev(seq_along(main_metric_order)), labels = utf8_text(unname(main_metric_label_map[main_metric_order])), las = 1, tick = FALSE, cex.axis = 1.08)
  box(col = "#D5DDD5", lwd = 1.1)
  mtext(utf8_text("Verde = evidencia favorable (q < 0.05 o p < 0.05); rojo = sin evidencia clara. Texto = p nominal y q FDR."), side = 1, outer = TRUE, line = 0.7, cex = 0.90, col = soft_ink)
  par(op)
  close_png()
}

# 2) Assumption validation plot for the simplified metrics.
assump_plot <- assumptions[assumptions$metric %in% main_metric_order, , drop = FALSE]
if (nrow(assump_plot) > 0) {
  assump_plot$metric_label_short <- main_metric_label_map[assump_plot$metric]
  assump_plot <- assump_plot[match(main_metric_order, assump_plot$metric), , drop = FALSE]
  threshold_x <- -log10(0.05)

  draw_assumption_panel <- function(values, labels, title_text, pass_label) {
    score <- -log10(pmax(values, 1e-12))
    xlim <- c(0, max(3, score, na.rm = TRUE) * 1.12)
    y <- rev(seq_along(labels))
    plot(score, y,
         xlim = xlim,
         ylim = c(0.5, length(labels) + 0.5),
         axes = FALSE,
         xlab = "-log10(p)", ylab = "",
         main = utf8_text(title_text),
         pch = 21,
         bg = ifelse(is.finite(values) & values < 0.05, "#E07A20", "#58B368"),
         col = "#202423",
         cex = 1.55,
         lwd = 1.2,
         cex.main = 1.15)
    x_major <- pretty(c(0, xlim[2]), n = 6)
    x_major <- x_major[x_major >= 0 & x_major <= xlim[2]]
    abline(v = x_major, col = adjustcolor(grid_col, alpha.f = 0.75), lwd = 1)
    abline(h = y, col = adjustcolor(grid_col, alpha.f = 0.55), lwd = 0.9)
    abline(v = threshold_x, lty = 2, col = "#4A4A4A", lwd = 1.4)
    points(score, y,
           pch = 21,
           bg = ifelse(is.finite(values) & values < 0.05, "#E07A20", "#58B368"),
           col = "#202423",
           cex = 1.55,
           lwd = 1.2)
    axis(1, at = x_major, labels = axis_label(x_major, x_major), cex.axis = 0.96)
    axis(2, at = y, labels = utf8_text(labels), las = 1, tick = FALSE, cex.axis = 0.96)
    text(score, y, labels = paste0("p=", fmt_p(values)), pos = 4, offset = 0.35, cex = 0.82, col = "#202423")
    box(col = panel_border, lwd = 1.1)
  }

  open_png("00_assumption_validation_main_metrics.png", width = 3200, height = 1650)
  op <- par(mfrow = c(1, 2), mar = c(4.4, 9.0, 3.6, 1.3), oma = c(2.5, 0, 2.1, 0), mgp = c(2.35, 0.72, 0), tcl = -0.22)
  draw_assumption_panel(
    assump_plot$shapiro_residual_p,
    assump_plot$metric_label_short,
    "Normalidad residual",
    "Cumple"
  )
  draw_assumption_panel(
    assump_plot$brown_forsythe_p,
    assump_plot$metric_label_short,
    "Homogeneidad de varianza",
    "Cumple"
  )
  mtext(utf8_text("Validación de supuestos"), side = 3, outer = TRUE, adj = 0.00, line = 0.8, cex = 1.36, font = 2, col = ink)
  mtext(utf8_text("Línea punteada: p = 0.05; verde = no rechaza el supuesto; naranja = posible incumplimiento."), side = 1, outer = TRUE, line = 0.6, cex = 0.92, col = soft_ink)
  par(op)
  close_png()
}

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
  "Primary metric set:",
  "- RQ1: Inmersion percibida (imm_total).",
  "- RQ2: Kills/min and golpes recibidos/min.",
  "- RQ2 context: supervivencia en segundos.",
  "- RQ3: cambio conjunto de inmersion y golpes recibidos frente a Control.",
  "",
  "Order of calculations:",
  "1. Load juiciness_clean_dataset.csv.",
  "2. Retain all rows and keep quality flags as audit metadata; FPS is not used as a treatment-level validity metric.",
  "2b. Added two simplified diagnostic figures inspired by generate_interpretable_pvalue_figures.R and anova_assumptions_and_alternatives.R.",
  "3. RQ1: factorial model for perceived immersion, residual checks, variance checks, and contrasts vs Control.",
  "4. RQ2: factorial/ART checks for kills/min and golpes recibidos/min; survival is contextual.",
  "5. RQ2 supplementary counts: negative-binomial models with duration offset for total kills and total hits.",
  "6. RQ3: all treatments contrasted against Control for immersion and hits/min.",
  "7. RQ3: trade-off classification plus a quadrant plot for immersion vs hits/min.",
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
  paste(sprintf("- analysis/images/research_questions/%s", final_figure_files), collapse = "\n"),
  "",
  "Main tables:",
  "- analysis/results/research_questions/00_primary_factorial_tests_main_metrics.csv",
  "- analysis/results/research_questions/00_primary_factorial_tests_main_metrics.txt",
  "- analysis/results/research_questions/07_rq3_all_treatments_vs_control.csv",
  "- analysis/results/research_questions/11_all_treatments_vs_control.csv",
  "- analysis/results/research_questions/11_all_treatments_vs_control_pq_values.txt",
  "- analysis/results/research_questions/12_rq2_directional_treatment_contrasts.csv",
  "- analysis/results/research_questions/14_rq3_tradeoff_classification.csv"
)

writeLines(summary_lines, file.path(out_dir, "summary.txt"))

guide_lines <- c(
  "# Final Research Figures",
  "",
  "Use only these figures for the academic presentation unless a reviewer asks for diagnostics.",
  "The two additional diagnostic plots were inspired by useful ideas found in generate_interpretable_pvalue_figures.R and anova_assumptions_and_alternatives.R.",
  "",
  "## Diagnostics",
  "",
  "- `00_assumption_validation_main_metrics.png`: residual normality and Brown-Forsythe variance checks for the four simplified metrics.",
  "- `00_art_factorial_main_metrics.png`: ART factorial matrix for the four simplified metrics with p and q shown inside each cell.",
  "",
  "## RQ1",
  "",
  "- `RQ1_immersion_giq_effects.png`: Inmersion percibida by treatment. It includes raw observations, median/IQR, mean, 95% CI and the relevant factorial p-value.",
  "- `RQ1_immersion_vs_control_forest.png`: difference in GIQ against Control for all non-control treatments with 95% CI.",
  "",
  "## RQ2",
  "",
  "- `RQ2_kills_per_min.png`: offensive performance by treatment.",
  "- `RQ2_hits_per_min.png`: received hits per minute by treatment. Lower values indicate better defensive/motor performance.",
  "- `RQ2_performance_effects_directional_forest.png`: directional treatment contrasts against Control for all non-control treatments. Right means better performance.",
  "- `RQ2_survival_time_context.png`: contextual survival-time figure. It helps interpret rates and counts but is not the main performance metric.",
  "- `RQ2_count_models_duration_offset.png`: supplementary duration-adjusted total hits.",
  "- `RQ2_count_model_rate_ratios.png`: supplementary duration-offset count-model rate ratios for all treatments, with Control shown as reference = 1.",
  "",
  "## RQ3",
  "",
  "- `RQ3_immersion_vs_control_forest.png`: all non-control treatments against Control for Inmersion percibida.",
  "- `RQ3_hits_vs_control_forest.png`: all non-control treatments against Control for Golpes recibidos/min.",
  "- `RQ3_immersion_hits_tradeoff_quadrants.png`: one point per treatment showing change in GIQ vs change in hits/min against Control; p-values appear above each point and significant treatments are highlighted.",
  "- `RQ3_giq_performance_correlation.png`: secondary individual-level Spearman association, not the primary trade-off test.",
  "- `14_rq3_tradeoff_classification.csv`: treatment-level interpretation table for the trade-off decision.",
  "",
  "## Calculation Order",
  "",
  "1. juiciness_clean_dataset.csv.",
  "2. Retain all rows and keep quality flags as audit metadata; do not use FPS as a treatment-level validity figure.",
  "3. RQ1 GIQ factorial model with assumptions and all-treatment contrasts vs Control.",
  "4. Use ANOVA for metrics that pass residual-normality and variance checks; use ART for metrics that do not.",
  "5. RQ2 supplementary count models with duration offset.",
  "6. RQ3 all-treatment contrasts vs Control.",
  "7. RQ3 trade-off quadrant and classification using immersion vs received hits.",
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