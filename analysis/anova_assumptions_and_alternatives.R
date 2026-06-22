# Diagnose ANOVA assumptions and run defensible alternatives on the Hard Clean set.
#
# Outputs:
#   analysis/anova_assumption_checks_hard_clean.csv
#   analysis/anova_type1_vs_type3_hard_clean.csv
#   analysis/anova_permutation_sensitivity_hard_clean.csv
#   analysis/rank_anova_sensitivity_hard_clean.csv
#   analysis/count_model_alternatives_hard_clean.csv
#   analysis/survival_model_alternative_hard_clean.csv
#   analysis/anova_alternatives_summary_hard_clean.txt

suppressPackageStartupMessages({
  library(stats)
  library(MASS)
})

set.seed(20260622)

output_dir <- "analysis"
data_dir <- file.path("supabase_data", "r_outputs_existing_metrics")
run_level_path <- file.path(data_dir, "juicy_vs_existing_metrics_run_level.csv")
flags_path <- file.path(output_dir, "problematic_flags_long.csv")
cfg_path <- file.path("supabase_data", "PlayerIDConfig_rows.csv")
form_path <- "Formulario Integrado InvCC26a Experimentos .csv"
if (!file.exists(form_path)) {
  form_path <- file.path("analysis", "Formulario Integrado InvCC26a Experimentos .csv")
}

stopifnot(file.exists(run_level_path), file.exists(flags_path), file.exists(cfg_path), file.exists(form_path))

condition_levels <- c(
  "C0_baseline", "C1_shake", "C2_zoom", "C3_recoil",
  "C4_shake_zoom", "C5_shake_recoil", "C6_zoom_recoil", "C7_all"
)

hard_clean_rules <- c(
  "duration_lt_30s",
  "zero_input_total",
  "zero_kills_after_120s",
  "fps_min_lt_15_or_drop_gt_10pct"
)

metric_labels <- c(
  giq_total = "GIQ total",
  engagement = "GIQ engagement",
  engrossment = "GIQ engrossment",
  total_immersion = "GIQ total immersion",
  duration_seconds = "Supervivencia",
  kill_rate = "Kills/s",
  input_rate = "Inputs/s",
  damage_taken_rate = "Dano recibido/s",
  jitter_rate = "Jitter/s",
  distance_rate = "Distancia/s",
  nearest_enemy_dist_mean = "Distancia a enemigo",
  low_hp_ratio = "Proporcion HP bajo"
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
  factor(
    as_bool(x),
    levels = c(FALSE, TRUE),
    labels = c("Ausente", "Presente")
  )
}

prepare_design <- function(df, outcome) {
  required <- c(outcome, "camera_shake", "camera_zoom", "camera_recoil")
  if (!all(required %in% names(df))) return(NULL)

  out <- df[
    is.finite(df[[outcome]]) &
      !is.na(df$camera_shake) &
      !is.na(df$camera_zoom) &
      !is.na(df$camera_recoil),
  ]
  if (nrow(out) < 8) return(NULL)

  out$Y <- as.numeric(out[[outcome]])
  out$Shake <- safe_bool_factor(out$camera_shake)
  out$Zoom <- safe_bool_factor(out$camera_zoom)
  out$Recoil <- safe_bool_factor(out$camera_recoil)
  out$condition_cell <- interaction(out$Shake, out$Zoom, out$Recoil, drop = TRUE)

  if (length(unique(out$Shake)) < 2 || length(unique(out$Zoom)) < 2 || length(unique(out$Recoil)) < 2) {
    return(NULL)
  }
  out
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

extract_aov_table <- function(fit, metric, metric_label, model_label = "ANOVA Type I") {
  tab <- as.data.frame(summary(fit)[[1]])
  tab$term <- trimws(rownames(tab))
  rownames(tab) <- NULL
  residual_ss <- tab$`Sum Sq`[tab$term == "Residuals"]
  residual_df <- tab$Df[tab$term == "Residuals"]
  tab <- tab[tab$term != "Residuals", ]
  if (nrow(tab) == 0 || length(residual_ss) == 0) return(NULL)

  data.frame(
    metric = metric,
    metric_label = metric_label,
    model = model_label,
    term = tab$term,
    df_effect = tab$Df,
    df_residual = residual_df,
    statistic = tab$`F value`,
    p_value = tab$`Pr(>F)`,
    partial_eta_squared = tab$`Sum Sq` / (tab$`Sum Sq` + residual_ss),
    stringsAsFactors = FALSE
  )
}

type3_wald_table <- function(fit, metric, metric_label) {
  beta <- coef(fit)
  vc <- vcov(fit)
  sigma2 <- summary(fit)$sigma^2
  df_res <- df.residual(fit)
  assign <- attr(model.matrix(fit), "assign")
  terms <- attr(terms(fit), "term.labels")

  rows <- lapply(seq_along(terms), function(i) {
    idx <- which(assign == i)
    idx <- idx[idx %in% seq_along(beta)]
    if (length(idx) == 0) return(NULL)
    beta_i <- beta[idx]
    vc_i <- vc[idx, idx, drop = FALSE]
    if (any(!is.finite(beta_i)) || any(!is.finite(vc_i))) return(NULL)
    q <- length(idx)
    f_value <- as.numeric(t(beta_i) %*% solve(vc_i, beta_i) / q)
    p_value <- pf(f_value, q, df_res, lower.tail = FALSE)
    eta <- (f_value * q) / (f_value * q + df_res)
    data.frame(
      metric = metric,
      metric_label = metric_label,
      model = "ANOVA Type III Wald",
      term = terms[i],
      df_effect = q,
      df_residual = df_res,
      statistic = f_value,
      p_value = p_value,
      partial_eta_squared = eta,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}

brown_forsythe_p <- function(df) {
  med <- ave(df$Y, df$condition_cell, FUN = function(x) median(x, na.rm = TRUE))
  abs_dev <- abs(df$Y - med)
  fit <- aov(abs_dev ~ condition_cell, data = df)
  tab <- summary(fit)[[1]]
  as.numeric(tab$`Pr(>F)`[1])
}

assumption_check <- function(df, metric, metric_label) {
  fit <- aov(Y ~ Shake * Zoom * Recoil, data = df)
  residuals_fit <- residuals(fit)
  residuals_fit <- residuals_fit[is.finite(residuals_fit)]
  shapiro <- if (length(residuals_fit) >= 3 && length(residuals_fit) <= 5000) {
    shapiro.test(residuals_fit)
  } else {
    NULL
  }
  cell_sizes <- table(df$condition_cell)

  data.frame(
    metric = metric,
    metric_label = metric_label,
    n = nrow(df),
    min_cell_n = min(cell_sizes),
    max_cell_n = max(cell_sizes),
    residual_shapiro_W = if (is.null(shapiro)) NA_real_ else unname(shapiro$statistic),
    residual_shapiro_p = if (is.null(shapiro)) NA_real_ else shapiro$p.value,
    residual_normality_flag = if (is.null(shapiro)) NA_character_ else ifelse(shapiro$p.value < 0.05, "fails_p_lt_0.05", "no_evidence_against_normality"),
    brown_forsythe_p = brown_forsythe_p(df),
    variance_flag = ifelse(brown_forsythe_p(df) < 0.05, "heterogeneous_p_lt_0.05", "no_evidence_against_equal_variance"),
    stringsAsFactors = FALSE
  )
}

permutation_table <- function(df, metric, metric_label, B = 999) {
  old_options <- options(contrasts = c("contr.sum", "contr.poly"))
  fit <- lm(Y ~ Shake * Zoom * Recoil, data = df)
  observed <- type3_wald_table(fit, metric, metric_label)
  options(old_options)
  if (is.null(observed)) return(NULL)
  observed$model <- "ANOVA Type III permutation"

  terms <- observed$term
  f_perm <- matrix(NA_real_, nrow = B, ncol = length(terms), dimnames = list(NULL, terms))
  for (b in seq_len(B)) {
    df$Y_perm <- sample(df$Y)
    old_options <- options(contrasts = c("contr.sum", "contr.poly"))
    fit_b <- lm(Y_perm ~ Shake * Zoom * Recoil, data = df)
    tab_b <- type3_wald_table(fit_b, metric, metric_label)
    options(old_options)
    for (term in terms) {
      f_perm[b, term] <- tab_b$statistic[tab_b$term == term]
    }
  }

  observed$permutations <- B
  observed$parametric_p_value <- observed$p_value
  observed$p_value <- vapply(seq_along(terms), function(i) {
    f_obs <- observed$statistic[i]
    (sum(f_perm[, i] >= f_obs, na.rm = TRUE) + 1) / (sum(is.finite(f_perm[, i])) + 1)
  }, numeric(1))
  observed
}

rank_anova_table <- function(df, metric, metric_label) {
  df$Y <- rank(df$Y, ties.method = "average")
  old_options <- options(contrasts = c("contr.sum", "contr.poly"))
  fit <- lm(Y ~ Shake * Zoom * Recoil, data = df)
  out <- type3_wald_table(fit, metric, metric_label)
  options(old_options)
  out
}

model_wald_terms <- function(fit, metric, metric_label, model_label, reference = c("F", "Chisq")) {
  reference <- match.arg(reference)
  beta <- coef(fit)
  vc <- vcov(fit)
  assign <- attr(model.matrix(fit), "assign")
  terms <- attr(terms(fit), "term.labels")
  df_res <- df.residual(fit)

  rows <- lapply(seq_along(terms), function(i) {
    idx <- which(assign == i)
    idx <- idx[idx %in% seq_along(beta)]
    if (length(idx) == 0) return(NULL)
    beta_i <- beta[idx]
    vc_i <- vc[idx, idx, drop = FALSE]
    if (any(!is.finite(beta_i)) || any(!is.finite(vc_i))) return(NULL)
    q <- length(idx)
    wald <- as.numeric(t(beta_i) %*% solve(vc_i, beta_i))
    if (reference == "F") {
      statistic <- wald / q
      p_value <- pf(statistic, q, df_res, lower.tail = FALSE)
    } else {
      statistic <- wald
      p_value <- pchisq(statistic, q, lower.tail = FALSE)
    }
    data.frame(
      metric = metric,
      metric_label = metric_label,
      model = model_label,
      term = terms[i],
      df_effect = q,
      df_residual = df_res,
      statistic_reference = reference,
      statistic = statistic,
      p_value = p_value,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}

count_model_table <- function(df, count_var, metric_label) {
  required <- c(count_var, "duration_seconds", "camera_shake", "camera_zoom", "camera_recoil")
  if (!all(required %in% names(df))) return(NULL)

  x <- df[
    is.finite(df[[count_var]]) &
      is.finite(df$duration_seconds) &
      df$duration_seconds > 0 &
      df[[count_var]] >= 0,
  ]
  if (nrow(x) < 8 || length(unique(x[[count_var]])) < 2) return(NULL)

  x$count <- round(as.numeric(x[[count_var]]))
  x$Shake <- safe_bool_factor(x$camera_shake)
  x$Zoom <- safe_bool_factor(x$camera_zoom)
  x$Recoil <- safe_bool_factor(x$camera_recoil)
  form <- count ~ Shake * Zoom * Recoil + offset(log(duration_seconds))

  old_options <- options(contrasts = c("contr.sum", "contr.poly"))
  poisson_fit <- glm(form, family = poisson(link = "log"), data = x)
  dispersion <- sum(residuals(poisson_fit, type = "pearson")^2) / df.residual(poisson_fit)

  quasi_fit <- glm(form, family = quasipoisson(link = "log"), data = x)
  quasi_rows <- model_wald_terms(
    quasi_fit,
    count_var,
    metric_label,
    "quasi-Poisson count model with duration offset",
    reference = "F"
  )

  nb_fit <- tryCatch(glm.nb(form, data = x), error = function(e) NULL, warning = function(w) {
    suppressWarnings(glm.nb(form, data = x))
  })
  options(old_options)
  nb_rows <- if (is.null(nb_fit)) NULL else model_wald_terms(
    nb_fit,
    count_var,
    metric_label,
    "negative binomial count model with duration offset",
    reference = "Chisq"
  )

  rows <- rbind(quasi_rows, nb_rows)
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  rows$n <- nrow(x)
  rows$poisson_dispersion <- dispersion
  rows
}

survival_model_table <- function(df) {
  if (!requireNamespace("survival", quietly = TRUE)) return(NULL)
  required <- c("duration_seconds", "died", "camera_shake", "camera_zoom", "camera_recoil")
  if (!all(required %in% names(df))) return(NULL)

  x <- df[is.finite(df$duration_seconds) & df$duration_seconds > 0, ]
  if (nrow(x) < 8 || length(unique(as_bool(x$died))) < 2) return(NULL)
  x$event <- as.integer(as_bool(x$died))
  x$Shake <- safe_bool_factor(x$camera_shake)
  x$Zoom <- safe_bool_factor(x$camera_zoom)
  x$Recoil <- safe_bool_factor(x$camera_recoil)

  fit <- survival::coxph(survival::Surv(duration_seconds, event) ~ Shake * Zoom * Recoil, data = x)
  tab <- drop1(fit, test = "Chisq")
  tab$term <- rownames(tab)
  tab <- tab[tab$term != "<none>", ]
  data.frame(
    metric = "duration_seconds",
    metric_label = "Supervivencia con censura por victoria",
    n = nrow(x),
    events = sum(x$event),
    model = "Cox proportional hazards",
    term = tab$term,
    statistic = tab$LRT,
    p_value = tab$`Pr(>Chi)`,
    stringsAsFactors = FALSE
  )
}

run_level_all <- read.csv(run_level_path, check.names = FALSE, stringsAsFactors = FALSE)
run_level_all$row_number <- seq_len(nrow(run_level_all))
flags <- read.csv(flags_path, check.names = FALSE, stringsAsFactors = FALSE)
hard_clean_rows <- unique(flags$row_number[flags$rule %in% hard_clean_rules])
run_level <- run_level_all[!run_level_all$row_number %in% hard_clean_rows, ]
run_level$condition <- factor(run_level$condition, levels = condition_levels)

form <- read.csv(form_path, check.names = FALSE, stringsAsFactors = FALSE)
cfg <- read.csv(cfg_path, check.names = FALSE, stringsAsFactors = FALSE)
giq <- build_giq(form, cfg)

analysis_specs <- list(
  list(data = giq, metric = "giq_total"),
  list(data = giq, metric = "engagement"),
  list(data = giq, metric = "engrossment"),
  list(data = giq, metric = "total_immersion"),
  list(data = run_level, metric = "duration_seconds"),
  list(data = run_level, metric = "kill_rate"),
  list(data = run_level, metric = "input_rate"),
  list(data = run_level, metric = "damage_taken_rate"),
  list(data = run_level, metric = "jitter_rate"),
  list(data = run_level, metric = "distance_rate"),
  list(data = run_level, metric = "nearest_enemy_dist_mean"),
  list(data = run_level, metric = "low_hp_ratio")
)

assumptions <- list()
anova_rows <- list()
type3_rows <- list()
permutation_rows <- list()
rank_rows <- list()

for (spec in analysis_specs) {
  metric <- spec$metric
  metric_label <- if (metric %in% names(metric_labels)) metric_labels[[metric]] else metric
  df <- prepare_design(spec$data, metric)
  if (is.null(df)) next

  assumptions[[metric]] <- assumption_check(df, metric, metric_label)

  fit_aov <- aov(Y ~ Shake * Zoom * Recoil, data = df)
  anova_rows[[metric]] <- extract_aov_table(fit_aov, metric, metric_label, "ANOVA Type I")

  old_options <- options(contrasts = c("contr.sum", "contr.poly"))
  fit_lm <- lm(Y ~ Shake * Zoom * Recoil, data = df)
  type3_rows[[metric]] <- type3_wald_table(fit_lm, metric, metric_label)
  options(old_options)

  permutation_rows[[metric]] <- permutation_table(df, metric, metric_label, B = 999)
  rank_rows[[metric]] <- rank_anova_table(df, metric, metric_label)
}

assumptions_df <- do.call(rbind, assumptions)
anova_type1 <- do.call(rbind, anova_rows)
anova_type3 <- do.call(rbind, type3_rows)
permutation_df <- do.call(rbind, permutation_rows)
rank_df <- do.call(rbind, rank_rows)

anova_type1$q_value <- p.adjust(anova_type1$p_value, method = "BH")
anova_type3$q_value <- p.adjust(anova_type3$p_value, method = "BH")
permutation_df$q_value <- p.adjust(permutation_df$p_value, method = "BH")
rank_df$q_value <- p.adjust(rank_df$p_value, method = "BH")

anova_compare <- merge(
  anova_type1,
  anova_type3,
  by = c("metric", "metric_label", "term"),
  suffixes = c("_type1", "_type3"),
  all = TRUE
)

count_specs <- list(
  total_kills = "Kills con offset por duracion",
  input_total = "Inputs con offset por duracion",
  hits_taken = "Golpes recibidos con offset por duracion",
  damage_events_total = "Eventos de dano con offset por duracion"
)

count_rows <- lapply(names(count_specs), function(v) count_model_table(run_level, v, count_specs[[v]]))
count_df <- do.call(rbind, count_rows[!vapply(count_rows, is.null, logical(1))])
count_df$q_value <- p.adjust(count_df$p_value, method = "BH")
survival_df <- survival_model_table(run_level)
if (!is.null(survival_df)) {
  survival_df$q_value <- p.adjust(survival_df$p_value, method = "BH")
}

write.csv(assumptions_df, file.path(output_dir, "anova_assumption_checks_hard_clean.csv"), row.names = FALSE, na = "")
write.csv(anova_compare, file.path(output_dir, "anova_type1_vs_type3_hard_clean.csv"), row.names = FALSE, na = "")
write.csv(permutation_df, file.path(output_dir, "anova_permutation_sensitivity_hard_clean.csv"), row.names = FALSE, na = "")
write.csv(rank_df, file.path(output_dir, "rank_anova_sensitivity_hard_clean.csv"), row.names = FALSE, na = "")
write.csv(count_df, file.path(output_dir, "count_model_alternatives_hard_clean.csv"), row.names = FALSE, na = "")
if (!is.null(survival_df)) {
  write.csv(survival_df, file.path(output_dir, "survival_model_alternative_hard_clean.csv"), row.names = FALSE, na = "")
}

sig_subset <- function(df, p_col = "p_value", alpha = 0.05) {
  if (is.null(df) || nrow(df) == 0 || !p_col %in% names(df)) return(df[0, ])
  df[is.finite(df[[p_col]]) & df[[p_col]] < alpha, ]
}

lines <- c(
  "ANOVA assumptions and alternatives - Hard Clean",
  paste0("Telemetry runs original: ", nrow(run_level_all)),
  paste0("Telemetry runs removed by Hard Clean: ", length(hard_clean_rows)),
  paste0("Telemetry runs retained: ", nrow(run_level)),
  paste0("GIQ responses with condition mapping: ", sum(!is.na(giq$condition))),
  "",
  "Normality check uses Shapiro-Wilk on ANOVA residuals, not raw variables.",
  "Variance check uses Brown-Forsythe by factorial condition cell.",
  "",
  "Residual normality failures (p < .05):"
)

normality_failures <- assumptions_df[assumptions_df$residual_shapiro_p < 0.05, ]
if (nrow(normality_failures) == 0) {
  lines <- c(lines, "  None.")
} else {
  lines <- c(lines, paste0(
    "  - ", normality_failures$metric_label,
    ": W=", sprintf("%.3f", normality_failures$residual_shapiro_W),
    ", p=", sprintf("%.4f", normality_failures$residual_shapiro_p)
  ))
}

lines <- c(lines, "", "Brown-Forsythe variance failures (p < .05):")
variance_failures <- assumptions_df[assumptions_df$brown_forsythe_p < 0.05, ]
if (nrow(variance_failures) == 0) {
  lines <- c(lines, "  None.")
} else {
  lines <- c(lines, paste0(
    "  - ", variance_failures$metric_label,
    ": p=", sprintf("%.4f", variance_failures$brown_forsythe_p)
  ))
}

lines <- c(lines, "", "Type III Wald ANOVA nominal effects (p < .05):")
type3_sig <- sig_subset(anova_type3)
if (nrow(type3_sig) == 0) {
  lines <- c(lines, "  None.")
} else {
  type3_sig <- type3_sig[order(type3_sig$p_value), ]
  lines <- c(lines, paste0(
    "  - ", type3_sig$metric_label,
    " / ", type3_sig$term,
    ": F=", sprintf("%.2f", type3_sig$statistic),
    ", p=", sprintf("%.4f", type3_sig$p_value),
    ", eta_p2=", sprintf("%.3f", type3_sig$partial_eta_squared)
  ))
}

lines <- c(lines, "", "Type III Wald ANOVA effects after BH-FDR (q < .05):")
type3_q_sig <- anova_type3[is.finite(anova_type3$q_value) & anova_type3$q_value < 0.05, ]
if (nrow(type3_q_sig) == 0) {
  lines <- c(lines, "  None.")
} else {
  type3_q_sig <- type3_q_sig[order(type3_q_sig$q_value), ]
  lines <- c(lines, paste0(
    "  - ", type3_q_sig$metric_label,
    " / ", type3_q_sig$term,
    ": p=", sprintf("%.4f", type3_q_sig$p_value),
    ", q=", sprintf("%.4f", type3_q_sig$q_value)
  ))
}

lines <- c(lines, "", "Permutation ANOVA nominal effects (p < .05; 999 permutations):")
perm_sig <- sig_subset(permutation_df)
if (nrow(perm_sig) == 0) {
  lines <- c(lines, "  None.")
} else {
  perm_sig <- perm_sig[order(perm_sig$p_value), ]
  lines <- c(lines, paste0(
    "  - ", perm_sig$metric_label,
    " / ", perm_sig$term,
    ": F=", sprintf("%.2f", perm_sig$statistic),
    ", p_perm=", sprintf("%.4f", perm_sig$p_value),
    ", p_param=", sprintf("%.4f", perm_sig$parametric_p_value)
  ))
}

lines <- c(lines, "", "Permutation ANOVA effects after BH-FDR (q < .05):")
perm_q_sig <- permutation_df[is.finite(permutation_df$q_value) & permutation_df$q_value < 0.05, ]
if (nrow(perm_q_sig) == 0) {
  lines <- c(lines, "  None.")
} else {
  perm_q_sig <- perm_q_sig[order(perm_q_sig$q_value), ]
  lines <- c(lines, paste0(
    "  - ", perm_q_sig$metric_label,
    " / ", perm_q_sig$term,
    ": p_perm=", sprintf("%.4f", perm_q_sig$p_value),
    ", q=", sprintf("%.4f", perm_q_sig$q_value)
  ))
}

lines <- c(lines, "", "Count-model nominal effects (p < .05):")
count_sig <- sig_subset(count_df)
if (nrow(count_sig) == 0) {
  lines <- c(lines, "  None.")
} else {
  count_sig <- count_sig[order(count_sig$p_value), ]
  lines <- c(lines, paste0(
    "  - ", count_sig$metric_label,
    " / ", count_sig$model,
    " / ", count_sig$term,
    ": p=", sprintf("%.4f", count_sig$p_value),
    ", dispersion=", sprintf("%.2f", count_sig$poisson_dispersion)
  ))
}

lines <- c(lines, "", "Count-model effects after BH-FDR (q < .05):")
count_q_sig <- count_df[is.finite(count_df$q_value) & count_df$q_value < 0.05, ]
if (nrow(count_q_sig) == 0) {
  lines <- c(lines, "  None.")
} else {
  count_q_sig <- count_q_sig[order(count_q_sig$q_value), ]
  lines <- c(lines, paste0(
    "  - ", count_q_sig$metric_label,
    " / ", count_q_sig$model,
    " / ", count_q_sig$term,
    ": p=", sprintf("%.4f", count_q_sig$p_value),
    ", q=", sprintf("%.4f", count_q_sig$q_value)
  ))
}

if (!is.null(survival_df)) {
  lines <- c(lines, "", "Cox survival model nominal effects (p < .05):")
  surv_sig <- sig_subset(survival_df)
  if (nrow(surv_sig) == 0) {
    lines <- c(lines, "  None.")
  } else {
    surv_sig <- surv_sig[order(surv_sig$p_value), ]
    lines <- c(lines, paste0(
      "  - ", surv_sig$term,
      ": LRT=", sprintf("%.2f", surv_sig$statistic),
      ", p=", sprintf("%.4f", surv_sig$p_value)
    ))
  }
}

lines <- c(
  lines,
  "",
  "Recommended interpretation:",
  "  1. Keep the Hard Clean set as the primary dataset.",
  "  2. Report Shapiro-Wilk residual diagnostics before ANOVA.",
  "  3. Treat the current ANOVA as descriptive/sensitivity when residual normality fails.",
  "  4. Prefer Type III or permutation ANOVA for the unbalanced Hard Clean factorial design.",
  "  5. For kills, inputs, hits, and damage events, prefer count models with duration offset over raw rate ANOVA.",
  "  6. For survival time, prefer a survival model when win/death censoring matters.",
  "  7. FPS is excluded from ANOVA and used only as a technical quality control."
)

writeLines(lines, file.path(output_dir, "anova_alternatives_summary_hard_clean.txt"))
cat(paste(lines, collapse = "\n"), "\n")
