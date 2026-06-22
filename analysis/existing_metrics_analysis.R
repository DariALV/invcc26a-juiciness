# Analysis plan for metrics already measured by juicy-vs.
#
# This script intentionally uses only data that the game recorded during the
# already executed experiment:
#   - PlayerIDConfig.csv
#   - Run.csv
#   - DamageTaken.csv
#   - GameSnapshot.csv
#   - RunFeedback.csv (optional, only if real responses exist)
#
# It does NOT use upgrades, synthetic-only metrics, task_accuracy, reaction_time,
# aim_shots, or aim_hits.
#
# Usage:
#   Rscript analysis/existing_metrics_analysis.R
#   JUICY_DATA_DIR=/path/to/supabase_csv_exports Rscript analysis/existing_metrics_analysis.R

suppressPackageStartupMessages({
  library(stats)
})

default_input_dir <- if (dir.exists("supabase_data")) "supabase_data" else "synthetic_data"
input_dir <- Sys.getenv("JUICY_DATA_DIR", unset = default_input_dir)
output_dir <- Sys.getenv("JUICY_OUTPUT_DIR", unset = file.path(input_dir, "r_outputs_existing_metrics"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

read_table <- function(name, required = TRUE) {
  candidates <- file.path(input_dir, c(paste0(name, "_rows.csv"), paste0(name, ".csv")))
  path <- candidates[file.exists(candidates)][1]
  if (is.na(path)) {
    if (required) {
      stop(
        "Missing required file for table ", name, ". Expected one of: ",
        paste(candidates, collapse = ", "),
        call. = FALSE
      )
    }
    return(NULL)
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

as_bool <- function(x) {
  if (is.logical(x)) return(x)
  lx <- tolower(trimws(as.character(x)))
  lx %in% c("true", "t", "1", "yes", "si", "sí")
}

as_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_div <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

require_cols <- function(df, cols, table_name) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(
      "Missing columns in ", table_name, ": ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

write_section <- function(con, title) {
  cat("\n", title, "\n", strrep("=", nchar(title)), "\n", sep = "", file = con)
}

Run <- read_table("Run")
PlayerIDConfig <- read_table("PlayerIDConfig")
DamageTaken <- read_table("DamageTaken")
GameSnapshot <- read_table("GameSnapshot")
RunFeedback <- read_table("RunFeedback", required = FALSE)

require_cols(
  Run,
  c(
    "id", "player_id", "duration_seconds", "final_round", "final_level",
    "total_kills", "total_damage_taken", "total_xp", "end_reason"
  ),
  "Run"
)
require_cols(
  PlayerIDConfig,
  c("id", "camera_shake", "camera_zoom", "camera_recoil"),
  "PlayerIDConfig"
)
require_cols(
  DamageTaken,
  c("run_id", "damage_amount"),
  "DamageTaken"
)
require_cols(
  GameSnapshot,
  c(
    "run_id", "hp", "max_hp", "enemies_alive", "nearest_enemy_dist",
    "projectiles_alive", "inputs_delta", "dir_changes_delta",
    "distance_moved", "speed", "fps"
  ),
  "GameSnapshot"
)

# ---------------------------------------------------------------------------
# 1) Run-level base data + experimental condition
# ---------------------------------------------------------------------------

Run$duration_seconds <- as_num(Run$duration_seconds)
Run$final_round <- as_num(Run$final_round)
Run$final_level <- as_num(Run$final_level)
Run$total_kills <- as_num(Run$total_kills)
Run$total_damage_taken <- as_num(Run$total_damage_taken)
Run$total_xp <- as_num(Run$total_xp)

PlayerIDConfig$camera_shake <- as_bool(PlayerIDConfig$camera_shake)
PlayerIDConfig$camera_zoom <- as_bool(PlayerIDConfig$camera_zoom)
PlayerIDConfig$camera_recoil <- as_bool(PlayerIDConfig$camera_recoil)

condition_map <- function(shake, zoom, recoil) {
  if (!shake && !zoom && !recoil) return("C0_baseline")
  if ( shake && !zoom && !recoil) return("C1_shake")
  if (!shake &&  zoom && !recoil) return("C2_zoom")
  if (!shake && !zoom &&  recoil) return("C3_recoil")
  if ( shake &&  zoom && !recoil) return("C4_shake_zoom")
  if ( shake && !zoom &&  recoil) return("C5_shake_recoil")
  if (!shake &&  zoom &&  recoil) return("C6_zoom_recoil")
  "C7_all"
}

config <- PlayerIDConfig[, c("id", "camera_shake", "camera_zoom", "camera_recoil")]
names(config)[names(config) == "id"] <- "player_id"

analysis_df <- merge(Run, config, by = "player_id", all.x = TRUE)
analysis_df$camera_shake <- factor(analysis_df$camera_shake, levels = c(FALSE, TRUE))
analysis_df$camera_zoom <- factor(analysis_df$camera_zoom, levels = c(FALSE, TRUE))
analysis_df$camera_recoil <- factor(analysis_df$camera_recoil, levels = c(FALSE, TRUE))

analysis_df$condition <- mapply(
  condition_map,
  as_bool(analysis_df$camera_shake),
  as_bool(analysis_df$camera_zoom),
  as_bool(analysis_df$camera_recoil)
)
analysis_df$condition <- factor(
  analysis_df$condition,
  levels = c(
    "C0_baseline", "C1_shake", "C2_zoom", "C3_recoil",
    "C4_shake_zoom", "C5_shake_recoil", "C6_zoom_recoil", "C7_all"
  )
)

# ---------------------------------------------------------------------------
# 2) Damage/vulnerability features from DamageTaken
# ---------------------------------------------------------------------------

DamageTaken$damage_amount <- as_num(DamageTaken$damage_amount)
damage_features <- aggregate(
  damage_amount ~ run_id,
  data = DamageTaken,
  FUN = function(x) c(
    hits_taken = length(x),
    damage_events_total = sum(x, na.rm = TRUE),
    mean_damage_per_hit = mean(x, na.rm = TRUE)
  )
)
damage_features <- data.frame(
  run_id = damage_features$run_id,
  hits_taken = damage_features$damage_amount[, "hits_taken"],
  damage_events_total = damage_features$damage_amount[, "damage_events_total"],
  mean_damage_per_hit = damage_features$damage_amount[, "mean_damage_per_hit"],
  row.names = NULL
)

# ---------------------------------------------------------------------------
# 3) Snapshot-derived features: movement, pressure, HP, and technical FPS
# ---------------------------------------------------------------------------

snapshot_numeric_cols <- c(
  "hp", "max_hp", "enemies_alive", "nearest_enemy_dist", "projectiles_alive",
  "inputs_delta", "dir_changes_delta", "distance_moved", "speed", "fps"
)
for (col in snapshot_numeric_cols) {
  GameSnapshot[[col]] <- as_num(GameSnapshot[[col]])
}

snapshot_features <- aggregate(
  cbind(
    inputs_delta, dir_changes_delta, distance_moved, speed, enemies_alive,
    nearest_enemy_dist, projectiles_alive, fps
  ) ~ run_id,
  data = GameSnapshot,
  FUN = function(x) c(
    sum = sum(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
)

flatten_aggregate <- function(df) {
  out <- data.frame(run_id = df$run_id)
  for (col in setdiff(names(df), "run_id")) {
    mat <- df[[col]]
    if (is.matrix(mat)) {
      for (sub in colnames(mat)) {
        out[[paste(col, sub, sep = "_")]] <- mat[, sub]
      }
    }
  }
  out
}

snapshot_features <- flatten_aggregate(snapshot_features)

GameSnapshot$low_hp <- GameSnapshot$hp <= 0.25 * GameSnapshot$max_hp
GameSnapshot$fps_drop <- GameSnapshot$fps < 50
low_hp <- aggregate(
  cbind(low_hp, fps_drop) ~ run_id,
  data = GameSnapshot,
  FUN = function(x) mean(x, na.rm = TRUE)
)
names(low_hp)[names(low_hp) == "low_hp"] <- "low_hp_ratio"
names(low_hp)[names(low_hp) == "fps_drop"] <- "fps_drop_ratio"

snapshot_features <- merge(snapshot_features, low_hp, by = "run_id", all.x = TRUE)

# Keep clear, analysis-facing names.
snapshot_keep <- data.frame(
  run_id = snapshot_features$run_id,
  input_total = snapshot_features$inputs_delta_sum,
  input_mean = snapshot_features$inputs_delta_mean,
  jitter_total = snapshot_features$dir_changes_delta_sum,
  jitter_mean = snapshot_features$dir_changes_delta_mean,
  distance_total = snapshot_features$distance_moved_sum,
  distance_mean = snapshot_features$distance_moved_mean,
  speed_mean = snapshot_features$speed_mean,
  speed_sd = snapshot_features$speed_sd,
  enemies_alive_mean = snapshot_features$enemies_alive_mean,
  enemies_alive_max = snapshot_features$enemies_alive_max,
  nearest_enemy_dist_mean = snapshot_features$nearest_enemy_dist_mean,
  projectiles_alive_mean = snapshot_features$projectiles_alive_mean,
  fps_mean = snapshot_features$fps_mean,
  fps_min = snapshot_features$fps_min,
  fps_drop_ratio = snapshot_features$fps_drop_ratio,
  low_hp_ratio = snapshot_features$low_hp_ratio
)

# ---------------------------------------------------------------------------
# 4) Optional subjective data measured in juicy-vs RunFeedback
# ---------------------------------------------------------------------------

feedback_features <- NULL
if (!is.null(RunFeedback) && nrow(RunFeedback) > 0) {
  feedback_cols <- intersect(
    c("run_id", "difficulty", "fun", "chaos", "monotony", "boredom", "stress", "style_liking"),
    names(RunFeedback)
  )
  if ("run_id" %in% feedback_cols && length(feedback_cols) > 1) {
    feedback_features <- RunFeedback[, feedback_cols, drop = FALSE]
    for (col in setdiff(names(feedback_features), "run_id")) {
      feedback_features[[col]] <- as_num(feedback_features[[col]])
    }
  }
}

# ---------------------------------------------------------------------------
# 5) Final run-level analysis dataset
# ---------------------------------------------------------------------------

analysis_df <- merge(
  analysis_df,
  damage_features,
  by.x = "id",
  by.y = "run_id",
  all.x = TRUE
)
analysis_df <- merge(
  analysis_df,
  snapshot_keep,
  by.x = "id",
  by.y = "run_id",
  all.x = TRUE
)
if (!is.null(feedback_features)) {
  analysis_df <- merge(
    analysis_df,
    feedback_features,
    by.x = "id",
    by.y = "run_id",
    all.x = TRUE
  )
}

analysis_df$hits_taken[is.na(analysis_df$hits_taken)] <- 0
analysis_df$damage_events_total[is.na(analysis_df$damage_events_total)] <- 0
analysis_df$damage_taken_rate <- safe_div(
  analysis_df$total_damage_taken,
  analysis_df$duration_seconds
)
analysis_df$hits_rate <- safe_div(
  analysis_df$hits_taken,
  analysis_df$duration_seconds
)
analysis_df$kill_rate <- safe_div(
  analysis_df$total_kills,
  analysis_df$duration_seconds
)
analysis_df$xp_rate <- safe_div(
  analysis_df$total_xp,
  analysis_df$duration_seconds
)
analysis_df$input_rate <- safe_div(
  analysis_df$input_total,
  analysis_df$duration_seconds
)
analysis_df$jitter_rate <- safe_div(
  analysis_df$jitter_total,
  analysis_df$duration_seconds
)
analysis_df$distance_rate <- safe_div(
  analysis_df$distance_total,
  analysis_df$duration_seconds
)
analysis_df$died <- analysis_df$end_reason == "death"

write.csv(
  analysis_df,
  file.path(output_dir, "juicy_vs_existing_metrics_run_level.csv"),
  row.names = FALSE,
  na = ""
)

# ---------------------------------------------------------------------------
# 6) Descriptives by condition
# ---------------------------------------------------------------------------

analysis_vars <- c(
  "duration_seconds",
  "final_round",
  "final_level",
  "damage_taken_rate",
  "hits_rate",
  "total_damage_taken",
  "kill_rate",
  "xp_rate",
  "jitter_rate",
  "input_rate",
  "distance_rate",
  "speed_mean",
  "enemies_alive_mean",
  "enemies_alive_max",
  "nearest_enemy_dist_mean",
  "projectiles_alive_mean",
  "low_hp_ratio",
  "fps_mean",
  "fps_min",
  "fps_drop_ratio"
)
analysis_vars <- analysis_vars[analysis_vars %in% names(analysis_df)]

if (!is.null(feedback_features)) {
  feedback_analysis_vars <- intersect(
    c("difficulty", "fun", "chaos", "monotony", "boredom", "stress", "style_liking"),
    names(analysis_df)
  )
  analysis_vars <- c(analysis_vars, feedback_analysis_vars)
}

desc_one <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(c(n = 0, mean = NA, sd = NA, median = NA, min = NA, max = NA, ci95_low = NA, ci95_high = NA))
  }
  se <- stats::sd(x) / sqrt(length(x))
  ci <- 1.96 * se
  c(
    n = length(x),
    mean = mean(x),
    sd = stats::sd(x),
    median = stats::median(x),
    min = min(x),
    max = max(x),
    ci95_low = mean(x) - ci,
    ci95_high = mean(x) + ci
  )
}

descriptives <- do.call(
  rbind,
  lapply(analysis_vars, function(v) {
    by_cond <- by(analysis_df[[v]], analysis_df$condition, desc_one)
    do.call(
      rbind,
      lapply(names(by_cond), function(cond) {
        data.frame(
          variable = v,
          condition = cond,
          t(as.matrix(by_cond[[cond]])),
          row.names = NULL
        )
      })
    )
  })
)

write.csv(
  descriptives,
  file.path(output_dir, "descriptives_by_condition.csv"),
  row.names = FALSE,
  na = ""
)

# ---------------------------------------------------------------------------
# 7) Statistical tests by category
# ---------------------------------------------------------------------------

anova_vars <- intersect(
  c(
    "duration_seconds",
    "final_round",
    "final_level",
    "damage_taken_rate",
    "hits_rate",
    "kill_rate",
    "xp_rate",
    "jitter_rate",
    "input_rate",
    "distance_rate",
    "speed_mean",
    "enemies_alive_mean",
    "nearest_enemy_dist_mean",
    "projectiles_alive_mean",
    "low_hp_ratio",
    "fps_mean",
    "fps_min",
    "fps_drop_ratio",
    "fun",
    "stress",
    "style_liking",
    "difficulty",
    "chaos",
    "monotony",
    "boredom"
  ),
  names(analysis_df)
)

partial_eta_squared <- function(aov_model) {
  tab <- summary(aov_model)[[1]]
  ss <- tab[["Sum Sq"]]
  names(ss) <- trimws(rownames(tab))
  ss_error <- ss["Residuals"]
  effects <- setdiff(names(ss), "Residuals")
  out <- data.frame(
    effect = effects,
    partial_eta_squared = as.numeric(ss[effects] / (ss[effects] + ss_error)),
    row.names = NULL
  )
  out
}

anova_results <- list()
assumption_results <- list()
kruskal_results <- list()

for (v in anova_vars) {
  df <- analysis_df[is.finite(analysis_df[[v]]), ]
  if (nrow(df) < 8 || length(unique(df$condition)) < 2) next

  formula_factorial <- as.formula(
    paste(v, "~ camera_shake * camera_zoom * camera_recoil")
  )
  model <- aov(formula_factorial, data = df)
  tab <- summary(model)[[1]]
  eta <- partial_eta_squared(model)
  tab_df <- data.frame(
    variable = v,
    effect = trimws(rownames(tab)),
    df = tab[["Df"]],
    sum_sq = tab[["Sum Sq"]],
    mean_sq = tab[["Mean Sq"]],
    f_value = tab[["F value"]],
    p_value = tab[["Pr(>F)"]],
    row.names = NULL
  )
  tab_df <- merge(tab_df, eta, by = "effect", all.x = TRUE)
  anova_results[[v]] <- tab_df

  residual_p <- NA_real_
  if (length(residuals(model)) >= 3 && length(residuals(model)) <= 5000) {
    residual_p <- tryCatch(
      shapiro.test(residuals(model))$p.value,
      error = function(e) NA_real_
    )
  }
  bartlett_p <- tryCatch(
    bartlett.test(df[[v]] ~ df$condition)$p.value,
    error = function(e) NA_real_
  )
  assumption_results[[v]] <- data.frame(
    variable = v,
    shapiro_residual_p = residual_p,
    bartlett_by_condition_p = bartlett_p
  )

  kw <- tryCatch(
    kruskal.test(df[[v]] ~ df$condition),
    error = function(e) NULL
  )
  if (!is.null(kw)) {
    kruskal_results[[v]] <- data.frame(
      variable = v,
      statistic = as.numeric(kw$statistic),
      df = as.numeric(kw$parameter),
      p_value = kw$p.value
    )
  }
}

anova_results_df <- do.call(rbind, anova_results)
assumption_results_df <- do.call(rbind, assumption_results)
kruskal_results_df <- do.call(rbind, kruskal_results)

write.csv(
  anova_results_df,
  file.path(output_dir, "factorial_anova_results.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  assumption_results_df,
  file.path(output_dir, "anova_assumption_checks.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  kruskal_results_df,
  file.path(output_dir, "kruskal_by_condition_results.csv"),
  row.names = FALSE,
  na = ""
)

# Count outcomes: use Poisson with exposure offset when possible.
count_models <- list()
for (v in intersect(c("hits_taken", "total_kills"), names(analysis_df))) {
  df <- analysis_df[is.finite(analysis_df[[v]]) & is.finite(analysis_df$duration_seconds), ]
  df <- df[df$duration_seconds > 0, ]
  if (nrow(df) < 8 || length(unique(df$condition)) < 2) next
  poisson_model <- glm(
    as.formula(paste(v, "~ camera_shake * camera_zoom * camera_recoil + offset(log(duration_seconds))")),
    data = df,
    family = poisson()
  )
  dispersion_ratio <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
  family_name <- if (is.finite(dispersion_ratio) && dispersion_ratio > 1.5) "quasipoisson" else "poisson"
  model <- if (family_name == "quasipoisson") {
    glm(
      as.formula(paste(v, "~ camera_shake * camera_zoom * camera_recoil + offset(log(duration_seconds))")),
      data = df,
      family = quasipoisson()
    )
  } else {
    poisson_model
  }
  tab <- coef(summary(model))
  statistic_col <- grep(" value$", colnames(tab), value = TRUE)[1]
  p_col <- grep("^Pr\\(", colnames(tab), value = TRUE)[1]
  count_models[[v]] <- data.frame(
    variable = v,
    model_family = family_name,
    term = rownames(tab),
    estimate = tab[, "Estimate"],
    std_error = tab[, "Std. Error"],
    statistic = tab[, statistic_col],
    p_value = tab[, p_col],
    rate_ratio = exp(tab[, "Estimate"]),
    dispersion_ratio = dispersion_ratio,
    row.names = NULL
  )
}

if (length(count_models) > 0) {
  write.csv(
    do.call(rbind, count_models),
    file.path(output_dir, "poisson_count_models.csv"),
    row.names = FALSE,
    na = ""
  )
}

# Logistic model for death, if both outcomes exist.
if ("died" %in% names(analysis_df) && length(unique(na.omit(analysis_df$died))) == 2) {
  glm_death <- glm(
    died ~ camera_shake * camera_zoom * camera_recoil,
    data = analysis_df,
    family = binomial()
  )
  death_tab <- coef(summary(glm_death))
  death_results <- data.frame(
    term = rownames(death_tab),
    estimate = death_tab[, "Estimate"],
    std_error = death_tab[, "Std. Error"],
    z_value = death_tab[, "z value"],
    p_value = death_tab[, "Pr(>|z|)"],
    odds_ratio = exp(death_tab[, "Estimate"]),
    row.names = NULL
  )
  write.csv(
    death_results,
    file.path(output_dir, "logistic_death_model.csv"),
    row.names = FALSE,
    na = ""
  )
}

# ---------------------------------------------------------------------------
# 8) Optional baseline comparisons with emmeans, if installed
# ---------------------------------------------------------------------------

if (requireNamespace("emmeans", quietly = TRUE)) {
  dunnett_results <- list()
  for (v in anova_vars) {
    df <- analysis_df[is.finite(analysis_df[[v]]), ]
    df <- df[!is.na(df$condition), ]
    if (nrow(df) < 8 || !"C0_baseline" %in% df$condition) next
    model <- lm(as.formula(paste(v, "~ condition")), data = df)
    cmp <- emmeans::contrast(
      emmeans::emmeans(model, ~ condition),
      method = "trt.vs.ctrl",
      ref = "C0_baseline",
      adjust = "dunnettx"
    )
    cmp_df <- as.data.frame(cmp)
    cmp_df$variable <- v
    dunnett_results[[v]] <- cmp_df
  }
  if (length(dunnett_results) > 0) {
    write.csv(
      do.call(rbind, dunnett_results),
      file.path(output_dir, "dunnett_vs_baseline.csv"),
      row.names = FALSE,
      na = ""
    )
  }
}

# ---------------------------------------------------------------------------
# 9) Compact report
# ---------------------------------------------------------------------------

report_path <- file.path(output_dir, "analysis_report_existing_metrics.txt")
con <- file(report_path, open = "wt", encoding = "UTF-8")
on.exit(close(con), add = TRUE)

write_section(con, "Input")
cat("Input directory: ", input_dir, "\n", sep = "", file = con)
cat("Output directory: ", output_dir, "\n", sep = "", file = con)

write_section(con, "Guarantee")
cat(
  paste(
    "This analysis uses only variables measured by juicy-vs:",
    "condition flags, Run summaries, DamageTaken events, GameSnapshot telemetry,",
    "and optional RunFeedback responses if present.",
    "Upgrade tables and unmeasured metrics such as task_accuracy, reaction_time_ms,",
    "aim_shots, and aim_hits are intentionally excluded.",
    sep = " "
  ),
  "\n",
  file = con
)

write_section(con, "Runs By Condition")
cat(
  paste(capture.output(print(table(analysis_df$condition, useNA = "ifany"))), collapse = "\n"),
  "\n",
  file = con
)

write_section(con, "Generated Files")
cat(
  paste(
    "juicy_vs_existing_metrics_run_level.csv",
    "descriptives_by_condition.csv",
    "factorial_anova_results.csv",
    "anova_assumption_checks.csv",
    "kruskal_by_condition_results.csv",
    "poisson_count_models.csv (if count models can be fit)",
    "logistic_death_model.csv (if death has both classes)",
    "dunnett_vs_baseline.csv (if emmeans is installed)",
    sep = "\n- "
  ),
  "\n",
  file = con
)

cat("Analysis complete. Outputs written to: ", output_dir, "\n", sep = "")
