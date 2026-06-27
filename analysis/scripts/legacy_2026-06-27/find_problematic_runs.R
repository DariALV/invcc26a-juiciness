# Identify runs that may distort ANOVA, interaction plots, and Dunnett tests.
#
# Inputs:
#   supabase_data/r_outputs_existing_metrics/juicy_vs_existing_metrics_run_level.csv
#
# Outputs:
#   analysis/problematic_runs_review.csv
#   analysis/problematic_flags_long.csv
#   analysis/problematic_data_quality_summary.txt
#
# The script does not exclude data automatically. It creates an audit list for
# manual review before rerunning the statistical analysis.

suppressPackageStartupMessages({
  library(stats)
})

output_dir <- "analysis"
data_path <- file.path(
  "supabase_data",
  "r_outputs_existing_metrics",
  "juicy_vs_existing_metrics_run_level.csv"
)

if (!file.exists(data_path)) {
  stop("Missing input file: ", data_path)
}

run_level <- read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)
run_level$row_number <- seq_len(nrow(run_level))

condition_levels <- c(
  "C0_baseline", "C1_shake", "C2_zoom", "C3_recoil",
  "C4_shake_zoom", "C5_shake_recoil", "C6_zoom_recoil", "C7_all"
)
run_level$condition <- factor(run_level$condition, levels = condition_levels)

analysis_vars <- intersect(
  c(
    "duration_seconds",
    "kill_rate",
    "input_rate",
    "xp_rate",
    "nearest_enemy_dist_mean",
    "damage_taken_rate",
    "jitter_rate",
    "distance_rate",
    "fps_mean",
    "fps_min",
    "fps_drop_ratio"
  ),
  names(run_level)
)

core_vars <- intersect(
  c(
    "kill_rate",
    "input_rate",
    "xp_rate",
    "nearest_enemy_dist_mean",
    "duration_seconds",
    "damage_taken_rate",
    "jitter_rate"
  ),
  names(run_level)
)

as_bool <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "si", "sí")
}

add_flag <- function(flags, rows, rule, variable, reason, value = NA_real_, threshold = NA_character_) {
  rows <- rows[!is.na(rows)]
  if (length(rows) == 0) return(flags)
  new_flags <- data.frame(
    row_number = rows,
    id = run_level$id[rows],
    player_id = run_level$player_id[rows],
    condition = as.character(run_level$condition[rows]),
    variable = variable,
    rule = rule,
    value = value,
    threshold = threshold,
    reason = reason,
    stringsAsFactors = FALSE
  )
  rbind(flags, new_flags)
}

flag_iqr_by_condition <- function(flags, variable) {
  if (!variable %in% names(run_level)) return(flags)
  for (condition in levels(run_level$condition)) {
    idx <- which(run_level$condition == condition & is.finite(run_level[[variable]]))
    values <- run_level[[variable]][idx]
    if (length(values) < 4) next
    qs <- as.numeric(quantile(values, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE))
    iqr <- qs[2] - qs[1]
    if (!is.finite(iqr) || iqr == 0) next
    low <- qs[1] - 1.5 * iqr
    high <- qs[2] + 1.5 * iqr
    bad <- idx[values < low | values > high]
    flags <- add_flag(
      flags,
      bad,
      "iqr_outlier_by_condition",
      variable,
      paste0("Value is outside 1.5 IQR within ", condition, "."),
      value = run_level[[variable]][bad],
      threshold = paste0("[", signif(low, 5), ", ", signif(high, 5), "]")
    )
  }
  flags
}

flag_cooks_distance <- function(flags, variable) {
  required <- c(variable, "camera_shake", "camera_zoom", "camera_recoil")
  if (!all(required %in% names(run_level))) return(flags)

  df <- run_level[
    is.finite(run_level[[variable]]) &
      !is.na(run_level$camera_shake) &
      !is.na(run_level$camera_zoom) &
      !is.na(run_level$camera_recoil),
  ]
  if (nrow(df) < 10) return(flags)

  df$camera_shake <- as_bool(df$camera_shake)
  df$camera_zoom <- as_bool(df$camera_zoom)
  df$camera_recoil <- as_bool(df$camera_recoil)
  model <- tryCatch(
    aov(as.formula(paste(variable, "~ camera_shake * camera_zoom * camera_recoil")), data = df),
    error = function(e) NULL
  )
  if (is.null(model)) return(flags)

  cooks <- cooks.distance(model)
  cutoff <- 4 / nrow(df)
  local_rows <- which(is.finite(cooks) & cooks > cutoff)
  if (length(local_rows) == 0) return(flags)

  original_rows <- df$row_number[local_rows]
  add_flag(
    flags,
    original_rows,
    "cooks_distance_gt_4_over_n",
    variable,
    "Run has high influence on the factorial ANOVA model.",
    value = cooks[local_rows],
    threshold = signif(cutoff, 5)
  )
}

flags <- data.frame()

for (variable in analysis_vars) {
  flags <- flag_iqr_by_condition(flags, variable)
}

for (variable in core_vars) {
  flags <- flag_cooks_distance(flags, variable)
}

if ("duration_seconds" %in% names(run_level)) {
  rows <- which(is.finite(run_level$duration_seconds) & run_level$duration_seconds < 30)
  flags <- add_flag(
    flags,
    rows,
    "duration_lt_30s",
    "duration_seconds",
    "Run is too short to represent stable gameplay.",
    value = run_level$duration_seconds[rows],
    threshold = "< 30"
  )
}

if ("input_total" %in% names(run_level)) {
  rows <- which(is.finite(run_level$input_total) & run_level$input_total == 0)
  flags <- add_flag(
    flags,
    rows,
    "zero_input_total",
    "input_total",
    "No player input was recorded.",
    value = run_level$input_total[rows],
    threshold = "= 0"
  )
}

if (all(c("total_kills", "duration_seconds") %in% names(run_level))) {
  rows <- which(
    is.finite(run_level$total_kills) &
      is.finite(run_level$duration_seconds) &
      run_level$total_kills == 0 &
      run_level$duration_seconds > 120
  )
  flags <- add_flag(
    flags,
    rows,
    "zero_kills_after_120s",
    "total_kills",
    "Long run with no kills recorded.",
    value = run_level$total_kills[rows],
    threshold = "kills = 0 and duration > 120s"
  )
}

if (all(c("fps_min", "fps_drop_ratio") %in% names(run_level))) {
  rows <- which(
    (is.finite(run_level$fps_min) & run_level$fps_min < 15) |
      (is.finite(run_level$fps_drop_ratio) & run_level$fps_drop_ratio > 0.10)
  )
  flags <- add_flag(
    flags,
    rows,
    "fps_min_lt_15_or_drop_gt_10pct",
    "fps",
    "Technical instability may distort gameplay metrics.",
    value = pmin(run_level$fps_min[rows], run_level$fps_drop_ratio[rows], na.rm = TRUE),
    threshold = "fps_min < 15 or fps_drop_ratio > 0.10"
  )
}

if (nrow(flags) == 0) {
  stop("No problematic runs were flagged with the current rules.")
}

flags <- flags[order(flags$row_number, flags$variable, flags$rule), ]

score <- aggregate(
  rule ~ row_number,
  data = flags,
  FUN = length
)
names(score)[2] <- "total_flags"
score$n_iqr <- aggregate(rule ~ row_number, flags[flags$rule == "iqr_outlier_by_condition", ], length)$rule[
  match(score$row_number, aggregate(rule ~ row_number, flags[flags$rule == "iqr_outlier_by_condition", ], length)$row_number)
]
score$n_cook <- aggregate(rule ~ row_number, flags[flags$rule == "cooks_distance_gt_4_over_n", ], length)$rule[
  match(score$row_number, aggregate(rule ~ row_number, flags[flags$rule == "cooks_distance_gt_4_over_n", ], length)$row_number)
]
score$n_rule <- score$total_flags
score$n_iqr[is.na(score$n_iqr)] <- 0
score$n_cook[is.na(score$n_cook)] <- 0

review_cols <- intersect(
  c(
    "row_number", "id", "player_id", "condition", "duration_seconds",
    "total_kills", "kill_rate", "input_total", "input_rate",
    "total_xp", "xp_rate", "nearest_enemy_dist_mean",
    "damage_taken_rate", "jitter_rate", "distance_rate",
    "fps_mean", "fps_min", "fps_drop_ratio"
  ),
  names(run_level)
)

review <- merge(score, run_level[, review_cols], by = "row_number", all.x = TRUE)
review <- review[order(-review$total_flags, -review$n_cook, -review$n_iqr, review$row_number), ]

review$flag_rules <- vapply(
  review$row_number,
  function(row) paste(unique(flags$rule[flags$row_number == row]), collapse = "; "),
  character(1)
)
review$flag_variables <- vapply(
  review$row_number,
  function(row) paste(unique(flags$variable[flags$row_number == row]), collapse = "; "),
  character(1)
)

write.csv(
  flags,
  file.path(output_dir, "problematic_flags_long.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  review,
  file.path(output_dir, "problematic_runs_review.csv"),
  row.names = FALSE,
  na = ""
)

summary_lines <- c(
  "Data quality audit for juicy-vs existing metrics",
  paste("Input:", data_path),
  paste("Total runs:", nrow(run_level)),
  paste("Flagged runs:", nrow(review)),
  "",
  "Flag counts by rule:",
  capture.output(print(sort(table(flags$rule), decreasing = TRUE))),
  "",
  "Flag counts by variable:",
  capture.output(print(sort(table(flags$variable), decreasing = TRUE))),
  "",
  "Top 20 runs for manual review:",
  capture.output(print(head(review, 20), row.names = FALSE))
)

writeLines(summary_lines, file.path(output_dir, "problematic_data_quality_summary.txt"))

cat("Wrote:\n")
cat(" - ", file.path(output_dir, "problematic_flags_long.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "problematic_runs_review.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "problematic_data_quality_summary.txt"), "\n", sep = "")
