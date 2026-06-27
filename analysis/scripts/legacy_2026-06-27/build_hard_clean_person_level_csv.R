# Build a Hard Clean, person-level analysis CSV from the Supabase telemetry export.
#
# Outputs:
#   analysis/hard_clean_person_level.csv
#   analysis/hard_clean_person_level_with_giq.csv
#   analysis/hard_clean_person_level_removed_duplicate_runs.csv
#   analysis/hard_clean_removed_runs.csv
#   analysis/hard_clean_person_level_summary.txt

suppressPackageStartupMessages({
  library(stats)
})

data_dir <- file.path("supabase_data", "r_outputs_existing_metrics")
run_level_path <- file.path(data_dir, "juicy_vs_existing_metrics_run_level.csv")
flags_path <- file.path("analysis", "problematic_flags_long.csv")
cfg_path <- file.path("supabase_data", "PlayerIDConfig_rows.csv")
form_path <- "Formulario Integrado InvCC26a Experimentos .csv"
if (!file.exists(form_path)) {
  form_path <- file.path("analysis", "Formulario Integrado InvCC26a Experimentos .csv")
}

out_path <- file.path("analysis", "hard_clean_person_level.csv")
joined_out_path <- file.path("analysis", "hard_clean_person_level_with_giq.csv")
duplicate_audit_path <- file.path("analysis", "hard_clean_person_level_removed_duplicate_runs.csv")
hard_clean_audit_path <- file.path("analysis", "hard_clean_removed_runs.csv")
summary_path <- file.path("analysis", "hard_clean_person_level_summary.txt")

hard_clean_rules <- c(
  "duration_lt_30s",
  "zero_input_total",
  "zero_kills_after_120s",
  "fps_min_lt_15_or_drop_gt_10pct"
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

stopifnot(file.exists(run_level_path), file.exists(flags_path))

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

collapse_unique <- function(x) paste(unique(x[!is.na(x) & nzchar(x)]), collapse = ";")

run_level <- read.csv(run_level_path, check.names = FALSE, stringsAsFactors = FALSE)
run_level$row_number <- seq_len(nrow(run_level))
run_level$player_id <- trimws(toupper(run_level$player_id))
run_level$condition <- as.character(run_level$condition)
run_level$condition_label <- unname(condition_labels[run_level$condition])

flags <- read.csv(flags_path, check.names = FALSE, stringsAsFactors = FALSE)
flags$row_number <- suppressWarnings(as.integer(flags$row_number))

hard_clean_flags <- flags[flags$rule %in% hard_clean_rules, , drop = FALSE]
hard_clean_rows <- sort(unique(hard_clean_flags$row_number))

if (nrow(hard_clean_flags) > 0) {
  flag_summary <- aggregate(rule ~ row_number, hard_clean_flags, collapse_unique)
  names(flag_summary)[names(flag_summary) == "rule"] <- "hard_clean_rules_triggered"
} else {
  flag_summary <- data.frame(row_number = integer(), hard_clean_rules_triggered = character())
}

hard_clean_removed <- merge(
  run_level[run_level$row_number %in% hard_clean_rows, , drop = FALSE],
  flag_summary,
  by = "row_number",
  all.x = TRUE,
  sort = FALSE
)
if (nrow(hard_clean_removed) > 0) {
  hard_clean_removed$removed_reason <- paste0(
    "hard_clean_rule:",
    hard_clean_removed$hard_clean_rules_triggered
  )
}
write.csv(hard_clean_removed, hard_clean_audit_path, row.names = FALSE)

hard_clean <- run_level[!run_level$row_number %in% hard_clean_rows, , drop = FALSE]
hard_clean <- hard_clean[order(hard_clean$player_id, hard_clean$started_at, hard_clean$row_number), ]

hard_clean$person_run_index <- ave(
  seq_len(nrow(hard_clean)),
  hard_clean$player_id,
  FUN = seq_along
)
hard_clean$person_run_count_hard_clean <- ave(
  hard_clean$row_number,
  hard_clean$player_id,
  FUN = length
)
hard_clean$duplicate_resolution <- "first_valid_hard_clean_run_by_started_at"

person_level <- hard_clean[hard_clean$person_run_index == 1, , drop = FALSE]
person_level$retained_for_person_level <- TRUE
person_level$condition <- factor(person_level$condition, levels = condition_levels)
person_level <- person_level[order(person_level$condition, person_level$player_id), ]
person_level$condition <- as.character(person_level$condition)

removed_duplicates <- hard_clean[hard_clean$person_run_index > 1, , drop = FALSE]
removed_duplicates$retained_for_person_level <- FALSE
removed_duplicates$removed_reason <- "duplicate_player_after_hard_clean"
removed_duplicates <- removed_duplicates[order(
  removed_duplicates$player_id,
  removed_duplicates$started_at,
  removed_duplicates$row_number
), ]

write.csv(person_level, out_path, row.names = FALSE)
write.csv(removed_duplicates, duplicate_audit_path, row.names = FALSE)

build_giq <- function(form, cfg) {
  giq_items <- form[, 38:61]
  giq_items[] <- lapply(giq_items, function(x) suppressWarnings(as.numeric(x)))

  out <- data.frame(
    player_id = trimws(toupper(form[[3]])),
    giq_mean = rowMeans(giq_items, na.rm = TRUE),
    giq_engagement = rowMeans(giq_items[, 1:9], na.rm = TRUE),
    giq_engrossment = rowMeans(giq_items[, 10:16], na.rm = TRUE),
    giq_total_immersion = rowMeans(giq_items[, 17:24], na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  if (nrow(cfg) > 0 && "id" %in% names(cfg)) {
    cfg$id <- trimws(toupper(cfg$id))
    cfg$camera_shake <- as_bool(cfg$camera_shake)
    cfg$camera_zoom <- as_bool(cfg$camera_zoom)
    cfg$camera_recoil <- as_bool(cfg$camera_recoil)
    cfg$condition_from_form_config <- mapply(
      condition_from_flags,
      cfg$camera_shake,
      cfg$camera_zoom,
      cfg$camera_recoil
    )
    out <- merge(
      out,
      cfg[, c("id", "condition_from_form_config")],
      by.x = "player_id",
      by.y = "id",
      all.x = TRUE,
      sort = FALSE
    )
  }

  out <- out[!duplicated(out$player_id), , drop = FALSE]
  out
}

giq_joined <- NULL
if (file.exists(form_path)) {
  form <- read.csv(form_path, check.names = FALSE, stringsAsFactors = FALSE)
  cfg <- if (file.exists(cfg_path)) {
    read.csv(cfg_path, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    data.frame()
  }
  giq <- build_giq(form, cfg)
  giq_joined <- merge(person_level, giq, by = "player_id", all.x = TRUE, sort = FALSE)
  giq_joined$giq_condition_matches_telemetry <- with(
    giq_joined,
    is.na(condition_from_form_config) | condition == condition_from_form_config
  )
  write.csv(giq_joined, joined_out_path, row.names = FALSE)
}

condition_counts_hard_clean <- as.data.frame(table(
  factor(hard_clean$condition, levels = condition_levels)
), stringsAsFactors = FALSE)
names(condition_counts_hard_clean) <- c("condition", "hard_clean_runs")
condition_counts_person <- as.data.frame(table(
  factor(person_level$condition, levels = condition_levels)
), stringsAsFactors = FALSE)
names(condition_counts_person) <- c("condition", "person_level_rows")
condition_counts <- merge(condition_counts_hard_clean, condition_counts_person, by = "condition")
condition_counts$condition_label <- unname(condition_labels[condition_counts$condition])
condition_counts <- condition_counts[, c("condition", "condition_label", "hard_clean_runs", "person_level_rows")]

duplicate_players <- sort(table(hard_clean$player_id), decreasing = TRUE)
duplicate_players <- duplicate_players[duplicate_players > 1]
multi_condition_duplicates <- split(hard_clean$condition, hard_clean$player_id)
multi_condition_duplicates <- multi_condition_duplicates[
  lengths(lapply(multi_condition_duplicates, unique)) > 1
]

summary_lines <- c(
  "Hard Clean person-level CSV",
  "===========================",
  sprintf("Source telemetry file: %s", run_level_path),
  sprintf("Original run-level rows: %d", nrow(run_level)),
  sprintf("Rows removed by Hard Clean: %d", length(hard_clean_rows)),
  sprintf("Rows retained after Hard Clean: %d", nrow(hard_clean)),
  sprintf("Unique players after Hard Clean: %d", length(unique(hard_clean$player_id))),
  sprintf("Duplicate players after Hard Clean: %d", length(duplicate_players)),
  sprintf("Duplicate runs removed for person-level CSV: %d", nrow(removed_duplicates)),
  sprintf("Final person-level rows: %d", nrow(person_level)),
  sprintf("Any duplicated player_id in final CSV: %s", ifelse(anyDuplicated(person_level$player_id) > 0, "YES", "NO")),
  sprintf("Duplicate players with more than one condition: %d", length(multi_condition_duplicates)),
  sprintf("Duplicate resolution rule: %s", unique(person_level$duplicate_resolution)[1]),
  "",
  "Condition counts:",
  paste(
    sprintf(
      "- %s: %d hard-clean runs -> %d person-level rows",
      condition_counts$condition_label,
      condition_counts$hard_clean_runs,
      condition_counts$person_level_rows
    ),
    collapse = "\n"
  ),
  "",
  "Outputs:",
  paste(
    c(out_path, duplicate_audit_path, hard_clean_audit_path, joined_out_path),
    collapse = "\n"
  )
)

if (!is.null(giq_joined)) {
  summary_lines <- c(
    summary_lines,
    "",
    sprintf("Person-level rows with matched GIQ mean: %d", sum(is.finite(giq_joined$giq_mean))),
    sprintf(
      "GIQ condition mismatches among matched rows: %d",
      sum(!giq_joined$giq_condition_matches_telemetry & is.finite(giq_joined$giq_mean), na.rm = TRUE)
    )
  )
}

writeLines(summary_lines, summary_path)
cat(paste(summary_lines, collapse = "\n"), "\n")
