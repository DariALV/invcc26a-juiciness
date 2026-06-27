# Generate presentation-oriented p-value figures from Hard Clean person-level results.
#
# These figures emphasize interpretation:
#   - nominal p-values are shown clearly;
#   - FDR-adjusted q-values are shown next to them;
#   - results are grouped by research question and evidence type.

suppressPackageStartupMessages({
  library(stats)
  library(grDevices)
  library(graphics)
})

out_dir <- file.path("analysis", "images", "hard_clean_person_pvalue_figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

art_path <- file.path("analysis", "hard_clean_person_art_results.csv")
count_path <- file.path("analysis", "hard_clean_person_count_model_results.csv")
tradeoff_path <- file.path("analysis", "hard_clean_person_tradeoff_correlations.csv")
data_path <- file.path("analysis", "hard_clean_person_level_with_giq.csv")

stopifnot(file.exists(art_path), file.exists(count_path), file.exists(tradeoff_path), file.exists(data_path))

art <- read.csv(art_path, check.names = FALSE, stringsAsFactors = FALSE)
count <- read.csv(count_path, check.names = FALSE, stringsAsFactors = FALSE)
tradeoff <- read.csv(tradeoff_path, check.names = FALSE, stringsAsFactors = FALSE)
d <- read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)
d <- d[is.finite(d$giq_mean), , drop = FALSE]

theme <- list(
  ink = "#1F2421",
  muted = "#52635B",
  grid = "#D8E4DA",
  pale = "#F4F8F5",
  green = "#1B8A5A",
  teal = "#087E8B",
  orange = "#D97924",
  red = "#B23A3A",
  purple = "#7B4BA0",
  gold = "#B7950B"
)

open_png <- function(filename, width = 2500, height = 1500, res = 200) {
  png(file.path(out_dir, filename), width = width, height = height, res = res)
  par(bg = "white", fg = theme$ink, col.axis = theme$ink, col.lab = theme$ink,
      col.main = theme$ink, family = "sans", xaxs = "i", yaxs = "i")
}

close_png <- function() dev.off()

fmt_p <- function(p) {
  ifelse(!is.finite(p), "", ifelse(p < 0.001, "p<.001", sprintf("p=%.3f", p)))
}

fmt_q <- function(q) {
  ifelse(!is.finite(q), "", ifelse(q < 0.001, "q<.001", sprintf("q=%.2f", q)))
}

draw_title <- function(title, subtitle = NULL) {
  title(title, adj = 0, cex.main = 1.42, font.main = 2, line = 1.2)
  if (!is.null(subtitle) && nzchar(subtitle)) {
    mtext(subtitle, side = 3, adj = 0, line = 0.1, cex = 0.90, col = theme$muted)
  }
}

get_art <- function(metric, term, rq, interpretation) {
  row <- art[art$metric == metric & art$term == term, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  row <- row[1, ]
  data.frame(
    rq = rq,
    test = "ART factorial",
    metric = row$metric_label,
    effect = gsub(":", " x ", row$term),
    p = row$p,
    q = row$q_all,
    statistic = row$F,
    statistic_label = "F",
    interpretation = interpretation,
    stringsAsFactors = FALSE
  )
}

get_count <- function(metric, term, rq, interpretation) {
  row <- count[count$metric == metric & count$term == term, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  row <- row[1, ]
  data.frame(
    rq = rq,
    test = "Conteo con offset",
    metric = row$metric_label,
    effect = gsub(":", " x ", row$term),
    p = row$p,
    q = row$q_all,
    statistic = row$statistic,
    statistic_label = "F",
    interpretation = interpretation,
    stringsAsFactors = FALSE
  )
}

get_tradeoff <- function(metric, rq, interpretation) {
  row <- tradeoff[tradeoff$metric == metric, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  row <- row[1, ]
  data.frame(
    rq = rq,
    test = "Spearman",
    metric = paste("GIQ vs", row$metric_label),
    effect = "Asociacion",
    p = row$p,
    q = row$q,
    statistic = row$rho,
    statistic_label = "rho",
    interpretation = interpretation,
    stringsAsFactors = FALSE
  )
}

requested_tests <- list(
  get_art("giq_mean", "Shake:Zoom", "RQ1: inmersion", "GIQ cambia con la combinacion Shake x Zoom"),
  get_art("duration_seconds", "Shake", "RQ2: desempeno", "Shake aparece asociado con supervivencia"),
  get_art("kill_rate", "Shake", "RQ2: desempeno", "Shake aparece asociado con kills/s"),
  get_art("distance_rate", "Shake:Zoom:Recoil", "RQ2: desempeno", "La combinacion completa aparece en distancia/s"),
  get_art("nearest_enemy_dist_mean", "Shake", "RQ2: desempeno", "Shake aparece en distancia al enemigo"),
  get_count("hits_taken", "Shake", "RQ2: conteos", "Shake aparece en golpes recibidos"),
  get_count("hits_taken", "Recoil", "RQ2: conteos", "Recoil aparece en golpes recibidos"),
  get_count("total_kills", "Shake:Recoil", "RQ2: conteos", "Shake x Recoil aparece en kills"),
  get_tradeoff("input_rate", "RQ3: trade-off", "Mayor GIQ se asocia nominalmente con mas inputs/s")
)
key_tests <- do.call(rbind, requested_tests[!vapply(requested_tests, is.null, logical(1))])

if (is.null(key_tests) || nrow(key_tests) == 0) {
  key_tests <- art[order(art$p), ][seq_len(min(9, nrow(art))), ]
  key_tests <- data.frame(
    rq = "Exploratorio",
    test = "ART factorial",
    metric = key_tests$metric_label,
    effect = gsub(":", " x ", key_tests$term),
    p = key_tests$p,
    q = key_tests$q_all,
    statistic = key_tests$F,
    statistic_label = "F",
    interpretation = "Efecto nominal",
    stringsAsFactors = FALSE
  )
}

key_tests$evidence <- ifelse(
  key_tests$p < 0.05 & key_tests$q >= 0.05,
  "Nominal: p < .05, q >= .05",
  ifelse(key_tests$q < 0.05, "FDR: q < .05", "No cruza p < .05")
)
key_tests$label <- paste0(key_tests$metric, " · ", key_tests$effect)
write.csv(key_tests, file.path("analysis", "hard_clean_person_key_pvalues.csv"), row.names = FALSE)

# 01: relevant p-values by research question.
open_png("01_pvalues_relevantes_por_pregunta.png")
par(mar = c(6, 14, 5.5, 3))
plot_df <- key_tests[order(key_tests$p, decreasing = TRUE), ]
y <- seq_len(nrow(plot_df))
cols <- ifelse(plot_df$q < 0.05, theme$green, ifelse(plot_df$p < 0.05, theme$orange, theme$muted))
plot(plot_df$p, y, xlim = c(0, max(0.08, plot_df$p, na.rm = TRUE)), yaxt = "n",
     pch = 19, cex = 1.4, col = cols, xlab = "p-value nominal", ylab = "")
segments(0, y, plot_df$p, y, col = cols, lwd = 3)
abline(v = 0.05, lty = 2, col = theme$red)
axis(2, at = y, labels = plot_df$label, las = 1, cex.axis = 0.68)
text(plot_df$p, y + 0.18, paste(fmt_p(plot_df$p), fmt_q(plot_df$q), sep = " / "), cex = 0.68)
grid(nx = NULL, ny = NA, col = theme$grid)
draw_title("P-values relevantes por pregunta", "Naranja = senal nominal; verde = sobrevive FDR")
close_png()

# 02: p nominal vs q FDR.
open_png("02_p_nominal_vs_q_fdr.png")
par(mar = c(6, 6, 5.5, 2))
plot(key_tests$p, key_tests$q, pch = 19, cex = 1.7, col = cols,
     xlab = "p-value nominal", ylab = "q-value FDR", xlim = c(0, max(0.08, key_tests$p, na.rm = TRUE)),
     ylim = c(0, max(0.55, key_tests$q, na.rm = TRUE)))
abline(v = 0.05, h = 0.05, lty = 2, col = theme$red)
text(key_tests$p, key_tests$q, labels = seq_len(nrow(key_tests)), pos = 3, cex = 0.7)
grid(col = theme$grid)
draw_title("P nominal frente a q FDR", "Separacion entre senales exploratorias y evidencia corregida")
close_png()

# 03: GIQ and trade-off.
open_png("03_giq_y_tradeoff_con_pvalues.png")
par(mar = c(6, 6, 5.5, 2))
if ("input_rate" %in% names(d)) {
  plot(d$giq_mean, d$input_rate, pch = 19, col = adjustcolor(theme$teal, alpha.f = 0.75),
       xlab = "Promedio en GIQ", ylab = "Inputs/s")
  abline(lm(input_rate ~ giq_mean, data = d), col = theme$ink, lwd = 2)
}
trade_row <- key_tests[grepl("GIQ vs", key_tests$metric), , drop = FALSE]
subtitle <- if (nrow(trade_row) > 0) paste(fmt_p(trade_row$p[1]), fmt_q(trade_row$q[1]), sep = " / ") else ""
draw_title("GIQ y trade-off de gameplay", subtitle)
close_png()

# 04: gameplay p-values only.
open_png("04_pvalues_gameplay_interpretables.png")
par(mar = c(6, 13, 5.5, 3))
game <- key_tests[grepl("RQ2", key_tests$rq), , drop = FALSE]
if (nrow(game) > 0) {
  game <- game[order(game$p, decreasing = TRUE), ]
  y <- seq_len(nrow(game))
  cols2 <- ifelse(game$q < 0.05, theme$green, ifelse(game$p < 0.05, theme$orange, theme$muted))
  plot(game$p, y, xlim = c(0, max(0.08, game$p, na.rm = TRUE)), yaxt = "n",
       pch = 19, cex = 1.4, col = cols2, xlab = "p-value nominal", ylab = "")
  segments(0, y, game$p, y, col = cols2, lwd = 3)
  abline(v = 0.05, lty = 2, col = theme$red)
  axis(2, at = y, labels = game$label, las = 1, cex.axis = 0.7)
  text(game$p, y + 0.18, paste(fmt_p(game$p), fmt_q(game$q), sep = " / "), cex = 0.7)
}
draw_title("P-values interpretables de gameplay", "Lectura de efectos sobre desempeno y conteos")
close_png()

# 05: interpretation card.
open_png("05_tarjeta_de_interpretacion_pvalues.png")
par(mar = c(2, 2, 4, 2))
plot.new()
draw_title("Como leer los p-values", "El analisis separa exploracion nominal de evidencia corregida")
text(0.02, 0.72, "p < .05: senal nominal que amerita interpretacion cuidadosa.", adj = 0, cex = 1.15)
text(0.02, 0.55, "q < .05: efecto que sobrevive correccion por multiples pruebas.", adj = 0, cex = 1.15)
text(0.02, 0.38, "Si p < .05 pero q >= .05, se presenta como hallazgo exploratorio.", adj = 0, cex = 1.15)
text(0.02, 0.21, "Las conclusiones deben priorizar ART/modelos de conteo cuando ANOVA no cumple supuestos.", adj = 0, cex = 1.15)
close_png()

summary_lines <- c(
  "Interpretable p-value figures",
  "=============================",
  sprintf("Source ART: %s", art_path),
  sprintf("Source counts: %s", count_path),
  sprintf("Source tradeoff: %s", tradeoff_path),
  "",
  "Key p-values:",
  paste(
    sprintf(
      "- %s / %s / %s: %s, %s",
      key_tests$rq,
      key_tests$metric,
      key_tests$effect,
      fmt_p(key_tests$p),
      fmt_q(key_tests$q)
    ),
    collapse = "\n"
  ),
  "",
  "Generated figures:",
  paste(
    paste0("- ", file.path(out_dir, list.files(out_dir, pattern = "\\.png$"))),
    collapse = "\n"
  )
)

writeLines(summary_lines, file.path("analysis", "hard_clean_person_pvalue_figures_summary.txt"))
cat(paste(summary_lines, collapse = "\n"), "\n")
