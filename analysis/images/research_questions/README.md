# Final Research Figures

Use only these figures for the academic presentation unless a reviewer asks for diagnostics.

## RQ1

- `RQ1_immersion_giq_effects.png`: Inmersión percibida by treatment, ordered from worst to best mean. It includes raw observations, median/IQR, mean, 95% CI and the relevant factorial p-value.

## RQ2

- `RQ2_damage_per_min.png`: boxplot for Daño/min by treatment, ordered from worst to best mean.
- `RQ2_hits_per_min.png`: boxplot for Golpes/min by treatment, ordered from worst to best mean.
- `RQ2_count_models_duration_offset.png`: boxplot for Golpes totales ajustados por duración de partida, ordered from worst to best mean, with the negative-binomial duration-offset p/q value.

## RQ3

- `RQ3_baseline_tradeoff_forest.png`: RQ3 forest plot with horizontal panels for Inmersión percibida, Daño/min and Golpes/Min against Control.
- `RQ3_giq_performance_correlation.png`: paired boxplots for perceived immersion and Golpes/min by treatment, each ordered from worst to best mean. The footer reports the GIQ vs Golpes/min Spearman result.

## Calculation Order

1. juiciness_clean_dataset.csv.
2. Retain all rows and keep quality flags as audit metadata.
3. RQ1 GIQ factorial model with assumptions.
4. RQ2 telemetry factorial/ART checks.
5. RQ2 count models with duration offset.
6. RQ3 isolated contrasts vs Control.
7. RQ3 Spearman correlations between GIQ and gameplay metrics.

The older analysis assets are archived under `analysis/archive/`.
