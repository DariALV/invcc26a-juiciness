# Final Research Figures

Use only these figures for the academic presentation unless a reviewer asks for diagnostics.

## RQ1

- `RQ1_immersion_giq_effects.png`: boxplot of Promedio en GIQ by treatment, ordered from lowest to highest mean. It includes raw observations, median/IQR, mean, 95% CI and the relevant factorial p-value.

## RQ2

- `RQ2_motor_performance_effects.png`: paired boxplots for Dano/min and Hits/min, each ordered from lowest to highest mean. These are the telemetry-rate outcomes with the clearest nominal ART signal.
- `RQ2_count_models_duration_offset.png`: boxplot for estimated total hits by treatment, ordered from lowest to highest mean, with the negative-binomial duration-offset p/q value.

## RQ3

- `RQ3_baseline_tradeoff_forest.png`: horizontal forest plot of isolated treatments against Control for GIQ and the main gameplay metrics, ordered from lowest to highest standardized effect. It shows standardized effects, 95% CI and p-values.
- `RQ3_giq_performance_correlation.png`: paired boxplots for Promedio en GIQ and Inputs/min by treatment, each ordered from lowest to highest mean. The subtitle reports the GIQ vs Inputs/min Spearman result.

## Calculation Order

1. Identified Supabase CSV.
2. Hard Clean and one row per participant.
3. RQ1 GIQ factorial model with assumptions.
4. RQ2 telemetry factorial/ART checks.
5. RQ2 count models with duration offset.
6. RQ3 isolated contrasts vs Control.
7. RQ3 Spearman correlations between GIQ and gameplay metrics.

The older figure directories are archived under `analysis/images/archive/legacy_figures_2026-06-27/`.
