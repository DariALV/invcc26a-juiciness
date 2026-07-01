# Final Research Figures

Use only these figures for the academic presentation unless a reviewer asks for diagnostics.
The two additional diagnostic plots were inspired by useful ideas found in generate_interpretable_pvalue_figures.R and anova_assumptions_and_alternatives.R.

## Diagnostics

- `00_assumption_validation_main_metrics.png`: residual normality and Brown-Forsythe variance checks for the four simplified metrics.
- `00_art_factorial_main_metrics.png`: ART factorial matrix for the four simplified metrics with p and q shown inside each cell.

## RQ1

- `RQ1_immersion_giq_effects.png`: Inmersion percibida by treatment. It includes raw observations, median/IQR, mean, 95% CI and the relevant factorial p-value.
- `RQ1_immersion_vs_control_forest.png`: difference in GIQ against Control for all non-control treatments with 95% CI.

## RQ2

- `RQ2_kills_per_min.png`: offensive performance by treatment.
- `RQ2_hits_per_min.png`: received hits per minute by treatment. Lower values indicate better defensive/motor performance.
- `RQ2_performance_effects_directional_forest.png`: directional treatment contrasts against Control for all non-control treatments. Right means better performance.
- `RQ2_survival_time_context.png`: contextual survival-time figure. It helps interpret rates and counts but is not the main performance metric.
- `RQ2_count_models_duration_offset.png`: supplementary duration-adjusted total hits.
- `RQ2_count_model_rate_ratios.png`: supplementary duration-offset count-model rate ratios for all treatments, with Control shown as reference = 1.

## RQ3

- `RQ3_immersion_vs_control_forest.png`: all non-control treatments against Control for Inmersion percibida.
- `RQ3_hits_vs_control_forest.png`: all non-control treatments against Control for Golpes recibidos/min.
- `RQ3_immersion_hits_tradeoff_quadrants.png`: one point per treatment showing change in GIQ vs change in hits/min against Control; p-values appear above each point and significant treatments are highlighted.
- `RQ3_giq_performance_correlation.png`: secondary individual-level Spearman association, not the primary trade-off test.
- `14_rq3_tradeoff_classification.csv`: treatment-level interpretation table for the trade-off decision.

## Calculation Order

1. juiciness_clean_dataset.csv.
2. Retain all rows and keep quality flags as audit metadata; do not use FPS as a treatment-level validity figure.
3. RQ1 GIQ factorial model with assumptions and all-treatment contrasts vs Control.
4. Use ANOVA for metrics that pass residual-normality and variance checks; use ART for metrics that do not.
5. RQ2 supplementary count models with duration offset.
6. RQ3 all-treatment contrasts vs Control.
7. RQ3 trade-off quadrant and classification using immersion vs received hits.

The older analysis assets are archived under `analysis/archive/`.
