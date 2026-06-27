# Figuras primordiales para responder las preguntas de investigación

Datos base del análisis restaurado:

- `analysis/hard_clean_person_level.csv`: telemetría Hard Clean con una fila por participante.
- `analysis/hard_clean_person_level_with_giq.csv`: telemetría Hard Clean unida con respuestas GIQ.
- `analysis/problematic_flags_long.csv`: auditoría de corridas excluidas o revisadas.

## RQ1: inmersión / GIQ

- `analysis/images/hard_clean_person_figures/02_giq_by_condition.png`
- `analysis/images/hard_clean_person_figures/03_giq_interaction_shake_zoom.png`
- `analysis/images/hard_clean_person_figures/08_immersion_control_matrix.png`

Estas figuras muestran el promedio en GIQ por tratamiento, la interacción Shake x Zoom y el patrón relativo de subescalas frente al control.

## RQ2: desempeño de gameplay

- `analysis/images/hard_clean_person_figures/04_gameplay_synthesis_vs_control.png`
- `analysis/images/hard_clean_person_figures/05_art_factorial_effect_map.png`
- `analysis/images/hard_clean_person_figures/06_count_model_effect_map.png`
- `analysis/images/validation_figures/03_count_model_effect_map.png`
- `analysis/images/validation_figures/07_art_factorial_effect_map.png`

Estas figuras concentran supervivencia, kills, inputs, daño, golpes, distancia y HP bajo, priorizando ART o modelos de conteo cuando los supuestos de ANOVA no se cumplen.

## RQ3: relación inmersión-desempeño

- `analysis/images/hard_clean_person_figures/07_baseline_forest_vs_control.png`
- `analysis/images/hard_clean_person_figures/09_tradeoff_giq_gameplay.png`
- `analysis/images/validation_figures/06_tradeoff_giq_gameplay.png`

Estas figuras permiten leer si los tratamientos se separan del control y si el GIQ se relaciona con métricas de desempeño observadas.

## Control metodológico

- `analysis/images/hard_clean_person_figures/01_assumption_diagnostics.png`
- `analysis/images/validation_figures/01_assumption_diagnostics.png`
- `analysis/images/presentation_figures/23_fps_technical_control.png`

Estas figuras explican por qué algunas métricas se tratan con alternativas no paramétricas o modelos especializados.
