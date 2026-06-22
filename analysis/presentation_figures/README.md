# Graficos recomendados para la presentacion

Estos PNG fueron regenerados con `analysis/generate_presentation_figures.R` usando los resultados de `supabase_data/r_outputs_existing_metrics`.

Principios aplicados:

- Una pregunta analitica por grafico.
- Fondo blanco, alto contraste y etiquetas legibles.
- Boxplots verticales con puntos individuales suaves para las figuras 02-10.
- Caja = IQR, linea central = mediana, punto verde = media y linea negra = IC 95% en las figuras 02-10.
- Incertidumbre visible mediante IC 95%.
- Linea punteada de referencia C0 cuando la comparacion contra baseline ayuda a interpretar.
- Ejes con unidades y nota cuando el eje X se ajusta al rango de los intervalos.
- El titulo principal de cada PNG usa el nombre de figura (`Figura XX · ...`); el subtitulo queda reservado para escala, muestra o metodo.
- Las etiquetas colocadas sobre puntos se posicionan con desplazamiento vertical, sin overlays de fondo.
- La figura 12 resume el ANOVA factorial como mapa de efectos: filas = resultados, columnas = factores/interacciones, color = eta cuadrado parcial.
- La tabla completa de ANOVA para investigacion esta en `LaTeX/listings/factorial_anova_research_summary.csv` e incluye F, p, q de Benjamini-Hochberg y eta cuadrado parcial.
- Las figuras 12a-12d quedan como apoyo visual para los dos resultados mas utiles del ANOVA: Kills/s e Inputs/s.
- Las figuras 12a-12d muestran cajas y bigotes por combinaciones de factores: Shake×Zoom y Shake×Recoil.
- El ranking filtrado de efectos significativos para Kills/s e Inputs/s esta en `LaTeX/listings/anova_significant_effects_ranked.csv`.
- La figura 13 incluye `C0 Base` como punto cero de comparacion.
- La figura 14 usa una sola matriz inmersion-control y etiquetas completas de tratamiento.
- Las figuras 15-18 muestran interacciones: shake en X, zoom como linea y displacement/recoil como faceta.
- La figura 19 resume efectos estimados de C1-C7 contra C0 usando el indice relativo de desempeno/control.
- Las figuras 20-21 muestran trade-offs entre inmersion subjetiva y desempeno/control.
- La figura 22 muestra correlaciones entre encuesta, desempeno, control, presion y FPS.
- La figura 23 es control metodologico de estabilidad tecnica.

Verificacion estadistica:

- Las comparaciones Dunnett contra `C0_baseline` fueron verificadas contra las diferencias directas de medias por condicion. La diferencia maxima observada fue de `5.897505e-13`, equivalente a ruido numerico.

Usar solo los archivos listados abajo como version recomendada para la presentacion.

Archivos principales:

| Archivo | Uso recomendado |
|---|---|
| `01_condition_matrix.png` | Diseno factorial 2x2x2. |
| `02_giq_subscales.png` | GIQ total y subescalas. |
| `03_giq_by_condition.png` | Inmersion subjetiva por condicion. |
| `04_duration_by_condition.png` | Supervivencia/desempeno global. |
| `05_kill_rate_by_condition.png` | Desempeno ofensivo. |
| `06_damage_rate_by_condition.png` | Vulnerabilidad por dano. |
| `07_input_rate_by_condition.png` | Control/input del jugador. |
| `08_distance_rate_by_condition.png` | Movimiento. |
| `09_jitter_rate_by_condition.png` | Estabilidad del control. |
| `10_enemy_distance_by_condition.png` | Presion espacial. |
| `12_factorial_anova_effect_map.png` | Mapa principal de efectos del ANOVA factorial 2x2x2. |
| `12a_anova_kill_rate_shake_zoom.png` | ANOVA factorial: Kills/s por Shake×Zoom. |
| `12b_anova_kill_rate_shake_recoil.png` | ANOVA factorial: Kills/s por Shake×Recoil. |
| `12c_anova_input_rate_shake_zoom.png` | ANOVA factorial: Inputs/s por Shake×Zoom. |
| `12d_anova_input_rate_shake_recoil.png` | ANOVA factorial: Inputs/s por Shake×Recoil. |
| `13_dunnett_significant_vs_c0.png` | Comparaciones significativas contra baseline C0. |
| `14_immersion_control_matrix.png` | Sintesis inmersion-control. |
| `15_interaction_giq_total.png` | Interaccion 2x2x2 sobre GIQ total. |
| `16_interaction_survival_time.png` | Interaccion 2x2x2 sobre supervivencia. |
| `17_interaction_damage_rate.png` | Interaccion 2x2x2 sobre dano recibido/s. |
| `18_interaction_jitter_rate.png` | Interaccion 2x2x2 sobre jitter/s. |
| `19_forest_control_index_vs_c0.png` | Forest plot de efectos C1-C7 contra C0. |
| `20_tradeoff_giq_survival.png` | Scatter de trade-off GIQ vs supervivencia. |
| `21_tradeoff_giq_damage.png` | Scatter de trade-off GIQ vs dano recibido/s. |
| `22_correlation_heatmap.png` | Heatmap de correlaciones entre variables clave. |
| `23_fps_technical_control.png` | Control tecnico de FPS por condicion. |

Los archivos compuestos antiguos `04_performance_damage.png`, `05_movement_control.png`, `06_pressure_vulnerability.png`, `07_significant_effects.png`, `08_dunnett_vs_c0.png` y `09_immersion_control_matrix.png` quedan reemplazados para la presentacion.
