# 🧃 Juiciness — Dashboard de análisis

Dashboard exploratorio (Streamlit + Plotly) de la telemetría del juego, pensado
para **balanceo y disfrute** (no para el experimento). Lee los datos en vivo
desde Supabase con la librería oficial `supabase-py`.

## Estructura

```
analysis/
├── app.py            # Dashboard principal (4 pestañas)
├── data_access.py    # Carga + caché de las tablas de Supabase
├── charts.py         # Paleta pastel y tema de Plotly
├── build_identified_camera_juiciness_csv.py
├── research_question_analysis.R
├── data/             # Insumos activos para RStudio/R
├── results/          # Tablas y resúmenes generados
├── images/           # Figuras finales
├── archive/          # CSV/TXT/figuras/scripts archivados
├── requirements.txt
└── .streamlit/
    ├── config.toml   # Tema visual pastel
    └── secrets.toml  # Credenciales (gitignored — NO se sube)
```

## Puesta en marcha

```powershell
cd analysis
python -m venv .venv
.\.venv\Scripts\Activate.ps1
pip install -r requirements.txt
streamlit run app.py
```

Se abrirá en el navegador (por defecto http://localhost:8501).

## Las 4 pestañas

| Pestaña | Qué muestra |
|---------|-------------|
| 📉 **Curva de dificultad** | Daño recibido a lo largo del tiempo, histograma de duración de runs, enemigo que más mata, curva de supervivencia. |
| 🛠️ **Viabilidad de builds** | Pick rate por mejora (elegida/ofrecida), mejoras más frecuentes, aporte de daño/kills por canal. |
| 🙋 **Desempeño por jugador** | Boxplot/barras/strip por métrica, progresión entre partidas, tabla resumen. |
| 🔗 **Correlaciones** | Matriz de correlación y scatter personalizable entre métricas. |

Todos los gráficos respetan los **filtros globales** de la barra lateral
(jugador, versión del build, rango de fechas) y tienen controles propios para
personalizar la vista. El botón **🔄 Recargar datos** limpia la caché y
re-consulta Supabase.

## Credenciales

`.streamlit/secrets.toml` contiene `SUPABASE_URL` y `SUPABASE_KEY` (publishable
key, solo lectura). Está en `.gitignore`. Si clonas en otra máquina, crea ese
archivo con tus credenciales.

Para scripts ejecutados desde la raíz del repositorio también se puede usar:

```powershell
copy analysis\.streamlit\secrets.example.toml analysis\.streamlit\secrets.toml
```

Luego complete `SUPABASE_URL` y `SUPABASE_KEY` localmente. No suba
`secrets.toml` al repositorio.

## CSV identificado con telemetría fresca

Para reconstruir el CSV de cámara sin anonimización y con métricas nuevas
(`total_kills`, `kill_rate`, `input_total`, `input_rate`, entre otras), use:

```powershell
python analysis/build_identified_camera_juiciness_csv.py --live
```

Salidas locales:

- `analysis/data/camera_juiciness_identified_fresh.csv`
- `analysis/data/camera_juiciness_identified_fresh_audit.csv`

El CSV principal reemplaza `anon_id` por `player_id` y `run_id`. La auditoría
registra cuántas corridas candidatas tenía cada participante y cuál fue
seleccionada. Si un participante repitió runs, el script conserva una sola fila
para análisis y selecciona la corrida que mejor coincide con el formulario y las
métricas del CSV base.

## Análisis vigente de investigación

El análisis estadístico vigente usa como fuente principal:

- `analysis/data/juiciness_clean_dataset.csv`

Para reproducirlo:

```powershell
Rscript analysis/research_question_analysis.R
```

Orden del cálculo:

1. Cargar `analysis/data/juiciness_clean_dataset.csv`.
2. Retener todas las filas y conservar banderas de calidad como auditoría.
3. RQ1: modelo factorial para `Promedio en GIQ`, supuestos y seguimiento de interacción cuando aplica.
4. RQ2: telemetría de desempeño con modelos factoriales/ART y modelos de conteo con offset de duración.
5. RQ3: contrastes de tratamientos aislados contra Control y correlaciones Spearman entre GIQ y desempeño.

Salidas principales:

- `analysis/results/research_questions/summary.txt`
- `analysis/results/research_questions/01_assumption_checks.csv`
- `analysis/results/research_questions/03_factorial_anova.csv`
- `analysis/results/research_questions/04_art_factorial_sensitivity.csv`
- `analysis/results/research_questions/06_count_models_duration_offset.csv`
- `analysis/results/research_questions/07_rq3_isolated_vs_control.csv`
- `analysis/results/research_questions/08_rq3_giq_performance_spearman.csv`
- `analysis/images/research_questions/`

El dataset limpio activo ya contiene una fila por participante y se usa como
base directa para GIQ, condiciones factoriales y métricas observadas de juego.

## Figuras finales

Use únicamente estas figuras para responder las preguntas de investigación:

- `analysis/images/research_questions/RQ1_immersion_giq_effects.png`
- `analysis/images/research_questions/RQ2_damage_per_min.png`
- `analysis/images/research_questions/RQ2_hits_per_min.png`
- `analysis/images/research_questions/RQ2_count_models_duration_offset.png`
- `analysis/images/research_questions/RQ3_baseline_tradeoff_forest.png`
- `analysis/images/research_questions/RQ3_giq_performance_correlation.png`

Las figuras antiguas no fueron eliminadas; quedaron archivadas en:

- `analysis/archive/legacy/figures_2026-06-27/`

Los CSV/TXT antiguos sueltos quedaron archivados en:

- `analysis/archive/data/csv/`
- `analysis/archive/data/txt/`
- `analysis/archive/legacy/outputs_2026-06-27/`

Los scripts anteriores quedaron archivados en:

- `analysis/archive/legacy/scripts_2026-06-27/`

## Opcional

- `pip install statsmodels` habilita la línea de tendencia (OLS) en el scatter
  de Correlaciones.
