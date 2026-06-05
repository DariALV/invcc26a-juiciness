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

## Opcional

- `pip install statsmodels` habilita la línea de tendencia (OLS) en el scatter
  de Correlaciones.
