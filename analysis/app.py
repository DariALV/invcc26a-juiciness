"""Dashboard exploratorio de telemetria de Juiciness.

Analisis para balanceo y disfrute del juego (no del experimento).
Ejecutar con:  streamlit run app.py
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import streamlit as st

import charts
import data_access as da

st.set_page_config(
    page_title="Juiciness · Analisis",
    page_icon="🧃",
    layout="wide",
)

# Canales de dano ofensivo conocidos (para etiquetas amables).
CHANNEL_LABELS = {
    "arrow": "Flecha",
    "burn": "Quemadura",
    "aura": "Aura",
    "death_arrows": "Flechas de muerte",
}


# ---------------------------------------------------------------------------
# Carga + barra lateral de filtros
# ---------------------------------------------------------------------------
st.title("🧃 Juiciness — Dashboard de telemetria")

# Acceso directo a la pagina de analisis de una run individual.
hdr_l, hdr_r = st.columns([3, 1])
with hdr_r:
    st.page_link(
        "pages/1_Run_individual.py",
        label="🔬 Ver una run individual",
        use_container_width=True,
    )

with st.sidebar:
    st.header("⚙️ Controles")
    st.page_link(
        "pages/1_Run_individual.py",
        label="🔬 Run individual",
        use_container_width=True,
    )
    if st.button("🔄 Recargar datos", use_container_width=True):
        st.cache_data.clear()
        st.rerun()

data = da.load_all()
runs_all = da.runs_enriched(data)

if runs_all.empty:
    st.info("Todavia no hay runs registradas. Juega una partida y pulsa *Recargar datos*.")
    st.stop()

with st.sidebar:
    st.subheader("Filtros")

    players = sorted(runs_all["player_id"].dropna().unique().tolist())
    sel_players = st.multiselect("Jugador", players, default=players)

    builds = sorted(runs_all["build_version"].dropna().unique().tolist())
    sel_builds = st.multiselect("Version del build", builds, default=builds)

    # Rango de fechas (sobre started_at).
    dates = runs_all["started_at"].dropna()
    if not dates.empty:
        dmin, dmax = dates.min().date(), dates.max().date()
        date_range = st.date_input("Rango de fechas", (dmin, dmax), min_value=dmin, max_value=dmax)
    else:
        date_range = None

# --- Aplicar filtros a las runs ---
mask = pd.Series(True, index=runs_all.index)
if sel_players:
    mask &= runs_all["player_id"].isin(sel_players)
if sel_builds:
    mask &= runs_all["build_version"].isin(sel_builds)
if date_range and isinstance(date_range, (tuple, list)) and len(date_range) == 2:
    lo, hi = date_range
    d = runs_all["started_at"].dt.date
    mask &= (d >= lo) & (d <= hi)

runs = runs_all[mask].copy()
run_ids = set(runs["id"].tolist())

if runs.empty:
    st.warning("Ningun dato coincide con los filtros seleccionados.")
    st.stop()


def child(table: str) -> pd.DataFrame:
    """Devuelve una tabla hija filtrada a las runs seleccionadas."""
    df = data[table]
    if df.empty or "run_id" not in df.columns:
        return df
    return df[df["run_id"].isin(run_ids)].copy()


dmg = child("DamageTaken")
choices = child("UpgradeChoice")
builds_df = child("RunBuild")
stats = child("UpgradeStats")
snaps = child("GameSnapshot")
feedback = child("RunFeedback")

# ---------------------------------------------------------------------------
# Tarjetas de resumen (KPIs)
# ---------------------------------------------------------------------------
c1, c2, c3, c4, c5 = st.columns(5)
c1.metric("Runs", f"{len(runs):,}")
c2.metric("Jugadores", runs["player_id"].nunique())
c3.metric("Duracion media", f"{runs['duration_seconds'].mean():.0f} s")
c4.metric("Kills medios", f"{runs['total_kills'].mean():.0f}")
c5.metric("Dano recibido medio", f"{runs['total_damage_taken'].mean():.0f}")

st.divider()

(tab_dif, tab_builds, tab_players, tab_corr, tab_heat, tab_stress, tab_fb,
 tab_rel, tab_timeline, tab_pace, tab_dec, tab_path, tab_perf, tab_power) = st.tabs([
    "📉 Curva de dificultad", "🛠️ Viabilidad de builds", "🙋 Desempeno por jugador",
    "🔗 Correlaciones", "🗺️ Mapa de calor", "🧠 Estres / flow", "💬 Feedback",
    "🔬 Relaciones", "🎞️ Timeline", "📈 Ritmo", "🎯 Decisiones", "🧭 Trayectoria",
    "🖥️ Rendimiento", "📊 Curva de poder",
])


# ===========================================================================
# PESTANA 1 — Curva de dificultad
# ===========================================================================
with tab_dif:
    st.subheader("¿Donde se vuelve dificil el juego?")
    if dmg.empty:
        st.info("No hay eventos de dano recibido para los filtros actuales.")
    else:
        cc1, cc2, cc3 = st.columns([1, 1, 1])
        bin_size = cc1.slider("Tamano de bin (segundos)", 5, 60, 15, step=5)
        breakdown = cc2.selectbox("Desglosar por", ["(ninguno)", "enemy_type", "damage_type"])
        agg_mode = cc3.selectbox("Agregacion", ["Promedio por run", "Total"])

        d = dmg.dropna(subset=["game_time_seconds"]).copy()
        d["bin"] = (d["game_time_seconds"] // bin_size) * bin_size

        n_runs = max(runs["id"].nunique(), 1)
        group_cols = ["bin"] + ([breakdown] if breakdown != "(ninguno)" else [])
        g = d.groupby(group_cols)["damage_amount"].sum().reset_index()
        if agg_mode == "Promedio por run":
            g["damage_amount"] = g["damage_amount"] / n_runs
            ylabel = "Dano recibido (promedio/run)"
        else:
            ylabel = "Dano recibido (total)"

        color = breakdown if breakdown != "(ninguno)" else None
        fig = px.area(g, x="bin", y="damage_amount", color=color, markers=False)
        fig.update_layout(xaxis_title="Tiempo de juego (s)", yaxis_title=ylabel)
        st.plotly_chart(charts.style(fig, "Dano recibido a lo largo del tiempo"), use_container_width=True)

        col_a, col_b = st.columns(2)

        # Histograma de tiempos de muerte (duracion de la run).
        with col_a:
            fig2 = px.histogram(runs, x="duration_seconds", nbins=20)
            fig2.update_layout(xaxis_title="Duracion de la run (s)", yaxis_title="# runs")
            fig2.update_traces(marker_line_color="white", marker_line_width=1)
            st.plotly_chart(charts.style(fig2, "¿Cuanto sobreviven los jugadores?", height=360), use_container_width=True)

        # Que enemigo mata mas.
        with col_b:
            deaths = runs["death_enemy"].dropna()
            if deaths.empty:
                st.info("Sin datos de enemigo causante de muerte.")
            else:
                dc = deaths.value_counts().reset_index()
                dc.columns = ["enemy", "muertes"]
                fig3 = px.bar(dc, x="muertes", y="enemy", orientation="h", color="enemy")
                fig3.update_layout(showlegend=False, yaxis_title="", xaxis_title="# muertes")
                st.plotly_chart(charts.style(fig3, "¿Que enemigo te mata mas?", height=360), use_container_width=True)

        # Curva de supervivencia (% de runs aun vivas en el tiempo t).
        st.markdown("##### Curva de supervivencia")
        durations = runs["duration_seconds"].dropna().sort_values()
        if not durations.empty:
            tmax = int(durations.max())
            ts = np.arange(0, tmax + 1, max(1, tmax // 100))
            total = len(durations)
            surv = [(durations >= t).sum() / total * 100 for t in ts]
            fig4 = go.Figure(go.Scatter(x=ts, y=surv, fill="tozeroy", line=dict(color=charts.ACCENT)))
            fig4.update_layout(xaxis_title="Tiempo de juego (s)", yaxis_title="% runs aun vivas")
            st.plotly_chart(charts.style(fig4, height=320), use_container_width=True)


# ===========================================================================
# PESTANA 2 — Viabilidad de builds
# ===========================================================================
with tab_builds:
    st.subheader("¿Que mejoras dominan y cuales nadie elige?")

    # --- Pick rate: elegida / veces ofrecida ---
    if choices.empty:
        st.info("No hay elecciones de mejora para los filtros actuales.")
    else:
        offered = pd.concat([choices["option_1"], choices["option_2"], choices["option_3"]]).dropna()
        offered_counts = offered.value_counts()
        selected_counts = choices["selected_option"].dropna().value_counts()
        pr = pd.DataFrame({"ofrecida": offered_counts, "elegida": selected_counts}).fillna(0)
        pr["pick_rate"] = (pr["elegida"] / pr["ofrecida"]).where(pr["ofrecida"] > 0, 0) * 100
        pr = pr.sort_values("pick_rate", ascending=True).reset_index(names="upgrade")

        min_off = st.slider("Minimo de veces ofrecida (filtrar ruido)", 1, max(1, int(pr["ofrecida"].max())), 1)
        prf = pr[pr["ofrecida"] >= min_off]

        fig = px.bar(
            prf, x="pick_rate", y="upgrade", orientation="h",
            color="pick_rate", color_continuous_scale=charts.PASTEL_SCALE,
            hover_data=["ofrecida", "elegida"],
        )
        fig.update_layout(xaxis_title="Pick rate (%)", yaxis_title="", coloraxis_showscale=False)
        st.plotly_chart(charts.style(fig, "Pick rate por mejora (elegida / ofrecida)", height=480), use_container_width=True)
        st.caption("Pick rate alto = mejora deseada cuando aparece. Pick rate bajo con muchas ofertas = candidata a buff.")

    col_a, col_b = st.columns(2)

    # --- Mejoras mas comunes en builds finales ---
    with col_a:
        if builds_df.empty:
            st.info("Sin datos de RunBuild.")
        else:
            top = builds_df.groupby("upgrade_name").agg(
                veces=("run_id", "nunique"), nivel_medio=("level", "mean"),
            ).sort_values("veces", ascending=True).reset_index()
            fig2 = px.bar(top, x="veces", y="upgrade_name", orientation="h",
                          color="nivel_medio", color_continuous_scale=charts.PASTEL_SCALE,
                          hover_data={"nivel_medio": ":.1f"})
            fig2.update_layout(xaxis_title="# runs que la tomaron", yaxis_title="")
            st.plotly_chart(charts.style(fig2, "Mejoras mas frecuentes", height=420), use_container_width=True)

    # --- Dano y kills por canal ---
    with col_b:
        if stats.empty:
            st.info("Sin datos de UpgradeStats (dano por canal).")
        else:
            ch = stats.groupby("damage_source").agg(
                dano=("total_damage", "sum"), kills=("total_kills", "sum"),
            ).reset_index()
            ch["canal"] = ch["damage_source"].map(CHANNEL_LABELS).fillna(ch["damage_source"])
            metric = st.radio("Metrica", ["dano", "kills"], horizontal=True, key="ch_metric")
            fig3 = px.bar(ch.sort_values(metric), x=metric, y="canal", orientation="h", color="canal")
            fig3.update_layout(showlegend=False, yaxis_title="", xaxis_title=metric.capitalize())
            st.plotly_chart(charts.style(fig3, "Aporte por canal de dano", height=420), use_container_width=True)


# ===========================================================================
# PESTANA 3 — Desempeno por jugador
# ===========================================================================
with tab_players:
    st.subheader("Comparacion entre jugadores y partidas")

    metric_map = {
        "Duracion (s)": "duration_seconds",
        "Kills": "total_kills",
        "XP total": "total_xp",
        "Dano recibido": "total_damage_taken",
        "Ronda final": "final_round",
        "Nivel final": "final_level",
    }
    cc1, cc2 = st.columns([1, 1])
    metric_label = cc1.selectbox("Metrica", list(metric_map.keys()))
    chart_kind = cc2.selectbox("Tipo de grafica", ["Boxplot", "Barras (promedio)", "Puntos (strip)"])
    metric = metric_map[metric_label]

    if chart_kind == "Boxplot":
        fig = px.box(runs, x="player_id", y=metric, color="player_id", points="all")
    elif chart_kind == "Barras (promedio)":
        agg = runs.groupby("player_id")[metric].mean().reset_index()
        fig = px.bar(agg, x="player_id", y=metric, color="player_id")
    else:
        fig = px.strip(runs, x="player_id", y=metric, color="player_id")
    fig.update_layout(showlegend=False, xaxis_title="", yaxis_title=metric_label)
    st.plotly_chart(charts.style(fig, f"{metric_label} por jugador"), use_container_width=True)

    col_a, col_b = st.columns([3, 2])

    # Progresion entre runs (orden temporal).
    with col_a:
        prog = runs.dropna(subset=["started_at"]).sort_values("started_at").copy()
        prog["n_run"] = prog.groupby("player_id").cumcount() + 1
        fig2 = px.line(prog, x="n_run", y=metric, color="player_id", markers=True)
        fig2.update_layout(xaxis_title="Numero de partida del jugador", yaxis_title=metric_label)
        st.plotly_chart(charts.style(fig2, "Progresion entre partidas", height=380), use_container_width=True)

    # Tabla resumen por jugador.
    with col_b:
        summary = runs.groupby("player_id").agg(
            runs=("id", "count"),
            dur_media=("duration_seconds", "mean"),
            kills_medio=("total_kills", "mean"),
            dano_medio=("total_damage_taken", "mean"),
        ).round(1).sort_values("runs", ascending=False)
        st.markdown("##### Resumen por jugador")
        st.dataframe(summary, use_container_width=True, height=380)


# ===========================================================================
# PESTANA 4 — Correlaciones
# ===========================================================================
with tab_corr:
    st.subheader("¿Que metricas se mueven juntas?")
    feats = da.build_run_features(data)
    feats = feats[feats["id"].isin(run_ids)] if not feats.empty else feats

    num_cols = [
        c for c in feats.columns
        if pd.api.types.is_numeric_dtype(feats[c]) and feats[c].notna().any()
    ]
    # Quitar columnas poco informativas.
    num_cols = [c for c in num_cols if feats[c].nunique() > 1]

    if len(num_cols) < 2:
        st.info("Se necesitan al menos 2 metricas con variacion (y varias runs) para correlacionar.")
    else:
        sel_cols = st.multiselect("Metricas a incluir", num_cols, default=num_cols)
        if len(sel_cols) >= 2:
            corr = feats[sel_cols].corr()
            fig = px.imshow(
                corr, text_auto=".2f", zmin=-1, zmax=1, aspect="auto",
                color_continuous_scale=charts.PASTEL_DIVERGING,
            )
            st.plotly_chart(charts.style(fig, "Matriz de correlacion", height=520), use_container_width=True)

        st.markdown("##### Explorar relacion entre dos metricas")
        sc1, sc2, sc3, sc4 = st.columns(4)
        x = sc1.selectbox("Eje X", num_cols, index=0)
        y = sc2.selectbox("Eje Y", num_cols, index=min(1, len(num_cols) - 1))
        color_opt = sc3.selectbox("Color", ["(ninguno)", "player_id"] + num_cols)
        show_trend = sc4.checkbox("Linea de tendencia", value=False)
        color = None if color_opt == "(ninguno)" else color_opt
        hover = ["player_id"] if "player_id" in feats.columns else None

        trendline = "ols" if (show_trend and color is None) else None
        try:
            fig2 = px.scatter(feats, x=x, y=y, color=color, trendline=trendline, hover_data=hover)
        except (ImportError, ModuleNotFoundError):
            st.caption("ℹ️ Instala `statsmodels` para la linea de tendencia.")
            fig2 = px.scatter(feats, x=x, y=y, color=color, hover_data=hover)
        st.plotly_chart(charts.style(fig2, f"{y} vs {x}", height=460), use_container_width=True)


# ===========================================================================
# PESTANA 5 — Mapa de calor (posicion del jugador)
# ===========================================================================
with tab_heat:
    st.subheader("¿Donde pasa el tiempo el jugador?")
    if snaps.empty or snaps["player_x"].dropna().empty:
        st.info("Aun no hay snapshots de posicion. Se generan ~1/segundo durante la partida.")
    else:
        s = snaps.dropna(subset=["player_x", "player_y"]).copy()
        cc1, cc2 = st.columns([1, 1])
        nbins = cc1.slider("Resolucion (celdas)", 20, 120, 50, step=10)
        weight = cc2.selectbox(
            "Ponderar por",
            ["Tiempo (densidad)", "Dano recibido", "Cambios de direccion (jitter)"],
        )
        wmap = {
            "Tiempo (densidad)": None,
            "Dano recibido": "dmg_taken_delta",
            "Cambios de direccion (jitter)": "dir_changes_delta",
        }
        z = wmap[weight]
        fig = px.density_heatmap(
            s, x="player_x", y="player_y", z=z, nbinsx=nbins, nbinsy=nbins,
            histfunc="avg" if z else "count", color_continuous_scale=charts.PASTEL_SCALE,
        )
        # Invertir Y: en pantalla +Y va hacia abajo.
        fig.update_yaxes(autorange="reversed", scaleanchor="x", scaleratio=1)
        fig.update_layout(xaxis_title="x", yaxis_title="y")
        st.plotly_chart(charts.style(fig, f"Mapa de calor — {weight}", height=560), use_container_width=True)
        st.caption("Zonas calientes = donde el jugador se queda mas / sufre mas / se mueve mas erratico.")


# ===========================================================================
# PESTANA 6 — Estres / flow (proxies conductuales)
# ===========================================================================
with tab_stress:
    st.subheader("Proxies de estres y concentracion")
    st.caption(
        "Son **proxies conductuales**, no medidas fisiologicas. Ideal: validarlos "
        "contra el cuestionario presencial."
    )
    if snaps.empty:
        st.info("Aun no hay snapshots para calcular proxies.")
    else:
        s = snaps.dropna(subset=["game_time_seconds"]).copy()

        proxy_map = {
            "Jitter (cambios de direccion)": "dir_changes_delta",
            "Actividad (inputs)": "inputs_delta",
            "Distancia recorrida": "distance_moved",
            "Presion (dist. enemigo +cercano)": "nearest_enemy_dist",
            "Dano recibido": "dmg_taken_delta",
            "Dano infligido": "dmg_dealt_delta",
            "HP": "hp",
        }
        cc1, cc2 = st.columns([1, 1])
        proxy_label = cc1.selectbox("Proxy a ver en el tiempo", list(proxy_map.keys()))
        smooth = cc2.slider("Suavizado (ventana, snapshots)", 1, 15, 3)
        col = proxy_map[proxy_label]

        # Curva temporal promediada entre runs (con suavizado opcional).
        g = s.groupby("game_time_seconds")[col].mean().reset_index().sort_values("game_time_seconds")
        if smooth > 1:
            g[col] = g[col].rolling(smooth, min_periods=1, center=True).mean()
        fig = px.line(g, x="game_time_seconds", y=col)
        fig.update_traces(line=dict(color=charts.ACCENT, width=2.5))
        fig.update_layout(xaxis_title="Tiempo de juego (s)", yaxis_title=proxy_label)
        st.plotly_chart(charts.style(fig, f"{proxy_label} a lo largo del tiempo"), use_container_width=True)

        col_a, col_b = st.columns(2)

        # Tiempo en HP bajo por run (proxy de tension sostenida).
        with col_a:
            s2 = s.dropna(subset=["hp", "max_hp"]).copy()
            if not s2.empty:
                thr = st.slider("Umbral de HP bajo (%)", 5, 50, 25, key="lowhp")
                s2["low"] = s2["hp"] <= (thr / 100.0) * s2["max_hp"]
                # Cada snapshot ~ SNAPSHOT_INTERVAL s; aproximamos segundos en HP bajo.
                low = s2.groupby("run_id")["low"].sum().reset_index(name="snaps_hp_bajo")
                low = low.merge(runs[["id", "player_id"]], left_on="run_id", right_on="id", how="left")
                fig2 = px.bar(low.sort_values("snaps_hp_bajo"), x="snaps_hp_bajo", y="player_id",
                              orientation="h", color="snaps_hp_bajo",
                              color_continuous_scale=charts.PASTEL_SCALE)
                fig2.update_layout(coloraxis_showscale=False, yaxis_title="",
                                   xaxis_title="~segundos en HP bajo")
                st.plotly_chart(charts.style(fig2, "Tension sostenida (HP bajo)", height=360),
                                use_container_width=True)

        # Jitter promedio por jugador (proxy de nerviosismo).
        with col_b:
            j = s.groupby("run_id")["dir_changes_delta"].mean().reset_index(name="jitter")
            j = j.merge(runs[["id", "player_id"]], left_on="run_id", right_on="id", how="left")
            jg = j.groupby("player_id")["jitter"].mean().reset_index().sort_values("jitter")
            fig3 = px.bar(jg, x="jitter", y="player_id", orientation="h", color="player_id")
            fig3.update_layout(showlegend=False, yaxis_title="", xaxis_title="Jitter medio")
            st.plotly_chart(charts.style(fig3, "Nerviosismo por jugador (jitter)", height=360),
                            use_container_width=True)


# ===========================================================================
# PESTANA 7 — Feedback (cuestionario post-run auto-reportado)
# ===========================================================================
with tab_fb:
    st.subheader("Auto-evaluacion de los jugadores")
    if feedback.empty:
        st.info("Aun no hay respuestas del cuestionario post-run.")
    else:
        likert = [c for c in da.FEEDBACK_LIKERT if c in feedback.columns]
        fb = feedback.merge(runs[["id", "player_id"]], left_on="run_id", right_on="id", how="left")

        k1, k2, k3 = st.columns(3)
        k1.metric("Respuestas", len(feedback))
        k2.metric("Tasa de respuesta", f"{len(feedback) / max(len(runs), 1) * 100:.0f}%")
        if "unnecessary_upgrades" in feedback.columns:
            pct = feedback["unnecessary_upgrades"].fillna(False).mean() * 100
            k3.metric("Vieron mejoras inutiles", f"{pct:.0f}%")

        # Promedio por dimension (1-7).
        means = feedback[likert].mean().reset_index()
        means.columns = ["dim", "media"]
        means["dim"] = means["dim"].map(da.FEEDBACK_LABELS).fillna(means["dim"])
        fig = px.bar(means.sort_values("media"), x="media", y="dim", orientation="h",
                     color="media", color_continuous_scale=charts.PASTEL_SCALE,
                     range_x=[1, 7])
        fig.update_layout(coloraxis_showscale=False, yaxis_title="", xaxis_title="Promedio (1-7)")
        st.plotly_chart(charts.style(fig, "Promedio por dimension"), use_container_width=True)

        col_a, col_b = st.columns(2)

        # Distribucion de cada dimension (cuanto coinciden o divergen).
        with col_a:
            long = feedback[likert].melt(var_name="dim", value_name="valor").dropna()
            long["dim"] = long["dim"].map(da.FEEDBACK_LABELS).fillna(long["dim"])
            figd = px.box(long, x="valor", y="dim", color="dim", points="all")
            figd.update_layout(showlegend=False, yaxis_title="", xaxis_title="1-7")
            st.plotly_chart(charts.style(figd, "Distribucion de respuestas", height=400),
                            use_container_width=True)

        # Validacion: auto-reporte vs metrica medida (cruzar percepcion con datos).
        with col_b:
            measured = {
                "Duracion (s)": "duration_seconds",
                "Dano recibido": "total_damage_taken",
                "Kills": "total_kills",
                "Ronda final": "final_round",
            }
            rep_label = st.selectbox("Auto-reporte", [da.FEEDBACK_LABELS.get(c, c) for c in likert])
            rep_col = next(c for c in likert if da.FEEDBACK_LABELS.get(c, c) == rep_label)
            meas_label = st.selectbox("Metrica medida", list(measured.keys()))
            meas_col = measured[meas_label]

            val = feedback.merge(runs[["id", meas_col]], left_on="run_id", right_on="id", how="left")
            val = val.dropna(subset=[rep_col, meas_col])
            if val.empty:
                st.info("Sin datos suficientes para cruzar percepcion y medicion.")
            else:
                figv = px.scatter(val, x=rep_col, y=meas_col)
                figv.update_traces(marker=dict(size=11, color=charts.ACCENT,
                                               line=dict(width=1, color="white")))
                figv.update_layout(xaxis_title=f"{rep_label} (1-7)", yaxis_title=meas_label)
                st.plotly_chart(charts.style(figv, f"{meas_label} vs {rep_label}", height=400),
                                use_container_width=True)

        # Comentarios libres.
        if "comments" in feedback.columns:
            com = fb[fb["comments"].fillna("").str.strip() != ""]
            if not com.empty:
                st.markdown("##### Comentarios")
                cols = [c for c in ["player_id", "comments"] if c in com.columns]
                st.dataframe(com[cols], use_container_width=True, hide_index=True)


# ===========================================================================
# PESTANA 8 — Relaciones entre tablas
# ===========================================================================
with tab_rel:
    st.subheader("Relaciones cruzando varias tablas")

    # --- 1. Build -> desempeno (RunBuild ⨝ Run) ---
    st.markdown("##### ¿Qué mejoras acompañan a las mejores runs?")
    if builds_df.empty:
        st.info("Sin datos de builds.")
    else:
        rbm = builds_df.merge(
            runs[["id", "duration_seconds", "total_kills", "total_xp"]],
            left_on="run_id", right_on="id", how="left",
        )
        mmap = {"Duracion (s)": "duration_seconds", "Kills": "total_kills", "XP total": "total_xp"}
        c1, c2 = st.columns(2)
        ml = c1.selectbox("Desempeño medio de las runs que la incluyen", list(mmap.keys()), key="rel_m")
        minr = c2.slider("Mínimo de runs con la mejora", 1, 6, 1, key="rel_minr")
        mc = mmap[ml]
        agg = rbm.groupby("upgrade_name").agg(runs=("run_id", "nunique"), val=(mc, "mean")).reset_index()
        agg = agg[agg["runs"] >= minr].sort_values("val")
        if agg.empty:
            st.info("Ninguna mejora supera el mínimo de runs.")
        else:
            fig = px.bar(agg, x="val", y="upgrade_name", orientation="h", color="val",
                         color_continuous_scale=charts.PASTEL_SCALE, hover_data=["runs"])
            fig.update_layout(coloraxis_showscale=False, yaxis_title="", xaxis_title=ml)
            st.plotly_chart(charts.style(fig, f"{ml} según mejora en el build", height=560),
                            use_container_width=True)
            st.caption("Con pocas runs es indicativo, no concluyente. 'runs' en el hover = tamaño de muestra.")

    # --- 2. Timing de eleccion (UpgradeChoice) ---
    st.markdown("##### ¿Cuándo se elige cada mejora? (temprano vs tarde)")
    if choices.empty:
        st.info("Sin elecciones de mejora.")
    else:
        sel = choices.dropna(subset=["selected_option", "game_time_seconds"])
        if sel.empty:
            st.info("Sin tiempos de elección.")
        else:
            topn = st.slider("Top N mejoras más elegidas", 5, 25, 12, key="rel_topn")
            top = sel["selected_option"].value_counts().head(topn).index
            selt = sel[sel["selected_option"].isin(top)]
            order = selt.groupby("selected_option")["game_time_seconds"].median().sort_values().index.tolist()
            figt = px.box(selt, x="game_time_seconds", y="selected_option", color="selected_option",
                          points="all", category_orders={"selected_option": order})
            figt.update_layout(showlegend=False, yaxis_title="", xaxis_title="Tiempo de juego al elegir (s)")
            st.plotly_chart(charts.style(figt, "Momento de elección por mejora", height=480),
                            use_container_width=True)

    # --- 3. Letalidad: enemigo x tipo de dano (DamageTaken) ---
    st.markdown("##### Letalidad: enemigo × tipo de daño")
    if dmg.empty:
        st.info("Sin daño recibido registrado.")
    else:
        piv = dmg.pivot_table(index="enemy_type", columns="damage_type",
                              values="damage_amount", aggfunc="sum", fill_value=0)
        figh = px.imshow(piv, text_auto=".0f", aspect="auto", color_continuous_scale=charts.PASTEL_SCALE)
        figh.update_layout(xaxis_title="Tipo de daño", yaxis_title="Enemigo")
        st.plotly_chart(charts.style(figh, "Daño recibido total por fuente", height=380),
                        use_container_width=True)


# ===========================================================================
# PESTANA 9 — Timeline (radiografia de una run con snapshots)
# ===========================================================================
with tab_timeline:
    st.subheader("Radiografía de una partida")
    if snaps.empty:
        st.info("Aún no hay snapshots. Juega una run con la versión que los registra.")
    else:
        runs_ws = runs[runs["id"].isin(snaps["run_id"].unique())].copy()
        if runs_ws.empty:
            st.info("Ninguna run filtrada tiene snapshots.")
        else:
            def _lbl(r):
                d = 0 if pd.isna(r["duration_seconds"]) else int(r["duration_seconds"])
                return f"{r['player_id']} · {d}s · {str(r['id'])[:8]}"
            runs_ws["lbl"] = runs_ws.apply(_lbl, axis=1)
            pick = st.selectbox("Elige una run", runs_ws["lbl"].tolist())
            rid = runs_ws[runs_ws["lbl"] == pick]["id"].iloc[0]
            s = snaps[snaps["run_id"] == rid].sort_values("game_time_seconds")
            picks = choices[choices["run_id"] == rid] if not choices.empty else pd.DataFrame()

            fig = make_subplots(
                rows=2, cols=1, shared_xaxes=True, vertical_spacing=0.09,
                row_heights=[0.55, 0.45],
                subplot_titles=("HP y presión espacial", "Daño recibido y enemigos vivos"),
            )
            fig.add_trace(go.Scatter(x=s["game_time_seconds"], y=s["hp"], name="HP",
                                     fill="tozeroy", line=dict(color=charts.PALETTE[2])), row=1, col=1)
            fig.add_trace(go.Scatter(x=s["game_time_seconds"], y=s["nearest_enemy_dist"],
                                     name="Dist. enemigo +cercano", line=dict(color=charts.PALETTE[0])), row=1, col=1)
            fig.add_trace(go.Bar(x=s["game_time_seconds"], y=s["dmg_taken_delta"],
                                 name="Daño recibido", marker_color=charts.PALETTE[1]), row=2, col=1)
            fig.add_trace(go.Scatter(x=s["game_time_seconds"], y=s["enemies_alive"],
                                     name="Enemigos vivos", line=dict(color=charts.PALETTE[4])), row=2, col=1)
            # Lineas punteadas: momentos de eleccion de mejora.
            if not picks.empty and "game_time_seconds" in picks.columns:
                for _, p in picks.iterrows():
                    t = p["game_time_seconds"]
                    if pd.notna(t):
                        fig.add_vline(x=t, line_dash="dot", line_color=charts.PALETTE[5], opacity=0.5)
            fig.update_layout(template="pastel", height=640, barmode="overlay",
                              legend=dict(orientation="h", y=1.12))
            fig.update_xaxes(title_text="Tiempo de juego (s)", row=2, col=1)
            st.plotly_chart(fig, use_container_width=True)
            st.caption("Líneas punteadas = el jugador eligió una mejora en ese instante.")

            # Mini-resumen de movimiento de la run.
            m1, m2, m3 = st.columns(3)
            m1.metric("Snapshots", len(s))
            m2.metric("Distancia total", f"{s['distance_moved'].sum():.0f}")
            m3.metric("Jitter total", f"{int(s['dir_changes_delta'].sum())}")


# ===========================================================================
# PESTANA 10 — Ritmo & economia
# ===========================================================================
with tab_pace:
    st.subheader("Ritmo de progresión y economía")

    # --- Curva de leveling: tiempo medio en alcanzar cada nivel ---
    st.markdown("##### ¿Qué tan rápido se sube de nivel?")
    if snaps.empty or "level" not in snaps.columns:
        st.info("Sin snapshots para reconstruir el leveling.")
    else:
        sl = snaps.dropna(subset=["level", "game_time_seconds"])
        first = sl.groupby(["run_id", "level"])["game_time_seconds"].min().reset_index()
        avg = first.groupby("level")["game_time_seconds"].agg(["mean", "count"]).reset_index()
        figl = px.line(avg, x="level", y="mean", markers=True, hover_data=["count"])
        figl.update_traces(line=dict(color=charts.ACCENT, width=2.5))
        figl.update_layout(xaxis_title="Nivel", yaxis_title="Tiempo medio para alcanzarlo (s)")
        st.plotly_chart(charts.style(figl, "Curva de leveling"), use_container_width=True)
        st.caption("Tramos planos = se sube muy rápido; tramos empinados = cuesta llegar.")

    col_a, col_b = st.columns(2)

    # --- Balance de combate: daño recibido vs infligido en el tiempo ---
    with col_a:
        if snaps.empty:
            st.info("Sin snapshots.")
        else:
            sb = snaps.dropna(subset=["game_time_seconds"]).copy()
            binw = 5
            sb["bin"] = (sb["game_time_seconds"] // binw) * binw
            g = sb.groupby("bin")[["dmg_taken_delta", "dmg_dealt_delta"]].mean().reset_index()
            figc = go.Figure()
            figc.add_trace(go.Scatter(x=g["bin"], y=g["dmg_dealt_delta"], name="Daño infligido",
                                      fill="tozeroy", line=dict(color=charts.PALETTE[2])))
            figc.add_trace(go.Scatter(x=g["bin"], y=g["dmg_taken_delta"], name="Daño recibido",
                                      fill="tozeroy", line=dict(color=charts.PALETTE[1])))
            figc.update_layout(xaxis_title="Tiempo (s)", yaxis_title="Daño medio / bin")
            st.plotly_chart(charts.style(figc, "Balance de combate en el tiempo", height=380),
                            use_container_width=True)

    # --- Economia: XP total vs duracion ---
    with col_b:
        if runs["total_xp"].notna().any():
            figx = px.scatter(runs, x="duration_seconds", y="total_xp", color="player_id",
                              hover_data=["total_kills"])
            figx.update_traces(marker=dict(size=11, line=dict(width=1, color="white")))
            figx.update_layout(showlegend=False, xaxis_title="Duración (s)", yaxis_title="XP total")
            st.plotly_chart(charts.style(figx, "XP total vs duración", height=380),
                            use_container_width=True)


# ===========================================================================
# PESTANA 11 — Decisiones & trampas
# ===========================================================================
with tab_dec:
    st.subheader("Decisiones de mejora y mejoras 'trampa'")

    if choices.empty:
        st.info("Sin elecciones de mejora.")
    else:
        offered = pd.concat([choices["option_1"], choices["option_2"], choices["option_3"]]).dropna()
        off_counts = offered.value_counts()
        sel_counts = choices["selected_option"].dropna().value_counts()
        pr = pd.DataFrame({"ofrecida": off_counts, "elegida": sel_counts}).fillna(0)

        # --- Mejoras nunca/casi nunca elegidas pese a ofrecerse mucho ---
        st.markdown("##### 🚩 Candidatas a buff/rework (mucha oferta, poco elegidas)")
        thr = st.slider("Veces ofrecida mínima", 1, int(pr["ofrecida"].max()), max(3, 1), key="dec_thr")
        pr["pick_rate"] = (pr["elegida"] / pr["ofrecida"] * 100).round(0)
        traps = pr[(pr["ofrecida"] >= thr)].sort_values("pick_rate").head(20).reset_index(names="mejora")
        figt = px.bar(traps, x="ofrecida", y="mejora", orientation="h", color="pick_rate",
                      color_continuous_scale=charts.PASTEL_SCALE, range_color=[0, 100],
                      hover_data=["elegida", "pick_rate"])
        figt.update_layout(yaxis_title="", xaxis_title="Veces ofrecida",
                           coloraxis_colorbar_title="pick %")
        st.plotly_chart(charts.style(figt, "Ofrecidas mucho, elegidas poco", height=520),
                        use_container_width=True)
        st.caption("Barras largas y oscuras (pick% bajo) = la gente las ignora aunque aparezcan seguido.")

    # --- Tiempo de decision y rerolls (si hay datos nuevos) ---
    if not choices.empty and "decision_ms" in choices.columns and choices["decision_ms"].notna().any():
        col_a, col_b = st.columns(2)
        with col_a:
            dd = choices.dropna(subset=["decision_ms"]).copy()
            dd["seg"] = dd["decision_ms"] / 1000.0
            figd = px.histogram(dd, x="seg", nbins=20)
            figd.update_traces(marker_line_color="white", marker_line_width=1)
            figd.update_layout(xaxis_title="Tiempo de decisión (s)", yaxis_title="# elecciones")
            st.plotly_chart(charts.style(figd, "¿Cuánto tardan en decidir?", height=340),
                            use_container_width=True)
        with col_b:
            if "rerolls" in choices.columns and choices["rerolls"].notna().any():
                rr = choices["rerolls"].fillna(0).astype(int).value_counts().sort_index().reset_index()
                rr.columns = ["rerolls", "veces"]
                figr = px.bar(rr, x="rerolls", y="veces", color="rerolls",
                              color_continuous_scale=charts.PASTEL_SCALE)
                figr.update_layout(coloraxis_showscale=False, xaxis_title="Rerolls usados",
                                   yaxis_title="# elecciones")
                st.plotly_chart(charts.style(figr, "Uso de rerolls por elección", height=340),
                                use_container_width=True)
    else:
        st.info("Aún no hay datos de tiempo de decisión / rerolls (se registran en runs nuevas).")


# ===========================================================================
# PESTANA 12 — Trayectoria (recorrido espacial de una run)
# ===========================================================================
with tab_path:
    st.subheader("Recorrido del jugador en una partida")
    if snaps.empty or snaps["player_x"].dropna().empty:
        st.info("Sin snapshots de posición.")
    else:
        runs_ws = runs[runs["id"].isin(snaps["run_id"].unique())].copy()
        if runs_ws.empty:
            st.info("Ninguna run filtrada tiene snapshots.")
        else:
            def _lblp(r):
                d = 0 if pd.isna(r["duration_seconds"]) else int(r["duration_seconds"])
                return f"{r['player_id']} · {d}s · {str(r['id'])[:8]}"
            runs_ws["lbl"] = runs_ws.apply(_lblp, axis=1)
            pick = st.selectbox("Elige una run", runs_ws["lbl"].tolist(), key="path_run")
            rid = runs_ws[runs_ws["lbl"] == pick]["id"].iloc[0]
            s = snaps[snaps["run_id"] == rid].sort_values("game_time_seconds").dropna(subset=["player_x", "player_y"])

            figp = go.Figure()
            # Trazo del recorrido.
            figp.add_trace(go.Scatter(
                x=s["player_x"], y=s["player_y"], mode="lines+markers",
                line=dict(color="rgba(168,199,231,0.5)", width=2),
                marker=dict(size=7, color=s["game_time_seconds"],
                            colorscale=charts.PASTEL_SCALE, showscale=True,
                            colorbar=dict(title="t (s)")),
                name="Recorrido",
            ))
            # Inicio y fin marcados.
            if not s.empty:
                figp.add_trace(go.Scatter(x=[s["player_x"].iloc[0]], y=[s["player_y"].iloc[0]],
                                          mode="markers", marker=dict(size=14, color=charts.PALETTE[2]),
                                          name="Inicio"))
                figp.add_trace(go.Scatter(x=[s["player_x"].iloc[-1]], y=[s["player_y"].iloc[-1]],
                                          mode="markers", marker=dict(size=14, color=charts.PALETTE[1]),
                                          name="Fin"))
            figp.update_yaxes(autorange="reversed", scaleanchor="x", scaleratio=1)
            figp.update_layout(template="pastel", height=560, xaxis_title="x", yaxis_title="y")
            st.plotly_chart(figp, use_container_width=True)
            st.caption("Color = avance del tiempo. Verde = inicio, rosa = fin. Útil para ver kiting y zonas seguras.")


# ===========================================================================
# PESTANA 13 — Rendimiento (FPS) y caos en pantalla
# ===========================================================================
with tab_perf:
    st.subheader("Rendimiento técnico y caos en pantalla")
    if snaps.empty or "fps" not in snaps.columns or snaps["fps"].dropna().empty:
        st.info("Aún no hay snapshots con FPS (se registran en runs nuevas a 100ms).")
    else:
        s = snaps.dropna(subset=["game_time_seconds"]).copy()
        binw = st.slider("Bin temporal (s)", 1, 20, 5, key="perf_bin")
        s["bin"] = (s["game_time_seconds"] // binw) * binw

        # FPS medio en el tiempo.
        g = s.groupby("bin")["fps"].mean().reset_index()
        figf = px.line(g, x="bin", y="fps")
        figf.update_traces(line=dict(color=charts.ACCENT, width=2.5))
        figf.update_layout(xaxis_title="Tiempo (s)", yaxis_title="FPS medio")
        st.plotly_chart(charts.style(figf, "FPS a lo largo del tiempo"), use_container_width=True)
        st.caption("Caídas de FPS = momentos donde la juiciness + cantidad de entidades pesa. Correlacionable con las condiciones de cámara.")

        col_a, col_b = st.columns(2)

        # FPS vs densidad de entidades (¿el caos tira el framerate?).
        with col_a:
            if "projectiles_alive" in s.columns:
                s["entidades"] = s["enemies_alive"].fillna(0) + s["projectiles_alive"].fillna(0)
                xlbl = "Enemigos + proyectiles"
            else:
                s["entidades"] = s["enemies_alive"]
                xlbl = "Enemigos vivos"
            figd = px.scatter(s, x="entidades", y="fps", color="player_id"
                              if "player_id" in s.columns else None, opacity=0.45)
            figd.update_layout(showlegend=False, xaxis_title=xlbl, yaxis_title="FPS")
            st.plotly_chart(charts.style(figd, "FPS vs entidades en pantalla", height=380),
                            use_container_width=True)

        # Caos en pantalla (apilado).
        with col_b:
            ccols = [c for c in ["enemies_alive", "projectiles_alive", "xp_orbs_alive"] if c in s.columns]
            gc = s.groupby("bin")[ccols].mean().reset_index()
            lbl = {"enemies_alive": "Enemigos", "projectiles_alive": "Proyectiles", "xp_orbs_alive": "Orbes XP"}
            long = gc.melt(id_vars="bin", var_name="tipo", value_name="cantidad")
            long["tipo"] = long["tipo"].map(lbl).fillna(long["tipo"])
            figc = px.area(long, x="bin", y="cantidad", color="tipo")
            figc.update_layout(xaxis_title="Tiempo (s)", yaxis_title="Cantidad media")
            st.plotly_chart(charts.style(figc, "Caos en pantalla en el tiempo", height=380),
                            use_container_width=True)

        # FPS por jugador (distribucion).
        if "player_id" in s.columns:
            figb = px.box(s, x="player_id", y="fps", color="player_id", points=False)
            figb.update_layout(showlegend=False, xaxis_title="", yaxis_title="FPS")
            st.plotly_chart(charts.style(figb, "Distribución de FPS por jugador", height=360),
                            use_container_width=True)


# ===========================================================================
# PESTANA 14 — Curva de poder del build
# ===========================================================================
with tab_power:
    st.subheader("Evolución del build: poder vs amenaza")
    statcols = {
        "Daño": "damage", "Penetración": "pierce", "Cadencia (disp/s)": "fire_rate",
        "Flechas": "arrows", "Velocidad mov.": "move_speed", "Vel. flecha": "arrow_speed",
        "Regeneración": "regen", "Daño de área": "area_damage", "Frec. área": "area_attack_rate",
        "Objetivos área": "area_max_targets", "Esquiva": "dodge_chance", "Reflexión": "reflect_chance",
        "Venganza": "death_arrow_chance", "Vampirismo": "heal_on_kill", "Suerte": "luck",
        "Mult. XP": "xp_multiplier",
    }
    present = {k: v for k, v in statcols.items()
               if v in snaps.columns and snaps[v].notna().any()}
    if snaps.empty or not present:
        st.info("Aún no hay snapshots con stats del build (runs nuevas a 100ms).")
    else:
        s = snaps.dropna(subset=["game_time_seconds"]).copy()
        binw = st.slider("Bin temporal (s)", 1, 20, 5, key="power_bin")
        s["bin"] = (s["game_time_seconds"] // binw) * binw

        sel = st.multiselect("Stats a ver en el tiempo", list(present.keys()),
                             default=list(present.keys())[:3])
        if sel:
            cols = [present[k] for k in sel]
            g = s.groupby("bin")[cols].mean().reset_index()
            inv = {v: k for k, v in present.items()}
            long = g.melt(id_vars="bin", var_name="stat", value_name="valor")
            long["stat"] = long["stat"].map(inv).fillna(long["stat"])
            figp = px.line(long, x="bin", y="valor", color="stat")
            figp.update_layout(xaxis_title="Tiempo (s)", yaxis_title="Valor medio")
            st.plotly_chart(charts.style(figp, "Curva de poder del build"), use_container_width=True)
            st.caption("Cómo crece cada stat a lo largo de la partida (promedio entre runs).")

        col_a, col_b = st.columns(2)

        # Poder (daño del build) vs amenaza (enemigos vivos), dos ejes.
        with col_a:
            if "damage" in s.columns and "enemies_alive" in s.columns:
                g2 = s.groupby("bin").agg(damage=("damage", "mean"),
                                          enemigos=("enemies_alive", "mean")).reset_index()
                fig2 = make_subplots(specs=[[{"secondary_y": True}]])
                fig2.add_trace(go.Scatter(x=g2["bin"], y=g2["damage"], name="Daño del build",
                                          line=dict(color=charts.PALETTE[2], width=2.5)), secondary_y=False)
                fig2.add_trace(go.Scatter(x=g2["bin"], y=g2["enemigos"], name="Enemigos vivos",
                                          line=dict(color=charts.PALETTE[1], width=2.5)), secondary_y=True)
                fig2.update_layout(template="pastel", height=380,
                                   legend=dict(orientation="h", y=1.15))
                fig2.update_xaxes(title_text="Tiempo (s)")
                fig2.update_yaxes(title_text="Daño", secondary_y=False)
                fig2.update_yaxes(title_text="Enemigos", secondary_y=True)
                st.plotly_chart(fig2, use_container_width=True)
                st.caption("¿El poder del jugador sigue el ritmo de la amenaza?")

        # Kills acumulados en el tiempo.
        with col_b:
            if "kills_so_far" in s.columns:
                gk = s.groupby("bin")["kills_so_far"].mean().reset_index()
                figk = px.area(gk, x="bin", y="kills_so_far")
                figk.update_traces(line=dict(color=charts.ACCENT))
                figk.update_layout(xaxis_title="Tiempo (s)", yaxis_title="Kills acumulados (medio)")
                st.plotly_chart(charts.style(figk, "Kills acumulados en el tiempo", height=380),
                                use_container_width=True)
                st.caption("La pendiente = ritmo de kills (kills/seg).")
