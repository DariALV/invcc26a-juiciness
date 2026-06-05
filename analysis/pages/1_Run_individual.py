"""Pagina aislada: estadisticas concretas de UNA run de UN jugador.

Es totalmente independiente del dashboard global (`app.py`). Mientras la
pagina principal agrega a todos los participantes, aqui se diseccciona una
unica partida: mejoras tomadas (y cuando), una grafica por cada stat del
snapshot, dano recibido, trayectoria, etc.

Controles globales de la pagina (barra lateral):
- Seleccion de jugador + run.
- Filtro global de rango de tiempo (min..max de la run): TODO lo que se muestra
  se recorta a ese rango.
- Interruptor para dibujar lineas verticales que separan rondas en las graficas
  temporales.

Ejecutar el dashboard con `streamlit run app.py`; esta pagina aparece sola en
la navegacion lateral.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import streamlit as st

import charts
import data_access as da

st.set_page_config(
    page_title="Juiciness · Run individual",
    page_icon="🔬",
    layout="wide",
)

# ---------------------------------------------------------------------------
# Catalogos de stats del snapshot, agrupados para la "seccion de stats".
# Cada entrada: columna -> etiqueta amable.
# ---------------------------------------------------------------------------
BUILD_STATS = {
    "damage": "Daño",
    "pierce": "Penetración",
    "arrows": "Flechas",
    "fire_rate": "Cadencia (disp/s)",
    "arrow_speed": "Vel. flecha",
    "move_speed": "Vel. movimiento",
    "regen": "Regeneración",
    "heal_on_kill": "Vampirismo",
    "dodge_chance": "Esquiva",
    "reflect_chance": "Reflexión",
    "death_arrow_chance": "Venganza (flechas muerte)",
    "area_damage": "Daño de área",
    "area_attack_rate": "Frec. área",
    "area_max_targets": "Objetivos área",
    "luck": "Suerte",
    "xp_multiplier": "Mult. XP",
}

COMBAT_STATS = {
    "hp": "HP",
    "nearest_enemy_dist": "Dist. enemigo +cercano",
    "enemies_alive": "Enemigos vivos",
    "projectiles_alive": "Proyectiles vivos",
    "xp_orbs_alive": "Orbes XP vivos",
    "kills_so_far": "Kills acumulados",
    "dmg_taken_delta": "Daño recibido (Δ)",
    "dmg_dealt_delta": "Daño infligido (Δ)",
    "level": "Nivel",
    "xp": "XP",
    "fps": "FPS",
    "speed": "Velocidad actual",
    "aim_angle": "Ángulo de apuntado",
}

BEHAVIOR_STATS = {
    "inputs_delta": "Inputs (Δ)",
    "dir_changes_delta": "Cambios de dirección / jitter (Δ)",
    "distance_moved": "Distancia recorrida (Δ)",
}

CHANNEL_LABELS = {
    "arrow": "Flecha",
    "burn": "Quemadura",
    "aura": "Aura",
    "death_arrows": "Flechas de muerte",
}


# ---------------------------------------------------------------------------
# Helpers de rondas + graficas temporales
# ---------------------------------------------------------------------------
def round_boundaries(s: pd.DataFrame) -> list[tuple[float, int]]:
    """Instantes (game_time_seconds) donde cambia el numero de ronda.

    Devuelve [(t, ronda_nueva), ...] para dibujar separadores verticales.
    """
    if s.empty or "round" not in s.columns:
        return []
    d = s.dropna(subset=["round", "game_time_seconds"]).sort_values("game_time_seconds")
    bounds: list[tuple[float, int]] = []
    prev = None
    for _, row in d.iterrows():
        r = row["round"]
        if prev is not None and r != prev:
            bounds.append((float(row["game_time_seconds"]), int(r)))
        prev = r
    return bounds


def add_rounds(fig: go.Figure, bounds: list[tuple[float, int]], show: bool,
               annotate: bool = True) -> go.Figure:
    """Anade lineas verticales punteadas en cada cambio de ronda."""
    if not show or not bounds:
        return fig
    for t, r in bounds:
        fig.add_vline(
            x=t, line_dash="dash", line_color=charts.PALETTE[4], opacity=0.45,
            annotation_text=f"R{r}" if annotate else None,
            annotation_position="top",
            annotation_font_size=10,
            annotation_font_color=charts.TEXT,
        )
    return fig


def aggregate_series(s: pd.DataFrame, col: str, bin_s: float,
                     agg: str, smooth: int) -> pd.DataFrame:
    """Recorta a (tiempo, col), opcionalmente agrupa en bins y/o suaviza.

    `bin_s` = 0 deja los snapshots crudos (~100 ms). Con bin>0 agrupa por
    ventanas de `bin_s` segundos aplicando `agg`. `smooth` aplica una media
    movil sobre el resultado.
    """
    d = s.dropna(subset=["game_time_seconds", col])[["game_time_seconds", col]].copy()
    if d.empty:
        return d
    d = d.sort_values("game_time_seconds")
    if bin_s and bin_s > 0:
        d["game_time_seconds"] = (d["game_time_seconds"] // bin_s) * bin_s
        d = d.groupby("game_time_seconds")[col].agg(agg).reset_index()
    if smooth and smooth > 1:
        d[col] = d[col].rolling(smooth, min_periods=1, center=True).mean()
    return d


def stat_chart(d: pd.DataFrame, col: str, label: str,
               bounds: list[tuple[float, int]], show_rounds: bool,
               kind: str, height: int = 320) -> go.Figure | None:
    """Grafica temporal (a ancho completo) de una sola stat ya agregada."""
    if d.empty:
        return None
    if kind == "Área":
        fig = px.area(d, x="game_time_seconds", y=col)
        fig.update_traces(line=dict(color=charts.ACCENT, width=2))
    elif kind == "Barras":
        fig = px.bar(d, x="game_time_seconds", y=col)
        fig.update_traces(marker_color=charts.ACCENT)
    else:  # Linea
        fig = px.line(d, x="game_time_seconds", y=col, markers=len(d) <= 60)
        fig.update_traces(line=dict(color=charts.ACCENT, width=2))
    fig.update_layout(xaxis_title="Tiempo de juego (s)", yaxis_title="",
                      margin=dict(l=50, r=24, t=48, b=40))
    add_rounds(fig, bounds, show_rounds)
    return charts.style(fig, label, height=height)


def render_stats_section(s: pd.DataFrame, items: dict[str, str],
                         bounds: list[tuple[float, int]], show_rounds: bool,
                         key_prefix: str) -> None:
    """Una grafica por fila (ancho completo) + controles de agrupacion comunes."""
    present = [(c, l) for c, l in items.items()
               if c in s.columns and s[c].notna().any()]
    if not present:
        st.info("Ninguna de estas stats tiene datos en el rango seleccionado.")
        return

    # Controles que aplican a TODAS las graficas de la seccion.
    c1, c2, c3, c4 = st.columns(4)
    bin_s = c1.slider("Agrupar cada (s) · 0 = crudo", 0.0, 15.0, 1.0, 0.5,
                      key=f"{key_prefix}_bin",
                      help="Promedia los snapshots (~100 ms) en ventanas de N segundos "
                           "para que la línea no se vea tan picada.")
    agg = c2.selectbox("Agregación", ["mean", "max", "min", "median", "sum", "last"],
                       key=f"{key_prefix}_agg",
                       help="Cómo combinar los snapshots dentro de cada ventana.")
    smooth = c3.slider("Suavizado (ventana)", 1, 25, 1, key=f"{key_prefix}_smooth",
                       help="Media móvil sobre el resultado. 1 = sin suavizar.")
    kind = c4.selectbox("Tipo de gráfica", ["Línea", "Área", "Barras"],
                        key=f"{key_prefix}_kind")

    # Una grafica por fila, a ancho completo (evita el render en blanco de columnas).
    for c, l in present:
        d = aggregate_series(s, c, bin_s, agg, smooth)
        fig = stat_chart(d, c, l, bounds, show_rounds, kind)
        if fig is not None:
            st.plotly_chart(fig, use_container_width=True, key=f"{key_prefix}_{c}")
        else:
            st.caption(f"Sin datos para *{l}* en este rango.")


# ---------------------------------------------------------------------------
# Carga + navegacion
# ---------------------------------------------------------------------------
with st.sidebar:
    st.page_link("app.py", label="⬅️ Volver al dashboard global", use_container_width=True)
    st.header("⚙️ Controles")
    if st.button("🔄 Recargar datos", use_container_width=True):
        st.cache_data.clear()
        st.rerun()

st.title("🔬 Análisis de una run individual")
st.caption("Radiografía completa de una sola partida de un solo jugador. "
           "Selecciona la run en la barra lateral.")

data = da.load_all()
runs_all = da.runs_enriched(data)
snaps_all = data["GameSnapshot"]

if runs_all.empty:
    st.info("Todavía no hay runs registradas. Juega una partida y pulsa *Recargar datos*.")
    st.stop()

if snaps_all.empty:
    st.warning("No hay snapshots todavía. Esta página necesita runs grabadas con "
               "la versión que registra snapshots (~10/segundo).")
    st.stop()

# Solo runs que tienen snapshots (sin ellos no hay nada que diseccionar).
runs_with_snaps = runs_all[runs_all["id"].isin(snaps_all["run_id"].unique())].copy()
if runs_with_snaps.empty:
    st.warning("Ninguna run tiene snapshots asociados todavía.")
    st.stop()


# ---------------------------------------------------------------------------
# Selector de jugador + run (barra lateral)
# ---------------------------------------------------------------------------
with st.sidebar:
    st.subheader("Selección de run")

    players = sorted(runs_with_snaps["player_id"].dropna().unique().tolist())
    sel_player = st.selectbox("Jugador", players)

    rp = runs_with_snaps[runs_with_snaps["player_id"] == sel_player].copy()
    rp = rp.sort_values("started_at", ascending=False)

    def _run_label(r: pd.Series) -> str:
        dur = 0 if pd.isna(r["duration_seconds"]) else int(r["duration_seconds"])
        when = ""
        if pd.notna(r.get("started_at")):
            when = pd.Timestamp(r["started_at"]).tz_convert(None).strftime("%d/%m %H:%M") + " · "
        end = r.get("end_reason") or ""
        return f"{when}{dur}s · {end} · {str(r['id'])[:8]}"

    rp["lbl"] = rp.apply(_run_label, axis=1)
    sel_lbl = st.selectbox("Run", rp["lbl"].tolist())
    run_row = rp[rp["lbl"] == sel_lbl].iloc[0]
    rid = run_row["id"]


# Snapshots completos de la run elegida (eje de tiempo en segundos con precision ms).
s_full = snaps_all[snaps_all["run_id"] == rid].sort_values("game_time_seconds").copy()
s_full = s_full.dropna(subset=["game_time_seconds"])

if s_full.empty:
    st.warning("Esta run no tiene snapshots con tiempo válido.")
    st.stop()


# ---------------------------------------------------------------------------
# Filtro GLOBAL de rango de tiempo + toggle de separadores de ronda
# ---------------------------------------------------------------------------
t_min = float(s_full["game_time_seconds"].min())
t_max = float(s_full["game_time_seconds"].max())

with st.sidebar:
    st.subheader("Filtros globales")
    if t_max > t_min:
        t_lo, t_hi = st.slider(
            "Rango de tiempo (s)",
            min_value=round(t_min, 1), max_value=round(t_max, 1),
            value=(round(t_min, 1), round(t_max, 1)), step=0.5,
        )
    else:
        t_lo, t_hi = t_min, t_max
        st.caption(f"Run muy corta ({t_min:.1f}s); sin rango que filtrar.")

    show_rounds = st.checkbox("Líneas separadoras de ronda", value=True,
                              help="Dibuja una línea vertical en cada cambio de ronda "
                                   "sobre todas las gráficas temporales.")

# Aplicar el filtro de tiempo a TODO.
s = s_full[(s_full["game_time_seconds"] >= t_lo) & (s_full["game_time_seconds"] <= t_hi)].copy()
bounds = round_boundaries(s)


def child_in_range(table: str) -> pd.DataFrame:
    """Tabla hija de esta run, recortada al rango de tiempo global."""
    df = data[table]
    if df.empty or "run_id" not in df.columns:
        return df.iloc[0:0]
    d = df[df["run_id"] == rid].copy()
    if "game_time_seconds" in d.columns:
        g = pd.to_numeric(d["game_time_seconds"], errors="coerce")
        d = d[(g >= t_lo) & (g <= t_hi)]
    return d


choices = child_in_range("UpgradeChoice")
dmg = child_in_range("DamageTaken")
builds_df = data["RunBuild"][data["RunBuild"]["run_id"] == rid].copy() if not data["RunBuild"].empty else pd.DataFrame()
ustats = data["UpgradeStats"][data["UpgradeStats"]["run_id"] == rid].copy() if not data["UpgradeStats"].empty else pd.DataFrame()


# ---------------------------------------------------------------------------
# Cabecera: KPIs de la run
# ---------------------------------------------------------------------------
def g(col, default=np.nan):
    v = run_row.get(col, default)
    return v

st.markdown(f"#### {sel_player} · run `{str(rid)[:8]}`")
k1, k2, k3, k4, k5, k6 = st.columns(6)
dur = g("duration_seconds")
k1.metric("Duración", f"{int(dur)} s" if pd.notna(dur) else "—")
k2.metric("Ronda final", f"{int(g('final_round'))}" if pd.notna(g("final_round")) else "—")
k3.metric("Nivel final", f"{int(g('final_level'))}" if pd.notna(g("final_level")) else "—")
k4.metric("Kills", f"{int(g('total_kills'))}" if pd.notna(g("total_kills")) else "—")
k5.metric("Daño recibido", f"{int(g('total_damage_taken'))}" if pd.notna(g("total_damage_taken")) else "—")
k6.metric("XP total", f"{int(g('total_xp'))}" if pd.notna(g("total_xp")) else "—")

end_reason = g("end_reason")
extra = []
if pd.notna(end_reason):
    extra.append(f"**Final:** {end_reason}")
if pd.notna(g("dodges_used")):
    extra.append(f"**Esquivas:** {int(g('dodges_used'))}")
if pd.notna(g("heals_used")):
    extra.append(f"**Curas:** {int(g('heals_used'))}")
if "auto_aim" in s.columns and s["auto_aim"].notna().any():
    pct_auto = s["auto_aim"].fillna(False).mean() * 100
    extra.append(f"**Auto-aim:** {pct_auto:.0f}% del tiempo")
if extra:
    st.caption(" · ".join(extra))

if t_hi < t_max or t_lo > t_min:
    st.info(f"Mostrando solo **{t_lo:.1f}s – {t_hi:.1f}s** de la run "
            f"({len(s)} de {len(s_full)} snapshots). Ajusta el rango en la barra lateral.")

st.divider()

(tab_resumen, tab_mejoras, tab_build, tab_combat, tab_mov, tab_path, tab_dmg) = st.tabs([
    "📋 Resumen", "⬆️ Mejoras", "🎚️ Stats del build", "⚔️ Estado de combate",
    "🏃 Movimiento", "🧭 Trayectoria", "💢 Daño recibido",
])


# ===========================================================================
# RESUMEN — vista compuesta de la run
# ===========================================================================
with tab_resumen:
    st.subheader("Radiografía de la partida")

    # Grafica compuesta: HP + enemigos + dano recibido, con mejoras marcadas.
    fig = go.Figure()
    if "hp" in s.columns:
        fig.add_trace(go.Scatter(x=s["game_time_seconds"], y=s["hp"], name="HP",
                                 fill="tozeroy", line=dict(color=charts.PALETTE[2])))
    if "enemies_alive" in s.columns:
        fig.add_trace(go.Scatter(x=s["game_time_seconds"], y=s["enemies_alive"],
                                 name="Enemigos vivos", line=dict(color=charts.PALETTE[4]),
                                 yaxis="y2"))
    if "dmg_taken_delta" in s.columns:
        fig.add_trace(go.Bar(x=s["game_time_seconds"], y=s["dmg_taken_delta"],
                             name="Daño recibido", marker_color=charts.PALETTE[1],
                             opacity=0.6))
    fig.update_layout(
        template="pastel", height=440, barmode="overlay",
        legend=dict(orientation="h", y=1.13),
        xaxis_title="Tiempo de juego (s)",
        yaxis=dict(title="HP / daño"),
        yaxis2=dict(title="Enemigos", overlaying="y", side="right", showgrid=False),
    )
    add_rounds(fig, bounds, show_rounds)
    # Marcar cuando se eligio cada mejora.
    if not choices.empty and "game_time_seconds" in choices.columns:
        for _, p in choices.dropna(subset=["game_time_seconds"]).iterrows():
            fig.add_vline(x=float(p["game_time_seconds"]), line_dash="dot",
                          line_color=charts.PALETTE[8], opacity=0.5)
    st.plotly_chart(fig, use_container_width=True)
    st.caption("Líneas **punteadas** = mejora elegida. Líneas **discontinuas** = cambio de ronda "
               "(si está activo el toggle).")

    cc1, cc2, cc3 = st.columns(3)
    cc1.metric("Snapshots en rango", len(s))
    if "distance_moved" in s.columns:
        cc2.metric("Distancia recorrida", f"{s['distance_moved'].sum():.0f}")
    if "dir_changes_delta" in s.columns:
        cc3.metric("Jitter total", f"{int(s['dir_changes_delta'].sum())}")

    # Aporte por canal de dano de esta run.
    if not ustats.empty:
        st.markdown("##### Aporte por canal de daño (toda la run)")
        ch = ustats.groupby("damage_source").agg(
            dano=("total_damage", "sum"), kills=("total_kills", "sum")).reset_index()
        ch["canal"] = ch["damage_source"].map(CHANNEL_LABELS).fillna(ch["damage_source"])
        col_a, col_b = st.columns(2)
        figd = px.bar(ch.sort_values("dano"), x="dano", y="canal", orientation="h", color="canal")
        figd.update_layout(showlegend=False, yaxis_title="", xaxis_title="Daño")
        col_a.plotly_chart(charts.style(figd, "Daño por canal", height=320), use_container_width=True)
        figk = px.bar(ch.sort_values("kills"), x="kills", y="canal", orientation="h", color="canal")
        figk.update_layout(showlegend=False, yaxis_title="", xaxis_title="Kills")
        col_b.plotly_chart(charts.style(figk, "Kills por canal", height=320), use_container_width=True)


# ===========================================================================
# MEJORAS — que agarro y cuando
# ===========================================================================
with tab_mejoras:
    st.subheader("Mejoras tomadas en la run")

    if choices.empty:
        st.info("No hay elecciones de mejora registradas para esta run / rango.")
    else:
        ch = choices.sort_values("game_time_seconds").copy()
        # Tabla cronologica.
        show = pd.DataFrame({
            "Tiempo (s)": ch["game_time_seconds"].round(1) if "game_time_seconds" in ch else np.nan,
            "Ronda": ch["round"] if "round" in ch else np.nan,
            "Mejora elegida": ch["selected_option"] if "selected_option" in ch else "",
        })
        if "decision_ms" in ch.columns:
            show["Decisión (s)"] = (pd.to_numeric(ch["decision_ms"], errors="coerce") / 1000).round(2)
        if "rerolls" in ch.columns:
            show["Rerolls"] = pd.to_numeric(ch["rerolls"], errors="coerce").fillna(0).astype(int)
        if {"option_1", "option_2", "option_3"} <= set(ch.columns):
            show["Opciones ofrecidas"] = ch[["option_1", "option_2", "option_3"]].apply(
                lambda r: " / ".join([str(x) for x in r if pd.notna(x)]), axis=1)

        st.markdown("##### Cronología de elecciones")
        st.dataframe(show.reset_index(drop=True), use_container_width=True, hide_index=True)

        # Timeline visual: cuando se agarro cada mejora.
        sel = ch.dropna(subset=["game_time_seconds", "selected_option"])
        if not sel.empty:
            fig = px.scatter(
                sel, x="game_time_seconds", y="selected_option",
                color="selected_option",
                hover_data=[c for c in ["round", "decision_ms", "rerolls"] if c in sel.columns],
            )
            fig.update_traces(marker=dict(size=13, line=dict(width=1, color="white")))
            fig.update_layout(showlegend=False, xaxis_title="Tiempo de juego (s)", yaxis_title="")
            add_rounds(fig, bounds, show_rounds)
            st.plotly_chart(charts.style(fig, "¿Cuándo agarró cada mejora?", height=460),
                            use_container_width=True)

        # Tiempo de decision por eleccion.
        if "decision_ms" in ch.columns and ch["decision_ms"].notna().any():
            dd = ch.dropna(subset=["decision_ms", "game_time_seconds"]).copy()
            dd["seg"] = pd.to_numeric(dd["decision_ms"], errors="coerce") / 1000.0
            figt = px.bar(dd, x="game_time_seconds", y="seg",
                          hover_data=["selected_option"], color="seg",
                          color_continuous_scale=charts.PASTEL_SCALE)
            figt.update_layout(coloraxis_showscale=False, xaxis_title="Tiempo de juego (s)",
                               yaxis_title="Tiempo en decidir (s)")
            add_rounds(figt, bounds, show_rounds)
            st.plotly_chart(charts.style(figt, "Cuánto tardó en decidir cada mejora", height=320),
                            use_container_width=True)

    # Build final (niveles alcanzados por mejora).
    if not builds_df.empty:
        st.markdown("##### Build final (niveles por mejora)")
        bf = builds_df.copy()
        if "level" in bf.columns:
            bf["level"] = pd.to_numeric(bf["level"], errors="coerce")
            bf = bf.sort_values("level")
            figb = px.bar(bf, x="level", y="upgrade_name", orientation="h",
                          color="level", color_continuous_scale=charts.PASTEL_SCALE)
            figb.update_layout(coloraxis_showscale=False, yaxis_title="", xaxis_title="Nivel")
            st.plotly_chart(charts.style(figb, "Niveles del build al terminar", height=460),
                            use_container_width=True)


# ===========================================================================
# STATS DEL BUILD — una grafica por cada stat (estilo menu de pausa)
# ===========================================================================
with tab_build:
    st.subheader("Evolución de cada stat del build")
    st.caption("Una gráfica independiente por stat (como el menú de pausa), a ancho completo. "
               "Los controles de abajo agrupan/suavizan **todas** las gráficas. "
               "Respeta el rango de tiempo y los separadores de ronda globales.")
    render_stats_section(s, BUILD_STATS, bounds, show_rounds, key_prefix="build")


# ===========================================================================
# ESTADO DE COMBATE — hp, enemigos, dano, fps, etc.
# ===========================================================================
with tab_combat:
    st.subheader("Estado de combate momento a momento")
    st.caption("Una gráfica por cada métrica de estado del snapshot, a ancho completo.")
    render_stats_section(s, COMBAT_STATS, bounds, show_rounds, key_prefix="combat")


# ===========================================================================
# MOVIMIENTO — proxies de comportamiento
# ===========================================================================
with tab_mov:
    st.subheader("Comportamiento y movimiento")
    st.caption("Inputs, cambios de dirección (jitter) y distancia recorrida por snapshot. "
               "Para estas métricas acumulativas suele ir bien la agregación **sum** por bin.")
    render_stats_section(s, BEHAVIOR_STATS, bounds, show_rounds, key_prefix="behav")


# ===========================================================================
# TRAYECTORIA — recorrido espacial de la run
# ===========================================================================
with tab_path:
    st.subheader("Recorrido del jugador")
    sp = s.dropna(subset=["player_x", "player_y"])
    if sp.empty:
        st.info("Sin posiciones registradas en este rango.")
    else:
        fig = go.Figure()
        fig.add_trace(go.Scatter(
            x=sp["player_x"], y=sp["player_y"], mode="lines+markers",
            line=dict(color="rgba(168,199,231,0.5)", width=2),
            marker=dict(size=7, color=sp["game_time_seconds"],
                        colorscale=charts.PASTEL_SCALE, showscale=True,
                        colorbar=dict(title="t (s)")),
            name="Recorrido",
        ))
        fig.add_trace(go.Scatter(x=[sp["player_x"].iloc[0]], y=[sp["player_y"].iloc[0]],
                                 mode="markers", marker=dict(size=15, color=charts.PALETTE[2]),
                                 name="Inicio"))
        fig.add_trace(go.Scatter(x=[sp["player_x"].iloc[-1]], y=[sp["player_y"].iloc[-1]],
                                 mode="markers", marker=dict(size=15, color=charts.PALETTE[1]),
                                 name="Fin"))
        fig.update_yaxes(autorange="reversed", scaleanchor="x", scaleratio=1)
        fig.update_layout(template="pastel", height=560, xaxis_title="x", yaxis_title="y")
        st.plotly_chart(fig, use_container_width=True)
        st.caption("Color = avance del tiempo. Verde = inicio del rango, rosa = fin.")

        # Mapa de calor de permanencia en el rango.
        st.markdown("##### Mapa de calor de permanencia")
        nbins = st.slider("Resolución (celdas)", 20, 100, 45, step=5, key="ind_heat_bins")
        figh = px.density_heatmap(sp, x="player_x", y="player_y", nbinsx=nbins, nbinsy=nbins,
                                  color_continuous_scale=charts.PASTEL_SCALE)
        figh.update_yaxes(autorange="reversed", scaleanchor="x", scaleratio=1)
        figh.update_layout(xaxis_title="x", yaxis_title="y")
        st.plotly_chart(charts.style(figh, "¿Dónde se queda más?", height=520),
                        use_container_width=True)


# ===========================================================================
# DANO RECIBIDO — eventos puntuales de esta run
# ===========================================================================
with tab_dmg:
    st.subheader("Daño recibido durante la run")
    if dmg.empty:
        st.info("No hay eventos de daño recibido en este rango.")
    else:
        d = dmg.copy()
        d["game_time_seconds"] = pd.to_numeric(d["game_time_seconds"], errors="coerce")
        d["damage_amount"] = pd.to_numeric(d["damage_amount"], errors="coerce")
        d = d.dropna(subset=["game_time_seconds"])

        color = None
        if "enemy_type" in d.columns and d["enemy_type"].notna().any():
            color = "enemy_type"
        elif "damage_type" in d.columns:
            color = "damage_type"

        fig = px.bar(d, x="game_time_seconds", y="damage_amount", color=color,
                     hover_data=[c for c in ["enemy_type", "damage_type", "round"] if c in d.columns])
        fig.update_layout(xaxis_title="Tiempo de juego (s)", yaxis_title="Daño por golpe")
        add_rounds(fig, bounds, show_rounds)
        st.plotly_chart(charts.style(fig, "Cada golpe recibido", height=420),
                        use_container_width=True)

        col_a, col_b = st.columns(2)
        if color == "enemy_type" or "enemy_type" in d.columns:
            by_enemy = d.groupby("enemy_type")["damage_amount"].sum().reset_index().sort_values("damage_amount")
            fige = px.bar(by_enemy, x="damage_amount", y="enemy_type", orientation="h", color="enemy_type")
            fige.update_layout(showlegend=False, yaxis_title="", xaxis_title="Daño total")
            col_a.plotly_chart(charts.style(fige, "Daño total por enemigo", height=340),
                               use_container_width=True)
        if "damage_type" in d.columns:
            by_type = d.groupby("damage_type")["damage_amount"].sum().reset_index().sort_values("damage_amount")
            figt = px.bar(by_type, x="damage_amount", y="damage_type", orientation="h", color="damage_type")
            figt.update_layout(showlegend=False, yaxis_title="", xaxis_title="Daño total")
            col_b.plotly_chart(charts.style(figt, "Daño total por tipo", height=340),
                               use_container_width=True)

        # HP antes/despues si esta disponible.
        if {"hp_before_hit", "hp_after_hit"} <= set(d.columns):
            d["hp_before_hit"] = pd.to_numeric(d["hp_before_hit"], errors="coerce")
            d["hp_after_hit"] = pd.to_numeric(d["hp_after_hit"], errors="coerce")
            figh = go.Figure()
            figh.add_trace(go.Scatter(x=d["game_time_seconds"], y=d["hp_after_hit"],
                                      mode="lines+markers", name="HP tras el golpe",
                                      line=dict(color=charts.PALETTE[2])))
            add_rounds(figh, bounds, show_rounds)
            figh.update_layout(template="pastel", height=320, xaxis_title="Tiempo (s)",
                               yaxis_title="HP")
            st.plotly_chart(figh, use_container_width=True)
