"""Capa de acceso a datos: lee las tablas de telemetria desde Supabase.

Usa la libreria oficial `supabase-py`. Cada tabla se descarga completa (con
paginacion, porque PostgREST devuelve como maximo 1000 filas por peticion) y se
cachea en memoria con `st.cache_data` para no re-consultar en cada interaccion.
Pulsa "Recargar datos" en la barra lateral para limpiar la cache.
"""

from __future__ import annotations

import pandas as pd
import streamlit as st
from supabase import Client, create_client

# Tamano de pagina de PostgREST (limite duro del servidor).
_PAGE_SIZE = 1000

# Tablas de telemetria que nos interesan.
TABLES = [
    "Run", "UpgradeChoice", "DamageTaken", "RunBuild",
    "UpgradeStats", "PlayerIDConfig", "GameSnapshot", "RunFeedback",
]

# Dimensiones Likert (1-7) del cuestionario post-run.
FEEDBACK_LIKERT = ["difficulty", "fun", "chaos", "monotony", "boredom", "stress", "style_liking"]
FEEDBACK_LABELS = {
    "difficulty": "Dificultad",
    "fun": "Diversion",
    "chaos": "Caos",
    "monotony": "Monotonia",
    "boredom": "Aburrimiento",
    "stress": "Estres",
    "style_liking": "Gusto por el estilo",
}


@st.cache_resource(show_spinner=False)
def get_client() -> Client:
    """Crea (una sola vez) el cliente de Supabase a partir de los secrets."""
    url = st.secrets["SUPABASE_URL"]
    key = st.secrets["SUPABASE_KEY"]
    return create_client(url, key)


def _fetch_all(table: str) -> pd.DataFrame:
    """Descarga una tabla completa paginando de 1000 en 1000."""
    client = get_client()
    rows: list[dict] = []
    start = 0
    while True:
        resp = (
            client.table(table)
            .select("*")
            .range(start, start + _PAGE_SIZE - 1)
            .execute()
        )
        batch = resp.data or []
        rows.extend(batch)
        if len(batch) < _PAGE_SIZE:
            break
        start += _PAGE_SIZE
    return pd.DataFrame(rows)


@st.cache_data(ttl=600, show_spinner="Cargando datos de Supabase...")
def load_all() -> dict[str, pd.DataFrame]:
    """Carga todas las tablas y devuelve un diccionario {nombre: DataFrame}.

    Tambien normaliza tipos: fechas a datetime y columnas numericas a numero.
    """
    data = {name: _fetch_all(name) for name in TABLES}

    # --- Normalizacion de tipos ---
    run = data["Run"]
    if not run.empty:
        for col in ("started_at", "ended_at"):
            if col in run.columns:
                run[col] = pd.to_datetime(run[col], errors="coerce", utc=True)
        for col in (
            "duration_seconds", "final_round", "final_level",
            "total_kills", "total_damage_taken", "total_xp",
        ):
            if col in run.columns:
                run[col] = pd.to_numeric(run[col], errors="coerce")

    dmg = data["DamageTaken"]
    if not dmg.empty:
        for col in ("game_time_seconds", "round", "damage_amount", "hp_before_hit", "hp_after_hit"):
            if col in dmg.columns:
                dmg[col] = pd.to_numeric(dmg[col], errors="coerce")

    uc = data["UpgradeChoice"]
    if not uc.empty:
        for col in ("game_time_seconds", "round"):
            if col in uc.columns:
                uc[col] = pd.to_numeric(uc[col], errors="coerce")

    rb = data["RunBuild"]
    if not rb.empty and "level" in rb.columns:
        rb["level"] = pd.to_numeric(rb["level"], errors="coerce")

    us = data["UpgradeStats"]
    if not us.empty:
        for col in ("total_damage", "total_kills"):
            if col in us.columns:
                us[col] = pd.to_numeric(us[col], errors="coerce")

    snap = data["GameSnapshot"]
    if not snap.empty:
        snap_num = [
            "game_time_seconds", "game_time_ms", "player_x", "player_y", "hp", "max_hp",
            "enemies_alive", "nearest_enemy_dist", "dmg_taken_delta", "dmg_dealt_delta",
            "level", "xp", "inputs_delta", "dir_changes_delta", "distance_moved",
            "aim_shots", "aim_hits",
            # Columnas densas (FPS, estado, stats del build).
            "fps", "speed", "aim_angle", "projectiles_alive", "xp_orbs_alive",
            "round", "kills_so_far", "damage", "pierce", "regen", "move_speed",
            "arrows", "fire_rate", "arrow_speed", "heal_on_kill", "dodge_chance",
            "reflect_chance", "death_arrow_chance", "area_damage", "area_attack_rate",
            "area_max_targets", "luck", "xp_multiplier",
        ]
        for col in snap_num:
            if col in snap.columns:
                snap[col] = pd.to_numeric(snap[col], errors="coerce")
        # Usa la precision de ms como eje de tiempo (segundos float) en todos los
        # graficos basados en snapshots, sin tocar el resto del codigo.
        if "game_time_ms" in snap.columns and snap["game_time_ms"].notna().any():
            snap["game_time_seconds"] = snap["game_time_ms"] / 1000.0

    fb = data["RunFeedback"]
    if not fb.empty:
        for col in FEEDBACK_LIKERT:
            if col in fb.columns:
                fb[col] = pd.to_numeric(fb[col], errors="coerce")
        if "created_at" in fb.columns:
            fb["created_at"] = pd.to_datetime(fb["created_at"], errors="coerce", utc=True)

    return data


def runs_enriched(data: dict[str, pd.DataFrame]) -> pd.DataFrame:
    """Une Run con la config del jugador (PlayerIDConfig) por player_id."""
    run = data["Run"].copy()
    cfg = data["PlayerIDConfig"]
    if run.empty:
        return run
    if not cfg.empty:
        run = run.merge(
            cfg.rename(columns={"id": "player_id"}),
            on="player_id",
            how="left",
        )
    return run


def build_run_features(data: dict[str, pd.DataFrame]) -> pd.DataFrame:
    """Tabla de una fila por run con metricas agregadas (para correlaciones).

    Combina los totales de Run con el conteo de mejoras y el dano por canal.
    """
    run = runs_enriched(data)
    if run.empty:
        return run

    feats = run.copy()

    # Numero de mejoras distintas y niveles totales tomados.
    rb = data["RunBuild"]
    if not rb.empty:
        agg = rb.groupby("run_id").agg(
            n_upgrades=("upgrade_name", "nunique"),
            total_levels=("level", "sum"),
        )
        feats = feats.merge(agg, left_on="id", right_index=True, how="left")

    # Dano por canal, pivoteado a columnas (dmg_arrow, dmg_burn, ...).
    us = data["UpgradeStats"]
    if not us.empty:
        pivot = us.pivot_table(
            index="run_id", columns="damage_source",
            values="total_damage", aggfunc="sum",
        )
        pivot.columns = [f"dmg_{c}" for c in pivot.columns]
        feats = feats.merge(pivot, left_on="id", right_index=True, how="left")

    # Agregados por run desde los snapshots (proxies + rendimiento + poder).
    snap = data["GameSnapshot"]
    if not snap.empty:
        specs = {
            "jitter_medio": ("dir_changes_delta", "mean"),
            "inputs_medio": ("inputs_delta", "mean"),
            "dist_recorrida": ("distance_moved", "sum"),
            "presion_media": ("nearest_enemy_dist", "mean"),
            "fps_medio": ("fps", "mean"),
            "fps_min": ("fps", "min"),
            "enemigos_pico": ("enemies_alive", "max"),
            "enemigos_medio": ("enemies_alive", "mean"),
            "proyectiles_pico": ("projectiles_alive", "max"),
            "velocidad_media": ("speed", "mean"),
            "damage_final": ("damage", "max"),
        }
        specs = {k: v for k, v in specs.items() if v[0] in snap.columns}
        if specs:
            sa = snap.groupby("run_id").agg(**specs)
            feats = feats.merge(sa, left_on="id", right_index=True, how="left")

    return feats
