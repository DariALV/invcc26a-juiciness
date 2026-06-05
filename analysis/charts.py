"""Helpers de visualizacion con Plotly y una paleta pastel coherente.

Centraliza el tema para que todas las graficas se vean igual de bonitas.
Importa PALETTE y usa `style(fig)` sobre cualquier figura de Plotly.
"""

from __future__ import annotations

import plotly.graph_objects as go
import plotly.io as pio

# Paleta pastel (suficientes colores para series categoricas).
PALETTE = [
    "#A8C7E7",  # azul
    "#F7B7C2",  # rosa
    "#B5E0C6",  # verde menta
    "#F9D9A0",  # durazno
    "#C9B6E4",  # lila
    "#A0DDE6",  # turquesa
    "#F6C6A8",  # melocoton
    "#D7E3A0",  # verde lima suave
    "#E7B7DA",  # malva
    "#B7C4E0",  # azul grisaceo
]

# Color de acento (mismo primaryColor del tema).
ACCENT = "#A8C7E7"
GRID = "#E8E4F0"
TEXT = "#3A3A4A"

# Plantilla base de Plotly con look pastel.
_pastel_template = go.layout.Template()
_pastel_template.layout = go.Layout(
    colorway=PALETTE,
    font=dict(family="sans-serif", color=TEXT, size=13),
    paper_bgcolor="rgba(0,0,0,0)",
    plot_bgcolor="rgba(0,0,0,0)",
    title=dict(font=dict(size=18, color=TEXT)),
    xaxis=dict(gridcolor=GRID, zerolinecolor=GRID, linecolor=GRID),
    yaxis=dict(gridcolor=GRID, zerolinecolor=GRID, linecolor=GRID),
    legend=dict(bgcolor="rgba(255,255,255,0.6)", bordercolor=GRID, borderwidth=1),
    margin=dict(l=50, r=30, t=60, b=50),
)
pio.templates["pastel"] = _pastel_template

# Escala de color continua pastel (para heatmaps).
PASTEL_SCALE = [
    [0.0, "#F0ECF7"],
    [0.5, "#A8C7E7"],
    [1.0, "#6E8FB8"],
]

# Escala divergente pastel (para correlaciones -1..1).
PASTEL_DIVERGING = [
    [0.0, "#F7B7C2"],   # rosa (negativo)
    [0.5, "#FBFAFD"],   # neutro
    [1.0, "#A8C7E7"],   # azul (positivo)
]


def style(fig: go.Figure, title: str | None = None, height: int = 420) -> go.Figure:
    """Aplica la plantilla pastel y ajustes comunes a una figura."""
    fig.update_layout(template="pastel", height=height)
    if title is not None:
        fig.update_layout(title=title)
    return fig
