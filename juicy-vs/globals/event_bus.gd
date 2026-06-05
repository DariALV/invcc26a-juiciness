extends Node

signal apply_camera_shake(intensity: float)

signal player_health_changed(before: float, after: float)
signal add_player_health(amount: float)

## Emitida cuando la partida termina (victoria o derrota). La escucha la seleccion de
## mejoras para cerrarse/ignorar subidas de nivel y no tapar la pantalla de fin.
signal game_finished
