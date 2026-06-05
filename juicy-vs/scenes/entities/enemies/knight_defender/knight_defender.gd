class_name KnightDefender extends BaseEnemy

## Caballero DEFENSOR: orbita/protege al Rey y solo persigue al jugador si está cerca.
##
## Steering (se arma por código para no depender de sub-recursos en la escena):
##   - seek "king":   radio GRANDE, force BAJA  -> tiende a quedarse cerca del Rey.
##   - seek "player":  radio CHICO,  force ALTA  -> si el jugador entra al radio,
##                     lo persigue con fuerza (lo "intercepta").
##   - flee "enemy":   separación para no apilarse con otros defensores.
##
## El Rey debe estar en el grupo "king" (lo hace king.gd) para que el seek lo encuentre
## vía SpatialManager.

## Radio dentro del cual el defensor sigue al Rey (lo mantiene cerca).
@export var king_radius: float = 420.0
## Fuerza con la que sigue al Rey (baja = orbita suelto).
@export var king_force: float = 1.0
## Radio dentro del cual reacciona al jugador (chico = solo si está cerca).
@export var player_radius: float = 130.0
## Fuerza con la que intercepta al jugador (alta = lo persigue agresivo).
@export var player_force: float = 5.0

func _ready() -> void:
	super()
	add_to_group("knight_defender")  # para el escudo del Rey (invulnerable si hay defensores)
	_setup_steering()

func _setup_steering() -> void:
	var fc := _force_component()
	if fc == null:
		return
	fc.rebuild_behaviors([
		{
			"type": "seek",
			"targets": [
				{"group": "king", "radius": king_radius, "force": king_force},
				{"group": "player", "radius": player_radius, "force": player_force},
			],
		},
		{
			"type": "flee",
			"targets": [
				{"group": "enemy", "radius": 26.0, "force": 2.0},
			],
		},
	])

func _force_component() -> ForceComponent:
	for c in get_children():
		if c is ForceComponent:
			return c
	return null
