class_name MoveSpeedUpgrade extends PlayerUpgrade

## Aumenta la velocidad de movimiento del jugador (max_speed del
## MovementComponent). Soporta bono fijo y multiplicador.
@export var flat_bonus: float = 0.0
@export var multiplier: float = 1.0

func apply_upgrade() -> void:
	var mv: MovementComponent = LevelManager.player.movement
	mv.max_speed = mv.max_speed * multiplier + flat_bonus
