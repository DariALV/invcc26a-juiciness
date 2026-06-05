class_name RegenUpgrade extends PlayerUpgrade

## Aumenta la regeneracion de vida del jugador en 'regen_increase' puntos por
## segundo. Apilable.
@export var regen_increase: float = 0.5

func apply_upgrade() -> void:
	var hc: HealthComponent = LevelManager.player.health
	hc.regen_per_second += regen_increase
