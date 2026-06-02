class_name MaxHealthUpgrade extends PlayerUpgrade

## Sube la vida maxima del jugador y lo cura por la cantidad ganada. 'multiplier'
## escala la vida maxima actual y 'flat_bonus' suma una cantidad fija.
@export var flat_bonus: float = 0.0
@export var multiplier: float = 1.0

func apply_upgrade() -> void:
	var hc: HealthComponent = LevelManager.player.health
	var bonus: float = hc.max_health * (multiplier - 1.0) + flat_bonus
	hc.max_health += bonus
	hc.apply_heal(bonus)
