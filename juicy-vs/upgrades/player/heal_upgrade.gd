class_name HealUpgrade extends PlayerUpgrade

## Cura al jugador al instante. Si 'heal_full' es true restaura toda la vida;
## si no, cura 'heal_amount' puntos.
@export var heal_amount: float = 5.0
@export var heal_full: bool = false

func apply_upgrade() -> void:
	var hc: HealthComponent = LevelManager.player.health
	if heal_full:
		hc.apply_heal(hc.max_health)
	else:
		hc.apply_heal(heal_amount)
