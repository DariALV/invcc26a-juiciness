class_name LifeOnKillUpgrade extends PlayerUpgrade

## Cura al jugador 'heal_increase' de vida por cada enemigo derrotado. Apilable.
@export var heal_increase: float = 0.01

func apply_upgrade() -> void:
	LevelManager.player.heal_on_kill += heal_increase
