class_name ReflectionUpgrade extends PlayerUpgrade

## Da al jugador una probabilidad de reflejar proyectiles enemigos: el proyectil se
## vuelve aliado y daña a los enemigos (los following projectiles pasan a perseguir
## enemigos). Apilable.
@export var chance_increase: float = 0.05

func apply_upgrade() -> void:
	var p: Player = LevelManager.player
	p.reflect_chance = minf(1.0, p.reflect_chance + chance_increase)
