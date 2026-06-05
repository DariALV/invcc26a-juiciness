class_name DodgeUpgrade extends PlayerUpgrade

## Da al jugador una probabilidad de esquivar (anular) un golpe, mostrando
## "Esquivado". Apilable.
@export var chance_increase: float = 0.04

func apply_upgrade() -> void:
	var p: Player = LevelManager.player
	p.dodge_chance = minf(1.0, p.dodge_chance + chance_increase)
