class_name AreaTargetsUpgrade extends PlayerUpgrade

## Aumenta cuantas entidades simultaneas puede dañar el aura por ataque (las mas
## cercanas dentro del area). La base es 5; cada mejora suma 'targets_increase'.
@export var targets_increase: int = 3

func apply_upgrade() -> void:
	LevelManager.player.area_max_targets += targets_increase

## Solo tiene sentido si ya existe daño en área (aura activa). Se bloquea hasta entonces.
func is_unlocked() -> bool:
	return LevelManager.player != null and LevelManager.player.area_damage > 0.0
