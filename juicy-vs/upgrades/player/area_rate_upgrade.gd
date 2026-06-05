class_name AreaRateUpgrade extends PlayerUpgrade

## Aumenta la frecuencia de ataque del aura del jugador. El ritmo base es 1 ataque
## por segundo (multiplicador 1.0); cada mejora suma a ese multiplicador
## (+0.25 = +25% de frecuencia).
@export var rate_increase: float = 0.25

func apply_upgrade() -> void:
	LevelManager.player.area_attack_rate += rate_increase

## Solo tiene sentido si ya existe daño en área (aura activa). Se bloquea hasta entonces.
func is_unlocked() -> bool:
	return LevelManager.player != null and LevelManager.player.area_damage > 0.0
