class_name MagnetUpgrade extends PlayerUpgrade

## Amplia el radio de recogida/area del jugador de forma ADITIVA: cada mejora suma
## 'range_increase' (fraccion del radio base) al bono total. Tomar varias mejoras se
## SUMA, no se multiplica: +0.6 +0.6 +0.6 = +180% (x2.8), no x1.6 x1.6 x1.6.
@export var range_increase: float = 0.6

func apply_upgrade() -> void:
	LevelManager.player.add_pickup_range(range_increase)
