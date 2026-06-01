class_name PierceProjectileUpgrade extends ProjectileUpgrade

@export var extra_pierce: int = 1

func apply_upgrade(arrow: Arrow):
	arrow.pierce += 1
