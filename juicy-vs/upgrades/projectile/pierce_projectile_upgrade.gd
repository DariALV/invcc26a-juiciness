class_name PierceProjectileUpgrade extends ProjectileUpgrade

@export var extra_pierce: int = 1

func apply_upgrade(projectile: BaseProjectile):
	projectile.pierce += extra_pierce
