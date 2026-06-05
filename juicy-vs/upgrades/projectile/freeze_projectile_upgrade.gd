class_name FreezeProjectileUpgrade extends ProjectileUpgrade

## Hace que las flechas congelen a los enemigos al impactar: los ralentiza durante
## 'duration' segundos al 'slow_factor' de su velocidad y los tinta de celeste.
@export var duration: float = 2.0
@export var slow_factor: float = 0.4

func apply_upgrade(projectile: BaseProjectile):
	projectile.freeze_enabled = true
	projectile.freeze_duration = duration
	projectile.freeze_slow_factor = slow_factor
