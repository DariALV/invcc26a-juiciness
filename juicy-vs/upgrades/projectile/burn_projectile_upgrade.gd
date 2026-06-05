class_name BurnProjectileUpgrade extends ProjectileUpgrade

## Hace que las flechas prendan fuego a los enemigos al impactar: les aplican
## 'dps' de dano por segundo durante 'duration' segundos (en ticks de 'tick'
## segundos) y los tinta de anaranjado.
@export var duration: float = 3.0
@export var dps: float = 1.0
@export var tick: float = 0.5

func apply_upgrade(projectile: BaseProjectile):
	projectile.burn_enabled = true
	projectile.burn_duration = duration
	projectile.burn_dps = dps
	projectile.burn_tick = tick
