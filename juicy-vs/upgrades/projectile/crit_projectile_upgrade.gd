class_name CritProjectileUpgrade extends ProjectileUpgrade

## Da a las flechas una probabilidad de golpe critico (dano multiplicado). Apilable:
## cada copia suma 'chance_increase' a la probabilidad y eleva el multiplicador al
## mayor de los configurados.
@export var chance_increase: float = 0.1
@export var multiplier: float = 2.0

func apply_upgrade(projectile: BaseProjectile):
	projectile.crit_chance = minf(1.0, projectile.crit_chance + chance_increase)
	projectile.crit_multiplier = maxf(projectile.crit_multiplier, multiplier)
