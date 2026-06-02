class_name DamageProjectileUpgrade extends ProjectileUpgrade

## Aumenta el dano de cada flecha. 'multiplier' se aplica primero y luego se
## suma 'flat_bonus', de modo que el mismo script sirve para mejoras aditivas
## (+1 dano) o multiplicativas (x1.5 dano) segun como se configure el recurso.
@export var flat_bonus: float = 0.0
@export var multiplier: float = 1.0

func apply_upgrade(projectile: BaseProjectile):
	projectile.damage = projectile.damage * multiplier + flat_bonus
