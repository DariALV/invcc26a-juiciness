class_name ChainProjectileUpgrade extends ProjectileUpgrade

## Otorga la habilidad de REBOTE: tras impactar, la flecha se redirige al enemigo no
## golpeado mas cercano dentro de 'chain_range'. Como ahora la perforacion se mide por
## dano, el rebote solo tiene sentido mientras a la flecha le quede dano por infligir; ya
## no agrega ninguna penetracion numerica.
@export var chain_range: float = 220.0

func apply_upgrade(projectile: BaseProjectile):
	projectile.chain_enabled = true
	projectile.chain_range = maxf(projectile.chain_range, chain_range)
