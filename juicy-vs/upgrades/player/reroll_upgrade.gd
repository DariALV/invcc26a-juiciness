class_name RerollUpgrade extends PlayerUpgrade

## Otorga rerolls que se RENUEVAN en cada subida de nivel (no un total acumulado).
## Cada mejora de reroll aporta 'rerolls_per_level' rerolls por subida de nivel; con
## la epica + la legendaria se obtienen 2 por nivel. Solo se puede tomar una vez por
## rareza (max_taken = 1).
@export var rerolls_per_level: int = 1

func apply_upgrade() -> void:
	UpgradeManager.add_reroll_source(rerolls_per_level)
