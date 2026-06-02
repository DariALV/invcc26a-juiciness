class_name XpGainUpgrade extends PlayerUpgrade

## Incrementa el multiplicador global de experiencia del ExperienceManager,
## de modo que cada enemigo derrotado otorga mas XP.
@export var multiplier_bonus: float = 0.2

func apply_upgrade() -> void:
	ExperienceManager.xp_multiplier += multiplier_bonus
