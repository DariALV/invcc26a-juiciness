class_name DeathArrowsUpgrade extends PlayerUpgrade

## VENGANZA. Mejora híbrida (ofensiva de doble propósito), rebalanceada con datos: era el
## arma más débil y la menos elegida en su tier bajo. Ahora, además de la probabilidad de
## que un enemigo al morir lance flechas en círculo, otorga un % de daño base del jugador.
## Como las flechas de muerte escalan con el daño del jugador, este bono también las
## potencia: la mejora vale el pick aunque la proc no salte, y se refuerza a sí misma.
@export var chance_increase: float = 0.01
## % de daño base que suma al jugador (0.05 = +5%). Multiplicativo sobre el daño de flecha.
@export var damage_bonus: float = 0.05

func apply_upgrade() -> void:
	LevelManager.player.death_arrow_chance += chance_increase
	UpgradeManager.bonus_damage_mult *= (1.0 + damage_bonus)
