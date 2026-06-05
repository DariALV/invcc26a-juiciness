class_name AreaDamageUpgrade extends PlayerUpgrade

## Aumenta el daño del aura del jugador: cualquier entidad dentro del area de
## recogida sufre este daño cada cierto intervalo. El daño base es 0 (sin aura) y
## cada mejora lo incrementa de forma aditiva.
@export var damage_increase: float = 0.1

func apply_upgrade() -> void:
	LevelManager.player.area_damage += damage_increase
