extends Area2D
class_name HitboxComponent

signal collision_detected()
## Emitida con el HurtboxComponent realmente golpeado (tras la deduplicacion), para
## que el proyectil pueda aplicar efectos de estado a esa entidad concreta.
signal hit_landed(hurtbox: Area2D)

@export var damage : float = 1
@export var one_hit_per_area: bool = true
## Canal de dano para la telemetria (UpgradeStats). Lo fijan los proyectiles del
## jugador ("arrow", "death_arrows"). Vacio = no se atribuye (golpes a enemigos sin
## fuente, o golpes que recibe el jugador).
@export var damage_source: String = ""

var ignore_collision: Dictionary = {}

## Vida realmente removida al objetivo en el ultimo golpe. La fija el HurtboxComponent y
## la lee el proyectil para descontar su presupuesto de dano (perforacion por dano).
var last_damage_dealt: float = 0.0

# Lo invoca el HurtboxComponent cuando procesa el golpe (entidad viva, daño
# aplicado). Asi el proyectil solo se "consume" si de verdad impacto algo: si la
# entidad ya murio por otra flecha del mismo frame, esta no se gasta y sigue.
func register_hit(hurtbox: Area2D, dealt: float = 0.0) -> void:
	if one_hit_per_area:
		var id := hurtbox.get_instance_id()
		if ignore_collision.has(id):
			return
		ignore_collision[id] = true
	last_damage_dealt = dealt
	collision_detected.emit()
	hit_landed.emit(hurtbox)
