extends Area2D
class_name HitboxComponent

signal collision_detected()

@export var damage : float = 1
@export var one_hit_per_area: bool = true

var ignore_collision: Dictionary = {}

# Lo invoca el HurtboxComponent cuando procesa el golpe (entidad viva, daño
# aplicado). Asi el proyectil solo se "consume" si de verdad impacto algo: si la
# entidad ya murio por otra flecha del mismo frame, esta no se gasta y sigue.
func register_hit(hurtbox: Area2D) -> void:
	if one_hit_per_area:
		var id := hurtbox.get_instance_id()
		if ignore_collision.has(id):
			return
		ignore_collision[id] = true
	collision_detected.emit()
