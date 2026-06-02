extends Area2D
class_name HurtboxComponent

signal collision(area: Area2D)

@export var health : HealthComponent
@export var one_hit_per_area: bool = true

var ignore_collision: Dictionary = {}

func _on_area_entered(area : Area2D):
	if one_hit_per_area:
		var id := area.get_instance_id()
		if ignore_collision.has(id):
			return
		ignore_collision[id] = true
	if area is HitboxComponent and health and not health.isDead:
		collision.emit(area)
		health.apply_damage(area.damage)
		# Avisa al hitbox que el golpe impacto, para que el proyectil se consuma
		# solo cuando de verdad hizo daño (no si la entidad ya estaba muerta).
		area.register_hit(self)
