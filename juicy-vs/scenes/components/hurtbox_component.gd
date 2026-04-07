extends Area2D
class_name HurtboxComponent

@export var health : HealthComponent



func _on_area_entered(area : Area2D):
	if area is HitboxComponent and health:
		health.apply_damage(area.damage)
