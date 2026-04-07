extends Area2D
class_name HurtboxComponent

@export var health : HealthComponent



func _on_area_entered(area : Area2D):
	# todo: Chequear que el area sea un HitboxComponent
	if health:
		# todo: Aplicar el daño del HitboxComponent 
		health.apply_damage(10)
