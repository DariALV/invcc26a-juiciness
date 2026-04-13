extends Area2D
class_name HurtboxComponent

@export var health : HealthComponent

func _on_area_entered(area : Area2D):
	if area is HitboxComponent and health and not health.isDead:
		health.apply_damage(area.damage)
		print("Daño realizado al jugador. Vida actual: ", health.current_health, ". Daño recibido: ", area.damage)
