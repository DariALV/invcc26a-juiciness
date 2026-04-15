extends Area2D
class_name HurtboxComponent

@export var knockback_on_damage : bool = false
@export var knockback_all : bool = true
@export_enum("player", "enemy") var knockback_group: String = "enemy"
@export var health : HealthComponent

func _on_area_entered(area : Area2D):
	if area is HitboxComponent and health and not health.isDead:
		health.apply_damage(area.damage)
		if knockback_on_damage:
			if knockback_all:
				var entities = get_tree().get_nodes_in_group(knockback_group)
				var parent : Node2D = get_parent() as Node2D
				var point : Vector2 = parent.global_position
				for entity in entities:
					if entity.global_position.distance_to(point) < entity.knockback.max_distance:
						entity.knockback.apply_knockback(point)
				
			
			
		print("Daño realizado al jugador. Vida actual: ", health.current_health, ". Daño recibido: ", area.damage)
