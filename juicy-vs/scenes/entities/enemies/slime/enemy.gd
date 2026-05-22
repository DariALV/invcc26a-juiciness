extends CharacterBody2D
class_name Enemy

@onready var animated_sprite : AnimatedSprite2D = $AnimatedSprite2D
@onready var stats : StatsComponent = $StatsComponent

func _process(delta):
	var player = get_tree().get_first_node_in_group("player") as Player
	var direction = global_position.direction_to(player.global_position)
	if (direction.x > 0):
		animated_sprite.flip_h = false
	elif (direction.x < 0):
		animated_sprite.flip_h = true
	
	if (stats.current_speed == Vector2.ZERO):
		animated_sprite.play("idle")
	else:
		animated_sprite.play("running")
		
