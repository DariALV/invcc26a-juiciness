class_name Necromancer extends BaseEnemy

@onready var shoot_component = $ShootComponent

func _process(delta):
	var player = LevelManager.player
	var direction = global_position.direction_to(player.global_position)
	if (direction.x > 0):
		animated_sprite.flip_h = false
	elif (direction.x < 0):
		animated_sprite.flip_h = true
	if (movement.current_speed == Vector2.ZERO):
		animated_sprite.play("idle")
	else:
		animated_sprite.play("running")
	shoot_component.shoot_direction = direction
