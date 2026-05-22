class_name FleeBehavior extends SteeringBehavior

func get_force(t: SteeringTarget, parent: CharacterBody2D, closest_pos: Vector2, max_speed: float) -> Vector2:
	var direction = closest_pos.direction_to(parent.global_position)
	if direction == Vector2.ZERO:
		direction = Vector2(randf(), randf())
	var desired_velocity = direction * max_speed
	return (desired_velocity - parent.velocity) * t.force_multiplier
