class_name SeekBehavior extends SteeringBehavior

func get_force(t: SteeringTarget, parent: CharacterBody2D, closest_pos: Vector2, max_speed: float) -> Vector2:
	var desired_velocity = parent.global_position.direction_to(closest_pos) * max_speed
	return (desired_velocity - parent.velocity) * t.force_multiplier
