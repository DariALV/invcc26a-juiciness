class_name SteeringBehavior extends Node

@export var targets: Array[SteeringTarget]

#TODO: Implementar
#func remove_target(target_group: String, radius: float, force_multiplier: float):
	#targets.push_back(SteeringTarget.new(target_group, radius, force_multiplier))

func add_target(target_group: String, radius: float, force_multiplier: float):
	var t := SteeringTarget.new()
	t.target_group = target_group
	t.radius = radius
	t.force_multiplier = force_multiplier
	targets.push_back(t)

@warning_ignore("unused_parameter")
func get_force(t: SteeringTarget, parent: CharacterBody2D, closest_pos: Vector2, max_speed: float) -> Vector2:
	return Vector2.ZERO

func calculate(parent: CharacterBody2D, max_speed: float) -> Vector2:
	var final_force = Vector2.ZERO
	for t in targets:
		var closest_entity =  QuadTreePartitioning.get_closest_entity(parent, t)
		if (closest_entity):
			final_force += get_force(t, parent, closest_entity.global_position, max_speed)
	return final_force
