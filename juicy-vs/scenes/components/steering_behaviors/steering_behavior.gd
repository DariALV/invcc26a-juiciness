class_name SteeringBehavior extends Node
 
const PLAYER_GROUP := "player"
 
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
		var closest_entity: Node2D
		if t.target_group == PLAYER_GROUP:
			closest_entity = _get_player(parent, t)
		else:
			closest_entity = SpatialManager.get_closest_entity(parent, t)
		if closest_entity:
			final_force += get_force(t, parent, closest_entity.global_position, max_speed)
	return final_force
 
func _get_player(parent: CharacterBody2D, t: SteeringTarget) -> Node2D:
	var player := LevelManager.player as Node2D
	if player == null or player == parent:
		return null
	if parent.global_position.distance_squared_to(player.global_position) <= t.radius * t.radius:
		return player
	return null
 
