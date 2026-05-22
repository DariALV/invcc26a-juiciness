extends Node


var trees_by_group = {}

#TODO: Implementar mas adelante la particion

func insert_entity(entity: CharacterBody2D):
	pass

func get_closest_entity(parent: Node2D, target: SteeringTarget) -> Node2D:
	var closest_entity: Node2D = null
	var closest_squared_distance = target.radius * target.radius
	var group = get_tree().get_nodes_in_group(target.target_group) as Array[Node2D]
	for e in group:
		if e != parent:
			var squared_distance: float = parent.global_position.distance_squared_to(e.position)
			if squared_distance < closest_squared_distance:
				closest_entity = e
				closest_squared_distance = squared_distance
	return closest_entity
