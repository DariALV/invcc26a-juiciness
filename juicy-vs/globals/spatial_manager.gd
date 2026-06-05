extends Node

@export var cell_size: float = 32

var _grids := {}
var _frame_cache := {}

func _cell_key(pos: Vector2) -> Vector2i:
	return Vector2i(floori(pos.x / cell_size), floori(pos.y / cell_size))

func _ensure_grid(group: String) -> Dictionary:
	var frame := Engine.get_physics_frames()
	if _frame_cache.get(group, -1) == frame:
		return _grids[group]
	var grid := {}
	for e in get_tree().get_nodes_in_group(group):
		var key := _cell_key(e.global_position)
		if not grid.has(key):
			grid[key] = []
		grid[key].append(e)
	_grids[group] = grid
	_frame_cache[group] = frame
	return grid

func get_closest_entity(parent: Node2D, target: SteeringTarget) -> Node2D:
	var grid := _ensure_grid(target.target_group)
	var origin := parent.global_position
	var center := _cell_key(origin)
	var cx := center.x
	var cy := center.y
	var closest_entity: Node2D = null
	var closest_squared_distance := target.radius * target.radius
	var max_rings := 128
	var k := 0
	while k <= max_rings:
		if k >= 1:
			var ring_min := float(k - 1) * cell_size
			if ring_min * ring_min >= closest_squared_distance:
				break
		var dx := -k
		while dx <= k:
			var dy := -k
			while dy <= k:
				if maxi(absi(dx), absi(dy)) == k:
					var bucket = grid.get(Vector2i(cx + dx, cy + dy))
					if bucket != null:
						for e in bucket:
							if e != parent:
								var squared_distance: float = origin.distance_squared_to(e.global_position)
								if squared_distance < closest_squared_distance:
									closest_entity = e
									closest_squared_distance = squared_distance
				dy += 1
			dx += 1
		k += 1
	return closest_entity
