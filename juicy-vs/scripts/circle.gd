class_name Circle extends Resource

@export var center: Vector2
@export var radius: float = 0

func _init(c: Vector2 = Vector2.ZERO, r: float = 1) -> void:
	center = c
	radius = r

func is_point_inside(point: Vector2) -> bool:
	return Geometry2D.is_point_in_circle(point, center, radius)

func point_in_edge(direction: Vector2) -> Vector2:
	return center + direction * radius

func random_point_in_edge() -> Vector2:
	return point_in_edge(Vector2(randf(), randf()).normalized())

func random_points_in_edge(count: int) -> Array[Vector2]:
	var points: Array[Vector2] = []
	for i in count:
		points.push_back(random_point_in_edge())
	return points

func spaced_points_in_edge(count: int, angle_offset: float = 0) -> Array[Vector2]:
	var points: Array[Vector2] = []
	for i in count:
		var angle = i * (360.0/count)
		var direction = Vector2.RIGHT.rotated(deg_to_rad(angle) + angle_offset)
		points.push_back(point_in_edge(direction))
	return points
	
