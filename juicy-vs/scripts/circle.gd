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

func random_direction() -> Vector2:
	return Vector2.RIGHT.rotated(randf() * TAU)

func random_point_in_edge() -> Vector2:
	return point_in_edge(random_direction())

func random_point_inside() -> Vector2:
	# sqrt(randf()) para una distribucion uniforme en area (sin acumular en el centro)
	return center + random_direction() * radius * sqrt(randf())

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

func spread_points_in_edge(count: int, spread_angle: float, angle_offset: float = 0) -> Array[Vector2]:
	# Reparte 'count' direcciones equitativamente sobre 'spread_angle' (en grados),
	# centradas en angle_offset (radianes). Siempre coloca un vector en cada extremo:
	# p.ej. spread 60 y count 5 => angulos -30, -15, 0, 15, 30 (+ offset).
	var points: Array[Vector2] = []
	if count <= 0:
		return points
	if count == 1:
		points.push_back(point_in_edge(Vector2.RIGHT.rotated(angle_offset)))
		return points
	var spread = deg_to_rad(spread_angle)
	var step = spread / (count - 1)
	var start = -spread / 2.0
	for i in count:
		var direction = Vector2.RIGHT.rotated(start + i * step + angle_offset)
		points.push_back(point_in_edge(direction))
	return points
