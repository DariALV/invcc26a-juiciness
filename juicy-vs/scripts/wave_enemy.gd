class_name WaveEnemy extends Resource

@export var amount: int = 0
@export var health_multiplier: float = 0
@export var scene: PackedScene

var spawned_count: int = 0

func can_spawn():
	return spawned_count < amount

func spawn(parent_node: Node2D, spawn_distance: float):
	if can_spawn():
		spawned_count += 1
		var enemy = scene.instantiate() as CharacterBody2D
		enemy.global_position = get_spawn_position(spawn_distance)
		parent_node.add_child(enemy)
		GlobalData.enemies_alive += 1
		if enemy.health and enemy.health is HealthComponent:
			enemy.health.died.connect(on_enemy_died)

func get_spawn_position(spawn_distance: float):
	var player : Player = GlobalData.get_player()
	if player:
		var player_pos : Vector2 = player.global_position
		var angle = randf() * 2 * PI
		var spawn_direction : Vector2 = polar_to_cartesian(angle, spawn_distance)
		return player_pos + spawn_direction
	return Vector2.ZERO

func polar_to_cartesian(angle : float, length : float) -> Vector2:
	# sin(angle) = y/h => y = h * sin(angle)
	var y = length * sin(angle)
	# cos(angle) = x/h => x = h * cos(angle)
	var x = length * cos(angle)
	return Vector2(x, y)

func on_enemy_died():
	GlobalData.enemies_alive -= 1
	GlobalData.enemies_dead += 1
