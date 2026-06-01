class_name Wave extends Resource

@export var enemies: Array[WaveEnemy] = []
@export var min_enemies: int = 0

var total_enemies = 0

func is_wave_completed() -> bool:
	for enemy in enemies:
		if enemy.can_spawn():
			return false
	return total_enemies < min_enemies

func spawn_random_enemy(parent_node: Node2D, spawn_distance: float):
	var enemy: WaveEnemy = enemies.pick_random()
	var tries: int = 100
	while tries > 0:
		if enemy.can_spawn():
			enemy.spawn(parent_node, spawn_distance)
			total_enemies += 1
			break
		else:
			enemy = enemies.pick_random()
			tries -= 1
			
