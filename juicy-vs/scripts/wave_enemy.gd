class_name WaveEnemy extends Resource

## Enemy scene instanced for this entry.
@export var scene: PackedScene
## Total amount of this enemy spawned across the whole wave.
@export var amount: int = 0
## OBSOLETO: la vida ahora se escala con el multiplicador GLOBAL por oleada
## (GlobalData.wave_health_multiplier). Se conserva por compatibilidad con los .tres
## y el menu de debug, pero ya no se aplica.
@export var health_multiplier: float = 1.0
## Multiplica la velocidad de movimiento del enemigo al spawnear. 1.0 = sin cambio.
@export var move_speed_multiplier: float = 1.0
## Pick weight in the random draw. A weight of 3 spawns 3x as often as a weight of 1.
@export var weight: float = 1.0
## Start of the wave-progress window (0..1) in which this type may spawn.
@export_range(0.0, 1.0) var spawn_window_start: float = 0.0
## End of the wave-progress window (0..1). e.g. start 0.8 / end 1.0 spawns only in the
## last 20% of the wave, handy for tanks or a boss.
@export_range(0.0, 1.0) var spawn_window_end: float = 1.0

var spawned_count: int = 0

func reset() -> void:
	spawned_count = 0

func can_spawn() -> bool:
	return spawned_count < amount

func is_in_window(t: float) -> bool:
	return t >= spawn_window_start and t <= spawn_window_end

func spawn(parent_node: Node2D, spawn_distance: float) -> void:
	if not can_spawn():
		return
	spawned_count += 1
	var enemy = scene.instantiate() as CharacterBody2D
	enemy.global_position = get_spawn_position(spawn_distance)
	parent_node.add_child(enemy)
	# Solo contamos los enemigos de la OLEADA (no los invocados por un SpawnerComponent,
	# p. ej. los zombies de los necromantes, que no forman parte de la oleada).
	GlobalData.enemies_alive += 1
	if enemy.health and enemy.health is HealthComponent:
		# Multiplicador GLOBAL de vida de la oleada (reemplaza al por-entidad).
		var hp_mult: float = GlobalData.wave_health_multiplier
		if hp_mult != 1.0:
			enemy.health.max_health *= hp_mult
			enemy.health.current_health *= hp_mult
		enemy.health.died.connect(on_enemy_died)
	if move_speed_multiplier != 1.0:
		var mv = enemy.get_node_or_null("MovementComponent")
		if mv:
			mv.max_speed *= move_speed_multiplier

func get_spawn_position(spawn_distance: float):
	var player : Player = LevelManager.player
	if player:
		var player_pos : Vector2 = player.global_position
		var angle = randf() * 2 * PI
		var spawn_direction : Vector2 = polar_to_cartesian(angle, spawn_distance)
		return player_pos + spawn_direction
	return Vector2.ZERO

func polar_to_cartesian(angle : float, length : float) -> Vector2:
	var y = length * sin(angle)
	var x = length * cos(angle)
	return Vector2(x, y)

func on_enemy_died():
	GlobalData.enemies_alive -= 1
	GlobalData.enemies_dead += 1
