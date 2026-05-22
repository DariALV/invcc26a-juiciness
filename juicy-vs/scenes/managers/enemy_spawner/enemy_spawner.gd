extends Node

@export var enemy_scene : PackedScene
@export var spawn_cooldown : float = 1
@export var spawn_distance : float = 50

@onready var timer : Timer = $Timer

func _ready():
	timer.wait_time = spawn_cooldown
	timer.timeout.connect(spawn_slime)

func spawn_slime():
	var enemy : Enemy = enemy_scene.instantiate() as Enemy
	enemy.global_position = get_spawn_position()
	get_parent().add_child(enemy)

func get_spawn_position():
	var player : Player = get_tree().get_first_node_in_group("player") as Player
	var player_pos : Vector2 = player.global_position
	var angle = randf() * 2 * PI
	var spawn_direction : Vector2 = polar_to_cartesian(angle, spawn_distance)
	return player_pos + spawn_direction

#TODO: Aislar a clase global
func polar_to_cartesian(angle : float, length : float) -> Vector2:
	# sin(angle) = y/h => y = h * sin(angle)
	var y = length * sin(angle)
	# cos(angle) = x/h => x = h * cos(angle)
	var x = length * cos(angle)
	return Vector2(x, y)
	
