class_name EnemySpawner extends Node

signal waves_completed

@export var node_spawn_position : Node2D = null

@export var waves: Array[Wave]

@export var enemy_scene : PackedScene
@export var spawn_cooldown : float = 1
@export var spawn_distance : float = 50

@onready var timer : Timer = $Timer

var current_wave: int = 0

func _ready():
	timer.wait_time = spawn_cooldown
	timer.timeout.connect(spawn_enemy_from_current_wave)

func spawn_enemy_from_current_wave():
	if not waves.is_empty() and node_spawn_position:
		waves[current_wave].spawn_random_enemy(node_spawn_position, spawn_distance)
		if waves[current_wave].is_wave_completed():
			go_to_next_wave()

func go_to_next_wave():
	if current_wave < waves.size():
		current_wave += 1
	else:
		waves_completed.emit()
