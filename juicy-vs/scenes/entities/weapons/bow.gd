extends Node2D
class_name Bow

@export var arrow_scene : PackedScene
@export var shoot_speed : float = 1
@export var arrow_count : int = 1
@export var arrow_speed: float = 100
@export var target_group : String = "enemy"

@onready var shoot_timer : Timer = $ShootTimer

var shoot_direction: Vector2

var shooting_circle: Circle = Circle.new()

func _ready():
	shoot_timer.wait_time = 1.0/shoot_speed
	shoot_timer.timeout.connect(spawn_arrows)

func spawn_arrows():
	var directions = shooting_circle.spaced_points_in_edge(arrow_count, shoot_direction.angle())
	for dir in directions:
		var arrow : Arrow = arrow_scene.instantiate() as Arrow
		arrow.target_group = target_group
		get_parent().add_child(arrow)
		arrow.global_position = global_position
		arrow.rotation = dir.angle()
		arrow.movement_component.max_speed = arrow_speed
		arrow.movement_component.current_speed =  dir * arrow.movement_component.max_speed
		if target_group == "enemy":
			UpgradeManager.apply_projectile_upgrades(arrow)
