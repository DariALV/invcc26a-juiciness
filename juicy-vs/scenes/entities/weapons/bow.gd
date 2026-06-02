extends Node2D
class_name Bow

@export var arrow_scene : PackedScene:
	set(value):
		arrow_scene = value
		if shoot_component:
			shoot_component.projectile_scene = value
@export var shoot_speed : float = 1:
	set(value):
		shoot_speed = value
		if shoot_component:
			shoot_component.shoot_speed = value
@export var arrow_count : int = 1:
	set(value):
		arrow_count = value
		if shoot_component:
			shoot_component.projectile_count = value
@export var arrow_speed: float = 100:
	set(value):
		arrow_speed = value
		if shoot_component:
			shoot_component.projectile_speed = value
@export var target_group : String = "enemy":
	set(value):
		target_group = value
		if shoot_component:
			shoot_component.target_group = value

@onready var shoot_component : ShootComponent = $ShootComponent

var shoot_direction: Vector2:
	set(value):
		shoot_direction = value
		if shoot_component:
			shoot_component.shoot_direction = value

func _ready():
	shoot_component.projectile_scene = arrow_scene
	shoot_component.shoot_speed = shoot_speed
	shoot_component.projectile_count = arrow_count
	shoot_component.projectile_speed = arrow_speed
	shoot_component.target_group = target_group
	shoot_component.shoot_direction = shoot_direction
	shoot_component.start()
