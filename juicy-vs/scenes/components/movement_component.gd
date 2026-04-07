extends Node
class_name MovementComponent

@export var speed : float = 0
@export var max_speed : float = 0

func calculate_velocity(direction : Vector2) -> Vector2:
	return direction.normalized() * speed

func increase_speed(amount : float):
	speed = min(max_speed, speed + amount);
